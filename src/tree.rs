use crate::escape::{UnescapedRef, UnescapedRoute};
use crate::{InsertError, MatchError, Params};

use std::cell::UnsafeCell;
use std::cmp::min;
use std::collections::VecDeque;
use std::ops::Range;
use std::{fmt, mem};

/// A radix tree used for URL path matching.
///
/// See [the crate documentation](crate) for details.
pub struct Node<T> {
    // This node's prefix.
    pub(crate) prefix: UnescapedRoute,

    // The priority of this node.
    //
    // Nodes with more children are higher priority and searched first.
    pub(crate) priority: u32,

    // Whether this node contains a wildcard child.
    pub(crate) wild_child: bool,

    // The first character of any static children, for fast linear search.
    pub(crate) indices: Vec<u8>,

    // The type of this node.
    pub(crate) node_type: NodeType,

    // The children of this node.
    pub(crate) children: Vec<Node<T>>,

    // The value stored at this node.
    //
    // See `Node::at` for why an `UnsafeCell` is necessary.
    value: Option<UnsafeCell<T>>,

    // A parameter name remapping, stored at nodes that hold values.
    pub(crate) remapping: ParamRemapping,
}

/// The types of nodes a tree can hold.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub(crate) enum NodeType {
    /// The root path.
    Root,

    /// A route parameter, e.g. '/{id}'.
    ///
    /// The leaves of a parameter node are suffixes within
    /// the segment, i.e. before the next '/', sorted by length.
    /// This allows for a reverse linear search to determine the
    /// correct leaf. It would also be possible to use a reverse
    /// prefix-tree here, but is likely not worth the complexity.
    Param,

    /// A catch-all parameter, e.g. '/{*file}'.
    CatchAll,

    /// A static prefix, e.g. '/foo'.
    Static,
}

/// Safety: We expose `value` per Rust's usual borrowing rules, so we can just
/// delegate these traits.
unsafe impl<T: Send> Send for Node<T> {}
unsafe impl<T: Sync> Sync for Node<T> {}

impl<T> Node<T> {
    // Insert a route into the tree.
    pub fn insert(&mut self, route: String, val: T) -> Result<(), InsertError> {
        let route = UnescapedRoute::new(route.into_bytes());
        let (route, remapping) = normalize_params(route)?;
        let mut remaining = route.as_ref();

        self.priority += 1;

        // If the tree is empty, insert the root node.
        if self.prefix.is_empty() && self.children.is_empty() {
            let last = self.insert_route(remaining, val)?;
            last.remapping = remapping;
            self.node_type = NodeType::Root;
            return Ok(());
        }

        let mut current = self;
        'walk: loop {
            // Find the common prefix between the route and the current node.
            let len = min(remaining.len(), current.prefix.len());
            let common_prefix = (0..len)
                .find(|&i| {
                    remaining[i] != current.prefix[i]
                    // Make sure not confuse the start of a wildcard with an escaped `{`.
                        || remaining.is_escaped(i) != current.prefix.is_escaped(i)
                })
                .unwrap_or(len);

            // If this node has a longer prefix than we need, we have to fork and extract the
            // common prefix into a shared parent.
            if current.prefix.len() > common_prefix {
                // Move the non-matching suffix into a child node.
                let suffix = current.prefix.as_ref().slice_off(common_prefix).to_owned();
                let child = Node {
                    prefix: suffix,
                    value: current.value.take(),
                    indices: current.indices.clone(),
                    wild_child: current.wild_child,
                    children: mem::take(&mut current.children),
                    remapping: mem::take(&mut current.remapping),
                    priority: current.priority - 1,
                    node_type: NodeType::Static,
                };

                // The current node now only holds the common prefix.
                current.children = vec![child];
                current.indices = vec![current.prefix[common_prefix]];
                current.prefix = current
                    .prefix
                    .as_ref()
                    .slice_until(common_prefix)
                    .to_owned();
                current.wild_child = false;
                continue;
            }

            if remaining.len() == common_prefix {
                // This node must not already contain a value.
                if current.value.is_some() {
                    return Err(InsertError::conflict(&route, remaining, current));
                }

                // Insert the value.
                current.value = Some(UnsafeCell::new(val));
                current.remapping = remapping;
                return Ok(());
            }

            // Otherwise, the route has a remaining non-matching suffix.
            //
            // We have to search deeper.
            remaining = remaining.slice_off(common_prefix);
            let next = remaining[0];

            // For parameters with a suffix, we have to find the matching suffix or
            // create a new child node.
            if current.node_type == NodeType::Param {
                let terminator = remaining
                    .iter()
                    .position(|&b| b == b'/')
                    // Include the '/' in the suffix.
                    .map(|b| b + 1)
                    .unwrap_or(remaining.len());

                let suffix = remaining.slice_until(terminator);

                for (i, child) in current.children.iter().enumerate() {
                    // Find a matching suffix.
                    if *child.prefix == **suffix {
                        current = &mut current.children[i];
                        current.priority += 1;
                        continue 'walk;
                    }
                }

                // Multiple parameters within the same segment, e.g. `/{foo}{bar}`.
                if matches!(find_wildcard(suffix), Ok(Some(_))) {
                    return Err(InsertError::InvalidParamSegment);
                }

                // If there is no matching suffix, create a new suffix node.
                let child = Node {
                    prefix: suffix.to_owned(),
                    node_type: NodeType::Static,
                    priority: 1,
                    ..Node::default()
                };

                let child = current.add_suffix_child(child);
                current = &mut current.children[child];

                // If this is the final route segment, insert the value.
                if terminator == remaining.len() {
                    current.value = Some(UnsafeCell::new(val));
                    current.remapping = remapping;
                    return Ok(());
                }

                // Otherwise, the previous node will hold only the suffix and we
                // need to create a new child for the remaining route.
                remaining = remaining.slice_off(terminator);

                // Create a static node unless we are inserting a parameter.
                if remaining[0] != b'{' || remaining.is_escaped(0) {
                    let child = current.add_child(Node {
                        node_type: NodeType::Static,
                        priority: 1,
                        ..Node::default()
                    });
                    current.indices.push(remaining[0]);
                    current = &mut current.children[child];
                }

                // Insert the remaining route.
                let last = current.insert_route(remaining, val)?;
                last.remapping = remapping;
                return Ok(());
            }

            // Find a child node that matches the next character in the route.
            for mut i in 0..current.indices.len() {
                if next == current.indices[i] {
                    // Make sure not confuse the start of a wildcard with an escaped `{` or `}`.
                    if matches!(next, b'{' | b'}') && !remaining.is_escaped(0) {
                        continue;
                    }

                    // Continue searching in the child.
                    i = current.update_child_priority(i);
                    current = &mut current.children[i];
                    continue 'walk;
                }
            }

            // We couldn't find a matching child.
            //
            // If we're not inserting a wildcard we have to create a static child.
            if (next != b'{' || remaining.is_escaped(0)) && current.node_type != NodeType::CatchAll
            {
                current.indices.push(next);
                let child = current.add_child(Node::default());
                let child = current.update_child_priority(child);

                // Insert into the newly created node.
                let last = current.children[child].insert_route(remaining, val)?;
                last.remapping = remapping;
                return Ok(());
            }

            // We're trying to insert a wildcard.
            //
            // If this node already has a wildcard child, we have to make sure it matches.
            if current.wild_child {
                // Wildcards are always the last child.
                current = current.children.last_mut().unwrap();
                current.priority += 1;

                // Make sure the route parameter matches.
                if let Some(wildcard) = remaining.get(..current.prefix.len()) {
                    if *wildcard != *current.prefix {
                        return Err(InsertError::conflict(&route, remaining, current));
                    }
                }

                // Catch-all routes cannot have children.
                if current.node_type == NodeType::CatchAll {
                    return Err(InsertError::conflict(&route, remaining, current));
                }

                // Continue with the wildcard node.
                continue 'walk;
            }

            // Otherwise, create a new node for the wildcard and insert the route.
            let last = current.insert_route(remaining, val)?;
            last.remapping = remapping;
            return Ok(());
        }
    }

    /// Removes a route from the tree, returning the value if the route already existed.
    ///
    /// The provided path should be the same as the one used to insert the route, including
    /// wildcards.
    pub fn remove(&mut self, route: String) -> Option<T> {
        let route = UnescapedRoute::new(route.into_bytes());
        let (route, remapping) = normalize_params(route).ok()?;
        let mut remaining = route.unescaped();

        // Check if we are removing the root node.
        if remaining == self.prefix.unescaped() {
            let value = self.value.take().map(UnsafeCell::into_inner);

            // If the root node has no children, we can reset it.
            if self.children.is_empty() {
                *self = Node::default();
            }

            return value;
        }

        let mut current = self;
        'walk: loop {
            // The path is longer than this node's prefix, search deeper.
            if remaining.len() > current.prefix.len() {
                let (prefix, rest) = remaining.split_at(current.prefix.len());

                // The prefix matches.
                if prefix == current.prefix.unescaped() {
                    let first = rest[0];
                    remaining = rest;

                    // If there is a single child node, we can continue searching in the child.
                    if current.children.len() == 1 {
                        // The route matches, remove the node.
                        if current.children[0].prefix.unescaped() == remaining {
                            return current.remove_child(0, &remapping);
                        }

                        // Otherwise, continue searching.
                        current = &mut current.children[0];
                        continue 'walk;
                    }

                    // Find a child node that matches the next character in the route.
                    if let Some(i) = current.indices.iter().position(|&c| c == first) {
                        // The route matches, remove the node.
                        if current.children[i].prefix.unescaped() == remaining {
                            return current.remove_child(i, &remapping);
                        }

                        // Otherwise, continue searching.
                        current = &mut current.children[i];
                        continue 'walk;
                    }

                    // If the node has a matching wildcard child, continue searching in the child.
                    if current.wild_child
                        && remaining.first().zip(remaining.get(2)) == Some((&b'{', &b'}'))
                    {
                        // The route matches, remove the node.
                        if current.children.last_mut().unwrap().prefix.unescaped() == remaining {
                            return current.remove_child(current.children.len() - 1, &remapping);
                        }

                        current = current.children.last_mut().unwrap();
                        continue 'walk;
                    }
                }
            }

            // Could not find a match.
            return None;
        }
    }

    /// Remove the child node at the given index, if the route parameters match.
    fn remove_child(&mut self, i: usize, remapping: &ParamRemapping) -> Option<T> {
        // Require an exact match to remove a route.
        //
        // For example, `/{a}` cannot be used to remove `/{b}`.
        if self.children[i].remapping != *remapping {
            return None;
        }

        // If the node does not have any children, we can remove it completely.
        let value = if self.children[i].children.is_empty() {
            // Removing a single child with no indices.
            if self.children.len() == 1 && self.indices.is_empty() {
                self.wild_child = false;
                self.children.remove(0).value.take()
            } else {
                // Remove the child node.
                let child = self.children.remove(i);

                match child.node_type {
                    // Remove the index if we removed a static prefix.
                    NodeType::Static => {
                        self.indices.remove(i);
                    }
                    // Otherwise, we removed a wildcard.
                    _ => self.wild_child = false,
                }

                child.value
            }
        }
        // Otherwise, remove the value but preserve the node.
        else {
            self.children[i].value.take()
        };

        value.map(UnsafeCell::into_inner)
    }

    // Adds a child to this node, keeping wildcards at the end.
    fn add_child(&mut self, child: Node<T>) -> usize {
        let len = self.children.len();

        if self.wild_child && len > 0 {
            self.children.insert(len - 1, child);
            len - 1
        } else {
            self.children.push(child);
            len
        }
    }

    // Adds a suffix child to this node, keeping suffixes sorted by ascending length.
    fn add_suffix_child(&mut self, child: Node<T>) -> usize {
        let i = self
            .children
            .partition_point(|node| node.prefix.len() >= child.prefix.len());
        self.children.insert(i, child);
        i
    }

    // Increments priority of the given child node, reordering the children if necessary.
    //
    // Returns the new index of the node.
    fn update_child_priority(&mut self, i: usize) -> usize {
        self.children[i].priority += 1;
        let priority = self.children[i].priority;

        // Move the node to the front as necessary.
        let mut updated = i;
        while updated > 0 && self.children[updated - 1].priority < priority {
            self.children.swap(updated - 1, updated);
            updated -= 1;
        }

        // Update the position of the indices to match.
        if updated != i {
            self.indices[updated..=i].rotate_right(1);
        }

        updated
    }

    // Insert a route at this node.
    //
    // If the route starts with a wildcard, a child node will be created for the parameter
    // and `wild_child` will be set on the parent.
    fn insert_route(
        &mut self,
        mut prefix: UnescapedRef<'_>,
        val: T,
    ) -> Result<&mut Node<T>, InsertError> {
        let mut current = self;

        loop {
            // Search for a wildcard segment.
            let Some(wildcard) = find_wildcard(prefix)? else {
                // There is no wildcard, simply insert into the current node.
                current.value = Some(UnsafeCell::new(val));
                current.prefix = prefix.to_owned();
                return Ok(current);
            };

            // Insering a catch-all route.
            if prefix[wildcard.clone()][1] == b'*' {
                // Ensure there is no suffix after the parameter, e.g. `/foo/{*x}/bar`.
                if wildcard.end != prefix.len() {
                    return Err(InsertError::InvalidCatchAll);
                }

                // Add the prefix before the wildcard into the current node.
                if wildcard.start > 0 {
                    current.prefix = prefix.slice_until(wildcard.start).to_owned();
                    prefix = prefix.slice_off(wildcard.start);
                }

                // Add the catch-all as a child node.
                let child = current.add_child(Node {
                    prefix: prefix.to_owned(),
                    node_type: NodeType::CatchAll,
                    value: Some(UnsafeCell::new(val)),
                    priority: 1,
                    ..Node::default()
                });
                current.wild_child = true;
                return Ok(&mut current.children[child]);
            }

            // Otherwise, we're inserting a regular route parameter.
            assert_eq!(prefix[wildcard.clone()][0], b'{');

            // Add the prefix before the wildcard into the current node.
            if wildcard.start > 0 {
                current.prefix = prefix.slice_until(wildcard.start).to_owned();
                prefix = prefix.slice_off(wildcard.start);
            }

            // Find the end of this route segment.
            let terminator = prefix
                .iter()
                .position(|&b| b == b'/')
                // Include the '/' in the suffix.
                .map(|b| b + 1)
                .unwrap_or(prefix.len());

            let wildcard = prefix.slice_until(wildcard.len());
            let suffix = prefix.slice_until(terminator).slice_off(wildcard.len());
            prefix = prefix.slice_off(terminator);

            // Multiple parameters within the same segment, e.g. `/{foo}{bar}`.
            if matches!(find_wildcard(suffix), Ok(Some(_))) {
                return Err(InsertError::InvalidParamSegment);
            }

            // Add the parameter as a child node.
            let child = current.add_child(Node {
                priority: 1,
                node_type: NodeType::Param,
                prefix: wildcard.to_owned(),
                ..Node::default()
            });

            current.wild_child = true;
            current = &mut current.children[child];

            // Add the static suffix before the '/', if there is one.
            if !suffix.is_empty() {
                let child = current.add_suffix_child(Node {
                    priority: 1,
                    node_type: NodeType::Static,
                    prefix: suffix.to_owned(),
                    ..Node::default()
                });

                current = &mut current.children[child];
            }

            // If the route ends here, insert the value.
            if prefix.is_empty() {
                current.value = Some(UnsafeCell::new(val));
                return Ok(current);
            }

            // If there is a static segment after the '/', setup the node
            // for the rest of the route.
            if prefix[0] != b'{' || prefix.is_escaped(0) {
                assert!(prefix[0] != b'{');
                current.indices.push(prefix[0]);
                let child = current.add_child(Node {
                    priority: 1,
                    ..Node::default()
                });
                current = &mut current.children[child];
            }
        }
    }
}

/// A wildcard node that was skipped during a tree search.
///
/// Contains the state necessary to backtrack to the given node.
struct Skipped<'node, 'path, T> {
    // The node that was skipped.
    node: &'node Node<T>,

    /// The path at the time we skipped this node.
    path: &'path [u8],

    // The number of parameters that were present.
    params: usize,
}

#[rustfmt::skip]
macro_rules! backtracker {
    ($skipped:ident, $path:ident, $node:ident, $params:ident, $backtracking:ident, $walk:lifetime) => {
        macro_rules! try_backtrack {
            () => {
                // Try backtracking to any matching wildcard nodes that we skipped while
                // traversing the tree.
                while let Some(skipped) = $skipped.pop() {
                    if skipped.path.ends_with($path) {
                        // Restore the search state.
                        $path = skipped.path;
                        $node = skipped.node;
                        $params.truncate(skipped.params);
                        $backtracking = true;
                        continue $walk;
                    }
                }
            };
        }
    };
}

impl<T> Node<T> {
    // Returns the node matching the given path.
    //
    // Returning an `UnsafeCell` allows us to avoid duplicating the logic between `Node::at` and
    // `Node::at_mut`, as Rust doesn't have a great way of abstracting over mutability.
    pub fn at<'node, 'path>(
        &'node self,
        mut path: &'path [u8],
    ) -> Result<(&'node UnsafeCell<T>, Params<'node, 'path>), MatchError> {
        let mut node = self;
        let mut backtracking = false;
        let mut params = Params::new();
        let mut skipped: Vec<Skipped<'_, '_, T>> = Vec::new();

        'walk: loop {
            // Initialize the backtracker.
            backtracker!(skipped, path, node, params, backtracking, 'walk);

            // Reached the end of the
            if path.len() <= node.prefix.len() {
                // Check for an exact match.
                if *path == *node.prefix {
                    // Found the matching value.
                    if let Some(ref value) = node.value {
                        // Remap the keys of any route parameters we accumulated during the
                        params.for_each_key_mut(|(i, key)| *key = &node.remapping[i]);
                        return Ok((value, params));
                    }
                }

                try_backtrack!();
                return Err(MatchError::NotFound);
            }

            // Otherwise, the path is longer than this node's prefix, search deeper.
            let (prefix, rest) = path.split_at(node.prefix.len());

            // The prefix does not match.
            if *prefix != *node.prefix {
                try_backtrack!();
                return Err(MatchError::NotFound);
            }

            let previous = path;
            path = rest;

            // If we are currently backtracking, avoid searching static children
            // that we already searched.
            if !backtracking {
                let next = path[0];

                // Find a child node that matches the next character in the path.
                if let Some(i) = node.indices.iter().position(|&c| c == next) {
                    // Keep track of wildcard routes that we skip.
                    //
                    // We may end up needing to backtrack later in case we do not find a
                    // match.
                    if node.wild_child {
                        skipped.push(Skipped {
                            node,
                            path: previous,
                            params: params.len(),
                        });
                    }

                    // Continue searching.
                    node = &node.children[i];
                    continue 'walk;
                }
            }

            // We didn't find a matching static child.
            //
            // If there are no wildcards, then there are no matching routes in the tree.
            if !node.wild_child {
                try_backtrack!();
                return Err(MatchError::NotFound);
            }

            // Continue searching in the wildcard child, which is kept at the end of the list.
            node = node.children.last().unwrap();
            match node.node_type {
                NodeType::Param => {
                    // Check for more path segments.
                    let slash = path.iter().position(|&c| c == b'/');
                    let terminator = match slash {
                        // Double `//` implying an empty parameter, no match.
                        Some(0) => {
                            try_backtrack!();
                            return Err(MatchError::NotFound);
                        }

                        // Found another segment.
                        Some(i) => i + 1,

                        // This is the last path segment.
                        None => path.len(),
                    };

                    for child in node.children.iter() {
                        // Ensure there is a possible match with a non-zero suffix.
                        if child.prefix.len() >= terminator {
                            continue;
                        }

                        let suffix_start = terminator - child.prefix.len();
                        let (param, suffix) = path[..terminator].split_at(suffix_start);

                        // Continue searching if the suffix matches.
                        if *suffix == *child.prefix {
                            node = child;
                            path = &path[suffix_start..];
                            backtracking = false;
                            params.push(b"", param);
                            continue 'walk;
                        }
                    }

                    // If this is the last path segment and there is a matching
                    // value without a suffix, we have a match.
                    let value = match node.value {
                        // Found the matching value.
                        Some(ref value) if slash.is_none() => value,

                        _ => {
                            try_backtrack!();
                            return Err(MatchError::NotFound);
                        }
                    };

                    // Store the parameter value.
                    // Parameters are normalized so the key is irrelevant for now.
                    params.push(b"", path);

                    // Remap the keys of any route parameters we accumulated during the
                    params.for_each_key_mut(|(i, key)| *key = &node.remapping[i]);

                    return Ok((value, params));
                }

                NodeType::CatchAll => {
                    // Catch-all segments are only allowed at the end of the route, meaning
                    // this node must contain the value.
                    let value = match node.value {
                        // Found the matching value.
                        Some(ref value) => value,
                        // Otherwise, there are no matching routes in the tree.
                        None => return Err(MatchError::NotFound),
                    };

                    // Remap the keys of any route parameters we accumulated during the
                    params.for_each_key_mut(|(i, key)| *key = &node.remapping[i]);

                    // Store the final catch-all parameter (`{*...}`).
                    let key = &node.prefix[2..node.prefix.len() - 1];
                    params.push(key, path);

                    return Ok((value, params));
                }

                _ => unreachable!(),
            }
        }
    }

    /// Test helper that ensures route priorities are consistent.
    #[cfg(feature = "__test_helpers")]
    pub fn check_priorities(&self) -> Result<u32, (u32, u32)> {
        let mut priority: u32 = 0;
        for child in &self.children {
            priority += child.check_priorities()?;
        }

        if self.value.is_some() {
            priority += 1;
        }

        if self.priority != priority {
            return Err((self.priority, priority));
        }

        Ok(priority)
    }
}

impl<T> Node<T> {
    /// Iterates over the tree and calls the given visitor function
    /// with fully resolved path and its value.
    pub fn for_each<V: FnMut(String, T)>(self, mut visitor: V) {
        let mut queue = VecDeque::from([(self.prefix.clone(), self)]);

        // Perform a BFS on the routing tree.
        while let Some((mut prefix, mut node)) = queue.pop_front() {
            denormalize_params(&mut prefix, &node.remapping);

            if let Some(value) = node.value.take() {
                let path = String::from_utf8(prefix.unescaped().to_vec()).unwrap();
                visitor(path, value.into_inner());
            }

            // Traverse the child nodes.
            for child in node.children {
                let mut prefix = prefix.clone();
                prefix.append(&child.prefix);
                queue.push_back((prefix, child));
            }
        }
    }
}

/// An ordered list of route parameters keys for a specific route.
///
/// To support conflicting routes like `/{a}/foo` and `/{b}/bar`, route parameters
/// are normalized before being inserted into the tree. Parameter remapping are
/// stored at nodes containing values, containing the "true" names of all route parameters
/// for the given route.
type ParamRemapping = Vec<Vec<u8>>;

/// Returns `path` with normalized route parameters, and a parameter remapping
/// to store at the node for this route.
///
/// Note that the parameter remapping may contain unescaped characters.
fn normalize_params(
    mut path: UnescapedRoute,
) -> Result<(UnescapedRoute, ParamRemapping), InsertError> {
    let mut start = 0;
    let mut original = ParamRemapping::new();

    // Parameter names are normalized alphabetically.
    let mut next = b'a';

    loop {
        // Find a wildcard to normalize.
        let mut wildcard = match find_wildcard(path.as_ref().slice_off(start))? {
            Some(wildcard) => wildcard,
            // No wildcard, we are done.
            None => return Ok((path, original)),
        };

        wildcard.start += start;
        wildcard.end += start;

        // Ensure the parameter has a valid name.
        if wildcard.len() < 2 {
            return Err(InsertError::InvalidParam);
        }

        // We don't need to normalize catch-all parameters, as they are always
        // at the end of a route.
        if path[wildcard.clone()][1] == b'*' {
            start = wildcard.end;
            continue;
        }

        // Normalize the parameter.
        let removed = path.splice(wildcard.clone(), vec![b'{', next, b'}']);

        // Preserve the original name for remapping.
        let mut removed = removed.skip(1).collect::<Vec<_>>();
        removed.pop();
        original.push(removed);

        next += 1;
        if next > b'z' {
            panic!("Too many route parameters.");
        }

        // Continue the search after the parameter we just normalized.
        start = wildcard.start + 3;
    }
}

/// Restores `route` to it's original, denormalized form.
pub(crate) fn denormalize_params(route: &mut UnescapedRoute, params: &ParamRemapping) {
    let mut start = 0;
    let mut i = 0;

    loop {
        // Find a wildcard to denormalize.
        let mut wildcard = match find_wildcard(route.as_ref().slice_off(start)).unwrap() {
            Some(w) => w,
            None => return,
        };

        wildcard.start += start;
        wildcard.end += start;

        // Get the corresponding parameter remapping.
        let mut next = match params.get(i) {
            Some(param) => param.clone(),
            None => return,
        };

        // Denormalize this parameter.
        next.insert(0, b'{');
        next.push(b'}');
        let _ = route.splice(wildcard.clone(), next.clone());

        i += 1;
        start = wildcard.start + next.len();
    }
}

// Searches for a wildcard segment and checks the path for invalid characters.
fn find_wildcard(path: UnescapedRef<'_>) -> Result<Option<Range<usize>>, InsertError> {
    for (start, &c) in path.iter().enumerate() {
        // Found an unescaped closing brace without a corresponding opening brace.
        if c == b'}' && !path.is_escaped(start) {
            return Err(InsertError::InvalidParam);
        }

        // Keep going until we find an unescaped opening brace.
        if c != b'{' || path.is_escaped(start) {
            continue;
        }

        // Ensure there is a non-empty parameter name.
        if path.get(start + 1) == Some(&b'}') {
            return Err(InsertError::InvalidParam);
        }

        // Find the corresponding closing brace.
        for (i, &c) in path.iter().enumerate().skip(start + 2) {
            match c {
                b'}' => {
                    // This closing brace was escaped, keep searching.
                    if path.is_escaped(i) {
                        continue;
                    }

                    // Ensure catch-all parameters have a non-empty name.
                    if path.get(i - 1) == Some(&b'*') {
                        return Err(InsertError::InvalidParam);
                    }

                    return Ok(Some(start..i + 1));
                }
                // `*` and `/` are invalid in parameter names.
                b'*' | b'/' => return Err(InsertError::InvalidParam),
                _ => {}
            }
        }

        // Missing closing brace.
        return Err(InsertError::InvalidParam);
    }

    Ok(None)
}

impl<T> Clone for Node<T>
where
    T: Clone,
{
    fn clone(&self) -> Node<T> {
        let value = self.value.as_ref().map(|value| {
            // Safety: We only expose `&mut T` through `&mut self`.
            let value = unsafe { &*value.get() };
            UnsafeCell::new(value.clone())
        });

        Node {
            value,
            prefix: self.prefix.clone(),
            wild_child: self.wild_child,
            node_type: self.node_type.clone(),
            indices: self.indices.clone(),
            children: self.children.clone(),
            remapping: self.remapping.clone(),
            priority: self.priority,
        }
    }
}

impl<T> Default for Node<T> {
    fn default() -> Node<T> {
        Node {
            remapping: ParamRemapping::new(),
            prefix: UnescapedRoute::default(),
            wild_child: false,
            node_type: NodeType::Static,
            indices: Vec::new(),
            children: Vec::new(),
            value: None,
            priority: 0,
        }
    }
}

impl<T> fmt::Debug for Node<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Safety: We only expose `&mut T` through `&mut self`.
        let value = unsafe { self.value.as_ref().map(|x| &*x.get()) };

        let mut f = f.debug_struct("Node");
        f.field("value", &value)
            .field("prefix", &self.prefix)
            .field("node_type", &self.node_type)
            .field("children", &self.children);

        // Extra information for debugging purposes.
        #[cfg(test)]
        {
            let indices = self
                .indices
                .iter()
                .map(|&x| char::from_u32(x as _))
                .collect::<Vec<_>>();

            let params = self
                .remapping
                .iter()
                .map(|x| std::str::from_utf8(x).unwrap())
                .collect::<Vec<_>>();

            f.field("indices", &indices).field("params", &params);
        }

        f.finish()
    }
}
