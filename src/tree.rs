use crate::{InsertError, MatchError, Params};

use std::cell::UnsafeCell;
use std::cmp::min;
use std::mem;
use std::str;

/// A successful match consisting of the registered value and the URL parameters, returned by
/// [`Node::at`](crate::Node::at).
#[derive(Debug)]
pub struct Match<'k, 'v, V> {
    /// The value stored under the matched node.
    pub value: V,
    /// The route parameters. See [parameters](crate#parameters) for more details.
    pub params: Params<'k, 'v>,
}

/// The types of nodes the tree can hold
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
enum NodeType {
    /// The root path
    Root,
    /// A URL parameter, ex: `/:id`. See `Param`
    Param,
    /// A wilcard parameter, ex: `/*static`
    CatchAll,
    /// Anything else
    Static,
}

/// A node in a radix tree ordered by priority.
///
/// Priority is just the number of values registered in sub nodes
/// (children, grandchildren, and so on..).
pub struct Node<T> {
    path: Vec<u8>,
    wild_child: bool,
    node_type: NodeType,
    indices: Vec<u8>,
    children: Vec<Self>,
    // See `at_inner` for why an unsafe cell is needed.
    value: Option<UnsafeCell<T>>,
    priority: u32,
}

impl<T> Default for Node<T> {
    fn default() -> Self {
        Self {
            path: Vec::new(),
            wild_child: false,
            node_type: NodeType::Static,
            indices: Vec::new(),
            children: Vec::new(),
            value: None,
            priority: 0,
        }
    }
}

impl<T> Node<T> {
    /// Construct a new `Node`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a value in the tree under the given route.
    /// ```rust
    /// # use matchit::Node;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/users/:id", "A User")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert(&mut self, route: impl Into<String>, val: T) -> Result<(), InsertError> {
        let route = route.into();

        self.priority += 1;

        // Empty tree
        if self.path.is_empty() && self.children.is_empty() {
            self.insert_child(route.as_bytes(), &route, val)?;
            self.node_type = NodeType::Root;
            return Ok(());
        }

        self.insert_helper(route.as_bytes(), &route, val)
    }

    #[inline]
    fn insert_helper(&mut self, mut prefix: &[u8], route: &str, val: T) -> Result<(), InsertError> {
        // Find the longest common prefix.
        // This also implies that the common prefix contains no ':' or '*'
        // since the existing key can't contain those chars.
        let mut i = 0;
        let max = min(prefix.len(), self.path.len());

        while i < max && prefix[i] == self.path[i] {
            i += 1;
        }

        // Split edge
        if i < self.path.len() {
            let mut child = Self {
                path: self.path[i..].to_owned(),
                wild_child: self.wild_child,
                indices: self.indices.to_owned(),
                value: self.value.take(),
                priority: self.priority - 1,
                ..Self::default()
            };

            mem::swap(&mut self.children, &mut child.children);

            self.children = vec![child];
            self.indices = self.path[i..=i].to_owned();
            self.path = prefix[..i].to_owned();
            self.wild_child = false;
        }

        // Make new node a child of this node
        if prefix.len() > i {
            prefix = &prefix[i..];

            if self.wild_child {
                return self.children[0].wild_child_conflict(prefix, route, val);
            }

            let idxc = prefix[0];

            // `/` after param
            if self.node_type == NodeType::Param && idxc == b'/' && self.children.len() == 1 {
                self.children[0].priority += 1;
                return self.children[0].insert_helper(prefix, route, val);
            }

            // Check if a child with the next path byte exists
            for mut i in 0..self.indices.len() {
                if idxc == self.indices[i] {
                    i = self.incr_child_priority(i);
                    return self.children[i].insert_helper(prefix, route, val);
                }
            }

            // Otherwise insert it
            if idxc != b':' && idxc != b'*' {
                self.indices.push(idxc);

                self.children.push(Self::default());

                let child = self.incr_child_priority(self.indices.len() - 1);
                return self.children[child].insert_child(prefix, route, val);
            }

            return self.insert_child(prefix, route, val);
        } else {
            // Otherwise add value to current node
            if self.value.is_some() {
                return Err(InsertError::conflict(route, &prefix, &self.path));
            }

            self.value = Some(UnsafeCell::new(val));
        }

        Ok(())
    }

    // Increments priority of the given child and reorders if necessary
    // returns the new position (index) of the child
    fn incr_child_priority(&mut self, pos: usize) -> usize {
        self.children[pos].priority += 1;
        let prio = self.children[pos].priority;
        // adjust position (move to front)
        let mut new_pos = pos;

        while new_pos > 0 && self.children[new_pos - 1].priority < prio {
            // swap node positions
            self.children.swap(new_pos - 1, new_pos);
            new_pos -= 1;
        }

        // build new index char string
        if new_pos != pos {
            self.indices = [
                &self.indices[..new_pos],    // unchanged prefix, might be empty
                &self.indices[pos..=pos],    // the index char we move
                &self.indices[new_pos..pos], // rest without char at 'pos'
                &self.indices[pos + 1..],
            ]
            .concat();
        }

        new_pos
    }

    #[inline]
    fn wild_child_conflict(
        &mut self,
        prefix: &[u8],
        route: &str,
        val: T,
    ) -> Result<(), InsertError> {
        self.priority += 1;

        // Check if the wildcard matches
        if prefix.len() >= self.path.len()
      && self.path == prefix[..self.path.len()]
      // Adding a child to a CatchAll Node is not possible
      && self.node_type != NodeType::CatchAll
      // Check for longer wildcard, e.g. :name and :names
      && (self.path.len() >= prefix.len() || prefix[self.path.len()] == b'/')
        {
            self.insert_helper(prefix, route, val)
        } else {
            Err(InsertError::conflict(&route, &prefix, &self.path))
        }
    }

    fn insert_child(&mut self, mut prefix: &[u8], route: &str, val: T) -> Result<(), InsertError> {
        let (wildcard, wildcard_index, valid) = find_wildcard(&prefix);

        if wildcard_index.is_none() {
            self.value = Some(UnsafeCell::new(val));
            self.path = prefix.to_owned();
            return Ok(());
        };

        let mut wildcard_index = wildcard_index.unwrap();
        let wildcard = wildcard.unwrap();

        // the wildcard name must not contain ':' and '*'
        if !valid {
            return Err(InsertError::TooManyParams);
        };

        // check if the wildcard has a name
        if wildcard.len() < 2 {
            return Err(InsertError::UnnamedParam);
        }

        // check if this Node existing children which would be
        // unreachable if we insert the wildcard here
        if !self.children.is_empty() {
            return Err(InsertError::conflict(&route, &prefix, &self.path));
        }

        // Param
        if wildcard[0] == b':' {
            // Insert prefix before the current wildcard
            if wildcard_index > 0 {
                self.path = prefix[..wildcard_index].to_owned();
                prefix = &prefix[wildcard_index..];
            }

            let child = Self {
                node_type: NodeType::Param,
                path: wildcard.to_owned(),
                ..Self::default()
            };

            self.wild_child = true;
            self.children = vec![child];
            self.children[0].priority += 1;

            // If the path doesn't end with the wildcard, then there
            // will be another non-wildcard subpath starting with '/'

            if wildcard.len() < prefix.len() {
                prefix = &prefix[wildcard.len()..];
                let child = Self {
                    priority: 1,
                    ..Self::default()
                };

                self.children[0].children = vec![child];
                return self.children[0].children[0].insert_child(prefix, route, val);
            }
            // Otherwise we're done. Insert the value in the new leaf
            self.children[0].value = Some(UnsafeCell::new(val));
            return Ok(());
        }

        // catch all
        if wildcard_index + wildcard.len() != prefix.len() {
            return Err(InsertError::InvalidCatchAll);
        }

        if !self.path.is_empty() && self.path[self.path.len() - 1] == b'/' {
            return Err(InsertError::conflict(&route, &prefix, &self.path));
        }

        // Currently fixed width 1 for '/'
        wildcard_index = wildcard_index
            .checked_sub(1)
            .ok_or(InsertError::MalformedPath)?;

        if prefix[wildcard_index] != b'/' {
            return Err(InsertError::InvalidCatchAll);
        }

        // first node: CatchAll Node with empty path
        let child = Self {
            wild_child: true,
            node_type: NodeType::CatchAll,
            ..Self::default()
        };

        self.path = prefix[..wildcard_index].to_owned();
        self.children = vec![child];
        self.indices = vec![b'/'];
        self.children[0].priority += 1;

        // Second node: node holding the variable
        let child = Self {
            path: prefix[wildcard_index..].to_owned(),
            node_type: NodeType::CatchAll,
            value: Some(UnsafeCell::new(val)),
            priority: 1,
            ..Self::default()
        };

        self.children[0].children = vec![child];

        Ok(())
    }

    /// Tries to find a value in the router matching the given path.
    /// If no value can be found it returns a trailing slash redirect recommendation, see [`tsr`](crate::MatchError::tsr).
    /// ```rust
    /// # use matchit::Node;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    ///
    /// let matched = matcher.at("/home").unwrap();
    /// assert_eq!(*matched.value, "Welcome!");
    /// # Ok(())
    /// # }
    /// ```
    pub fn at<'p>(&self, path: &'p str) -> Result<Match<'_, 'p, &T>, MatchError> {
        match self.at_inner(path.as_bytes()) {
            Ok(v) => Ok(Match {
                // SAFETY: We have an immutable reference to self, and we only
                // expose mutable references to value through &mut self, so we
                // can give safely expose an immmutable reference to value of
                // self's lifetime
                value: unsafe { &*v.value.get() },
                params: v.params,
            }),
            Err(e) => Err(e),
        }
    }

    /// Tries to find a value in the router matching the given path, and returns a mutable
    /// reference to it. If no value can be found it returns a trailing slash redirect
    /// recommendation, see [`tsr`](crate::MatchError::tsr).
    pub fn at_mut<'p>(&mut self, path: &'p str) -> Result<Match<'_, 'p, &mut T>, MatchError> {
        match self.at_inner(path.as_bytes()) {
            Ok(v) => Ok(Match {
                // SAFETY: We have a unique reference to self, so we can safely
                // expose a unique reference to value of self's lifetime
                value: unsafe { &mut *v.value.get() },
                params: v.params,
            }),
            Err(e) => Err(e),
        }
    }

    // It's a bit sad that we have to introduce unsafe here, but rust doesn't really have a way
    // to abstract over mutability, so it avoids having to duplicate logic between `at` and
    // `at_mut`.

    #[inline(always)]
    fn at_inner<'p>(&self, path: &'p [u8]) -> Result<Match<'_, 'p, &UnsafeCell<T>>, MatchError> {
        let mut current = self;
        let mut path = path;
        let mut params = Params::new();

        // outer loop for walking the tree to get a path's value
        'walk: loop {
            let prefix = &current.path;
            if path.len() > prefix.len() {
                if prefix == &path[..prefix.len()] {
                    path = &path[prefix.len()..];

                    // If this node does not have a wildcard (Param or CatchAll)
                    // child, we can just look up the next child node and continue
                    // to walk down the tree
                    if !current.wild_child {
                        let idxc = path[0];
                        if let Some(i) = current.indices.iter().position(|&c| c == idxc) {
                            current = &current.children[i];
                            continue 'walk;
                        }

                        // Nothing found.
                        // We can recommend to redirect to the same URL without a
                        // trailing slash if a leaf exists for that path.
                        let tsr = path == b"/" && current.value.is_some();
                        return Err(MatchError::new(tsr));
                    }

                    current = &current.children[0];
                    match current.node_type {
                        NodeType::Param => {
                            // find param end (either '/' or path end)
                            let end = path.iter().position(|&c| c == b'/').unwrap_or(path.len());

                            params.push(&current.path[1..], &path[..end]);

                            // we need to go deeper!
                            if end < path.len() {
                                // ... but we can't
                                if current.children.is_empty() {
                                    let tsr = path.len() == end + 1;
                                    return Err(MatchError::new(tsr));
                                }

                                path = &path[end..];
                                current = &current.children[0];
                                continue 'walk;
                            }

                            if let Some(value) = current.value.as_ref() {
                                return Ok(Match { value, params });
                            } else if current.children.len() == 1 {
                                current = &current.children[0];

                                // No value found. Check if a value for this path + a
                                // trailing slash exists for TSR recommendation
                                let tsr = (current.path == b"/" && current.value.is_some())
                                    || (current.path.is_empty() && current.indices == b"/");
                                return Err(MatchError::new(tsr));
                            }

                            return Err(MatchError::new(false));
                        }
                        NodeType::CatchAll => {
                            params.push(&current.path[2..], path);

                            return match current.value.as_ref() {
                                Some(value) => Ok(Match { value, params }),
                                None => Err(MatchError::new(false)),
                            };
                        }
                        _ => unreachable!(),
                    }
                }
            } else if path == prefix {
                // We should have reached the node containing the value.
                // Check if this node has a value registered.
                if let Some(value) = current.value.as_ref() {
                    return Ok(Match { value, params });
                }

                // If there is no value for this route, but this route has a
                // wildcard child, there must be a value for this path with an
                // additional trailing slash
                if path == b"/" && current.wild_child && current.node_type != NodeType::Root {
                    return Err(MatchError::new(true));
                }

                // No value found. Check if a value for this path + a
                // trailing slash exists for trailing slash recommendation
                if let Some(i) = current.indices.iter().position(|&c| c == b'/') {
                    current = &current.children[i];
                    let tsr = (current.path.len() == 1 && current.value.is_some())
                        || (current.node_type == NodeType::CatchAll
                            && current.children[0].value.is_some());
                    return Err(MatchError::new(tsr));
                }

                return Err(MatchError::new(false));
            }

            // Nothing found. We can recommend to redirect to the same URL with an
            // extra trailing slash if a leaf exists for that path
            let tsr = (path == b"/")
                || (prefix.len() == path.len() + 1
                    && prefix[path.len()] == b'/'
                    && path == &prefix[..prefix.len() - 1]
                    && current.value.is_some());

            return Err(MatchError::new(tsr));
        }
    }

    /// Makes a case-insensitive match of the given path and tries to find a handler.
    /// It can optionally also fix trailing slashes.
    /// If the match is successful, it returns the case corrected path.
    /// ```rust
    /// # use matchit::Node;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    ///
    /// let path = matcher.path_ignore_case("/HoMe/", true).unwrap();
    /// assert_eq!(path, "/home");
    /// # Ok(())
    /// # }
    /// ````
    pub fn path_ignore_case(
        &self,
        path: impl AsRef<str>,
        fix_trailing_slash: bool,
    ) -> Option<String> {
        let path = path.as_ref();
        let mut insensitive_path = Vec::with_capacity(path.len() + 1);
        let found = self.path_ignore_case_helper(
            path.as_bytes(),
            &mut insensitive_path,
            [0; 4],
            fix_trailing_slash,
        );
        if found {
            Some(String::from_utf8(insensitive_path).unwrap())
        } else {
            None
        }
    }

    fn path_ignore_case_helper(
        &self,
        mut path: &[u8],
        insensitive_path: &mut Vec<u8>,
        mut buf: [u8; 4],
        fix_trailing_slash: bool,
    ) -> bool {
        let lower_path: &[u8] = &path.to_ascii_lowercase();
        if lower_path.len() >= self.path.len()
            && (self.path.is_empty()
                || lower_path[1..self.path.len()].eq_ignore_ascii_case(&self.path[1..]))
        {
            insensitive_path.extend_from_slice(&self.path);

            path = &path[self.path.len()..];

            if !path.is_empty() {
                let cached_lower_path = <&[u8]>::clone(&lower_path);

                // If this node does not have a wildcard (param or catchAll) child,
                // we can just look up the next child node and continue to walk down
                // the tree
                if !self.wild_child {
                    // skip char bytes already processed
                    buf = shift_n_bytes(buf, self.path.len());

                    if buf[0] == 0 {
                        // process a new char
                        let mut current_char = 0 as char;

                        // find char start
                        // chars are up to 4 byte long,
                        // -4 would definitely be another char
                        let mut off = 0;
                        for j in 0..min(self.path.len(), 3) {
                            let i = self.path.len() - j;
                            if char_start(cached_lower_path[i]) {
                                // read char from cached path
                                current_char = str::from_utf8(&cached_lower_path[i..])
                                    .unwrap()
                                    .chars()
                                    .next()
                                    .unwrap();
                                off = j;
                                break;
                            }
                        }

                        current_char.encode_utf8(&mut buf);

                        // skip already processed bytes
                        buf = shift_n_bytes(buf, off);

                        for i in 0..self.indices.len() {
                            // lowercase matches
                            if self.indices[i] == buf[0] {
                                // must use a recursive approach since both the
                                // uppercase byte and the lowercase byte might exist
                                // as an index
                                if self.children[i].path_ignore_case_helper(
                                    path,
                                    insensitive_path,
                                    buf,
                                    fix_trailing_slash,
                                ) {
                                    return true;
                                }

                                if insensitive_path.len() > self.children[i].path.len() {
                                    let prev_len =
                                        insensitive_path.len() - self.children[i].path.len();
                                    insensitive_path.truncate(prev_len);
                                }

                                break;
                            }
                        }

                        // same for uppercase char, if it differs
                        let up = current_char.to_ascii_uppercase();
                        if up != current_char {
                            up.encode_utf8(&mut buf);
                            buf = shift_n_bytes(buf, off);

                            for i in 0..self.indices.len() {
                                if self.indices[i] == buf[0] {
                                    return self.children[i].path_ignore_case_helper(
                                        path,
                                        insensitive_path,
                                        buf,
                                        fix_trailing_slash,
                                    );
                                }
                            }
                        }
                    } else {
                        // old char not finished
                        for i in 0..self.indices.len() {
                            if self.indices[i] == buf[0] {
                                // continue with child node
                                return self.children[i].path_ignore_case_helper(
                                    path,
                                    insensitive_path,
                                    buf,
                                    fix_trailing_slash,
                                );
                            }
                        }
                    }

                    // Nothing found. We can recommend to redirect to the same URL
                    // without a trailing slash if a leaf exists for that path
                    return fix_trailing_slash && path == [b'/'] && self.value.is_some();
                }

                return self.children[0].path_ignore_case_match_helper(
                    path,
                    insensitive_path,
                    buf,
                    fix_trailing_slash,
                );
            } else {
                // We should have reached the node containing the value.
                // Check if this node has a value registered.
                if self.value.is_some() {
                    return true;
                }

                // No value found.
                // Try to fix the path by adding a trailing slash
                if fix_trailing_slash {
                    for i in 0..self.indices.len() {
                        if self.indices[i] == b'/' {
                            if (self.children[i].path.len() == 1
                                && self.children[i].value.is_some())
                                || (self.children[i].node_type == NodeType::CatchAll
                                    && self.children[i].children[0].value.is_some())
                            {
                                insensitive_path.push(b'/');
                                return true;
                            }
                            return false;
                        }
                    }
                }
                return false;
            }
        }

        // Nothing found.
        // Try to fix the path by adding / removing a trailing slash
        if fix_trailing_slash {
            if path == [b'/'] {
                return true;
            }
            if lower_path.len() + 1 == self.path.len()
                && self.path[lower_path.len()] == b'/'
                && lower_path[1..].eq_ignore_ascii_case(&self.path[1..lower_path.len()])
                && self.value.is_some()
            {
                insensitive_path.extend_from_slice(&self.path);
                return true;
            }
        }

        false
    }

    fn path_ignore_case_match_helper(
        &self,
        mut path: &[u8],
        insensitive_path: &mut Vec<u8>,
        buf: [u8; 4],
        fix_trailing_slash: bool,
    ) -> bool {
        match self.node_type {
            NodeType::Param => {
                let mut end = 0;

                while end < path.len() && path[end] != b'/' {
                    end += 1;
                }

                insensitive_path.extend_from_slice(&path[..end]);

                if end < path.len() {
                    if !self.children.is_empty() {
                        path = &path[end..];

                        return self.children[0].path_ignore_case_helper(
                            path,
                            insensitive_path,
                            buf,
                            fix_trailing_slash,
                        );
                    }

                    // ... but we can't
                    if fix_trailing_slash && path.len() == end + 1 {
                        return true;
                    }
                    return false;
                }

                if self.value.is_some() {
                    return true;
                } else if fix_trailing_slash
                    && self.children.len() == 1
                    && self.children[0].path == [b'/']
                    && self.children[0].value.is_some()
                {
                    // No value found. Check if a value for this path + a
                    // trailing slash exists
                    insensitive_path.push(b'/');
                    return true;
                }

                false
            }
            NodeType::CatchAll => {
                insensitive_path.extend_from_slice(path);
                true
            }
            _ => unreachable!(),
        }
    }
}

// Shift bytes in array by n bytes left
pub const fn shift_n_bytes(bytes: [u8; 4], n: usize) -> [u8; 4] {
    match u32::from_ne_bytes(bytes).overflowing_shr((n * 8) as u32) {
        (_, true) => [0; 4],
        (shifted, false) => shifted.to_ne_bytes(),
    }
}

// This function is ported from go.
// Reports whether the byte could be the first byte of an encoded,
// possibly invalid char. Second and subsequent bytes always have
// the top two bits set to 10.
const fn char_start(b: u8) -> bool {
    b & 0xC0 != 0x80
}

// Search for a wildcard segment and check the name for invalid characters.
fn find_wildcard(path: &[u8]) -> (Option<&[u8]>, Option<usize>, bool) {
    // Find start
    for (start, &c) in path.iter().enumerate() {
        // A wildcard starts with ':' (param) or '*' (catch-all)
        if c != b':' && c != b'*' {
            continue;
        };

        // Find end and check for invalid characters
        let mut valid = true;

        for (end, &c) in path[start + 1..].iter().enumerate() {
            match c {
                b'/' => return (Some(&path[start..start + 1 + end]), Some(start), valid),
                b':' | b'*' => valid = false,
                _ => (),
            };
        }
        return (Some(&path[start..]), Some(start), valid);
    }
    (None, None, false)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn params(vec: Vec<(&'static str, &'static str)>) -> Params<'static, 'static> {
        let mut params = Params::new();
        for (key, value) in vec {
            params.push(key.as_bytes(), value.as_bytes());
        }
        params
    }

    struct TestRequest {
        path: &'static str,
        should_be_nil: bool,
        route: &'static str,
        params: Params<'static, 'static>,
    }

    impl TestRequest {
        pub fn new(
            path: &'static str,
            should_be_nil: bool,
            route: &'static str,
            params: Params<'static, 'static>,
        ) -> TestRequest {
            TestRequest {
                path,
                should_be_nil,
                route,
                params,
            }
        }
    }

    type TestRequests = Vec<TestRequest>;

    fn check_requests(tree: &mut Node<String>, requests: TestRequests) {
        for request in requests {
            let res = tree.at(request.path);

            match res {
                Err(_) => {
                    if !request.should_be_nil {
                        panic!("Expected non-nil value for route '{}'", request.path);
                    }
                }
                Ok(result) => {
                    if request.should_be_nil {
                        panic!("Expected nil value for route '{}'", request.path);
                    }
                    let value = result.value;
                    if value != request.route {
                        panic!(
                            "Wrong value for route '{}'. Expected '{}', found '{}')",
                            request.path, value, request.route
                        );
                    }
                    assert_eq!(
                        result.params, request.params,
                        "Wrong params for route '{}'",
                        request.path
                    );

                    // test at_mut
                    let res_mut = tree.at_mut(request.path).unwrap();
                    res_mut.value.push_str("CHECKED");

                    let res = tree.at(request.path).unwrap();
                    assert!(res.value.contains("CHECKED"));

                    let res_mut = tree.at_mut(request.path).unwrap();
                    *res_mut.value = res_mut.value.replace("CHECKED", "");
                }
            };
        }
    }

    fn check_priorities(n: &mut Node<String>) -> u32 {
        let mut prio: u32 = 0;
        for i in 0..n.children.len() {
            prio += check_priorities(&mut n.children[i]);
        }

        if n.value.is_some() {
            prio += 1;
        }

        if n.priority != prio {
            panic!(
                "priority mismatch for node '{}': found '{}', expected '{}'",
                str::from_utf8(&n.path).unwrap(),
                n.priority,
                prio
            )
        }

        prio
    }

    #[test]
    fn test_tree_add_and_get() {
        let mut tree = Node::new();

        let routes = vec![
            "/hi",
            "/contact",
            "/co",
            "/c",
            "/a",
            "/ab",
            "/doc/",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/ʯ",
            "/β",
        ];

        for route in routes {
            tree.insert(route, route.to_owned()).unwrap();
        }

        check_requests(
            &mut tree,
            vec![
                TestRequest::new("/a", false, "/a", Params::new()),
                TestRequest::new("/", true, "", Params::new()),
                TestRequest::new("/hi", false, "/hi", Params::new()),
                TestRequest::new("/contact", false, "/contact", Params::new()),
                TestRequest::new("/co", false, "/co", Params::new()),
                TestRequest::new("/con", true, "", Params::new()), // key mismatch
                TestRequest::new("/cona", true, "", Params::new()), // key mismatch
                TestRequest::new("/no", true, "", Params::new()),  // no matching child
                TestRequest::new("/ab", false, "/ab", Params::new()),
                TestRequest::new("/ʯ", false, "/ʯ", Params::new()),
                TestRequest::new("/β", false, "/β", Params::new()),
            ],
        );

        check_priorities(&mut tree);
    }

    #[test]
    fn test_tree_wildcard() {
        let mut tree = Node::new();

        let routes = vec![
            "/",
            "/cmd/:tool/:sub",
            "/cmd/:tool/",
            "/src/*filepath",
            "/search/",
            "/search/:query",
            "/user_:name",
            "/user_:name/about",
            "/files/:dir/*filepath",
            "/doc/",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/info/:user/public",
            "/info/:user/project/:project",
        ];

        for route in routes {
            tree.insert(route, route.to_owned()).unwrap();
        }

        check_requests(
            &mut tree,
            vec![
                TestRequest::new("/", false, "/", Params::new()),
                TestRequest::new(
                    "/cmd/test/",
                    false,
                    "/cmd/:tool/",
                    params(vec![("tool", "test")]),
                ),
                TestRequest::new("/cmd/test", true, "", params(vec![("tool", "test")])),
                TestRequest::new(
                    "/cmd/test/3",
                    false,
                    "/cmd/:tool/:sub",
                    params(vec![("tool", "test"), ("sub", "3")]),
                ),
                TestRequest::new(
                    "/src/",
                    false,
                    "/src/*filepath",
                    params(vec![("filepath", "/")]),
                ),
                TestRequest::new(
                    "/src/some/file.png",
                    false,
                    "/src/*filepath",
                    params(vec![("filepath", "/some/file.png")]),
                ),
                TestRequest::new("/search/", false, "/search/", Params::new()),
                TestRequest::new(
                    "/search/someth!ng+in+ünìcodé",
                    false,
                    "/search/:query",
                    params(vec![("query", "someth!ng+in+ünìcodé")]),
                ),
                TestRequest::new(
                    "/search/someth!ng+in+ünìcodé/",
                    true,
                    "",
                    params(vec![("query", "someth!ng+in+ünìcodé")]),
                ),
                TestRequest::new(
                    "/user_rustacean",
                    false,
                    "/user_:name",
                    params(vec![("name", "rustacean")]),
                ),
                TestRequest::new(
                    "/user_rustacean/about",
                    false,
                    "/user_:name/about",
                    params(vec![("name", "rustacean")]),
                ),
                TestRequest::new(
                    "/files/js/inc/framework.js",
                    false,
                    "/files/:dir/*filepath",
                    params(vec![("dir", "js"), ("filepath", "/inc/framework.js")]),
                ),
                TestRequest::new(
                    "/info/gordon/public",
                    false,
                    "/info/:user/public",
                    params(vec![("user", "gordon")]),
                ),
                TestRequest::new(
                    "/info/gordon/project/go",
                    false,
                    "/info/:user/project/:project",
                    params(vec![("user", "gordon"), ("project", "go")]),
                ),
            ],
        );

        check_priorities(&mut tree);
    }

    type TestRoute = (&'static str, bool);

    fn test_routes(routes: Vec<TestRoute>) {
        let mut tree = Node::new();

        for route in routes {
            let res = tree.insert(route.0, ());

            if route.1 {
                if res.is_ok() {
                    panic!("no panic for conflicting route '{}'", route.0);
                }
            } else if res.is_err() {
                panic!("unexpected panic for route '{}': {:?}", route.0, res);
            }
        }
    }

    #[test]
    fn test_tree_wildcard_conflict() {
        let routes = vec![
            ("/cmd/:tool/:sub", false),
            ("/cmd/vet", true),
            ("/src/*filepath", false),
            ("/src/*filepathx", true),
            ("/src/", true),
            ("/src1/", false),
            ("/src1/*filepath", true),
            ("/src2*filepath", true),
            ("/search/:query", false),
            ("/search/invalid", true),
            ("/user_:name", false),
            ("/user_x", true),
            ("/user_:name", true),
            ("/id:id", false),
            ("/id/:id", true),
        ];
        test_routes(routes);
    }

    #[test]
    fn test_tree_child_conflict() {
        let routes = vec![
            ("/cmd/vet", false),
            ("/cmd/:tool/:sub", true),
            ("/src/AUTHORS", false),
            ("/src/*filepath", true),
            ("/user_x", false),
            ("/user_:name", true),
            ("/id/:id", false),
            ("/id:id", true),
            ("/:id", true),
            ("/*filepath", true),
        ];

        test_routes(routes);
    }

    #[test]
    fn test_tree_duplicate_path() {
        let mut tree = Node::new();

        let routes = vec![
            "/",
            "/doc/",
            "/src/*filepath",
            "/search/:query",
            "/user_:name",
        ];

        for route in routes {
            tree.insert(route, route.to_owned()).unwrap();
            let res = tree.insert(route, route.to_owned());
            assert_eq!(res, Err(InsertError::Conflict { with: route.into() }));
        }

        check_requests(
            &mut tree,
            vec![
                TestRequest::new("/", false, "/", Params::new()),
                TestRequest::new("/doc/", false, "/doc/", Params::new()),
                TestRequest::new(
                    "/src/some/file.png",
                    false,
                    "/src/*filepath",
                    params(vec![("filepath", "/some/file.png")]),
                ),
                TestRequest::new(
                    "/search/someth!ng+in+ünìcodé",
                    false,
                    "/search/:query",
                    params(vec![("query", "someth!ng+in+ünìcodé")]),
                ),
                TestRequest::new(
                    "/user_rustacean",
                    false,
                    "/user_:name",
                    params(vec![("name", "rustacean")]),
                ),
            ],
        );
    }

    #[test]
    fn test_empty_wildcard_name() {
        let mut tree = Node::new();
        let routes = vec!["/user:", "/user:/", "/cmd/:/", "/src/*"];

        for route in routes {
            assert_eq!(
                tree.insert(route, route.to_owned()),
                Err(InsertError::UnnamedParam)
            );
        }
    }

    #[test]
    fn test_tree_catch_all_conflict() {
        let routes = vec![
            ("/src/*filepath/x", true),
            ("/src2/", false),
            ("/src2/*filepath/x", true),
        ];

        test_routes(routes);
    }

    #[test]
    fn test_tree_catch_all_conflict_root() {
        let routes = vec![("/", false), ("/*filepath", true)];

        test_routes(routes);
    }

    #[test]
    fn test_tree_double_wildcard() {
        let routes = vec!["/:foo:bar", "/:foo:bar/", "/:foo*bar"];

        for route in routes {
            let mut tree = Node::new();
            let res = tree.insert(route, route.to_owned());
            assert_eq!(res, Err(InsertError::TooManyParams));
        }
    }

    #[test]
    fn test_tree_trailing_slash_redirect() {
        let mut tree = Node::new();
        let routes = vec![
            "/hi",
            "/b/",
            "/search/:query",
            "/cmd/:tool/",
            "/src/*filepath",
            "/x",
            "/x/y",
            "/y/",
            "/y/z",
            "/0/:id",
            "/0/:id/1",
            "/1/:id/",
            "/1/:id/2",
            "/aa",
            "/a/",
            "/admin",
            "/admin/:category",
            "/admin/:category/:page",
            "/doc",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/no/a",
            "/no/b",
            "/api/hello/:name",
        ];

        for route in routes {
            let res = tree.insert(route, route.to_owned());

            if res.is_err() {
                panic!("panic inserting route '{}': {:?}", route, res);
            }
        }

        let tsr_routes = vec![
            "/hi/",
            "/b",
            "/search/rustacean/",
            "/cmd/vet",
            "/src",
            "/x/",
            "/y",
            "/0/go/",
            "/1/go",
            "/a",
            "/admin/",
            "/admin/config/",
            "/admin/config/permissions/",
            "/doc/",
        ];

        for route in tsr_routes {
            let res = tree.at(route);

            match res {
                Ok(_) => {
                    panic!("non-nil value for TSR route '{}'", route);
                }
                Err(err) => {
                    if !err.tsr() {
                        panic!("expected TSR recommendation for route '{}'", route);
                    }
                }
            }
        }

        let no_tsr_routes = vec!["/", "/no", "/no/", "/_", "/_/", "/api/world/abc"];

        for route in no_tsr_routes {
            let res = tree.at(route);

            match res {
                Ok(_) => {
                    panic!("non-nil value for TSR route '{}'", route);
                }
                Err(err) => {
                    if err.tsr() {
                        panic!("expected no TSR recommendation for route '{}'", route);
                    }
                }
            }
        }
    }

    #[test]
    fn test_tree_root_trailing_slash_redirect() {
        let mut tree = Node::new();

        tree.insert("/:test", "/:test".to_owned()).unwrap();

        let res = tree.at("/");

        match res {
            Ok(_) => {
                panic!("non-nil value for route '/'");
            }
            Err(err) => {
                if err.tsr() {
                    panic!("expected no TSR recommendation for route '/'");
                }
            }
        }
    }

    #[test]
    fn test_tree_find_case_insensitive_path() {
        let mut tree = Node::new();

        let routes = vec![
            "/hi",
            "/b/",
            "/ABC/",
            "/search/:query",
            "/cmd/:tool/",
            "/src/*filepath",
            "/x",
            "/x/y",
            "/y/",
            "/y/z",
            "/0/:id",
            "/0/:id/1",
            "/1/:id/",
            "/1/:id/2",
            "/aa",
            "/a/",
            "/doc",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/doc/go/away",
            "/no/a",
            "/no/b",
            "/Π",
            "/u/apfêl/",
            "/u/äpfêl/",
            "/u/öpfêl",
            "/v/Äpfêl/",
            "/v/Öpfêl",
            "/w/♬",
            "/w/♭/",
            "/w/𠜎",
            "/w/𠜏/",
            "/loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong",
    ];

        for route in &routes {
            tree.insert(*route, route.to_owned()).unwrap();
        }

        // Check out == in for all registered routes
        // With fixTrailingSlash = true
        for route in &routes {
            let out = tree.path_ignore_case(route, true);
            match out {
                None => panic!("Route '{}' not found!", route),
                Some(out) => {
                    if out != *route {
                        panic!("Wrong result for route '{}': {}", route, out);
                    }
                }
            };
        }

        // With fixTrailingSlash = false
        for route in &routes {
            let out = tree.path_ignore_case(route, false);
            match out {
                None => panic!("Route '{}' not found!", route),
                Some(out) => {
                    if out != *route {
                        panic!("Wrong result for route '{}': {}", route, out);
                    }
                }
            };
        }

        let tests = vec![
            ("/HI", "/hi", false),
            ("/HI/", "/hi", true),
            ("/B", "/b/", true),
            ("/B/", "/b/", false),
            ("/abc", "/ABC/", true),
            ("/abc/", "/ABC/", false),
            ("/aBc", "/ABC/", true),
            ("/aBc/", "/ABC/", false),
            ("/abC", "/ABC/", true),
            ("/abC/", "/ABC/", false),
            ("/SEARCH/QUERY", "/search/QUERY", false),
            ("/SEARCH/QUERY/", "/search/QUERY", true),
            ("/CMD/TOOL/", "/cmd/TOOL/", false),
            ("/CMD/TOOL", "/cmd/TOOL/", true),
            ("/SRC/FILE/PATH", "/src/FILE/PATH", false),
            ("/x/Y", "/x/y", false),
            ("/x/Y/", "/x/y", true),
            ("/X/y", "/x/y", false),
            ("/X/y/", "/x/y", true),
            ("/X/Y", "/x/y", false),
            ("/X/Y/", "/x/y", true),
            ("/Y/", "/y/", false),
            ("/Y", "/y/", true),
            ("/Y/z", "/y/z", false),
            ("/Y/z/", "/y/z", true),
            ("/Y/Z", "/y/z", false),
            ("/Y/Z/", "/y/z", true),
            ("/y/Z", "/y/z", false),
            ("/y/Z/", "/y/z", true),
            ("/Aa", "/aa", false),
            ("/Aa/", "/aa", true),
            ("/AA", "/aa", false),
            ("/AA/", "/aa", true),
            ("/aA", "/aa", false),
            ("/aA/", "/aa", true),
            ("/A/", "/a/", false),
            ("/A", "/a/", true),
            ("/DOC", "/doc", false),
            ("/DOC/", "/doc", true),
            ("/NO", "", true),
            ("/DOC/GO", "", true),
            // [TODO] unicode vs ascii case sensitivity
            // ("/π", "/Π", false),
            // ("/π/", "/Π", true),
            // ("/u/ÄPFÊL/", "/u/äpfêl/", false),
            // ("/u/ÄPFÊL", "/u/äpfêl/", true),
            // ("/u/ÖPFÊL/", "/u/öpfêl", true),
            // ("/u/ÖPFÊL", "/u/öpfêl", false),
            // ("/v/äpfêL/", "/v/Äpfêl/", false),
            // ("/v/äpfêL", "/v/Äpfêl/", true),
            // ("/v/öpfêL/", "/v/Öpfêl", true),
            // ("/v/öpfêL", "/v/Öpfêl", false),
            ("/w/♬/", "/w/♬", true),
            ("/w/♭", "/w/♭/", true),
            ("/w/𠜎/", "/w/𠜎", true),
            ("/w/𠜏", "/w/𠜏/", true),
            (
                "/lOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOng/",
                "/loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong",
                true
            ),
    ];

        struct Test {
            inn: &'static str,
            out: &'static str,
            slash: bool,
        }

        let tests: Vec<Test> = tests
            .into_iter()
            .map(|test| Test {
                inn: test.0,
                out: test.1,
                slash: test.2,
            })
            .collect();

        // With fixTrailingSlash = true
        for test in &tests {
            let res = tree.path_ignore_case(test.inn, true).unwrap_or_default();
            if res != test.out {
                panic!("Wrong result for route '{}': {}", res, test.out);
            }
        }

        // without fix trailing slash = false
        for test in &tests {
            let res = tree.path_ignore_case(test.inn, false);
            match res {
                None => (),
                Some(res) => {
                    if test.slash {
                        // test needs a trailing slash fix. It must not be found!
                        panic!(
                            "Found without fix_trailing_slash: {}; got {}",
                            test.inn, res
                        );
                    }
                    if res != test.out {
                        panic!("Wrong result for route '{}': {}", res, test.out);
                    }
                }
            };
        }
    }

    #[test]
    fn test_tree_wildcard_conflict_ex() {
        let conflicts = vec![
            ("/who/are/foo", "/who/are/*you"),
            ("/who/are/foo/", "/who/are/*you"),
            ("/who/are/foo/bar", "/who/are/*you"),
            ("/conxxx", "/con:tact"),
            ("/conooo/xxx", "/con:tact"),
            ("/whose/:users/:user", "/whose/:users/:name"),
        ];

        for conflict in conflicts {
            let mut tree = Node::new();

            let routes = vec![
                "/con:tact",
                "/who/are/*you",
                "/who/foo/hello",
                "/whose/:users/:name",
            ];

            for route in routes {
                tree.insert(route, route.to_owned()).unwrap();
            }

            let res = tree.insert(conflict.0, conflict.0.to_owned());
            assert_eq!(
                res,
                Err(InsertError::Conflict {
                    with: conflict.1.into()
                })
            );
        }
    }

    #[test]
    fn malformed_path() {
        let mut tree = Node::new();
        assert_eq!(tree.insert("*x", 1), Err(InsertError::MalformedPath));
    }
}
