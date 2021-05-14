#![deny(rust_2018_idioms)]

//! [![Documentation](https://img.shields.io/badge/docs-0.3.2-4d76ae?style=for-the-badge)](https://docs.rs/matchit/0.3.2)
//! [![Version](https://img.shields.io/crates/v/matchit?style=for-the-badge)](https://crates.io/crates/matchit)
//! [![License](https://img.shields.io/crates/l/matchit?style=for-the-badge)](https://crates.io/crates/matchit)
//! [![Actions](https://img.shields.io/github/workflow/status/ibraheemdev/matchit/Rust/master?style=for-the-badge)](https://github.com/ibraheemdev/matchit/actions)
//!
//! Matches URL patterns with support for dynamic and wildcard segments.
//!
//! ```rust
//! use matchit::Node;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut matcher = Node::new();
//! matcher.insert("/home", "Welcome!")?;
//! matcher.insert("/users/:id", "A User")?;
//!
//! let matched = matcher.at("/users/1")?;
//! assert_eq!(matched.params.get("id"), Some("1"));
//! assert_eq!(*matched.value, "A User");
//! # Ok(())
//! # }
//! ```
//!
//! `matchit` relies on a tree structure which makes heavy use of *common prefixes*, it is effectively a [radix tree](https://en.wikipedia.org/wiki/Radix_tree). This makes lookups extremely fast. [See below for technical details](#how-does-it-work).
//!
//! The tree is optimized for high performance and a small memory footprint. It scales well even with very long paths and a large number of routes. A compressing dynamic trie (radix tree) structure is used for efficient matching.
//!
//! ### Parameters
//!
//! As you can see, `:id` is a *parameter*. The values are accessible via [`Params`](https://docs.rs/matchit/0.2.0/matchit/tree/struct.Params.html), which stores a vector of keys and values. You can get the value of a parameter either by its index in the vector, or by using the `Params::get(name)` method. For example, `:user` can be retrieved by `params.get("user")`.
//!
//! The registered path can contain two types of parameters:
//! ```text
//! Syntax    Type
//! :name     named parameter
//! *name     catch-all parameter
//! ```
//!
//! ### Named Parameters
//!
//! Named parameters are dynamic path segments. They match anything until the next `/` or the path end:
//!
//! ```text
//! Pattern: /user/:user
//!
//!  /user/gordon              match
//!  /user/you                 match
//!  /user/gordon/profile      no match
//!  /user/                    no match
//! ```
//!
//! **Note:** Since the tree only supports explicit matches, you can not register static routes and parameters for the same path segment. For example you can not register the patterns `/user/new` and `/user/:user` at the same time.
//!
//! ### Catch-All parameters
//!
//! The second type are *catch-all* parameters and have the form `*name`. Like the name suggests, they match everything. Therefore they must always be at the **end** of the pattern:
//!
//! ```text
//! Pattern: /src/*filepath
//!
//!  /src/                     match
//!  /src/somefile.go          match
//!  /src/subdir/somefile.go   match
//! ```
//!
//! ## How does it work?
//!
//! The matcher relies on a tree structure which makes heavy use of *common prefixes*, it is basically a *compact* [*prefix tree*](https://en.wikipedia.org/wiki/Trie) (or [*Radix tree*](https://en.wikipedia.org/wiki/Radix_tree)). Nodes with a common prefix share a parent. Here is a short example what the routing tree for the `GET` request method could look like:
//!
//! ```text
//! Priority   Path             Handle
//! 9          \                *<1>
//! 3          ├s               None
//! 2          |├earch\         *<2>
//! 1          |└upport\        *<3>
//! 2          ├blog\           *<4>
//! 1          |    └:post      None
//! 1          |         └\     *<5>
//! 2          ├about-us\       *<6>
//! 1          |        └team\  *<7>
//! 1          └contact\        *<8>
//! ```
//!
//! Every `*<num>` represents the memory address of a handler function (a pointer). If you follow a path trough the tree from the root to the leaf, you get the complete route path, e.g `/blog/:post`, where `:post` is just a placeholder ([*parameter*](#named-parameters)) for an actual post name. Unlike hash-maps, a tree structure also allows us to use dynamic parts like the `:post` parameter, since we actually match against the routing patterns instead of just comparing hashes. This works very well and efficiently.
//!
//! Because URL paths have a hierarchical structure and make use only of a limited set of characters (byte values), it is very likely that there are a lot of common prefixes. This allows us to easily reduce the routing into ever smaller problems. Moreover the matcher manages a separate tree for every request method. For one thing it is more space efficient than holding a method->handle map in every single node, it also allows us to greatly reduce the routing problem before even starting the look-up in the prefix-tree.
//!
//! For even better scalability, the child nodes on each tree level are ordered by priority, where the priority is just the number of handles registered in sub nodes (children, grandchildren, and so on..). This helps in two ways:
//!
//! 1. Nodes which are part of the most routing paths are evaluated first. This helps make more routes reachable as fast as possible.
//! 2. It acts as a cost compensation. The longest reachable path (highest cost) can always be evaluated first. The following scheme visualizes the tree structure. Nodes are evaluated from top to bottom and from left to right.
//!
//! ```text
//! ├------------
//! ├---------
//! ├-----
//! ├----
//! ├--
//! ├--
//! └-
//! ```
use std::borrow::Cow;
use std::cell::UnsafeCell;
use std::cmp::min;
use std::fmt;
use std::mem;
use std::ops::{Index, IndexMut};
use std::slice;
use std::str;

/// A successful match consisting of the registered value and the URL parameters, returned by
/// [`Node::at`](crate::Node::at).
#[derive(Debug, Clone)]
pub struct Match<V> {
    /// The value stored under the matched node.
    pub value: V,
    /// The route parameters. See [parameters](crate#parameters) for more details.
    pub params: Params,
}

/// Represents errors that can occur when inserting a new route.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum InsertError {
    /// The inserted path conflicts with an existing route.
    ///
    /// This error may unexpectedly come up when registering routes like the following:
    ///
    /// ```text
    /// /user/:id
    /// /user/get
    /// ```
    ///
    /// This is due to the strict requirements of the internal radix tree,
    /// and should be fixed in the future.
    Conflict {
        /// The existing route that the insertion is conflicting with.
        with: String,
    },
    /// Only one parameter per path segment is allowed.
    TooManyParams,
    /// Parameters must be registered with a name.
    UnnamedParam,
    /// Catch-all parameters are only allowed at the end of a path.
    InvalidCatchAll,
}

impl fmt::Display for InsertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Conflict { with } => {
                write!(
                    f,
                    "insertion failed due to conflict with previously registered path: {}",
                    with
                )
            }
            Self::TooManyParams => write!(f, "only one parameter is allowed per path segment"),
            Self::UnnamedParam => write!(f, "parameters must be registered with a name"),
            Self::InvalidCatchAll => write!(
                f,
                "catch-all parameters are only allowed at the end of a path"
            ),
        }
    }
}

impl std::error::Error for InsertError {}

impl InsertError {
    fn conflict(insert: &str, prefix: &[u8], existing: &[u8]) -> Self {
        let with = format!(
            "{}{}",
            &insert[..insert.rfind(str::from_utf8(prefix).unwrap()).unwrap()],
            str::from_utf8(&existing).unwrap(),
        );

        InsertError::Conflict { with }
    }
}

/// A failed match attempt, with trailing slash redirect information.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct MatchError {
    tsr: bool,
}

impl MatchError {
    /// Indicates whether a route exists at the same path with/without a trailing slash.
    /// ```rust
    /// # use matchit::Node;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/blog/", "Our blog.")?;
    ///
    /// if let Err(err) = matcher.at("/home/") {
    ///     assert!(err.tsr());
    /// }
    ///
    /// if let Err(err) = matcher.at("/blog") {
    ///     assert!(err.tsr());
    /// }
    ///
    /// if let Err(err) = matcher.at("/foobar") {
    ///     assert!(!err.tsr());
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn tsr(&self) -> bool {
        self.tsr
    }

    fn new(tsr: bool) -> Self {
        Self { tsr }
    }
}

impl fmt::Display for MatchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "no route registered under the given path")
    }
}

impl std::error::Error for MatchError {}

/// A single URL parameter, consisting of a key and a value.
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Default)]
pub struct Param {
    pub key: String,
    pub value: String,
}

impl Param {
    /// Create a new route parameter with the given key/value.
    pub fn new(key: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            key: key.into(),
            value: value.into(),
        }
    }
}

/// A list of parameters returned by a route match.
///
/// ```rust
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # let mut matcher = matchit::Node::new();
/// # matcher.insert("/users/:id", true).unwrap();
/// let matched = matcher.at("/users/1")?;
///
/// for param in &matched.params {
///     println!("key: {}, value: {}", param.key, param.value);
/// }
///
/// let id = matched.params.get("id");
/// assert_eq!(id, Some("1"));
/// # Ok(())
/// # }
/// ```
#[derive(Default, Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
pub struct Params(pub Vec<Param>);

impl Params {
    /// Returns the value of the first [`Param`] whose key matches the given name.
    pub fn get(&self, name: impl AsRef<str>) -> Option<&str> {
        self.0
            .iter()
            .find(|param| param.key == name.as_ref())
            .map(|param| param.value.as_ref())
    }

    /// Returns an iterator over the parameters in the list.
    pub fn iter(&self) -> slice::Iter<'_, Param> {
        self.0.iter()
    }

    /// Returns an iterator that allows modifying each value.
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, Param> {
        self.0.iter_mut()
    }

    /// Returns `true` if there are no parameters in the list.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Inserts a [`Param`] into the list.
    pub fn push(&mut self, param: Param) {
        self.0.push(param);
    }
}

impl IntoIterator for Params {
    type IntoIter = std::vec::IntoIter<Param>;
    type Item = Param;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a mut Params {
    type IntoIter = std::slice::IterMut<'a, Param>;
    type Item = &'a mut Param;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<'a> IntoIterator for &'a Params {
    type IntoIter = std::slice::Iter<'a, Param>;
    type Item = &'a Param;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl Index<usize> for Params {
    type Output = Param;

    #[inline]
    fn index(&self, i: usize) -> &Param {
        &self.0[i]
    }
}

impl IndexMut<usize> for Params {
    fn index_mut(&mut self, i: usize) -> &mut Param {
        &mut self.0[i]
    }
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
pub struct Node<'path, V> {
    path: Cow<'path, [u8]>,
    wild_child: bool,
    node_type: NodeType,
    indices: Cow<'path, [u8]>,
    children: Vec<Node<'path, V>>,
    // See `at_inner` for why an unsafe cell is needed.
    value: Option<UnsafeCell<V>>,
    priority: u32,
}

impl<V> Default for Node<'_, V> {
    fn default() -> Self {
        Self {
            path: Cow::default(),
            wild_child: false,
            node_type: NodeType::Static,
            indices: Cow::default(),
            children: Vec::new(),
            value: None,
            priority: 0,
        }
    }
}

impl<'path, V> Node<'path, V> {
    /// Construct a new `Node`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a value into the tree under the given path.
    /// ```rust
    /// # use matchit::Node;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/users/:id", "A User")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert(&mut self, path: &'path str, value: V) -> Result<(), InsertError> {
        self.priority += 1;

        // Empty tree
        if self.path.is_empty() && self.children.is_empty() {
            self.insert_child(path.as_ref(), path.as_ref(), value)?;
            self.node_type = NodeType::Root;
            return Ok(());
        }

        self.insert_helper(path.as_ref(), path, value)
    }

    #[inline]
    fn insert_helper(
        &mut self,
        mut path: &'path [u8],
        full_path: &str,
        value: V,
    ) -> Result<(), InsertError> {
        // Find the longest common prefix.
        // This also implies that the common prefix contains no ':' or '*'
        // since the existing key can't contain those chars.
        let mut i = 0;
        let max = min(path.len(), self.path.len());

        while i < max && path[i] == self.path[i] {
            i += 1;
        }

        // Split edge
        if i < self.path.len() {
            let mut child = Self {
                path: self.path[i..].to_owned().into(),
                wild_child: self.wild_child,
                indices: self.indices.clone(),
                value: self.value.take(),
                priority: self.priority - 1,
                ..Self::default()
            };

            mem::swap(&mut self.children, &mut child.children);

            self.children = vec![child];
            self.indices = self.path[i..i + 1].to_owned().into();
            self.path = path[..i].into();
            self.wild_child = false;
        }

        // Make new node a child of this node
        if path.len() > i {
            path = &path[i..];

            if self.wild_child {
                return self.children[0].wild_child_conflict(path, full_path, value);
            }

            let idxc = path[0];

            // `/` after param
            if self.node_type == NodeType::Param && idxc == b'/' && self.children.len() == 1 {
                self.children[0].priority += 1;
                return self.children[0].insert_helper(path, full_path, value);
            }

            // Check if a child with the next path byte exists
            for mut i in 0..self.indices.len() {
                if idxc == self.indices[i] {
                    i = self.incr_child_priority(i);
                    return self.children[i].insert_helper(path, full_path, value);
                }
            }

            // Otherwise insert it
            if idxc != b':' && idxc != b'*' {
                self.indices.to_mut().push(idxc);

                self.children.push(Self::default());

                let child = self.incr_child_priority(self.indices.len() - 1);
                return self.children[child].insert_child(path, full_path, value);
            }

            return self.insert_child(path, full_path, value);
        } else {
            // Otherwise add value to current node
            if self.value.is_some() {
                return Err(InsertError::conflict(full_path, path, &self.path));
            }

            self.value = Some(UnsafeCell::new(value));
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
            .concat()
            .into();
        }

        new_pos
    }

    #[inline]
    fn wild_child_conflict(
        &mut self,
        path: &'path [u8],
        full_path: &str,
        value: V,
    ) -> Result<(), InsertError> {
        self.priority += 1;

        // Check if the wildcard matches
        if path.len() >= self.path.len()
      && self.path == &path[..self.path.len()]
      // Adding a child to a CatchAll Node is not possible
      && self.node_type != NodeType::CatchAll
      // Check for longer wildcard, e.g. :name and :names
      && (self.path.len() >= path.len() || path[self.path.len()] == b'/')
        {
            self.insert_helper(path, full_path, value)
        } else {
            Err(InsertError::conflict(full_path, path, &self.path))
        }
    }

    fn insert_child(
        &mut self,
        mut path: &'path [u8],
        full_path: &str,
        value: V,
    ) -> Result<(), InsertError> {
        let (wildcard, wildcard_index, valid) = find_wildcard(path);

        if wildcard_index.is_none() {
            self.value = Some(UnsafeCell::new(value));
            self.path = path.into();
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
            return Err(InsertError::conflict(full_path, path, &self.path));
        }

        // Param
        if wildcard[0] == b':' {
            // Insert prefix before the current wildcard
            if wildcard_index > 0 {
                self.path = path[..wildcard_index].into();
                path = &path[wildcard_index..];
            }

            let child = Self {
                node_type: NodeType::Param,
                path: wildcard.into(),
                ..Self::default()
            };

            self.wild_child = true;
            self.children = vec![child];
            self.children[0].priority += 1;

            // If the path doesn't end with the wildcard, then there
            // will be another non-wildcard subpath starting with '/'

            if wildcard.len() < path.len() {
                path = &path[wildcard.len()..];
                let child = Self {
                    priority: 1,
                    ..Self::default()
                };

                self.children[0].children = vec![child];
                return self.children[0].children[0].insert_child(path, full_path, value);
            }
            // Otherwise we're done. Insert the value in the new leaf
            self.children[0].value = Some(UnsafeCell::new(value));
            return Ok(());
        }

        // catch all
        if wildcard_index + wildcard.len() != path.len() {
            return Err(InsertError::InvalidCatchAll);
        }

        if !self.path.is_empty() && self.path[self.path.len() - 1] == b'/' {
            return Err(InsertError::conflict(full_path, path, &self.path));
        }

        // Currently fixed width 1 for '/'
        wildcard_index -= 1;
        if path[wildcard_index] != b'/' {
            return Err(InsertError::InvalidCatchAll);
        }

        // first node: CatchAll Node with empty path
        let child = Self {
            wild_child: true,
            node_type: NodeType::CatchAll,
            ..Self::default()
        };

        self.path = path[..wildcard_index].into();
        self.children = vec![child];
        self.indices = slice::from_ref(&b'/').into();
        self.children[0].priority += 1;

        // Second node: node holding the variable
        let child = Self {
            path: path[wildcard_index..].into(),
            node_type: NodeType::CatchAll,
            value: Some(UnsafeCell::new(value)),
            priority: 1,
            ..Self::default()
        };

        self.children[0].children = vec![child];

        Ok(())
    }

    /// Returns the value registered at the given path.
    /// If no value can be found it returns a trailing slash redirect recommendation.
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
    pub fn at(&self, path: impl AsRef<str>) -> Result<Match<&V>, MatchError> {
        match self.at_inner(path) {
            Ok(v) => Ok(Match {
                // SAFETY: We have an immutable reference to `self`, so we can give out a immutable
                // reference to `value` of the same lifetime
                value: unsafe { &*v.value.get() },
                params: v.params,
            }),
            Err(e) => Err(e),
        }
    }

    /// Returns a mutable reference to the value registered at the given path.
    /// See [`Node::at`](crate::Node::at) for details.
    pub fn at_mut(&mut self, path: impl AsRef<str>) -> Result<Match<&mut V>, MatchError> {
        match self.at_inner(path) {
            Ok(v) => Ok(Match {
                // SAFETY: We have a unique reference to `self`, so we can give out a unique
                // reference to `value` of the same lifetime
                value: unsafe { &mut *v.value.get() },
                params: v.params,
            }),
            Err(e) => Err(e),
        }
    }

    // It's a bit sad that we have to introduce unsafecell here, but rust doesn't really have a way
    // to abstract over mutability, so it avoids having to duplicate logic between `at` and
    // `at_mut`.
    fn at_inner(&self, path: impl AsRef<str>) -> Result<Match<&'_ UnsafeCell<V>>, MatchError> {
        let mut current = self;
        let mut path = path.as_ref().as_bytes();
        let mut params = Params::default();

        // outer loop for walking the tree to get a path's value
        'walk: loop {
            let prefix = &current.path;
            if path.len() > prefix.len() {
                if prefix.as_ref() == &path[..prefix.len()] {
                    path = &path[prefix.len()..];

                    // If this node does not have a wildcard (Param or CatchAll)
                    // child, we can just look up the next child node and continue
                    // to walk down the tree
                    if !current.wild_child {
                        let idxc = path[0];
                        for (i, c) in current.indices.iter().enumerate() {
                            if idxc == *c {
                                current = &current.children[i];
                                continue 'walk;
                            }
                        }
                        // Nothing found.
                        // We can recommend to redirect to the same URL without a
                        // trailing slash if a leaf exists for that path.
                        let tsr = path == [b'/'] && current.value.is_some();
                        return Err(MatchError::new(tsr));
                    }

                    current = &current.children[0];
                    match current.node_type {
                        NodeType::Param => {
                            // find param end (either '/' or path end)
                            let mut end = 0;
                            while end < path.len() && path[end] != b'/' {
                                end += 1;
                            }

                            params.push(Param {
                                key: str::from_utf8(&current.path[1..]).unwrap().into(),
                                value: str::from_utf8(&path[..end]).unwrap().into(),
                            });

                            // we need to go deeper!
                            if end < path.len() {
                                if !current.children.is_empty() {
                                    path = &path[end..];

                                    current = &current.children[0];
                                    continue 'walk;
                                }

                                // ... but we can't
                                let tsr = path.len() == end + 1;
                                return Err(MatchError::new(tsr));
                            }

                            if let Some(value) = current.value.as_ref() {
                                return Ok(Match { value, params });
                            } else if current.children.len() == 1 {
                                current = &current.children[0];
                                // No value found. Check if a value for this path + a
                                // trailing slash exists for TSR recommendation
                                let tsr = (current.path.as_ref() == [b'/']
                                    && current.value.is_some())
                                    || (current.path.as_ref().is_empty()
                                        && current.indices.as_ref() == [b'/']);
                                return Err(MatchError::new(tsr));
                            }

                            return Err(MatchError::new(false));
                        }
                        NodeType::CatchAll => {
                            params.push(Param {
                                key: str::from_utf8(current.path[2..].as_ref()).unwrap().into(),
                                value: str::from_utf8(path).unwrap().into(),
                            });

                            return match current.value.as_ref() {
                                Some(value) => Ok(Match { value, params }),
                                None => Err(MatchError::new(false)),
                            };
                        }
                        _ => unreachable!(),
                    }
                }
            } else if path == prefix.as_ref() {
                // We should have reached the node containing the value.
                // Check if this node has a value registered.
                if let Some(value) = current.value.as_ref() {
                    return Ok(Match { value, params });
                }

                // If there is no value for this route, but this route has a
                // wildcard child, there must be a value for this path with an
                // additional trailing slash
                if path == [b'/'] && current.wild_child && current.node_type != NodeType::Root {
                    return Err(MatchError::new(true));
                }

                // No value found. Check if a value for this path + a
                // trailing slash exists for trailing slash recommendation
                for (i, c) in current.indices.iter().enumerate() {
                    if *c == b'/' {
                        current = &current.children[i];
                        let tsr = (current.path.len() == 1 && current.value.is_some())
                            || (current.node_type == NodeType::CatchAll
                                && current.children[0].value.is_some());
                        return Err(MatchError::new(tsr));
                    }
                }

                return Err(MatchError::new(false));
            }

            // Nothing found. We can recommend to redirect to the same URL with an
            // extra trailing slash if a leaf exists for that path
            let tsr = (path == [b'/'])
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

    // recursive case-insensitive match function used by `Node::find_case_insensitive_path`
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
            insensitive_path.extend_from_slice(self.path.as_ref());

            path = &path[self.path.len()..];

            if !path.is_empty() {
                let cached_lower_path = <&[u8]>::clone(&lower_path);

                // If this node does not have a wildcard (param or catchAll) child,
                // we can just look up the next child node and continue to walk down
                // the tree
                if !self.wild_child {
                    // skip char bytes already processed
                    buf = shift_n_bytes(buf, self.path.len());

                    if buf[0] != 0 {
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
                    } else {
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
                insensitive_path.extend_from_slice(self.path.as_ref());
                return true;
            }
        }

        false
    }

    // recursive case-insensitive match function used by n.findCaseInsensitivePath
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
                    && self.children[0].path.as_ref() == [b'/']
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
                insensitive_path.extend_from_slice(&path);
                true
            }
            _ => unreachable!(),
        }
    }
}

// Shift bytes in array by n bytes left
const fn shift_n_bytes(bytes: [u8; 4], n: usize) -> [u8; 4] {
    match n {
        0 => bytes,
        1 => [bytes[1], bytes[2], bytes[3], 0],
        2 => [bytes[2], bytes[3], 0, 0],
        3 => [bytes[3], 0, 0, 0],
        _ => [0; 4],
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

    struct TestRequest {
        path: &'static str,
        should_be_nil: bool,
        route: &'static str,
        params: Params,
    }

    impl TestRequest {
        pub fn new(
            path: &'static str,
            should_be_nil: bool,
            route: &'static str,
            params: Params,
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

    fn check_requests(tree: &mut Node<'static, String>, requests: TestRequests) {
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

    fn check_priorities(n: &mut Node<'_, String>) -> u32 {
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
    fn params() {
        let params = Params(vec![
            Param {
                key: "hello".to_owned(),
                value: "world".to_owned(),
            },
            Param {
                key: "rust-is".to_string(),
                value: "awesome".to_string(),
            },
        ]);

        assert_eq!(params.get("hello"), Some("world"));
        assert_eq!(params.get("rust-is"), Some("awesome"));
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
                TestRequest::new("/a", false, "/a", Params::default()),
                TestRequest::new("/", true, "", Params::default()),
                TestRequest::new("/hi", false, "/hi", Params::default()),
                TestRequest::new("/contact", false, "/contact", Params::default()),
                TestRequest::new("/co", false, "/co", Params::default()),
                TestRequest::new("/con", true, "", Params::default()), // key mismatch
                TestRequest::new("/cona", true, "", Params::default()), // key mismatch
                TestRequest::new("/no", true, "", Params::default()),  // no matching child
                TestRequest::new("/ab", false, "/ab", Params::default()),
                TestRequest::new("/ʯ", false, "/ʯ", Params::default()),
                TestRequest::new("/β", false, "/β", Params::default()),
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
                TestRequest::new("/", false, "/", Params::default()),
                TestRequest::new(
                    "/cmd/test/",
                    false,
                    "/cmd/:tool/",
                    Params(vec![Param::new("tool", "test")]),
                ),
                TestRequest::new(
                    "/cmd/test",
                    true,
                    "",
                    Params(vec![Param::new("tool", "test")]),
                ),
                TestRequest::new(
                    "/cmd/test/3",
                    false,
                    "/cmd/:tool/:sub",
                    Params(vec![Param::new("tool", "test"), Param::new("sub", "3")]),
                ),
                TestRequest::new(
                    "/src/",
                    false,
                    "/src/*filepath",
                    Params(vec![Param::new("filepath", "/")]),
                ),
                TestRequest::new(
                    "/src/some/file.png",
                    false,
                    "/src/*filepath",
                    Params(vec![Param::new("filepath", "/some/file.png")]),
                ),
                TestRequest::new("/search/", false, "/search/", Params::default()),
                TestRequest::new(
                    "/search/someth!ng+in+ünìcodé",
                    false,
                    "/search/:query",
                    Params(vec![Param::new("query", "someth!ng+in+ünìcodé")]),
                ),
                TestRequest::new(
                    "/search/someth!ng+in+ünìcodé/",
                    true,
                    "",
                    Params(vec![Param::new("query", "someth!ng+in+ünìcodé")]),
                ),
                TestRequest::new(
                    "/user_rustacean",
                    false,
                    "/user_:name",
                    Params(vec![Param::new("name", "rustacean")]),
                ),
                TestRequest::new(
                    "/user_rustacean/about",
                    false,
                    "/user_:name/about",
                    Params(vec![Param::new("name", "rustacean")]),
                ),
                TestRequest::new(
                    "/files/js/inc/framework.js",
                    false,
                    "/files/:dir/*filepath",
                    Params(vec![
                        Param::new("dir", "js"),
                        Param::new("filepath", "/inc/framework.js"),
                    ]),
                ),
                TestRequest::new(
                    "/info/gordon/public",
                    false,
                    "/info/:user/public",
                    Params(vec![Param::new("user", "gordon")]),
                ),
                TestRequest::new(
                    "/info/gordon/project/go",
                    false,
                    "/info/:user/project/:project",
                    Params(vec![
                        Param::new("user", "gordon"),
                        Param::new("project", "go"),
                    ]),
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
                TestRequest::new("/", false, "/", Params::default()),
                TestRequest::new("/doc/", false, "/doc/", Params::default()),
                TestRequest::new(
                    "/src/some/file.png",
                    false,
                    "/src/*filepath",
                    Params(vec![Param::new("filepath", "/some/file.png")]),
                ),
                TestRequest::new(
                    "/search/someth!ng+in+ünìcodé",
                    false,
                    "/search/:query",
                    Params(vec![Param::new("query", "someth!ng+in+ünìcodé")]),
                ),
                TestRequest::new(
                    "/user_rustacean",
                    false,
                    "/user_:name",
                    Params(vec![Param::new("name", "rustacean")]),
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
            tree.insert(route, route.to_owned()).unwrap();
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
            // ("/π", "/Π", false)
            // ("/π/", "/Π", true),
            // ("/u/ÄPFÊL/", "/u/äpfêl/", false)
            // ("/u/ÄPFÊL", "/u/äpfêl/", true),
            // ("/u/ÖPFÊL/", "/u/öpfêl", true),
            // ("/u/ÖPFÊL", "/u/öpfêl", false)
            // ("/v/äpfêL/", "/v/Äpfêl/", false)
            // ("/v/äpfêL", "/v/Äpfêl/", true),
            // ("/v/öpfêL/", "/v/Öpfêl", true),
            // ("/v/öpfêL", "/v/Öpfêl", false)
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
            let res = tree.path_ignore_case(test.inn, true);
            match res {
                None => (),
                Some(res) => {
                    if res != test.out {
                        panic!("Wrong result for route '{}': {}", res, test.out);
                    }
                }
            };
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
}

#[cfg(doctest)]
mod test_readme {
    macro_rules! doc_comment {
        ($x:expr) => {
            #[doc = $x]
            extern "C" {}
        };
    }

    doc_comment!(include_str!("../README.md"));
}
