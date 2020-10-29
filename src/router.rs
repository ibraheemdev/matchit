//! The router relies on a tree structure which makes heavy use of *common prefixes*,
//! it is basically a *compact* [*prefix tree*](https://en.wikipedia.org/wiki/Trie)
//! (or just [*Radix tree*](https://en.wikipedia.org/wiki/Radix_tree)). Nodes with a
//! common prefix also share a common parent. The radix tree implementation was derived
//! from [julienschmidt/httprouter](https://github.com/julienschmidt/httprouter)
//!
//! Here is a short example what the routing tree for the `GET` request method could look like:
//!
//! ```ignore
//! Priority   Path             value
//! 9          \                *<1>
//! 3          ├s               nil
//! 2          |├earch\         *<2>
//! 1          |└upport\        *<3>
//! 2          ├blog\           *<4>
//! 1          |    └:post      nil
//! 1          |         └\     *<5>
//! 2          ├about-us\       *<6>
//! 1          |        └team\  *<7>
//! 1          └contact\        *<8>
//! ```

//! Every `*<num>` represents the memory address of a value.
//! If you follow a path trough the tree from the root to the leaf, you get the complete
//! route path, e.g `\blog\:post\`, where `:post` is just a placeholder ([*parameter*](#named-parameters))
//! for an actual post name. Unlike hash-maps, a tree structure also allows us to use
//! dynamic parts like the `:post` parameter, since we actually match against the routing
//! patterns instead of just comparing hashes. This works very well and efficiently.

//! Since URL paths have a hierarchical structure and make use only of a limited set of
//! characters (byte values), it is very likely that there are a lot of common prefixes.
//! This allows us to easily reduce the routing into ever smaller problems. Moreover the
//! router manages a separate tree for every request method. For one thing it is more
//! space efficient than holding a method -> value map in every single node, it also allows
//! us to greatly reduce the routing problem before even starting the look-up in the prefix-tree.

//! For even better scalability, the child nodes on each tree level are ordered by priority,
//! where the priority is just the number of values registered in sub nodes (children, grandchildren, and so on..).
//! This helps in two ways:

//! 1. Nodes which are part of the most routing paths are evaluated first. This helps to
//! make as much routes as possible to be reachable as fast as possible.
//! 2. It is some sort of cost compensation. The longest reachable path (highest cost)
//! can always be evaluated first. The following scheme visualizes the tree structure.
//! Nodes are evaluated from top to bottom and from left to right.

//! ```ignore
//! ├------------
//! ├---------
//! ├-----
//! ├----
//! ├--
//! ├--
//! └-
//! ```
//!
use crate::tree::{Node, RouteLookup};
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;
use std::str;

/// Router is container which can be used to dispatch requests to different
/// handler functions via configurable routes
pub struct Router<K: Eq + Hash, V> {
  pub map: HashMap<K, Node<V>>,
}

impl<K: Eq + Hash, V> Default for Router<K, V> {
  fn default() -> Self {
    Router {
      map: HashMap::new(),
    }
  }
}

impl<K: Eq + Hash, V> Router<K, V> {
  pub fn with_capacity(capacity: usize) -> Self {
    Router {
      map: HashMap::with_capacity(capacity),
    }
  }

  /// Add registers a new request handle with the given key and value in the route map
  pub fn add(&mut self, key: K, value: V, path: &str) {
    if !path.starts_with('/') {
      panic!("path must begin with '/' in path '{}'", path);
    }

    self
      .map
      .entry(key)
      .or_insert_with(Node::default)
      .add_route(path, value);
  }

  /// Allows the manual lookup of a path in the route map.
  /// Returns the value and the path parameter values if the path is found.
  /// If no match can be found, a TSR (trailing slash redirect) recommendation is
  /// made if a match exists with an extra (without the) trailing slash for the
  /// given path.
  pub fn lookup(&mut self, key: &K, path: &str) -> Result<RouteLookup<V>, bool> {
    self
      .map
      .get_mut(key)
      .map(|n| n.get_value(path))
      .unwrap_or(Err(false))
  }
}
