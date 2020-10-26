//! This radix tree implementation was derived from [julienschmidt/httprouter](https://github.com/julienschmidt/httprouter)
//!
//! The router relies on a tree structure which makes heavy use of *common prefixes*,
//! it is basically a *compact* [*prefix tree*](https://en.wikipedia.org/wiki/Trie)
//! (or just [*Radix tree*](https://en.wikipedia.org/wiki/Radix_tree)). Nodes with a
//! common prefix also share a common parent. Here is a short example what the routing
//! tree for the `GET` request method could look like:
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
use crate::endpoint::Endpoint;
use http::Method;
use std::cmp::{min, Ordering};
use std::collections::HashMap;
use std::mem;
use std::ops::Index;
use std::str;

/// Router is container which can be used to dispatch requests to different
/// handler functions via configurable routes
pub struct Router {
  pub methods: HashMap<Method, Node<Endpoint>>,
}

impl Default for Router {
  fn default() -> Self {
    Router {
      methods: HashMap::with_capacity(7),
    }
  }
}

// The route returned by `lookup`
pub struct Route<'a, T> {
  pub endpoint: &'a T,
  pub params: Params,
}

impl Router {
  /// Add registers a new request handle with the given path and method.
  /// For GET, POST, PUT, PATCH and DELETE requests, the respective shortcut
  /// functions can be used.
  pub fn add(&mut self, method: Method, path: &str, handle: Endpoint) {
    if !path.starts_with('/') {
      panic!("path must begin with '/' in path '{}'", path);
    }

    self
      .methods
      .entry(method)
      .or_insert_with(Node::default)
      .add_route(path, handle);
  }

  /// Allows the manual lookup of a method and path.
  /// Returns the handle function and the path parameter values if the path is found.
  /// If no handle can be found, a TSR (trailing slash redirect) recommendation is
  /// made if a handle exists with an extra (without the) trailing slash for the
  /// given path.
  pub fn lookup(&mut self, method: &Method, path: &str) -> Result<Route<Endpoint>, bool> {
    self
      .methods
      .get_mut(method)
      .map(|n| n.get_value(path))
      .unwrap_or(Err(false))
  }

  /// returns a list of the allowed methods for a specific path
  /// eg: 'GET, PATCH, OPTIONS'
  pub fn allowed(&self, path: &str, req_method: Method) -> String {
    let mut allowed: Vec<Method> = Vec::new();
    match path {
      "*" => {
        for method in self.methods.keys() {
          if method != Method::OPTIONS {
            allowed.push(method.clone());
          }
        }
      }
      _ => {
        for method in self.methods.keys() {
          if method == req_method || method == Method::OPTIONS {
            continue;
          }
          if let Some(tree) = self.methods.get(&method.clone()) {
            if tree.get_value(path).is_ok() {
              allowed.push(method.clone());
            }
          };
        }
      }
    };

    if !allowed.is_empty() {
      allowed.push(Method::OPTIONS)
    }

    allowed.iter().map(|method| method.to_string()).collect()
  }
}

/// Param is a single URL parameter, consisting of a key and a value.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
  pub key: String,
  pub value: String,
}

impl Param {
  pub fn new(key: &str, value: &str) -> Param {
    Param {
      key: key.to_string(),
      value: value.to_string(),
    }
  }
}

/// Params is a Param-slice, as returned by the router.
/// The slice is ordered, the first URL parameter is also the first slice value.
/// It is therefore safe to read values by the index.
#[derive(Debug, PartialEq)]
pub struct Params(pub Vec<Param>);

impl Default for Params {
  fn default() -> Self {
    Params(Vec::new())
  }
}

impl Params {
  /// ByName returns the value of the first Param which key matches the given name.
  /// If no matching Param is found, an empty string is returned.
  pub fn by_name(&self, name: &str) -> Option<&str> {
    match self.0.iter().find(|param| param.key == name) {
      Some(param) => Some(&param.value),
      None => None,
    }
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn push(&mut self, p: Param) {
    self.0.push(p);
  }
}

impl Index<usize> for Params {
  type Output = str;

  fn index(&self, i: usize) -> &Self::Output {
    &(self.0)[i].value
  }
}

#[derive(PartialEq, PartialOrd, Debug)]
pub enum NodeType {
  Static,
  Root,
  Param,
  CatchAll,
}

/// A node in radix tree ordered by priority
/// priority is just the number of values registered in sub nodes
/// (children, grandchildren, and so on..).
// #[derive(Debug)]
pub struct Node<T> {
  path: Vec<u8>,
  wild_child: bool,
  node_type: NodeType,
  indices: Vec<u8>,
  children: Vec<Box<Node<T>>>,
  value: Option<T>,
  priority: u32,
}

impl<T> Default for Node<T> {
  fn default() -> Self {
    Node {
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
  /// increments priority of the given child and reorders if necessary
  /// returns the new position (index) of the child
  fn increment_child_prio(&mut self, pos: usize) -> usize {
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
        &self.indices[pos..pos + 1], // the index char we move
        &self.indices[new_pos..pos], // rest without char at 'pos'
        &self.indices[pos + 1..],
      ]
      .concat();
    }

    new_pos
  }

  /// add_route adds a node with the given value to the path.
  pub fn add_route(&mut self, path: &str, value: T) {
    let full_path = <&str>::clone(&path);
    self.priority += 1;

    // Empty tree
    if self.path.is_empty() && self.children.is_empty() {
      self.insert_child(path.as_ref(), full_path, value);
      self.node_type = NodeType::Root;
      return;
    }
    self.add_route_helper(path.as_ref(), full_path, value);
  }

  fn add_route_helper(&mut self, mut path: &[u8], full_path: &str, value: T) {
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
      let mut child = Node {
        path: self.path[i..].to_vec(),
        wild_child: self.wild_child,
        indices: self.indices.clone(),
        value: self.value.take(),
        priority: self.priority - 1,
        ..Node::default()
      };

      mem::swap(&mut self.children, &mut child.children);

      self.children = vec![Box::new(child)];
      self.indices = vec![self.path[i]];
      self.path = path[..i].to_vec();
      self.wild_child = false;
      self.value = None;
    }

    // Make new node a child of this node
    match path.len().cmp(&i) {
      Ordering::Greater => {
        path = &path[i..];

        if self.wild_child {
          return self.children[0].wild_child_conflict(path, full_path, value);
        }

        let idxc = path[0];

        // `/` after param
        if self.node_type == NodeType::Param && idxc == b'/' && self.children.len() == 1 {
          self.children[0].priority += 1;
          return self.children[0].add_route_helper(path, full_path, value);
        }

        // Check if a child with the next path byte exists
        for mut i in 0..self.indices.len() {
          if idxc == self.indices[i] {
            i = self.increment_child_prio(i);
            return self.children[i].add_route_helper(path, full_path, value);
          }
        }

        // Otherwise insert it
        if idxc != b':' && idxc != b'*' {
          self.indices.push(idxc);

          self.children.push(Box::new(Node::default()));

          let i = self.increment_child_prio(self.indices.len() - 1);
          return self.children[i].insert_child(path, full_path, value);
        }

        self.insert_child(path, full_path, value)
      }
      _ => {
        // Otherwise add value to current node
        if self.value.is_some() {
          panic!("a value is already registered for path '{}'", full_path);
        }

        self.value = Some(value);
      }
    }
  }

  fn wild_child_conflict(&mut self, path: &[u8], full_path: &str, value: T) {
    self.priority += 1;

    // Check if the wildcard matches
    if path.len() >= self.path.len()
      && self.path == &path[..self.path.len()]
      // Adding a child to a CatchAll Node is not possible
      && self.node_type != NodeType::CatchAll
      // Check for longer wildcard, e.g. :name and :names
      && (self.path.len() >= path.len() || path[self.path.len()] == b'/')
    {
      self.add_route_helper(path, full_path, value);
    } else {
      // Wildcard conflict
      let path_seg = if self.node_type == NodeType::CatchAll {
        str::from_utf8(path).unwrap()
      } else {
        str::from_utf8(path).unwrap().splitn(2, '/').next().unwrap()
      };

      let prefix = format!(
        "{}{}",
        &full_path[..full_path.find(path_seg).unwrap()],
        str::from_utf8(&self.path).unwrap(),
      );

      panic!(
        "'{}' in new path '{}' conflicts with existing wildcard '{}' in existing prefix '{}'",
        path_seg,
        full_path,
        str::from_utf8(&self.path).unwrap(),
        prefix
      );
    }
  }

  fn insert_child(&mut self, mut path: &[u8], full_path: &str, value: T) {
    let (wildcard, wildcard_index, valid) = find_wildcard(path);

    let wildcard = match wildcard_index {
      Some(_) => wildcard.unwrap(),
      // No wilcard found
      None => {
        self.value = Some(value);
        self.path = path.to_vec();
        return;
      }
    };

    let mut wildcard_index = wildcard_index.unwrap();

    // the wildcard name must not contain ':' and '*'
    if !valid {
      panic!(
        "only one wildcard per path segment is allowed, has: '{}' in path '{}'",
        str::from_utf8(wildcard).unwrap(),
        full_path
      );
    };

    // check if the wildcard has a name
    if wildcard.len() < 2 {
      panic!(
        "wildcards must be named with a non-empty name in path '{}'",
        full_path
      );
    }

    // check if this Node existing children which would be
    // unreachable if we insert the wildcard here
    if !self.children.is_empty() {
      panic!(
        "wildcard segment '{}' conflicts with existing children in path '{}'",
        str::from_utf8(wildcard).unwrap(),
        full_path
      )
    }

    // Param
    if wildcard[0] == b':' {
      // Insert prefix before the current wildcard
      if wildcard_index > 0 {
        self.path = path[..wildcard_index].to_vec();
        path = &path[wildcard_index..];
      }

      let child = Node {
        node_type: NodeType::Param,
        path: wildcard.to_vec(),
        ..Node::default()
      };

      self.wild_child = true;
      self.children = vec![Box::new(child)];
      self.children[0].priority += 1;

      // If the path doesn't end with the wildcard, then there
      // will be another non-wildcard subpath starting with '/'

      if wildcard.len() < path.len() {
        path = &path[wildcard.len()..];
        let child = Node {
          priority: 1,
          ..Node::default()
        };

        self.children[0].children = vec![Box::new(child)];
        return self.children[0].children[0].insert_child(path, full_path, value);
      }
      // Otherwise we're done. Insert the value in the new leaf
      self.children[0].value = Some(value);
      return;
    }

    // catch all
    if wildcard_index + wildcard.len() != path.len() {
      panic!(
        "catch-all routes are only allowed at the end of the path in path '{}'",
        full_path
      );
    }

    if !self.path.is_empty() && self.path[self.path.len() - 1] == b'/' {
      panic!(
        "catch-all conflicts with existing value for the path segment root in path '{}'",
        full_path
      );
    }

    // Currently fixed width 1 for '/'
    wildcard_index -= 1;
    if path[wildcard_index] != b'/' {
      panic!("no / before catch-all in path '{}'", full_path);
    }

    // first node: CatchAll Node with empty path
    let child = Node {
      wild_child: true,
      node_type: NodeType::CatchAll,
      ..Node::default()
    };

    self.path = path[..wildcard_index].to_vec();
    self.children = vec![Box::new(child)];
    self.indices = vec![b'/'];
    self.children[0].priority += 1;

    // Second node: node holding the variable
    let child = Node {
      path: path[wildcard_index..].to_vec(),
      node_type: NodeType::CatchAll,
      value: Some(value),
      priority: 1,
      ..Node::default()
    };

    self.children[0].children = vec![Box::new(child)];
  }

  /// Returns the value registered with the given path (key). The values of
  /// wildcards are saved to a map.
  /// If no value can be found, a TSR (trailing slash redirect) recommendation is
  /// made if a value exists with an extra (without the) trailing slash for the
  /// given path.
  pub fn get_value(&self, path: &str) -> Result<Route<T>, bool> {
    self.get_value_helper(path.as_ref(), Params::default())
  }

  // outer loop for walking the tree to get a path's value
  fn get_value_helper(&self, mut path: &[u8], ps: Params) -> Result<Route<T>, bool> {
    let prefix = self.path.clone();
    if path.len() > prefix.len() {
      if prefix == &path[..prefix.len()] {
        path = &path[prefix.len()..];

        // If this node does not have a wildcard (Param or CatchAll)
        // child, we can just look up the next child node and continue
        // to walk down the tree
        if !self.wild_child {
          let idxc = path[0];
          for i in 0..self.indices.len() {
            if idxc == self.indices[i] {
              return self.children[i].get_value_helper(path, ps);
            }
          }
          // Nothing found.
          // We can recommend to redirect to the same URL without a
          // trailing slash if a leaf exists for that path.
          let tsr = path == [b'/'] && self.value.is_some();
          return Err(tsr);
        }

        return self.children[0].handle_wild_child(path, ps);
      }
    } else if path == prefix {
      // We should have reached the node containing the value.
      // Check if this node has a value registered.
      if let Some(endpoint) = self.value.as_ref() {
        return Ok(Route {
          endpoint,
          params: ps,
        });
      }

      // If there is no value for this route, but this route has a
      // wildcard child, there must be a value for this path with an
      // additional trailing slash
      if path == [b'/'] && self.wild_child && self.node_type != NodeType::Root {
        return Err(true);
      }

      // No value found. Check if a value for this path + a
      // trailing slash exists for trailing slash recommendation
      for i in 0..self.indices.len() {
        if self.indices[i] == b'/' {
          let tsr = (prefix.len() == 1 && self.children[i].value.is_some())
            || (self.children[i].node_type == NodeType::CatchAll
              && self.children[i].children[0].value.is_some());
          return Err(tsr);
        }
      }

      return Err(false);
    }

    // Nothing found. We can recommend to redirect to the same URL with an
    // extra trailing slash if a leaf exists for that path
    let tsr = (path == [b'/'])
      || (prefix.len() == path.len() + 1
        && prefix[path.len()] == b'/'
        && path == &prefix[..prefix.len() - 1]
        && self.value.is_some());

    Err(tsr)
  }

  // helper function for handling a wildcard child used by `get_value`
  fn handle_wild_child(&self, mut path: &[u8], mut p: Params) -> Result<Route<T>, bool> {
    match self.node_type {
      NodeType::Param => {
        // find param end (either '/' or path end)
        let mut end = 0;
        while end < path.len() && path[end] != b'/' {
          end += 1;
        }

        p.push(Param {
          key: String::from_utf8(self.path[1..].to_vec()).unwrap(),
          value: String::from_utf8(path[..end].to_vec()).unwrap(),
        });

        // we need to go deeper!
        if end < path.len() {
          if !self.children.is_empty() {
            path = &path[end..];

            return self.children[0].get_value_helper(path, p);
          }

          // ... but we can't
          let tsr = path.len() == end + 1;
          return Err(tsr);
        }

        if let Some(endpoint) = self.value.as_ref() {
          return Ok(Route {
            endpoint,
            params: p,
          });
        } else if self.children.len() == 1 {
          // No value found. Check if a value for this path + a
          // trailing slash exists for TSR recommendation
          let tsr = self.children[0].path == [b'/'] && self.children[0].value.is_some();
          return Err(tsr);
        }

        Err(false)
      }
      NodeType::CatchAll => {
        p.push(Param {
          key: String::from_utf8(self.path[2..].to_vec()).unwrap(),
          value: String::from_utf8(path.to_vec()).unwrap(),
        });

        match self.value.as_ref() {
          Some(endpoint) => Ok(Route {
            endpoint,
            params: p,
          }),
          None => Err(false),
        }
      }
      _ => panic!("invalid node type"),
    }
  }

  pub fn find_case_insensitive_path(&self, path: &str, fix_trailing_slash: bool) -> Option<String> {
    let mut insensitive_path = Vec::with_capacity(path.len() + 1);
    let found = self.find_case_insensitive_path_helper(
      path.as_bytes(),
      &mut insensitive_path,
      [0; 4],
      fix_trailing_slash,
    );
    match found {
      true => Some(String::from_utf8(insensitive_path).unwrap()),
      false => None,
    }
  }

  // recursive case-insensitive lookup function used by n.find_case_insensitive_path
  fn find_case_insensitive_path_helper(
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
      insensitive_path.append(&mut self.path.clone());

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
                return self.children[i].find_case_insensitive_path_helper(
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
                if self.children[i].find_case_insensitive_path_helper(
                  path,
                  insensitive_path,
                  buf,
                  fix_trailing_slash,
                ) {
                  return true;
                }

                if insensitive_path.len() > self.children[i].path.len() {
                  let prev_len = insensitive_path.len() - self.children[i].path.len();
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
                  return self.children[i].find_case_insensitive_path_helper(
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

        return self.children[0].find_case_insensitive_path_match_helper(
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
              if (self.children[i].path.len() == 1 && self.children[i].value.is_some())
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
        insensitive_path.append(&mut self.path.clone());
        return true;
      }
    }

    false
  }

  // recursive case-insensitive lookup function used by n.findCaseInsensitivePath
  fn find_case_insensitive_path_match_helper(
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

        let mut path_k = path[..end].to_vec();
        insensitive_path.append(&mut path_k);

        if end < path.len() {
          if !self.children.is_empty() {
            path = &path[end..];

            return self.children[0].find_case_insensitive_path_helper(
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
        insensitive_path.append(&mut path.to_vec());
        true
      }
      _ => panic!("invalid node type"),
    }
  }
}

// Shift bytes in array by n bytes left
fn shift_n_bytes(bytes: [u8; 4], n: usize) -> [u8; 4] {
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
fn char_start(b: u8) -> bool {
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
  use std::panic;
  use std::sync::Mutex;

  struct TestRequest {
    path: &'static str,
    nil_value: bool,
    route: &'static str,
    ps: Params,
  }

  impl TestRequest {
    pub fn new(
      path: &'static str,
      nil_value: bool,
      route: &'static str,
      ps: Params,
    ) -> TestRequest {
      TestRequest {
        path,
        nil_value,
        route,
        ps,
      }
    }
  }

  type TestRequests = Vec<TestRequest>;

  fn check_requests<T: Fn() -> String>(tree: &mut Node<T>, requests: TestRequests) {
    for request in requests {
      let res = tree.get_value(request.path);

      match res {
        Err(_) => {
          if !request.nil_value {
            panic!("Expected non-nil value for route '{}'", request.path);
          }
        }
        Ok(route) => {
          if request.nil_value {
            panic!("Expected nil value for route '{}'", request.path);
          }
          let res = (route.endpoint)();
          if res != request.route {
            panic!(
              "Wrong value for route '{}'. Expected '{}', found '{}')",
              request.path, res, request.route
            );
          }
          if route.params != request.ps {
            panic!("Wrong params for route '{}'", request.path);
          }
        }
      };
    }
  }

  fn check_priorities<T: Fn() -> String>(n: &mut Node<T>) -> u32 {
    let mut prio: u32 = 0;
    for i in 0..n.children.len() {
      prio += check_priorities(&mut *n.children[i]);
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

  fn fake_value(val: &'static str) -> impl Fn() -> String {
    move || val.to_string()
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

    assert_eq!(params.by_name("hello"), Some("world"));
    assert_eq!(params.by_name("rust-is"), Some("awesome"));
  }

  #[test]
  fn test_tree_add_and_get() {
    let mut tree = Node::default();

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
      "/α",
      "/β",
    ];

    for route in routes {
      tree.add_route(route, fake_value(route));
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
        TestRequest::new("/α", false, "/α", Params::default()),
        TestRequest::new("/β", false, "/β", Params::default()),
      ],
    );

    check_priorities(&mut tree);
  }

  #[test]
  fn test_tree_wildcard() {
    let mut tree = Node::default();

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
      tree.add_route(route, fake_value(route));
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
    let tree = Mutex::new(Node::default());

    for route in routes {
      let recv = panic::catch_unwind(|| {
        let mut guard = match tree.lock() {
          Ok(guard) => guard,
          Err(poisoned) => poisoned.into_inner(),
        };
        guard.add_route(route.0, ());
      });

      if route.1 {
        if recv.is_ok() {
          // panic!("no panic for conflicting route '{}'", route.0);
        }
      } else if recv.is_err() {
        panic!("unexpected panic for route '{}': {:?}", route.0, recv);
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
    let tree = Mutex::new(Node::default());

    let routes = vec![
      "/",
      "/doc/",
      "/src/*filepath",
      "/search/:query",
      "/user_:name",
    ];

    for route in routes {
      let mut recv = panic::catch_unwind(|| {
        let mut guard = match tree.lock() {
          Ok(guard) => guard,
          Err(poisoned) => poisoned.into_inner(),
        };
        guard.add_route(route, fake_value(route));
      });

      if recv.is_err() {
        panic!("panic inserting route '{}': {:?}", route, recv);
      }

      recv = panic::catch_unwind(|| {
        let mut guard = match tree.lock() {
          Ok(guard) => guard,
          Err(poisoned) => poisoned.into_inner(),
        };
        guard.add_route(route, fake_value(route));
      });

      if recv.is_ok() {
        panic!("no panic while inserting duplicate route '{}'", route);
      }
    }

    check_requests(
      &mut tree.lock().unwrap_or_else(|poisoned| poisoned.into_inner()),
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
    let tree = Mutex::new(Node::default());
    let routes = vec!["/user:", "/user:/", "/cmd/:/", "/src/*"];

    for route in routes {
      let recv = panic::catch_unwind(|| {
        let mut guard = match tree.lock() {
          Ok(guard) => guard,
          Err(poisoned) => poisoned.into_inner(),
        };
        guard.add_route(route, fake_value(route));
      });

      if recv.is_ok() {
        panic!(
          "no panic while inserting route with empty wildcard name '{}",
          route
        );
      }
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
    let panic_msg = "only one wildcard per path segment is allowed";
    let routes = vec!["/:foo:bar", "/:foo:bar/", "/:foo*bar"];

    for route in routes {
      let tree = Mutex::new(Node::default());
      let recv = panic::catch_unwind(|| {
        let mut guard = match tree.lock() {
          Ok(guard) => guard,
          Err(poisoned) => poisoned.into_inner(),
        };
        guard.add_route(route, fake_value(route));
      });

      // [TODO] Check `recv`
      if recv.is_ok() {
        panic!(panic_msg);
      }
    }
  }

  #[test]
  fn test_tree_trailing_slash_redirect() {
    let tree = Mutex::new(Node::default());
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
      let recv = panic::catch_unwind(|| {
        let mut guard = match tree.lock() {
          Ok(guard) => guard,
          Err(poisoned) => poisoned.into_inner(),
        };
        guard.add_route(route, fake_value(route));
      });

      if recv.is_err() {
        panic!("panic inserting route '{}': {:?}", route, recv);
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
      let guard = match tree.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
      };
      let res = guard.get_value(route);

      match res {
        Ok(_) => {
          panic!("non-nil value for TSR route '{}'", route);
        }
        Err(tsr) => {
          if !tsr {
            panic!("expected TSR recommendation for route '{}'", route);
          }
        }
      }
    }

    let no_tsr_routes = vec!["/", "/no", "/no/", "/_", "/_/", "/api/world/abc"];

    for route in no_tsr_routes {
      let guard = match tree.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
      };
      let res = guard.get_value(route);

      match res {
        Ok(_) => {
          panic!("non-nil value for TSR route '{}'", route);
        }
        Err(tsr) => {
          if tsr {
            panic!("expected no TSR recommendation for route '{}'", route);
          }
        }
      }
    }
  }

  #[test]
  fn test_tree_root_trailing_slash_redirect() {
    let mut tree = Node::default();

    tree.add_route("/:test", fake_value("/:test"));

    let res = tree.get_value("/");

    match res {
      Ok(_) => {
        panic!("non-nil value for route '/'");
      }
      Err(tsr) => {
        if tsr {
          panic!("expected no TSR recommendation for route '/'");
        }
      }
    }
  }

  #[test]
  fn test_tree_find_case_insensitive_path() {
    let mut tree = Node::default();

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
      tree.add_route(route, fake_value(route));
    }

    // Check out == in for all registered routes
    // With fixTrailingSlash = true
    for route in &routes {
      let out = tree.find_case_insensitive_path(route, true);
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
      let out = tree.find_case_insensitive_path(route, false);
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
        true),
    ];

    struct Test {
      inn: &'static str,
      out: &'static str,
      slash: bool,
    };

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
      let res = tree.find_case_insensitive_path(test.inn, true);
      match res {
        None => (),
        Some(res) => {
          if res != test.out {
            panic!("Wrong result for route '{}': {}", res, test.out);
          }
        }
      };
    }

    // With fixTrailingSlash = false
    for test in &tests {
      let res = tree.find_case_insensitive_path(test.inn, false);
      match res {
        None => (),
        Some(res) => {
          if test.slash {
            // test needs a trailingSlash fix. It must not be found!
            panic!("Found without fixTrailingSlash: {}; got {}", test.inn, res);
          }
          if res != test.out {
            panic!("Wrong result for route '{}': {}", res, test.out);
          }
        }
      };
    }
  }

  #[test]
  #[should_panic(expected = "conflicts with existing wildcard")]
  fn test_tree_wildcard_conflict_ex() {
    let conflicts = vec![
      "/who/are/foo",
      "/who/are/foo/",
      "/who/are/foo/bar",
      "/conxxx",
      "xxx",
      "/conooo/xxx",
    ];

    for conflict in conflicts {
      // I have to re-create a 'tree', because the 'tree' will be
      // in an inconsistent state when the loop recovers from the
      // panic which threw by 'addRoute' function.
      let mut tree = Node::default();

      let routes = vec!["/con:tact", "/who/are/*you", "/who/foo/hello"];

      for route in routes {
        tree.add_route(route, fake_value(route));
      }
      tree.add_route(conflict, fake_value(conflict));
    }
  }

  // #[test]
  // [TODO]
  // #[should_panic(expected = "path must begin with '/' in path 'invalid'")]
  // fn handle_invalid_path() {
  // use crate::request::Request;
  // use crate::router::{Params, Router};
  // use hyper::{Body, Method, Response};

  // let mut router = Router::default();

  // TODO
  // router.handle(
  //   Method::GET,
  //   "invalid",
  //   |_req: Request, _: Params| -> Response<Body> { Response::new(Body::from("test")) },
  // );
  // }
}
