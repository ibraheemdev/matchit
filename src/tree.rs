use std::cmp::{min, Ordering};
use std::ops::Index;
use std::mem;
use std::str;

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
 // ByName returns the value of the first Param which key matches the given name.
 // If no matching Param is found, an empty string is returned.
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

fn count_params(path: &[u8]) -> u8 {
  let mut n: usize = 0;
  for &c in path {
    if c != b':' && c != b'*' {
      continue;
    }
    n += 1;
  }
  match n.cmp(&255) {
    Ordering::Greater => 255,
    _ => n as u8,
  }
}

#[derive(PartialEq, PartialOrd, Debug)]
pub enum NodeType {
  Static,
  Root,
  Param,
  CatchAll,
}

// A node in radix tree ordered by priority
// priority is just the number of handles registered in sub nodes 
// (children, grandchildren, and so on..).
#[derive(Debug)]
pub struct Node<T> {
  path: Vec<u8>,
  wild_child: bool,
  node_type: NodeType,
  max_params: u8,
  indices: Vec<u8>,
  children: Vec<Box<Node<T>>>,
  handle: Option<T>,
  priority: u32,
}

impl<T> Default for Node<T> {
  fn default() -> Self {
    Node {
      path: Vec::new(),
      wild_child: false,
      node_type: NodeType::Static,
      max_params: 0,
      indices: Vec::new(),
      children: Vec::new(),
      handle: None,
      priority: 0,
    }
  }
}
impl<T> Node<T> {
 // increments priority of the given child and reorders if necessary
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

 // add_route adds a node with the given handle to the path.
  pub fn add_route(&mut self, path: &str, handle: T) {
    let full_path = <&str>::clone(&path);
    self.priority += 1;
    let num_params = count_params(path.as_ref());

    // Empty tree
    if self.path.is_empty() && self.children.is_empty() {
      self.insert_child(num_params, path.as_ref(), full_path, handle);
      self.node_type = NodeType::Root;
      return;
    }
    self.walk_route(num_params, path.as_ref(), full_path, handle);
  }

  fn walk_route(&mut self, num_params: u8, mut path: &[u8], full_path: &str, handle: T) {
    // Update maximum params of the current node
    if num_params > self.max_params {
      self.max_params = num_params;
    }

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
        handle: self.handle.take(),
        priority: self.priority - 1,
        ..Node::default()
      };

      mem::swap(&mut self.children, &mut child.children);

      // Update grandchildren's max_params
      for grandchild in &child.children {
        if grandchild.max_params > child.max_params {
          child.max_params = grandchild.max_params;
        }
      }

      self.children = vec![Box::new(child)];
      self.indices = vec![self.path[i]];
      self.path = path[..i].to_vec();
      self.wild_child = false;
    }

    // Make new node a child of this node
    match path.len().cmp(&i) {
      Ordering::Greater => {
        path = &path[i..];

        if self.wild_child {
          return self.children[0].is_wild_child(num_params, path, full_path, handle);
        }

        let idxc = path[0];

        // `/` after param
        if self.node_type == NodeType::Param && idxc == b'/' && self.children.len() == 1 {
          self.children[0].priority += 1;
          return self.children[0].walk_route(num_params, path, full_path, handle);
        }

        // Check if a child with the next path byte exists
        for mut i in 0..self.indices.len() {
          if idxc == self.indices[i] {
            i = self.increment_child_prio(i);
            return self.children[i].walk_route(num_params, path, full_path, handle);
          }
        }

        // Otherwise insert it
        if idxc != b':' && idxc != b'*' {
          self.indices.push(idxc);

          let child = Node::<T> {
            max_params: num_params,
            ..Node::default()
          };
          self.children.push(Box::new(child));

          let i = self.increment_child_prio(self.indices.len() - 1);
          return self.children[i].insert_child(num_params, path, full_path, handle);
        }

        self.insert_child(num_params, path, full_path, handle)
      }
      Ordering::Equal => {
        // Make node a (in-path) leaf
        if self.handle.is_some() {
          panic!("a handle is already registered for path '{}'", full_path);
        }

        self.handle = Some(handle);
      }
      _ => (),
    }
  }

  fn is_wild_child(&mut self, mut num_params: u8, path: &[u8], full_path: &str, handle: T) {
    self.priority += 1;

    // Update maximum params of the child node
    if num_params > self.max_params {
      self.max_params = num_params;
    }

    // Check if the wildcard matches
    if path.len() >= self.path.len()
      && self.path == &path[..self.path.len()]
      // Adding a child to a CatchAll Node is not possible
      && self.node_type != NodeType::CatchAll
      // Check for longer wildcard, e.g. :name and :names
      && (self.path.len() >= path.len() || path[self.path.len()] == b'/')
    {
      num_params -= 1;
      self.walk_route(num_params, path, full_path, handle);
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

  fn insert_child(&mut self, num_params: u8, path: &[u8], full_path: &str, handle: T) {
    self.walk_tree_for_child(0, 0, num_params, path, full_path, handle);
  }

  // walking the tree to insert a child
  fn walk_tree_for_child(
    &mut self,
    mut offset: usize,
    mut i: usize,
    mut num_params: u8,
    path: &[u8],
    full_path: &str,
    handle: T,
  ) {
    match num_params.cmp(&0) {
      Ordering::Greater => {
        let max = path.len();
        let wildcard = path[i];

        // find prefix until first wildcard
        if wildcard != b':' && wildcard != b'*' {
          return self.walk_tree_for_child(offset, i + 1, num_params, path, full_path, handle);
        }

        // find wildcard end (either '/' or path end)
        let mut end = i + 1;
        while end < max && path[end] != b'/' {
          match path[end] {
            // the wildcard name must not contain ':' and '*'
            b':' | b'*' => panic!(
              "only one wildcard per path segment is allowed, has: '{}' in path '{}'",
              str::from_utf8(&path[i..]).unwrap(),
              full_path
            ),
            _ => end += 1,
          }
        }

        // check if this Node existing children which would be
        // unreachable if we insert the wildcard here
        if !self.children.is_empty() {
          panic!(
            "wildcard route '{}' conflicts with existing children in path '{}'",
            str::from_utf8(&path[i..end]).unwrap(),
            full_path
          )
        }

        // check if the wildcard has a name
        if end - i < 2 {
          panic!(
            "wildcards must be named with a non-empty name in path '{}'",
            full_path
          );
        }

        // Param
        if wildcard == b':' {
          // Insert prefix before the current wildcard
          if i > 0 {
            self.path = path[offset..i].to_vec();
            offset = i;
          }

          let child = Node {
            node_type: NodeType::Param,
            max_params: num_params,
            ..Node::default()
          };

          self.wild_child = true;
          self.children = vec![Box::new(child)];
          self.children[0].priority += 1;
          num_params -= 1;

          match end.cmp(&max) {
            Ordering::Less => {
              self.children[0].path = path[offset..end].to_vec();
              offset = end;

              let child = Node {
                max_params: num_params,
                priority: 1,
                ..Node::default()
              };

              self.children[0].children.push(Box::new(child));
              self.children[0].children[0].walk_tree_for_child(
                offset,
                i + 1,
                num_params,
                path,
                full_path,
                handle,
              );
            }
            _ => {
              self.children[0].walk_tree_for_child(
                offset,
                i + 1,
                num_params,
                path,
                full_path,
                handle,
              );
            }
          };
        } else {
          // catch all
          if end != max || num_params > 1 {
            panic!(
              "catch-all routes are only allowed at the end of the path in path '{}'",
              full_path
            );
          }

          if !self.path.is_empty() && self.path[self.path.len() - 1] == b'/' {
            panic!(
              "catch-all conflicts with existing handle for the path segment root in path '{}'",
              full_path
            );
          }

          // Currently fixed width 1 for '/'
          i -= 1;
          if path[i] != b'/' {
            panic!("no / before catch-all in path '{}'", full_path);
          }

          // first node: CatchAll Node with empty path
          let child = Node {
            wild_child: true,
            node_type: NodeType::CatchAll,
            max_params: 1,
            ..Node::default()
          };

          self.path = path[offset..i].to_vec();
          self.children = vec![Box::new(child)];
          self.indices = vec![path[i]];
          self.children[0].priority += 1;

          // Second node: node holding the variable
          let child = Node {
            path: path[i..].to_vec(),
            node_type: NodeType::CatchAll,
            max_params: 1,
            handle: Some(handle),
            priority: 1,
            ..Node::default()
          };

          self.children[0].children.push(Box::new(child));
        }
      }
      // If no wildcard was found, simply insert the path and handle
      _ => {
        self.path = path[offset..].to_vec();
        self.handle = Some(handle);
      }
    }
  }
}