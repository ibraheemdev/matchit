use std::cmp::{min, Ordering};
use std::mem;
use std::ops::Index;
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

  // Returns the handle registered with the given path (key). The values of
  // wildcards are saved to a map.
  // If no handle can be found, a TSR (trailing slash redirect) recommendation is
  // made if a handle exists with an extra (without the) trailing slash for the
  // given path.
  pub fn get_handler(&self, path: &str) -> (Option<&T>, Params, bool) {
    self.walk_tree_for_handler(path.as_ref(), Params::default())
  }

  // outer loop for walking the tree to get a path's handler
  fn walk_tree_for_handler(&self, mut path: &[u8], ps: Params) -> (Option<&T>, Params, bool) {
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
              return self.children[i].walk_tree_for_handler(path, ps);
            }
          }
          // Nothing found.
          // We can recommend to redirect to the same URL without a
          // trailing slash if a leaf exists for that path.
          let tsr = path == [b'/'] && self.handle.is_some();
          return (None, ps, tsr);
        }

        return self.children[0].handle_wild_child(path, ps);
      }
    } else if path == prefix {
      // We should have reached the node containing the handle.
      // Check if this node has a handle registered.
      if self.handle.is_some() {
        return (self.handle.as_ref(), ps, false);
      }

      // If there is no handle for this route, but this route has a
      // wildcard child, there must be a handle for this path with an
      // additional trailing slash
      if path == [b'/'] && self.wild_child && self.node_type != NodeType::Root {
        return (self.handle.as_ref(), ps, true);
      }

      // No handle found. Check if a handle for this path + a
      // trailing slash exists for trailing slash recommendation
      for i in 0..self.indices.len() {
        if self.indices[i] == b'/' {
          let tsr = (prefix.len() == 1 && self.children[i].handle.is_some())
            || (self.children[i].node_type == NodeType::CatchAll
              && self.children[i].children[0].handle.is_some());
          return (self.handle.as_ref(), ps, tsr);
        }
      }

      return (self.handle.as_ref(), ps, false);
    }

    // Nothing found. We can recommend to redirect to the same URL with an
    // extra trailing slash if a leaf exists for that path
    let tsr = (path == [b'/'])
      || (prefix.len() == path.len() + 1
        && prefix[path.len()] == b'/'
        && path == &prefix[..prefix.len() - 1]
        && self.handle.is_some());

    (None, ps, tsr)
  }

  fn handle_wild_child(&self, mut path: &[u8], mut p: Params) -> (Option<&T>, Params, bool) {
    match self.node_type {
      NodeType::Param => {
        // find param end (either '/' or path end)
        let mut end = 0;
        while end < path.len() && path[end] != b'/' {
          end += 1;
        }

        // save param value
        if p.is_empty() {
          // preallocate capacity
          p = Params(Vec::with_capacity(self.max_params as usize));
        }

        p.push(Param {
          key: String::from_utf8(self.path[1..].to_vec()).unwrap(),
          value: String::from_utf8(path[..end].to_vec()).unwrap(),
        });

        // we need to go deeper!
        if end < path.len() {
          if !self.children.is_empty() {
            path = &path[end..];

            return self.children[0].walk_tree_for_handler(path, p);
          }

          // ... but we can't
          let tsr = path.len() == end + 1;
          return (None, p, tsr);
        }

        if self.handle.is_some() {
          return (self.handle.as_ref(), p, false);
        } else if self.children.len() == 1 {
          // No handle found. Check if a handle for this path + a
          // trailing slash exists for TSR recommendation
          let tsr = self.children[0].path == [b'/'] && self.children[0].handle.is_some();
          return (None, p, tsr);
        }

        (None, p, false)
      }
      NodeType::CatchAll => {
        // save param value
        if p.is_empty() {
          // lazy allocation
          p = Params(Vec::with_capacity(self.max_params as usize));
        }

        p.push(Param {
          key: String::from_utf8(self.path[2..].to_vec()).unwrap(),
          value: String::from_utf8(path.to_vec()).unwrap(),
        });

        (self.handle.as_ref(), p, false)
      }
      _ => panic!("invalid node type"),
    }
  }

  pub fn find_case_insensitive_path(&self, path: &str, fix_trailing_slash: bool) -> (String, bool) {
    let mut insensitive_path = Vec::with_capacity(path.len() + 1);
    let found = self.find_case_insensitive_path_rec(
      path.as_bytes(),
      &mut insensitive_path,
      [0; 4],
      fix_trailing_slash,
    );
    (String::from_utf8(insensitive_path).unwrap(), found)
  }

  // recursive case-insensitive lookup function used by n.find_case_insensitive_path
  fn find_case_insensitive_path_rec(
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
                return self.children[i].find_case_insensitive_path_rec(
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
                if self.children[i].find_case_insensitive_path_rec(
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
                  return self.children[i].find_case_insensitive_path_rec(
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
          return fix_trailing_slash && path == [b'/'] && self.handle.is_some();
        }

        return self.children[0].find_case_insensitive_path_rec_match(
          path,
          insensitive_path,
          buf,
          fix_trailing_slash,
        );
      } else {
        // We should have reached the node containing the handle.
        // Check if this node has a handle registered.
        if self.handle.is_some() {
          return true;
        }

        // No handle found.
        // Try to fix the path by adding a trailing slash
        if fix_trailing_slash {
          for i in 0..self.indices.len() {
            if self.indices[i] == b'/' {
              if (self.children[i].path.len() == 1 && self.children[i].handle.is_some())
                || (self.children[i].node_type == NodeType::CatchAll
                  && self.children[i].children[0].handle.is_some())
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
        && self.handle.is_some()
      {
        insensitive_path.append(&mut self.path.clone());
        return true;
      }
    }

    false
  }

  // recursive case-insensitive lookup function used by n.findCaseInsensitivePath
  fn find_case_insensitive_path_rec_match(
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

            return self.children[0].find_case_insensitive_path_rec(
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

        if self.handle.is_some() {
          return true;
        } else if fix_trailing_slash
          && self.children.len() == 1
          && self.children[0].path == [b'/']
          && self.children[0].handle.is_some()
        {
          // No handle found. Check if a handle for this path + a
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

#[cfg(test)]
mod tests {
  use super::*;
  use std::panic;
  use std::sync::Mutex;

  struct TestRequest {
    path: &'static str,
    nil_handler: bool,
    route: &'static str,
    ps: Params,
  }

  impl TestRequest {
    pub fn new(
      path: &'static str,
      nil_handler: bool,
      route: &'static str,
      ps: Params,
    ) -> TestRequest {
      TestRequest {
        path,
        nil_handler,
        route,
        ps,
      }
    }
  }

  type TestRequests = Vec<TestRequest>;

  fn check_requests<T: Fn() -> String>(tree: &mut Node<T>, requests: TestRequests) {
    for request in requests {
      let (handler, ps, _) = tree.get_handler(request.path);

      if handler.is_none() {
        if !request.nil_handler {
          panic!("Expected non-nil handler for route '{}'", request.path);
        }
      } else if request.nil_handler {
        panic!("Expected nil handler for route '{}'", request.path);
      } else {
        match handler {
          Some(h) => {
            let res = h();
            if res != request.route {
              panic!(
                "Wrong handler for route '{}'. Expected '{}', found '{}')",
                request.path, res, request.route
              );
            }
          }
          None => {
            panic!("handle not found");
          }
        }
      }

      if ps != request.ps {
        panic!("Wrong params for route '{}'", request.path);
      }
    }
  }

  fn check_priorities<T: Fn() -> String>(n: &mut Node<T>) -> u32 {
    let mut prio: u32 = 0;
    for i in 0..n.children.len() {
      prio += check_priorities(&mut *n.children[i]);
    }

    if n.handle.is_some() {
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

  fn check_max_params<T: Fn() -> String>(n: &mut Node<T>) -> u8 {
    let mut max_params: u8 = 0;
    for i in 0..n.children.len() {
      let params = check_max_params(&mut *n.children[i]);

      if params > max_params {
        max_params = params;
      }
    }

    if n.node_type > NodeType::Root && !n.wild_child {
      max_params += 1;
    }

    if n.max_params != max_params {
      panic!(
        "max_params mismatch for node '{}': is '{}', should be '{}'",
        str::from_utf8(&n.path).unwrap(),
        n.max_params,
        max_params,
      )
    }

    max_params
  }

  fn fake_handler(val: &'static str) -> impl Fn() -> String {
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
  fn test_count_params() {
    assert_eq!(0, count_params(b"/path/test/other"));
    assert_eq!(2, count_params(b"/path/:param1/static/*catch-all"));
    assert_eq!(3, count_params(b"/path/:param1/:param2/*catch-all"));
    assert_eq!(255, count_params("/:param".repeat(256).as_bytes()));
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
      tree.add_route(route, fake_handler(route));
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
    check_max_params(&mut tree);
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
      tree.add_route(route, fake_handler(route));
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
    check_max_params(&mut tree);
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
          panic!("no panic for conflicting route '{}'", route.0);
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
        guard.add_route(route, fake_handler(route));
      });

      if recv.is_err() {
        panic!("panic inserting route '{}': {:?}", route, recv);
      }

      recv = panic::catch_unwind(|| {
        let mut guard = match tree.lock() {
          Ok(guard) => guard,
          Err(poisoned) => poisoned.into_inner(),
        };
        guard.add_route(route, fake_handler(route));
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
        guard.add_route(route, fake_handler(route));
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
        guard.add_route(route, fake_handler(route));
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
        guard.add_route(route, fake_handler(route));
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
      let (handler, _, tsr) = guard.get_handler(route);

      if handler.is_some() {
        panic!("non-nil handler for TSR route '{}'", route);
      } else if !tsr {
        panic!("expected TSR recommendation for route '{}'", route);
      }
    }

    let no_tsr_routes = vec!["/", "/no", "/no/", "/_", "/_/", "/api/world/abc"];

    for route in no_tsr_routes {
      let guard = match tree.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
      };
      let (handler, _, tsr) = guard.get_handler(route);

      if handler.is_some() {
        panic!("non-nil handler for TSR route '{}'", route);
      } else if tsr {
        panic!("expected TSR recommendation for route '{}'", route);
      }
    }
  }

  #[test]
  fn test_tree_root_trailing_slash_redirect() {
    let tree = Mutex::new(Node::default());

    let recv = panic::catch_unwind(|| {
      let mut guard = match tree.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
      };
      guard.add_route("/:test", fake_handler("/:test"));
    });

    if recv.is_err() {
      panic!("panic inserting test route: {:?}", recv);
    }

    let guard = match tree.lock() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };
    let (handler, _, tsr) = guard.get_handler("/");

    if handler.is_some() {
      panic!("non-nil handler");
    } else if tsr {
      panic!("expected no TSR recommendation");
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
      tree.add_route(route, fake_handler(route));
    }

    // Check out == in for all registered routes
    // With fixTrailingSlash = true
    for route in &routes {
      let (out, found) = tree.find_case_insensitive_path(route, true);
      if !found {
        panic!("Route '{}' not found!", route);
      } else if out != *route {
        panic!("Wrong result for route '{}': {}", route, out);
      }
    }

    // With fixTrailingSlash = false
    for route in &routes {
      let (out, found) = tree.find_case_insensitive_path(route, false);
      if !found {
        panic!("Route '{}' not found!", route);
      } else if out != *route {
        panic!("Wrong result for route '{}': {}", route, out);
      }
    }

    let tests = vec![
      ("/HI", "/hi", true, false),
      ("/HI/", "/hi", true, true),
      ("/B", "/b/", true, true),
      ("/B/", "/b/", true, false),
      ("/abc", "/ABC/", true, true),
      ("/abc/", "/ABC/", true, false),
      ("/aBc", "/ABC/", true, true),
      ("/aBc/", "/ABC/", true, false),
      ("/abC", "/ABC/", true, true),
      ("/abC/", "/ABC/", true, false),
      ("/SEARCH/QUERY", "/search/QUERY", true, false),
      ("/SEARCH/QUERY/", "/search/QUERY", true, true),
      ("/CMD/TOOL/", "/cmd/TOOL/", true, false),
      ("/CMD/TOOL", "/cmd/TOOL/", true, true),
      ("/SRC/FILE/PATH", "/src/FILE/PATH", true, false),
      ("/x/Y", "/x/y", true, false),
      ("/x/Y/", "/x/y", true, true),
      ("/X/y", "/x/y", true, false),
      ("/X/y/", "/x/y", true, true),
      ("/X/Y", "/x/y", true, false),
      ("/X/Y/", "/x/y", true, true),
      ("/Y/", "/y/", true, false),
      ("/Y", "/y/", true, true),
      ("/Y/z", "/y/z", true, false),
      ("/Y/z/", "/y/z", true, true),
      ("/Y/Z", "/y/z", true, false),
      ("/Y/Z/", "/y/z", true, true),
      ("/y/Z", "/y/z", true, false),
      ("/y/Z/", "/y/z", true, true),
      ("/Aa", "/aa", true, false),
      ("/Aa/", "/aa", true, true),
      ("/AA", "/aa", true, false),
      ("/AA/", "/aa", true, true),
      ("/aA", "/aa", true, false),
      ("/aA/", "/aa", true, true),
      ("/A/", "/a/", true, false),
      ("/A", "/a/", true, true),
      ("/DOC", "/doc", true, false),
      ("/DOC/", "/doc", true, true),
      ("/NO", "", false, true),
      ("/DOC/GO", "", false, true),
      // TODO unicode vs ascii case sensitivity
      // ("/π", "/Π", true, false),
      // ("/π/", "/Π", true, true),
      // ("/u/ÄPFÊL/", "/u/äpfêl/", true, false),
      // ("/u/ÄPFÊL", "/u/äpfêl/", true, true),
      // ("/u/ÖPFÊL/", "/u/öpfêl", true, true),
      // ("/u/ÖPFÊL", "/u/öpfêl", true, false),
      // ("/v/äpfêL/", "/v/Äpfêl/", true, false),
      // ("/v/äpfêL", "/v/Äpfêl/", true, true),
      // ("/v/öpfêL/", "/v/Öpfêl", true, true),
      // ("/v/öpfêL", "/v/Öpfêl", true, false),
      ("/w/♬/", "/w/♬", true, true),
      ("/w/♭", "/w/♭/", true, true),
      ("/w/𠜎/", "/w/𠜎", true, true),
      ("/w/𠜏", "/w/𠜏/", true, true),
      (
        "/lOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOng/",
        "/loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong",
        true, true),
    ];

    struct Test {
      inn: &'static str,
      out: &'static str,
      found: bool,
      slash: bool,
    };

    let tests: Vec<Test> = tests
      .into_iter()
      .map(|test| Test {
        inn: test.0,
        out: test.1,
        found: test.2,
        slash: test.3,
      })
      .collect();

    // With fixTrailingSlash = true
    for test in &tests {
      let (out, found) = tree.find_case_insensitive_path(test.inn, true);
      if found != test.found || (found && (out != test.out)) {
        panic!(
          "Wrong result for '{}': got {}, {}; want {} {}",
          test.inn, out, found, test.out, test.found
        );
      }
    }

    // With fixTrailingSlash = false
    for test in &tests {
      let (out, found) = tree.find_case_insensitive_path(test.inn, false);
      if test.slash {
        if found {
          // test needs a trailingSlash fix. It must not be found!
          panic!("Found without fixTrailingSlash: {}; got {}", test.inn, out);
        }
      } else if found != test.found || (found && (out != test.out)) {
        panic!(
          "Wrong result for '{}': got {}, {}; want {} {}",
          test.inn, out, found, test.out, test.found
        );
      }
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
        tree.add_route(route, fake_handler(route));
      }
      tree.add_route(conflict, fake_handler(conflict));
    }
  }
}
