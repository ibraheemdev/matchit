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