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