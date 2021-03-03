use std::collections::HashMap;

pub enum NodeType {
    Static,
    Regex,
    Param,
    CatchAll
}

pub struct Node<K, V> {
    typ: NodeType,
    label: u8,
    tail: u8,
    prefix: Vec<u8>,
    regex: regex::Regex,
    endpoints: HashMap<K, V>,
    children: [Vec<Node<K, V>>; 4]
}
