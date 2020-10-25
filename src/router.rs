//! The router relies on a radix tree structure which makes heavy use of *common prefixes*
//! see [tree.rs](../tree/index.html) for more details
use crate::tree::{Node, Params};
use http::Method;
use std::collections::HashMap;

/// Router is container which can be used to dispatch requests to different
/// handler functions via configurable routes
pub struct Router<T> {
  pub methods: HashMap<Method, Node<T>>,
}

impl<T> Default for Router<T> {
  fn default() -> Self {
    Router {
      methods: HashMap::with_capacity(7),
    }
  }
}

impl<T> Router<T> {
  /// add a handler for http method GET
  pub fn get(&mut self, path: &str, handle: T) {
    self.handle(Method::GET, path, handle);
  }

  /// add a handler for http method HEAD
  pub fn head(&mut self, path: &str, handle: T) {
    self.handle(Method::HEAD, path, handle);
  }

  /// add a handler for http method OPTIONS
  pub fn options(&mut self, path: &str, handle: T) {
    self.handle(Method::OPTIONS, path, handle);
  }

  /// add a handler for http method POST
  pub fn post(&mut self, path: &str, handle: T) {
    self.handle(Method::POST, path, handle);
  }

  /// add a handler for http method PUT
  pub fn put(&mut self, path: &str, handle: T) {
    self.handle(Method::PUT, path, handle);
  }

  /// add a handler for http method PATCH
  pub fn patch(&mut self, path: &str, handle: T) {
    self.handle(Method::PATCH, path, handle);
  }

  /// add a handler for http method DELTE
  pub fn delete(&mut self, path: &str, handle: T) {
    self.handle(Method::DELETE, path, handle);
  }

  /// Handle registers a new request handle with the given path and method.
  /// For GET, POST, PUT, PATCH and DELETE requests, the respective shortcut
  /// functions can be used.
  pub fn handle(&mut self, method: Method, path: &str, handle: T) {
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
  /// Otherwise, the third return value indicates whether a redirection to
  /// the same path without the trailing slash should be performed.
  pub fn lookup(&mut self, method: &Method, path: &str) -> (Option<&T>, Params, bool) {
    self
      .methods
      .get_mut(method)
      .map(|n| n.get_value(path))
      .unwrap_or((None, Params::default(), false))
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
            let (handle, _, _) = tree.get_value(path);
            if handle.is_some() {
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

#[cfg(test)]
mod tests {
  #[test]
  #[should_panic(expected = "path must begin with '/' in path 'invalid'")]
  fn handle_invalid_path() {
    use crate::router::{Params, Router};
    use hyper::{Body, Method, Response};

    let mut router = Router::default();

    router.handle(
      Method::GET,
      "invalid",
      |_req: crate::Request, _: Params| -> crate::Response { Response::new(Body::from("test")) },
    );
  }
}
