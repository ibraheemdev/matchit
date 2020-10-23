use crate::tree::{Node, Params};
use futures::future::Future;
use hyper::{Body, Response};
use std::collections::BTreeMap;
use std::pin::Pin;

pub type BoxFut = Pin<Box<dyn Future<Output = Result<Response<Body>, hyper::Error>>>>;

/// Router is container which can be used to dispatch requests to different
/// handler functions via configurable routes
pub struct Router<T> {
  pub trees: BTreeMap<String, Node<T>>,
}

impl<T> Default for Router<T> {
  fn default() -> Self {
    Router {
      trees: BTreeMap::new(),
    }
  }
}

impl<T> Router<T> {
  /// add a handler for http method GET
  pub fn get(&mut self, path: &str, handle: T) {
    self.handle("GET", path, handle);
  }

  /// add a handler for http method HEAD
  pub fn head(&mut self, path: &str, handle: T) {
    self.handle("HEAD", path, handle);
  }

  /// add a handler for http method OPTIONS
  pub fn options(&mut self, path: &str, handle: T) {
    self.handle("OPTIONS", path, handle);
  }

  /// add a handler for http method POST
  pub fn post(&mut self, path: &str, handle: T) {
    self.handle("POST", path, handle);
  }

  /// add a handler for http method PUT
  pub fn put(&mut self, path: &str, handle: T) {
    self.handle("PUT", path, handle);
  }

  /// add a handler for http method PATCH
  pub fn patch(&mut self, path: &str, handle: T) {
    self.handle("PATCH", path, handle);
  }

  /// add a handler for http method DELTE
  pub fn delete(&mut self, path: &str, handle: T) {
    self.handle("DELETE", path, handle);
  }

  /// Handle registers a new request handle with the given path and method.
  /// For GET, POST, PUT, PATCH and DELETE requests, the respective shortcut
  /// functions can be used.
  pub fn handle(&mut self, method: &str, path: &str, handle: T) {
    if !path.starts_with('/') {
      panic!("path must begin with '/' in path '{}'", path);
    }

    self
      .trees
      .entry(method.to_string())
      .or_insert_with(Node::default)
      .add_route(path, handle);
  }

  /// Allows the manual lookup of a method and path.
  /// Returns the handle function and the path parameter values if the path is found.
  /// Otherwise, the third return value indicates whether a redirection to
  /// the same path without the trailing slash should be performed.
  pub fn lookup(&mut self, method: &str, path: &str) -> (Option<&T>, Params, bool) {
    self
      .trees
      .get_mut(method)
      .map(|n| n.get_value(path))
      .unwrap_or((None, Params::default(), false))
  }

  /// returns a list of the allowed methods for a specific path
  /// eg: 'GET, PATCH, OPTIONS'
  pub fn allowed(&self, path: &str, req_method: &str) -> String {
    let mut allowed: Vec<String> = Vec::new();
    match path {
      "*" => {
        for method in self.trees.keys() {
          if method != "OPTIONS" {
            allowed.push(method.to_string());
          }
        }
      }
      _ => {
        for method in self.trees.keys() {
          if method == req_method || method == "OPTIONS" {
            continue;
          }

          if let Some(tree) = self.trees.get(method) {
            let (handle, _, _) = tree.get_value(path);

            if handle.is_some() {
              allowed.push(method.to_string());
            }
          };
        }
      }
    };

    if !allowed.is_empty() {
      allowed.push(String::from("OPTIONS"))
    }

    allowed.join(", ")
  }
}

#[cfg(test)]
mod tests {
  #[test]
  #[should_panic(expected = "path must begin with '/' in path 'invalid'")]
  fn handle_invalid_path() {
    use crate::router::{Params, Router};
    use hyper::{Body, Request, Response};

    let mut router = Router::default();

    router.handle(
      "GET",
      "invalid",
      |_req: Request<Body>, _: Params| -> Response<Body> { Response::new(Body::from("test")) },
    );
  }
}
