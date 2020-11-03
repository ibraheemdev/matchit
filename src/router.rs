//! Httprouter is a trie based high performance HTTP request router.
//!
//! A trivial example is:
//!  ```ignore
//!  import (
//!      "fmt"
//!      "github.com/julienschmidt/httprouter"
//!      "net/http"
//!      "log"
//!  )
//!
//!  func Index(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
//!      fmt.Fprint(w, "Welcome!\n")
//!  }
//!
//!  func Hello(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
//!      fmt.Fprintf(w, "hello, %s!\n", ps.ByName("name"))
//!  }
//!
//!  func main() {
//!      router := httprouter.New()
//!      router.GET("/", Index)
//!      router.GET("/hello/:name", Hello)
//!
//!      log.Fatal(http.ListenAndServe(":8080", router))
//!  }
//!  ```
//!
//! The router matches incoming requests by the request method and the path.
//! If a handle is registered for this path and method, the router delegates the
//! request to that function.
//! For the methods GET, POST, PUT, PATCH, DELETE and OPTIONS shortcut functions exist to
//! register handles, for all other methods router.Handle can be used.
//!
//! The registered path, against which the router matches incoming requests, can
//! contain two types of parameters:
//!  Syntax    Type
//!  :name     named parameter
//!  *name     catch-all parameter
//!
//! Named parameters are dynamic path segments. They match anything until the
//! next '/' or the path end:
//!  Path: /blog/:category/:post
//!
//!  Requests:
//!   /blog/rust/request-routers            match: category="rust", post="request-routers"
//!   /blog/rust/request-routers/           no match, but the router would redirect
//!   /blog/rust/                           no match
//!   /blog/rust/request-routers/comments   no match
//!
//! Catch-all parameters match anything until the path end, including the
//! directory index (the '/' before the catch-all). Since they match anything
//! until the end, catch-all parameters must always be the final path element.
//!  Path: /files/*filepath
//!
//!  Requests:
//!   /files/                             match: filepath="/"
//!   /files/LICENSE                      match: filepath="/LICENSE"
//!   /files/templates/article.html       match: filepath="/templates/article.html"
//!   /files                              no match, but the router would redirect
//!
//! The value of parameters is saved as a slice of the Param struct, consisting
//! each of a key and a value. The slice is passed to the Handle func as a third
//! parameter.
//! There are two ways to retrieve the value of a parameter:
//!  // by the name of the parameter
//!  let user = params.by_name("user") // defined by :user or *user
//!
//!  // by the index of the parameter. This way you can also get the name (key)
//!  thirdKey   := params[2].key   // the name of the 3rd parameter
//!  thirdValue := params[2].value // the value of the 3rd parameter
use crate::path::clean_path;
use crate::tree::{Node, RouteLookup};
use futures::future::{ok, BoxFuture};
use hyper::body::HttpBody;
use hyper::http::{header, StatusCode};
use hyper::{Body, Method, Request, Response};
use std::collections::HashMap;
use std::str;

pub trait Handler<'a> {
  type Error: Sync + Send + 'a;

  fn handle(
    &self,
    req: Request<impl HttpBody>,
  ) -> BoxFuture<'a, Result<Response<Body>, Box<Self::Error>>>;
}

/// Router is container which can be used to dispatch requests to different
/// handler functions via configurable routes
pub struct Router<T> {
  pub trees: HashMap<Method, Node<T>>,

  /// If enabled, adds the matched route path onto the http.Request context
  /// before invoking the handler.
  /// The matched route path is only added to handlers of routes that were
  /// registered when this option was enabled.
  pub save_matched_route_path: bool,

  /// Enables automatic redirection if the current route can't be matched but a
  /// handler for the path with (without) the trailing slash exists.
  /// For example if /foo/ is requested but a route only exists for /foo, the
  /// client is redirected to /foo with http status code 301 for GET requests
  /// and 307 for all other request methods.
  pub redirect_trailing_slash: bool,

  /// If enabled, the router tries to fix the current request path, if no
  /// handle is registered for it.
  /// First superfluous path elements like ../ or // are removed.
  /// Afterwards the router does a case-insensitive lookup of the cleaned path.
  /// If a handle can be found for this route, the router makes a redirection
  /// to the corrected path with status code 301 for GET requests and 307 for
  /// all other request methods.
  /// For example /FOO and /..//Foo could be redirected to /foo.
  /// RedirectTrailingSlash is independent of this option.
  pub redirect_fixed_path: bool,

  /// If enabled, the router checks if another method is allowed for the
  /// current route, if the current request can not be routed.
  /// If this is the case, the request is answered with 'Method Not Allowed'
  /// and HTTP status code 405.
  /// If no other Method is allowed, the request is delegated to the NotFound
  /// handler.
  pub handle_method_not_allowed: bool,

  /// If enabled, the router automatically replies to OPTIONS requests.
  /// Custom OPTIONS handlers take priority over automatic replies.
  pub handle_options: bool,

  /// An optional handler that is called on automatic OPTIONS requests.
  /// The handler is only called if HandleOPTIONS is true and no OPTIONS
  /// handler for the specific path was set.
  /// The "Allowed" header is set before calling the handler.
  pub global_options: Option<T>,

  /// Cached value of global (*) allowed methods
  pub global_allowed: String,

  /// Configurable handler which is called when no matching route is
  /// found.
  pub not_found: Option<T>,

  /// Configurable handler which is called when a request
  /// cannot be routed and HandleMethodNotAllowed is true.
  /// The "Allow" header with allowed request methods is set before the handler
  /// is called.
  pub method_not_allowed: Option<T>,
}

impl<T> Default for Router<T> {
  fn default() -> Self {
    Router::<T> {
      trees: HashMap::new(),
      redirect_trailing_slash: true,
      redirect_fixed_path: true,
      handle_method_not_allowed: true,
      handle_options: true,
      global_allowed: String::new(),
      global_options: None,
      method_not_allowed: None,
      not_found: None,
      save_matched_route_path: false,
    }
  }
}

impl<T> Router<T> {
  /// get is a shortcut for router.handle("Method::GET, path, handle)
  pub fn get(&mut self, path: &str, handle: T) {
    self.handle(Method::GET, path, handle);
  }

  /// head is a shortcut for router.handle(Method::HEAD, path, handle)
  pub fn head(&mut self, path: &str, handle: T) {
    self.handle(Method::HEAD, path, handle);
  }

  /// options is a shortcut for router.handle(Method::OPTIONS, path, handle)
  pub fn options(&mut self, path: &str, handle: T) {
    self.handle(Method::OPTIONS, path, handle);
  }

  /// post is a shortcut for router.handle(Method::POST, path, handle)
  pub fn post(&mut self, path: &str, handle: T) {
    self.handle(Method::POST, path, handle);
  }

  /// put is a shortcut for router.handle(Method::POST, path, handle)
  pub fn put(&mut self, path: &str, handle: T) {
    self.handle(Method::PUT, path, handle);
  }

  /// patch is a shortcut for router.handle(Method::PATCH, path, handle)
  pub fn patch(&mut self, path: &str, handle: T) {
    self.handle(Method::PATCH, path, handle);
  }

  /// delete is a shortcut for router.handle(Method::DELETE, path, handle)
  pub fn delete(&mut self, path: &str, handle: T) {
    self.handle(Method::DELETE, path, handle);
  }

  // Handle registers a new request handle with the given path and method.
  //
  // For GET, POST, PUT, PATCH and DELETE requests the respective shortcut
  // functions can be used.
  //
  // This function is intended for bulk loading and to allow the usage of less
  // frequently used, non-standardized or custom methods (e.g. for internal
  // communication with a proxy).
  pub fn handle(&mut self, method: Method, path: &str, handle: T) {
    if !path.starts_with('/') {
      panic!("path must begin with '/' in path '{}'", path);
    }

    if self.save_matched_route_path {
      // TODO
      // handle = r.saveMatchedRoutePath(path, handle)
    }

    self
      .trees
      .entry(method)
      .or_insert_with(Node::default)
      .add_route(path, handle);
  }

  /// Lookup allows the manual lookup of a method + path combo.
  /// This is e.g. useful to build a framework around this router.
  /// If the path was found, it returns the handle function and the path parameter
  /// values. Otherwise the third return value indicates whether a redirection to
  /// the same path with an extra / without the trailing slash should be performed.
  pub fn lookup(&mut self, method: &Method, path: &str) -> Result<RouteLookup<T>, bool> {
    self
      .trees
      .get_mut(method)
      .map(|n| n.get_value(path))
      .unwrap_or(Err(false))
  }

  // TODO
  pub fn serve_files() {
    unimplemented!()
  }

  // returns a list of the allowed methods for a specific path
  // eg: 'GET, PATCH, OPTIONS'
  pub fn allowed(&self, path: &str, req_method: &Method) -> String {
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
            let handler = tree.get_value(path);

            if handler.is_ok() {
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

impl<'a, T: Handler<'a>> Router<T> {
  pub fn serve_http(
    &self,
    mut req: Request<impl HttpBody>,
  ) -> BoxFuture<'a, Result<Response<Body>, Box<T::Error>>> {
    let root = self.trees.get(req.method());
    let path = req.uri().path();
    if let Some(root) = root {
      match root.get_value(path) {
        Ok(lookup) => {
          req.extensions_mut().insert(lookup.params);
          return lookup.value.handle(req);
        }
        Err(tsr) => {
          if req.method() != Method::CONNECT && path != "/" {
            let code = match *req.method() {
              // Moved Permanently, request with GET method
              Method::GET => StatusCode::MOVED_PERMANENTLY,
              // Permanent Redirect, request with same method
              _ => StatusCode::PERMANENT_REDIRECT,
            };

            if tsr && self.redirect_trailing_slash {
              let path = if path.len() > 1 && path.ends_with('/') {
                path[..path.len() - 1].to_string()
              } else {
                path.to_string() + "/"
              };

              return Box::pin(ok(
                Response::builder()
                  .header(header::LOCATION, path.as_str())
                  .status(code)
                  .body(Body::empty())
                  .unwrap(),
              ));
            };

            if self.redirect_fixed_path {
              if let Some(fixed_path) = root.find_case_insensitive_path(
                &clean_path(req.uri().path()),
                self.redirect_trailing_slash,
              ) {
                return Box::pin(ok(
                  Response::builder()
                    .header(header::LOCATION, fixed_path.as_str())
                    .status(code)
                    .body(Body::empty())
                    .unwrap(),
                ));
              }
            };
          };
        }
      }
    };

    if req.method() == Method::OPTIONS && self.handle_options {
      let allow = self.allowed(path, &Method::OPTIONS);
      if allow != "" {
        match &self.global_options {
          Some(handler) => return handler.handle(req),
          None => {
            return Box::pin(ok(
              Response::builder()
                .header(header::ALLOW, allow)
                .body(Body::empty())
                .unwrap(),
            ));
          }
        };
      }
    } else if self.handle_method_not_allowed {
      let allow = self.allowed(path, req.method());

      if !allow.is_empty() {
        if let Some(ref method_not_allowed) = self.method_not_allowed {
          return method_not_allowed.handle(req);
        }
        return Box::pin(ok(
          Response::builder()
            .header(header::ALLOW, allow)
            .status(StatusCode::METHOD_NOT_ALLOWED)
            .body(Body::empty())
            .unwrap(),
        ));
      }
    };

    match &self.not_found {
      Some(handler) => handler.handle(req),
      None => Box::pin(ok(
        Response::builder().status(404).body(Body::empty()).unwrap(),
      )),
    }
  }
}
