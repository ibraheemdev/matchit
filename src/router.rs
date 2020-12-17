//! `Router` is a lightweight high performance HTTP request router.
//! It is a Rust port of [`julienschmidt/httprouter`](https://github.com/julienschmidt/httprouter).
//!
//! This router supports variables in the routing pattern and matches against
//! the request method. It also scales better.
//!
//! The router is optimized for high performance and a small memory footprint.
//! It scales well even with very long paths and a large number of routes.
//! A compressing dynamic trie (radix tree) structure is used for efficient matching.
//!
//! Here is a simple example:
//! ```rust,no_run
//! use httprouter::{Router, Params, Handler, BoxedHandler};
//! use std::convert::Infallible;
//! use hyper::{Request, Response, Body, Error};
//!
//! async fn index(_: Request<Body>) -> Result<Response<Body>, Error> {
//!     Ok(Response::new("Hello, World!".into()))
//! }
//!
//! async fn hello(req: Request<Body>) -> Result<Response<Body>, Error> {
//!     let params = req.extensions().get::<Params>().unwrap();
//!     Ok(Response::new(format!("Hello, {}", params.by_name("user").unwrap()).into()))
//! }
//!
//! #[tokio::main]
//! async fn main() {
//!     let mut router: Router<BoxedHandler> = Router::default();
//!     router.get("/", Handler::new(index));
//!     router.get("/hello/:user", Handler::new(hello));
//!
//!     hyper::Server::bind(&([127, 0, 0, 1], 3000).into())
//!        .serve(router.into_service())
//!        .await;
//! }
//! ```
//!
//! The router matches incoming requests by the request method and the path.
//! If a handle is registered for this path and method, the router delegates the
//! request to that function.
//! For the methods GET, POST, PUT, PATCH, DELETE and OPTIONS shortcut functions exist to
//! register handles, for all other methods router.Handle can be used.
//!
//! The registered path, against which the router matches incoming requests, can
//! contain two types of parameters:
//! ```ignore
//!  Syntax    Type
//!  :name     named parameter
//!  *name     catch-all parameter
//! ```
//!
//! Named parameters are dynamic path segments. They match anything until the
//! next '/' or the path end:
//! ```ignore
//!  Path: /blog/:category/:post
//! ```
//!
//!  Requests:
//! ```ignore
//!   /blog/rust/request-routers            match: category="rust", post="request-routers"
//!   /blog/rust/request-routers/           no match, but the router would redirect
//!   /blog/rust/                           no match
//!   /blog/rust/request-routers/comments   no match
//! ```
//!
//! Catch-all parameters match anything until the path end, including the
//! directory index (the '/' before the catch-all). Since they match anything
//! until the end, catch-all parameters must always be the final path element.
//!  Path: /files/*filepath
//!
//!  Requests:
//! ```ignore
//!   /files/                             match: filepath="/"
//!   /files/LICENSE                      match: filepath="/LICENSE"
//!   /files/templates/article.html       match: filepath="/templates/article.html"
//!   /files                              no match, but the router would redirect
//! ```
//! The value of parameters is saved as a slice of the Param struct, consisting
//! each of a key and a value. The slice is passed to the Handle func as a third
//! parameter.
//! There are two ways to retrieve the value of a parameter:
//!  1) by the name of the parameter
//! ```ignore
//!  let user = params.by_name("user") // defined by :user or *user
//! ```
//!  2) by the index of the parameter. This way you can also get the name (key)
//! ```ignore
//!  thirdKey   := params[2].key   // the name of the 3rd parameter
//!  thirdValue := params[2].value // the value of the 3rd parameter
//! ```
use crate::tree::{Node, RouteLookup};
use http::Method;
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;
use std::str;

/// Router is container which can be used to dispatch requests to different
/// handler functions via configurable routes
pub struct Router<K: Eq + Hash, V> {
  trees: HashMap<K, Node<V>>,

  /// Enables automatic redirection if the current route can't be matched but a
  /// handler for the path with (without) the trailing slash exists.
  /// For example if `/foo/` is requested but a route only exists for `/foo`, the
  /// client is redirected to /foo with http status code 301 for `GET` requests
  /// and 307 for all other request methods.
  pub redirect_trailing_slash: bool,

  /// If enabled, the router tries to fix the current request path, if no
  /// handle is registered for it.
  /// First superfluous path elements like `../` or `//` are removed.
  /// Afterwards the router does a case-insensitive lookup of the cleaned path.
  /// If a handle can be found for this route, the router makes a redirection
  /// to the corrected path with status code 301 for `GET` requests and 307 for
  /// all other request methods.
  /// For example `/FOO` and `/..//Foo` could be redirected to `/foo`.
  /// `redirect_trailing_slash` is independent of this option.
  pub redirect_fixed_path: bool,

  /// If enabled, the router checks if another method is allowed for the
  /// current route, if the current request can not be routed.
  /// If this is the case, the request is answered with `MethodNotAllowed`
  /// and HTTP status code 405.
  /// If no other Method is allowed, the request is delegated to the `NotFound`
  /// handler.
  pub handle_method_not_allowed: bool,

  /// If enabled, the router automatically replies to OPTIONS requests.
  /// Custom `OPTIONS` handlers take priority over automatic replies.
  pub handle_options: bool,

  /// An optional handler that is called on automatic `OPTIONS` requests.
  /// The handler is only called if `handle_options` is true and no `OPTIONS`
  /// handler for the specific path was set.
  /// The `Allowed` header is set before calling the handler.
  pub global_options: Option<V>,

  /// Configurable handler which is called when no matching route is
  /// found.
  pub not_found: Option<V>,

  /// Configurable handler which is called when a request
  /// cannot be routed and `handle_method_not_allowed` is true.
  /// The `Allow` header with allowed request methods is set before the handler
  /// is called.
  pub method_not_allowed: Option<V>,
}

impl<K: Eq + Hash, V> Router<K, V> {
  /// Insert a value into the tree. Values are indexed by keys. For example, the httprouter uses
  /// http methods as keys for improved performance.
  /// ```rust
  /// use httprouter::Router;
  /// use http::Method;
  ///
  /// let mut router = Router::default();
  /// router.handle(, "/teapot", "I am a teapot");
  /// ```
  pub fn handle(&mut self, path: &str, key: K, value: V) {
    if !path.starts_with('/') {
      panic!("path must begin with '/' in path '{}'", path);
    }

    self
      .trees
      .entry(key)
      .or_insert_with(Node::default)
      .add_route(path, value);
  }

  /// Lookup allows the manual lookup of handler for a specific method and path.
  /// If the handler is not found, it returns a `Err(bool)` indicating whethre a redirection should be performed to the same path with a trailing slash
  /// ```rust
  /// use httprouter::Router;
  /// use http::Method;
  ///
  /// let mut router = Router::default();
  /// router.get("/home", "Welcome!");
  ///
  /// let res = router.lookup(&Method::GET, "/home").unwrap();
  /// assert_eq!(res.value, &"Welcome!");
  /// assert!(res.params.is_empty());
  /// ```
  pub fn lookup(&mut self, key: &K, path: &str) -> Result<RouteLookup<V>, bool> {
    self
      .trees
      .get_mut(key)
      .map_or(Err(false), |n| n.get_value(path))
  }

  /// [TODO]
  pub fn serve_files() {
    unimplemented!()
  }
}

impl<V> Router<Method, V> {
  /// Register a handler for GET requests
  pub fn get(&mut self, path: &str, handle: V) {
    self.handle(path, Method::GET, handle);
  }

  /// Register a handler for HEAD requests
  pub fn head(&mut self, path: &str, handle: V) {
    self.handle(path, Method::HEAD, handle);
  }

  /// Register a handler for OPTIONS requests
  pub fn options(&mut self, path: &str, handle: V) {
    self.handle(path, Method::OPTIONS, handle);
  }

  /// Register a handler for POST requests
  pub fn post(&mut self, path: &str, handle: V) {
    self.handle(path, Method::POST, handle);
  }

  /// Register a handler for PUT requests
  pub fn put(&mut self, path: &str, handle: V) {
    self.handle(path, Method::PUT, handle);
  }

  /// Register a handler for PATCH requests
  pub fn patch(&mut self, path: &str, handle: V) {
    self.handle(path, Method::PATCH, handle);
  }

  /// Register a handler for DELETE requests
  pub fn delete(&mut self, path: &str, handle: V) {
    self.handle(path, Method::DELETE, handle);
  }

  /// Returns a list of the allowed methods for a specific path
  /// ```rust
  /// use httprouter::Router;
  /// use http::Method;
  ///
  /// let mut router = Router::default();
  /// router.get("/products", "all products");
  /// router.post("/products", "product created");
  ///
  /// let allowed = router.allowed("/products");
  /// assert!(allowed.contains(&"GET".to_string()));
  /// assert!(allowed.contains(&"POST".to_string()));
  /// ```
  pub fn allowed(&self, path: &str) -> Vec<String> {
    let mut allowed: Vec<String> = Vec::new();
    match path {
      "*" => {
        for method in self.trees.keys() {
          if method != Method::OPTIONS {
            allowed.push(method.to_string());
          }
        }
      }
      _ => {
        for method in self.trees.keys() {
          if method == Method::OPTIONS {
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
      allowed.push(Method::OPTIONS.to_string())
    }

    allowed
  }
}

/// The default httprouter configuration
impl<K: Eq + Hash, V> Default for Router<K, V> {
  fn default() -> Self {
    Self {
      trees: HashMap::new(),
      redirect_trailing_slash: true,
      redirect_fixed_path: true,
      handle_method_not_allowed: true,
      handle_options: true,
      global_options: None,
      method_not_allowed: None,
      not_found: None,
    }
  }
}

#[cfg(feature = "hyper-server")]
pub mod hyper {
  use crate::path::clean_path;
  use crate::Router;
  use futures::future::{ok, Future};
  use hyper::service::Service;
  use hyper::{header, Body, Method, Request, Response, StatusCode};
  use std::pin::Pin;
  use std::sync::Arc;
  use std::task::{Context, Poll};

  pub trait Handler {
    fn new(handler: Self) -> Box<Self>
    where
      Self: Sized;

    fn handle(
      &self,
      req: Request<Body>,
    ) -> Pin<Box<dyn Future<Output = HandlerResult> + Send + Sync>>;
  }

  type HandlerResult = Result<Response<Body>, hyper::Error>;

  impl<F, R> Handler for F
  where
    F: Fn(Request<Body>) -> R,
    R: Future<Output = Result<Response<Body>, hyper::Error>> + Send + Sync + 'static,
  {
    fn new(handler: Self) -> Box<Self>
    where
      Self: Sized,
    {
      Box::new(handler)
    }

    fn handle(
      &self,
      req: Request<Body>,
    ) -> Pin<Box<dyn Future<Output = HandlerResult> + Send + Sync>> {
      Box::pin(self(req))
    }
  }

  pub type BoxedHandler = Box<dyn Handler + Send + Sync>;

  pub struct MakeRouterService(pub RouterService);

  impl<T> Service<T> for MakeRouterService {
    type Response = RouterService;
    type Error = hyper::Error;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

    fn poll_ready(&mut self, _: &mut Context) -> Poll<Result<(), Self::Error>> {
      Poll::Ready(Ok(()))
    }

    fn call(&mut self, _: T) -> Self::Future {
      let service = self.0.clone();
      let fut = async move { Ok(service) };
      Box::pin(fut)
    }
  }

  #[derive(Clone)]
  pub struct RouterService(pub Arc<Router<Method, BoxedHandler>>);

  impl Service<Request<Body>> for RouterService {
    type Response = Response<Body>;
    type Error = hyper::Error;
    type Future = Pin<Box<dyn Future<Output = HandlerResult> + Send + Sync>>;

    fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
      Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Request<Body>) -> Self::Future {
      self.0.serve(req)
    }
  }

  impl Router<Method, BoxedHandler> {
    /// Converts the `Router` into a hyper `Service`
    /// ```rust
    /// # use httprouter::Router;
    /// # use std::convert::Infallible;
    /// # async fn run() -> Result<(), Box<dyn std::error::Error>> {
    /// // Our router...
    /// let router = Router::default();
    ///
    /// // Convert it into a service...
    /// let service = router.into_service();
    ///
    /// // Serve with hyper
    /// hyper::Server::bind(&([127, 0, 0, 1], 3030).into())
    ///     .serve(service)
    ///     .await?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn into_service(self) -> MakeRouterService {
      MakeRouterService(RouterService(Arc::new(self)))
    }

    fn serve(
      &self,
      mut req: Request<Body>,
    ) -> Pin<Box<dyn Future<Output = HandlerResult> + Send + Sync>> {
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
                if let Some(fixed_path) =
                  root.find_case_insensitive_path(&clean_path(path), self.redirect_trailing_slash)
                {
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
        let allow = self.allowed(path).join(", ");
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
        let allow = self.allowed(path).join(", ");

        if !allow.is_empty() {
          if let Some(ref handler) = self.method_not_allowed {
            return handler.handle(req);
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
}
