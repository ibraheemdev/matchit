//! [`Router`](crate::Router) is a lightweight high performance HTTP request router.
//!
//! This router supports variables in the routing pattern and matches against
//! the request method. It also scales better.
//!
//! The router is optimized for high performance and a small memory footprint.
//! It scales well even with very long paths and a large number of routes.
//! A compressing dynamic trie (radix tree) structure is used for efficient matching.
//!
//! With the `hyper-server` feature enabled, the `Router` can be used as a router for a hyper server:
//!
//! ```rust,no_run
//! use httprouter::{Router, HyperRouter, Params, Handler};
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
//!     let mut router: HyperRouter = Router::default();
//!     router.get("/", Handler::new(index));
//!     router.get("/hello/:user", Handler::new(hello));
//!
//!     hyper::Server::bind(&([127, 0, 0, 1], 3000).into())
//!         .serve(router.into_service())
//!         .await;
//! }
//!```
//!
//! Because the `Router` is generic, it can be used to store arbitrary values. This makes it flexible enough to be used as a building block for larger frameworks:
//!
//!```rust
//! use httprouter::Router;
//! use hyper::Method;
//!
//! fn main() {
//!     let mut router: Router<Method, String> = Router::default();
//!     router.handle("/users/:id", Method::GET, "Welcome!".to_string());
//!
//!     let res = router.lookup(&Method::GET, "/users/200").unwrap();
//!    
//!     assert_eq!(res.params.by_name("id"), Some("200"));
//!     assert_eq!(res.value, &"Welcome!".to_string());
//! }
//!```
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
//! The value of parameters is saved as a `Vec` of the `Param` struct, consisting
//! each of a key and a value.
//!
//! There are two ways to retrieve the value of a parameter:
//!  1) by the name of the parameter
//! ```ignore
//!  # use httprouter::tree::Params;
//!  # let params = Params::default();

//!  let user = params.by_name("user") // defined by :user or *user
//! ```
//!  2) by the index of the parameter. This way you can also get the name (key)
//! ```rust
//!  # use httprouter::tree::Params;
//!  # let params = Params::default();
//!  let third_key = params[2].key;   // the name of the 3rd parameter
//!  let third_value = params[2].value; // the value of the 3rd parameter
//! ```
#[cfg(feature = "hyper-server")]
#[doc(inline)]
pub use self::hyper::{Handler, HyperRouter, MakeRouterService, RouterService};

use crate::tree::{Node, RouteLookup};
use http::Method;
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;
use std::str;

/// Router is container which can be used to dispatch requests to different
/// handlers via configurable routes.
///
/// Handlers (the value stored by the router) are indexed by keys. For example,
/// the [`HyperRouter`](crate::HyperRouter) uses HTTP methods as keys. This leads to increased
/// lookup performance.
pub struct Router<K: Eq + Hash, V> {
  trees: HashMap<K, Node<V>>,

  /// Enables automatic redirection if the current route can't be matched but a
  /// handler for the path with (without) the trailing slash exists.
  /// For example if `/foo/` is requested but a route only exists for `/foo`, the
  /// client is redirected to `/foo` with HTTP status code 301 for `GET` requests
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

  /// If enabled, the router automatically replies to `OPTIONS` requests.
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

  /// A configurable handler which is called when a request
  /// cannot be routed and `handle_method_not_allowed` is true.
  /// The `Allow` header with allowed request methods is set before the handler
  /// is called.
  pub method_not_allowed: Option<V>,
}

impl<K: Eq + Hash, V> Router<K, V> {
  /// Insert a value into the router for a specific path indexed by a key.
  /// ```rust
  /// use httprouter::Router;
  /// use hyper::Method;
  ///
  /// let mut router: Router<Method, String> = Router::default();
  /// router.handle("/teapot", Method::GET, "I am a teapot".to_string());
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
  /// If the handler is not found, it returns a `Err(bool)` indicating whether a redirection should be performed to the same path with a trailing slash
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

  /// TODO
  pub fn serve_files() {
    unimplemented!()
  }
}

impl<V> Router<Method, V> {
  /// Register a handler for `GET` requests
  pub fn get(&mut self, path: &str, handle: V) {
    self.handle(path, Method::GET, handle);
  }

  /// Register a handler for `HEAD` requests
  pub fn head(&mut self, path: &str, handle: V) {
    self.handle(path, Method::HEAD, handle);
  }

  /// Register a handler for `OPTIONS` requests
  pub fn options(&mut self, path: &str, handle: V) {
    self.handle(path, Method::OPTIONS, handle);
  }

  /// Register a handler for `POST` requests
  pub fn post(&mut self, path: &str, handle: V) {
    self.handle(path, Method::POST, handle);
  }

  /// Register a handler for `PUT` requests
  pub fn put(&mut self, path: &str, handle: V) {
    self.handle(path, Method::PUT, handle);
  }

  /// Register a handler for `PATCH` requests
  pub fn patch(&mut self, path: &str, handle: V) {
    self.handle(path, Method::PATCH, handle);
  }

  /// Register a handler for `DELETE` requests
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
#[doc(hidden)]
pub mod hyper {
  use crate::path::clean;
  use crate::Router;
  use futures::future::{ok, Future};
  use hyper::service::Service;
  use hyper::{header, Body, Method, Request, Response, StatusCode};
  use std::pin::Pin;
  use std::sync::Arc;
  use std::task::{Context, Poll};

  /// Represents a HTTP handler function.
  /// This trait is implemented for asynchronous functions that take a `Request` and return a
  /// `Result<Response<Body>, hyper::Error>`
  /// ```rust
  /// # use httprouter::Handler;
  /// # use hyper::{Request, Response, Body};
  /// async fn hello(_: Request<Body>) -> Result<Response<Body>, hyper::Error> {
  ///     Ok(Response::new(Body::empty()))
  /// }
  ///
  /// let handler: Box<dyn Handler> = Handler::new(hello);
  /// ```
  pub trait Handler {
    fn new(handler: Self) -> Box<Self>
    where
      Self: Sized;

    fn handle(
      &self,
      req: Request<Body>,
    ) -> Pin<Box<dyn Future<Output = hyper::Result<Response<Body>>> + Send + Sync>>;
  }

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
    ) -> Pin<Box<dyn Future<Output = hyper::Result<Response<Body>>> + Send + Sync>> {
      Box::pin(self(req))
    }
  }

  /// A `Router` indexed by HTTP methods storing `Handler` functions.
  /// This is the router that should be used with `Hyper`
  pub type HyperRouter = Router<Method, Box<dyn Handler + Send + Sync>>;

  /// Wraps a `Router` to provide better ergonomics. You can create a `MakeRouterService` with [`Router::into_service`](crate::Router::into_service).
  pub struct MakeRouterService(RouterService);

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

  /// A thread-safe `Router` that implements `hyper::Service`. This is useful when incorporating a
  /// `Router` into a larger `Service`. `RouterService` wraps the `Router` in an `Arc` for
  /// multithreading. If this is not useful, you might want to use [`Router::serve`](crate::Router::serve) directly instead.
  /// ```rust,no_run
  /// # use httprouter::{Router, HyperRouter};
  /// use httprouter::router::RouterService;
  /// use hyper::service::Service;
  /// # use hyper::service::{make_service_fn, service_fn};
  /// # use hyper::{Request, Body, Server};
  /// # use std::convert::Infallible;
  /// # use std::sync::Arc;
  ///
  /// # #[tokio::main]
  /// # async fn main() {
  /// let mut router: HyperRouter = Router::default();
  ///
  /// let service: RouterService = RouterService::new(router);
  ///    
  /// let make_svc = make_service_fn(move |_| {
  ///     let service = service.clone();
  ///     async move {
  ///         Ok::<_, Infallible>(service_fn(move |req: Request<Body>| {
  ///             let mut service = service.clone();
  ///             // do other things...
  ///             async move { service.call(req).await }
  ///         }))
  ///     }
  /// });
  ///
  /// let server = Server::bind(&([127, 0, 0, 1], 3000).into())
  ///     .serve(make_svc)
  ///     .await;
  /// # }
  #[derive(Clone)]
  pub struct RouterService(Arc<HyperRouter>);

  impl RouterService {
    /// Create a new `RouterService` from a [`Router`](crate::Router)
    pub fn new(router: HyperRouter) -> Self {
      RouterService(Arc::new(router))
    }
  }

  impl Service<Request<Body>> for RouterService {
    type Response = Response<Body>;
    type Error = hyper::Error;
    type Future = Pin<Box<dyn Future<Output = hyper::Result<Response<Body>>> + Send + Sync>>;

    fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
      Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Request<Body>) -> Self::Future {
      self.0.serve(req)
    }
  }

  impl Router<Method, Box<dyn Handler + Send + Sync>> {
    /// Converts the `Router` into a `MakeRouterService` which you can serve directly with `Hyper`.
    /// If you have an existing `Service` that you want to incorporate a `Router` into, see
    /// [`RouterService`](crate::router::RouterService).
    /// ```rust,no_run
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
      MakeRouterService(RouterService::new(self))
    }

    /// An asynchronous function from a `Request` to a `Response`.
    /// You will generally not need to use this function directly, and instead use [`MakeRouterService`](crate::router::MakeRouterService) or [`RouterService`](crate::router::RouterService).
    /// However, this may be useful in certain cases. For example, `RouterService` wraps the `Router` in an `Arc` which may introduce unneccesary overhead when using a single threaded runtime.
    /// ```rust,no_run
    /// # use httprouter::{Router, HyperRouter};
    /// # use httprouter::router::RouterService;
    /// # use hyper::service::{make_service_fn, service_fn};
    /// # use hyper::{Request, Body, Server};
    /// # use std::convert::Infallible;
    /// # use std::rc::Rc;
    ///
    /// # async fn run() {
    /// let mut router: HyperRouter = Router::default();
    ///
    /// let router = Rc::new(router);
    ///    
    /// let make_svc = make_service_fn(move |_| {
    ///     let router = router.clone();
    ///     async move {
    ///         Ok::<_, Infallible>(service_fn(move |req: Request<Body>| {
    ///             let router = router.clone();
    ///             async move { router.serve(req).await }
    ///         }))
    ///     }
    /// });
    ///
    /// let server = Server::bind(&([127, 0, 0, 1], 3000).into())
    ///     .executor(SingleThreadedExecutor)
    ///     .serve(make_svc)
    ///     .await;
    /// # }
    ///
    /// # #[derive(Clone, Copy, Debug)]
    /// # struct SingleThreadedExecutor;
    ///
    /// # impl<F> hyper::rt::Executor<F> for SingleThreadedExecutor
    /// # where
    /// #    F: std::future::Future + 'static, // not requiring `Send`
    /// # {
    /// #    fn execute(&self, fut: F) {
    /// #       tokio::task::spawn_local(fut);
    /// #    }
    /// # }
    /// ```
    pub fn serve(
      &self,
      mut req: Request<Body>,
    ) -> Pin<Box<dyn Future<Output = hyper::Result<Response<Body>>> + Send + Sync>> {
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
                  root.find_case_insensitive_path(&clean(path), self.redirect_trailing_slash)
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
