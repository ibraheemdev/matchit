use crate::handler::{Extractor, Factory, Handler, MakeService};
use crate::{Body, FromRequest, Method, Request, Response, Service, StatusCode, ToResponse};
use futures::future::{ready, BoxFuture, Future};

type BoxedService<Req, Res> = Box<
  dyn Service<
      Req,
      Response = Res,
      Error = hyper::Error,
      Future = BoxFuture<'static, Result<Res, hyper::Error>>,
    > + Send
    + Sync,
>;

/// Resource Route definition.
///
/// Route uses builder-like pattern for configuration.
pub struct Route {
  pub method: Option<Method>,
  pub handler: BoxedService<Request, Response<Body>>,
}

impl Route {
  #[allow(clippy::new_without_default)]
  /// Create new Route which matches any request
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::new().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn new() -> Self {
    Route {
      method: None,
      handler: Box::new(MakeService::new(Extractor::new(Handler::new(|| {
        ready(
          Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Body::default())
            .unwrap(),
        )
      })))),
    }
  }

  /// Create *Route* for http `GET` requests.
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::get().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn method(method: Method) -> Route {
    Route::new().set_method(method)
  }

  /// Create *Route* for http `GET` requests.
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::patch().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn get() -> Route {
    Route::new().set_method(Method::GET)
  }

  /// Create *Route* for http `POST` requests.
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::post().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn post() -> Route {
    Route::new().set_method(Method::POST)
  }

  /// Create *Route* for http `PUT` requests.
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::put().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn put() -> Route {
    Route::new().set_method(Method::PUT)
  }

  /// Create *Route* for http `PATCH` requests.
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::patch().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn patch() -> Route {
    Route::new().set_method(Method::PATCH)
  }

  /// Create *Route* for http `DELETE` requests.
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::delete().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn delete() -> Route {
    Route::new().set_method(Method::DELETE)
  }

  /// Create *Route* for http `HEAD` requests.
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::head().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn head() -> Route {
    Route::new().set_method(Method::HEAD)
  }

  /// Set handler function, use request extractors for parameters.
  /// ```rust
  /// use turbofish::{Route, Response, Body};
  ///
  /// Route::new().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn to<F, T, R, U>(mut self, handler: F) -> Self
  where
    F: Factory<T, R, U> + Send + Sync,
    T: FromRequest + 'static,
    R: Future<Output = U> + Send + Sync + 'static,
    U: ToResponse + 'static,
  {
    self.handler = Box::new(MakeService::new(Extractor::new(Handler::new(handler))));
    self
  }

  /// Assign the Route to an HTTP Method.
  pub fn set_method(mut self, method: Method) -> Self {
    self.method = Some(method);
    self
  }
}
