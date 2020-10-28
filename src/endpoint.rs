use crate::extractors::FromRequest;
use crate::handler::{Extractor, Factory, Handler};
use crate::responder::ToResponse;
use crate::Request;
use futures::future::{ready, BoxFuture, Future, FutureExt};
use http::{Method, StatusCode};
use hyper::service::Service;
use hyper::{Body, Response};
use std::task::{Context, Poll};

type BoxedMakeEndpoint<Req, Res> = Box<
  dyn Service<
      Req,
      Response = Res,
      Error = hyper::Error,
      Future = BoxFuture<'static, Result<Res, hyper::Error>>,
    > + Send
    + Sync,
>;

/// Resource endpoint definition
///
/// Endpoint uses builder-like pattern for configuration.
pub struct Endpoint {
  pub method: Option<Method>,
  pub handler: BoxedMakeEndpoint<Request, Response<Body>>,
}

impl Endpoint {
  #[allow(clippy::new_without_default)]
  /// Create new endpoint which matches any request
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::new().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn new() -> Self {
    Endpoint {
      method: None,
      handler: Box::new(MakeEndpoint::new(Extractor::new(Handler::new(|| {
        ready(
          Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Body::default())
            .unwrap(),
        )
      })))),
    }
  }

  /// Create *endpoint* for http `GET` requests.
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::get().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn method(method: Method) -> Endpoint {
    Endpoint::new().set_method(method)
  }

  /// Create *endpoint* for http `GET` requests.
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::patch().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn get() -> Endpoint {
    Endpoint::new().set_method(Method::GET)
  }

  /// Create *endpoint* for http `POST` requests.
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::post().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn post() -> Endpoint {
    Endpoint::new().set_method(Method::POST)
  }

  /// Create *endpoint* for http `PUT` requests.
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::put().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn put() -> Endpoint {
    Endpoint::new().set_method(Method::PUT)
  }

  /// Create *endpoint* for http `PATCH` requests.
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::patch().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn patch() -> Endpoint {
    Endpoint::new().set_method(Method::PATCH)
  }

  /// Create *endpoint* for http `DELETE` requests.
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::delete().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn delete() -> Endpoint {
    Endpoint::new().set_method(Method::DELETE)
  }

  /// Create *endpoint* for http `HEAD` requests.
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::head().to(|| async {
  ///   Response::new(Body::default())
  /// });
  /// ```
  pub fn head() -> Endpoint {
    Endpoint::new().set_method(Method::HEAD)
  }

  /// Set handler function, use request extractors for parameters.
  /// ```rust
  /// use turbofish::{Endpoint, Response, Body};
  ///
  /// Endpoint::new().to(|| async {
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
    self.handler = Box::new(MakeEndpoint::new(Extractor::new(Handler::new(handler))));
    self
  }

  /// Assign the endpoint to an HTTP Method.
  pub fn set_method(mut self, method: Method) -> Self {
    self.method = Some(method);
    self
  }
}

struct MakeEndpoint<T: Service<Request>> {
  service: T,
}

impl<T> MakeEndpoint<T>
where
  T::Future: 'static,
  T: Service<Request, Response = Response<Body>, Error = (hyper::Error, Request)>,
{
  fn new(service: T) -> Self {
    MakeEndpoint { service }
  }
}

impl<T> Service<Request> for MakeEndpoint<T>
where
  T::Future: 'static + Send,
  T: Service<Request, Response = Response<Body>, Error = (hyper::Error, Request)>,
{
  type Response = Response<Body>;
  type Error = hyper::Error;
  type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

  fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
    self.service.poll_ready(cx).map_err(|(e, _)| e)
  }

  fn call(&mut self, req: Request) -> Self::Future {
    self
      .service
      .call(req)
      .map(|res| match res {
        Ok(res) => Ok(res),
        Err((_err, _req)) => Ok(
          // [TODO] error response
          Response::new(Body::default()),
        ),
      })
      .boxed()
  }
}
