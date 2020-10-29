//! This module contains all the types used to convert user defined
//! handlers into `hyper::Service`. This is done through a chain of
//! services:
//! 1. User defined handler that implements the `Factory` trait
//! 2. Wrap user defined handler in a `Handler` which converts the
//!    response to a `http::Response<hyper::Body>`
//! 3. Wrap `Handler` in an `Extractor` that extracts extractors
//!    from the http request
//! 4. Finally, wrap `Extractor` in `MakeService` which converts result errors
//!    to an http response

#![allow(clippy::unknown_clippy_lints)]
use crate::{Body, FromRequest, Request, Response, Service, ToResponse};
use futures::future::{BoxFuture, Future, FutureExt};
use futures::ready;
use std::convert::Infallible;
use std::marker::PhantomData;
use std::pin::Pin;
use std::task::{Context, Poll};

/// Asynchronous handler factory that is implemented for async functions
/// that accept between 0 - 11 extractors [`FromRequest`]()
/// and return a responder [`ToResponse`]()
pub trait Factory<T, R, O>: Clone + 'static
where
  R: Future<Output = O>,
  O: ToResponse,
{
  fn call(&self, param: T) -> R;
}

/// `Handler` is a service that wraps a user defined handler,
/// calls it, and returns a `HandlerResponse` future which handles
/// converting the `Responder` to a response compatible with hyper (`http::Request<hyper::Body>`)
pub struct Handler<F, T, R, O>
where
  F: Factory<T, R, O>,
  R: Future<Output = O>,
  O: ToResponse,
{
  handler: F,
  _t: PhantomData<(T, R, O)>,
}

impl<F, T, R, O> Handler<F, T, R, O>
where
  F: Factory<T, R, O>,
  R: Future<Output = O>,
  O: ToResponse,
{
  /// Create a `Handler` from an asynchronous user-defined handler function (`Factory`)
  pub fn new(handler: F) -> Self {
    Handler {
      handler,
      _t: PhantomData,
    }
  }
}

impl<F, T, R, O> Clone for Handler<F, T, R, O>
where
  F: Factory<T, R, O>,
  R: Future<Output = O>,
  O: ToResponse,
{
  fn clone(&self) -> Self {
    Handler {
      handler: self.handler.clone(),
      _t: PhantomData,
    }
  }
}

impl<F, T, R, O> Service<(T, Request)> for Handler<F, T, R, O>
where
  F: Factory<T, R, O>,
  R: Future<Output = O>,
  O: ToResponse,
{
  type Response = Response<Body>;
  type Error = Infallible;
  type Future = HandlerResponse<R, O>;

  fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
    Poll::Ready(Ok(()))
  }

  fn call(&self, (param, req): (T, Request)) -> Self::Future {
    HandlerResponse {
      fut: self.handler.call(param),
      fut2: None,
      req: Some(req),
    }
  }
}

/// The future returned by the `Handler` service
/// converts responders to `http::Request<hyper::Body>`
#[pin_project::pin_project]
pub struct HandlerResponse<T, R>
where
  T: Future<Output = R>,
  R: ToResponse,
{
  #[pin]
  fut: T,
  #[pin]
  fut2: Option<R::Future>,
  req: Option<Request>,
}

impl<T, R> Future for HandlerResponse<T, R>
where
  T: Future<Output = R>,
  R: ToResponse,
{
  type Output = Result<Response<Body>, Infallible>;

  fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    let this = self.as_mut().project();

    if let Some(fut) = this.fut2.as_pin_mut() {
      return match fut.poll(cx) {
        Poll::Ready(Ok(res)) => Poll::Ready(Ok(res)),
        Poll::Pending => Poll::Pending,
        Poll::Ready(Err(_err)) => Poll::Ready(Ok(
          // [TODO] error response
          Response::new(Body::default()),
        )),
      };
    }

    match this.fut.poll(cx) {
      Poll::Ready(res) => {
        let fut = res.respond_to(this.req.as_ref().unwrap());
        self.as_mut().project().fut2.set(Some(fut));
        self.poll(cx)
      }
      Poll::Pending => Poll::Pending,
    }
  }
}

/// `Extractor` is a service that extracts `Extractors` from a request
/// by calling `from_request()`, and passes them to a `Handler`
pub struct Extractor<T: FromRequest, S> {
  service: S,
  _t: PhantomData<T>,
}

impl<T: FromRequest, S> Extractor<T, S> {
  /// Create an `Extractor` from a `Handler`
  pub fn new(service: S) -> Self {
    Extractor {
      service,
      _t: PhantomData,
    }
  }
}

impl<T: FromRequest, S> Service<Request> for Extractor<T, S>
where
  S: Service<(T, Request), Response = Response<Body>, Error = Infallible> + Clone,
{
  type Response = Response<Body>;
  type Error = (hyper::Error, Request);
  type Future = ExtractorResponse<T, S>;

  fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
    Poll::Ready(Ok(()))
  }

  fn call(&self, req: Request) -> Self::Future {
    let fut = T::from_request(&req);

    ExtractorResponse {
      req,
      fut,
      fut_s: None,
      service: self.service.clone(),
    }
  }
}

/// The future returned by the `Extractor` service.
/// Extracts `Extractors` from a request and passes them to `Handler`.
#[pin_project::pin_project]
pub struct ExtractorResponse<T: FromRequest, S: Service<(T, Request)>> {
  req: Request,
  service: S,
  #[pin]
  fut: T::Future,
  #[pin]
  fut_s: Option<S::Future>,
}

impl<T: FromRequest, S> Future for ExtractorResponse<T, S>
where
  S: Service<(T, Request), Response = Response<Body>, Error = Infallible>,
{
  type Output = Result<Response<Body>, (hyper::Error, Request)>;

  fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
    let this = self.as_mut().project();

    if let Some(fut) = this.fut_s.as_pin_mut() {
      return fut.poll(cx).map_err(|_| panic!());
    }

    match ready!(this.fut.poll(cx)) {
      Err(e) => Poll::Ready(Err((e.into(), this.req.clone()))),
      Ok(item) => {
        let fut = Some(this.service.call((item, this.req.clone())));
        self.as_mut().project().fut_s.set(fut);
        self.poll(cx)
      }
    }
  }
}

/// `MakeService` is a service that converts `Extractors` to hyper compatible services
/// by converting the error result to a `http::Response`
pub struct MakeService<T: Service<Request>> {
  service: T,
}

impl<T> MakeService<T>
where
  T::Future: 'static,
  T: Service<Request, Response = Response<Body>, Error = (hyper::Error, Request)>,
{
  /// Create an `MakeService` from a `Extractor`
  pub fn new(service: T) -> Self {
    MakeService { service }
  }
}

impl<T> Service<Request> for MakeService<T>
where
  T::Future: 'static + Send,
  T: Service<Request, Response = Response<Body>, Error = (hyper::Error, Request)>,
{
  type Response = Response<Body>;
  type Error = hyper::Error;
  type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

  fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
    Poll::Ready(Ok(()))
  }

  fn call(&self, req: Request) -> Self::Future {
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

/// Implement the `Factory` trait for a function that accepts zero extractors
impl<F, R, O> Factory<(), R, O> for F
where
  F: Fn() -> R + Clone + 'static,
  R: Future<Output = O>,
  O: ToResponse,
{
  fn call(&self, _: ()) -> R {
    (self)()
  }
}

/// This macro implements the `Factory` trait for tuples
/// This is what allows handlers to accept multiple extractors
macro_rules! factory_tuple ({ $(($n:tt, $T:ident)),+} => {
  impl<Func, $($T,)+ Res, O> Factory<($($T,)+), Res, O> for Func
  where Func: Fn($($T,)+) -> Res + Clone + 'static,
    Res: Future<Output = O>,
    O: ToResponse,
  {
    fn call(&self, param: ($($T,)+)) -> Res {
      (self)($(param.$n,)+)
    }
  }
});

/// This macro implements the `Factory` trait for tuples of length (1 - 11)
/// ie: user-defined handlers can accept between 0 - 11 extractors
#[rustfmt::skip]
mod m {
  use super::*;

  factory_tuple!((0, A));
  factory_tuple!((0, A), (1, B));
  factory_tuple!((0, A), (1, B), (2, C));
  factory_tuple!((0, A), (1, B), (2, C), (3, D));
  factory_tuple!((0, A), (1, B), (2, C), (3, D), (4, E));
  factory_tuple!((0, A), (1, B), (2, C), (3, D), (4, E), (5, F));
  factory_tuple!((0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G));
  factory_tuple!((0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G), (7, H));
  factory_tuple!((0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G), (7, H), (8, I));
  factory_tuple!((0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G), (7, H), (8, I), (9, J));
}
