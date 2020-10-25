use futures::future::{ok, Future, Ready};
use hyper::error::Error;
use std::rc::Rc;

/// `Request` is a `hyper::Request` wrapped in a reference-counting pointer.
/// The request needs to be wrapped in a pointer in order to be passed around
/// and cloned throughout the `turbo-rs` service chain
#[derive(Clone)]
pub struct Request(pub Rc<hyper::Request<hyper::Body>>);

/// Trait implemented by types that can be extracted from request.
///
/// Types that implement this trait can be used with `Route` handlers.
pub trait FromRequest: Sized {
  /// The associated error which can be returned.
  type Error: Into<Error>;

  /// Future that resolves to a Self
  type Future: Future<Output = Result<Self, Self::Error>>;

  /// Convert request to a Self
  fn from_request(req: &Request) -> Self::Future;
}

impl FromRequest for Request {
  type Error = Error;
  type Future = Ready<Result<Request, Error>>;

  #[inline]
  fn from_request(req: &Request) -> Self::Future {
    ok(req.clone())
  }
}
