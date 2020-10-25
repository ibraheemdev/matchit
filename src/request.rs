//! Request extractors
use futures::future::{ok, Ready};
use hyper::error::Error;
use std::future::Future;

/// Trait implemented by types that can be extracted from request.
///
/// Types that implement this trait can be used with `Route` handlers.
pub trait FromRequest: Sized {
  /// The associated error which can be returned.
  type Error: Into<Error>;

  /// Future that resolves to a Self
  type Future: Future<Output = Result<Self, Self::Error>>;

  /// Convert request to a Self
  fn from_request(req: crate::Request) -> Self::Future;
}

impl FromRequest for crate::Request {
  type Error = Error;
  type Future = Ready<Result<crate::Request, Error>>;

  #[inline]
  fn from_request(req: crate::Request) -> Self::Future {
    ok(req)
  }
}
