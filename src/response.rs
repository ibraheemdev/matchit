use crate::request::Request;
use futures::future::{ok, Future, Ready};
use hyper::{Body, Error, Response};

/// Trait implemented by types that can be converted to a http response.
///
/// Types that implement this trait can be used as the return type of a handler.
pub trait ToResponse: Send {
  /// The associated error which can be returned.
  type Error: Into<Error>;

  /// The future response value.
  type Future: Future<Output = Result<Response<Body>, Self::Error>> + Send;

  /// Convert itself to `Future` or `Error`.
  fn respond_to(self, req: &Request) -> Self::Future;
}

impl ToResponse for Response<Body> {
  type Error = Error;
  type Future = Ready<Result<Response<Body>, Error>>;

  #[inline]
  fn respond_to(self, _: &Request) -> Self::Future {
    ok(self)
  }
}
