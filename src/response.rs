use futures::future::{ok, Future, Ready};
use hyper::Error;

/// Trait implemented by types that can be converted to a http response.
///
/// Types that implement this trait can be used as the return type of a handler.
pub trait ToReponse {
  /// The associated error which can be returned.
  type Error: Into<Error>;

  /// The future response value.
  type Future: Future<Output = Result<crate::Response, Self::Error>>;

  /// Convert itself to `Future` or `Error`.
  fn respond_to(self, req: crate::Request) -> Self::Future;
}

impl ToReponse for crate::Response {
  type Error = Error;
  type Future = Ready<Result<crate::Response, Error>>;

  #[inline]
  fn respond_to(self, _: crate::Request) -> Self::Future {
    ok(self)
  }
}
