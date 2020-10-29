//! Respond to HTTP requests easily with implementations of the `ToResponse` trait
//!
//! A `Responder` is any type that can be converted into an HTTP Response
//! through the [`ToResponse`](./trait.ToResponse.html) trait
//!
//! [`ToResponse`](./trait.ToResponse.html) is implemented for the following
//! types so you can quickly create a `Response`:
//!
//! - [`http::Response<hyper::Body>`](https://docs.rs/http)
//!
//! # Example
//!
//! ```
//! use turbofish::{Response, Route, Body};
//!
//! // Returns a `200 OK` response with custom header and body.
//! let hello = Route::get().to(|| async {
//!   Response::builder()
//!     .header("my-custom-header", "some-value")
//!     .body(Body::from("Hello World"))
//!     .unwrap()
//! });
//! ```

use crate::Request;
use futures::future::{ok, Future, Ready};
use hyper::{Body, Error, Response};

/// Types that implement this trait can be used as the return type of a handler.
///
/// # Example
///
/// ```rust
/// use turbofish::{Request, Response, Route, ToResponse, Body};
/// use hyper::Error;
/// use futures::future::{ok, Future, Ready};
///
/// // our custom response type
/// pub struct MyResponse {
///   message: String
/// }
///
/// // implement `ToResponse` to allow directly responding to HTTP requests with `MyResponse`
/// impl ToResponse for MyResponse {
///   type Error = hyper::Error;
///   type Future = Ready<Result<Response<Body>, Self::Error>>;
///
///   fn respond_to(self, _: &Request) -> Self::Future {
///     let res = Response::builder()
///       .header("Content-Type", "text/utf-8")
///       .body(Body::from(self.message))
///       .unwrap();
///     ok(res)
///   }
/// }
///
/// // Returns a `200 OK` response with custom header and body.
/// let hello_world = Route::get().to(|| async {
///   MyResponse {
///     message: String::from("Hello World")
///   }
/// });
/// ```
pub trait ToResponse: Send + Sync {
  /// The associated error which can be returned.
  type Error: Into<Error>;

  /// The future response value.
  type Future: Future<Output = Result<Response<Body>, Self::Error>> + Send;

  /// Convert an HTTP request to itself or `Error`.
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
