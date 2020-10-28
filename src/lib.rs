#![deny(clippy::all)]
#![forbid(unsafe_code)]

pub mod app;
pub mod endpoint;
pub mod extractors;
pub mod responder;
pub mod router;

pub(crate) mod handler;

// re-export common components
#[doc(hidden)]
pub use app::App;
#[doc(hidden)]
pub use endpoint::Endpoint;
#[doc(hidden)]
pub use extractors::FromRequest;
#[doc(hidden)]
pub use http::{HeaderValue, Method, Response};
#[doc(hidden)]
pub use hyper::{Body, Error, StatusCode};
#[doc(hidden)]
pub use responder::ToResponse;

// #[macro_use]
// extern crate log;

/// `Request` is a `hyper::Request` wrapped in a reference-counting pointer.
/// The request needs to be wrapped in a pointer in order to be passed around
/// and cloned throughout the `turbofish` service chain
// [TODO] find another way to deal with service chaining, `Arc` is too expensive
#[derive(Clone)]
pub struct Request(pub std::sync::Arc<hyper::Request<hyper::Body>>);
