#![deny(clippy::all)]
#![forbid(unsafe_code)]

pub mod app;
pub mod endpoint;
pub mod handler;
pub mod request;
pub mod response;
pub mod router;

// re-export common components
#[doc(hidden)]
pub use endpoint::Endpoint;
#[doc(hidden)]
pub use http::{HeaderValue, Method, Response};
#[doc(hidden)]
pub use hyper::{Body, Error};
#[doc(hidden)]
pub use request::Request;

// #[macro_use]
// extern crate log;
