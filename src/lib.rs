#![deny(clippy::all)]
#![forbid(unsafe_code)]

pub mod app;
pub mod handler;
pub mod request;
pub mod response;
pub mod route;
pub mod router;
pub mod tree;

#[macro_use]
extern crate log;


pub(crate) type Request = hyper::Request<hyper::Body>;
pub(crate) type Response = hyper::Response<hyper::Body>;