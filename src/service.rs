//! `turbofish::Service` is effectively a clone of `hyper::Service`
//! except `call` accepts a immutable reference to self.
//! This is due to the requirements of turbofish handlers.

use std::future::Future;
use std::task::{Context, Poll};

/// An asynchronous function from a `Request` to a `Response`.
///
/// The `Service` trait is a simplified interface making it easy to write
/// network applications in a modular and reusable way, decoupled from the
/// underlying protocol.
///
/// # Functional
///
/// A `Service` is a function of a `Request`. It immediately returns a
/// `Future` representing the eventual completion of processing the
/// request. The actual request processing may happen at any time in the
/// future, on any thread or executor. The processing may depend on calling
/// other services. At some point in the future, the processing will complete,
/// and the `Future` will resolve to a response or error.
///
/// # Server
///
/// Requests received by the
/// server over the network are deserialized and then passed as an argument to the
/// server value. The returned response is sent back over the network.
///
/// As an example, here is how an HTTP request is processed by a server:
///
/// ```rust
/// # use std::pin::Pin;
/// # use std::task::{Poll, Context};
/// # use std::future::Future;
/// # use turbofish::Service;
///
/// use turbofish::{Request, Response, StatusCode};
///
/// struct HelloWorld;
///
/// impl Service<Request> for HelloWorld {
///     type Response = Response<Vec<u8>>;
///     type Error = hyper::Error;
///     type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>>>>;
///
///     fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
///         Poll::Ready(Ok(()))
///     }
///
///     fn call(&self, req: Request) -> Self::Future {
///         // create the body
///         let body: Vec<u8> = "hello, world!\n"
///             .as_bytes()
///             .to_owned();
///         // Create the HTTP response
///         let resp = Response::builder()
///             .status(StatusCode::OK)
///             .body(body)
///             .expect("Unable to create `http::Response`");
///         
///         // create a response in a future.
///         let fut = async {
///             Ok(resp)
///         };
///
///         // Return the response as an immediate future
///         Box::pin(fut)
///     }
/// }
/// ```
///
/// # Backpressure
///
/// Calling a `Service` which is at capacity (i.e., it is temporarily unable to process a
/// request) should result in an error. The caller is responsible for ensuring
/// that the service is ready to receive the request before calling it.
///
/// `Service` provides a mechanism by which the caller is able to coordinate
/// readiness. `Service::poll_ready` returns `Ready` if the service expects that
/// it is able to process a request.
pub trait Service<Request> {
  /// Responses given by the service.
  type Response;

  /// Errors produced by the service.
  type Error;

  /// The future response value.
  type Future: Future<Output = Result<Self::Response, Self::Error>>;

  /// Returns `Poll::Ready(Ok(()))` when the service is able to process requests.
  ///
  /// If the service is at capacity, then `Poll::Pending` is returned and the task
  /// is notified when the service becomes ready again. This function is
  /// expected to be called while on a task. Generally, this can be done with
  /// a simple `futures::future::poll_fn` call.
  ///
  /// If `Poll::Ready(Err(_))` is returned, the service is no longer able to service requests
  /// and the caller should discard the service instance.
  ///
  /// Once `poll_ready` returns `Poll::Ready(Ok(()))`, a request may be dispatched to the
  /// service using `call`. Until a request is dispatched, repeated calls to
  /// `poll_ready` must return either `Poll::Ready(Ok(()))` or `Poll::Ready(Err(_))`.
  fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>>;

  /// Process the request and return the response asynchronously.
  ///
  /// This function is expected to be callable off task. As such,
  /// implementations should take care to not call `poll_ready`.
  ///
  /// Before dispatching a request, `poll_ready` must be called and return
  /// `Poll::Ready(Ok(()))`.
  ///
  /// # Panics
  ///
  /// Implementations are permitted to panic if `call` is invoked without
  /// obtaining `Poll::Ready(Ok(()))` from `poll_ready`.
  fn call(&self, req: Request) -> Self::Future;
}
