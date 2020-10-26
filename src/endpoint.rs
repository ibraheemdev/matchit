use crate::handler::{Extract, Factory, Handler};
use crate::request::{FromRequest, Request};
use crate::response::ToResponse;
use futures::future::{Future, FutureExt, LocalBoxFuture};
use hyper::service::Service;
use hyper::{Body, Response};
use std::task::{Context, Poll};

type BoxedEndpointService<Req, Res> = Box<
  dyn Service<
    Req,
    Response = Res,
    Error = hyper::Error,
    Future = LocalBoxFuture<'static, Result<Res, hyper::Error>>,
  >,
>;

/// Resource Endpoint definition
pub struct Endpoint {
  pub handler: BoxedEndpointService<Request, Response<Body>>,
}

impl Endpoint {
  pub fn new<F, T, R, U>(handler: F) -> Self
  where
    F: Factory<T, R, U>,
    T: FromRequest + 'static,
    R: Future<Output = U> + 'static,
    U: ToResponse + 'static,
  {
    Endpoint {
      handler: Box::new(EndpointService::new(Extract::new(Handler::new(handler)))),
    }
  }
}

struct EndpointService<T: Service<Request>> {
  service: T,
}

impl<T> EndpointService<T>
where
  T::Future: 'static,
  T: Service<Request, Response = Response<Body>, Error = (hyper::Error, Request)>,
{
  fn new(service: T) -> Self {
    EndpointService { service }
  }
}

impl<T> Service<Request> for EndpointService<T>
where
  T::Future: 'static,
  T: Service<Request, Response = Response<Body>, Error = (hyper::Error, Request)>,
{
  type Response = Response<Body>;
  type Error = hyper::Error;
  type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

  fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
    self.service.poll_ready(cx).map_err(|(e, _)| e)
  }

  fn call(&mut self, req: Request) -> Self::Future {
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
      .boxed_local()
  }
}

#[cfg(test)]
mod test {
  use crate::endpoint::Endpoint;
  use crate::request::Request;
  use hyper::{Body, Response};

  #[test]
  fn test() {
    Endpoint::new(index);
    Endpoint::new(index1);
    Endpoint::new(index2);
  }

  async fn index() -> Response<Body> {
    Response::default()
  }

  async fn index1(_: Request) -> Response<Body> {
    Response::default()
  }

  async fn index2(_: Request, _: Request) -> Response<Body> {
    Response::default()
  }
}
