use crate::handler::{Extract, Factory, Handler};
use crate::request::FromRequest;
use crate::response::ToReponse;
use futures::future::{Future, FutureExt, LocalBoxFuture};
use hyper::service::Service;
use hyper::{Response};
use std::task::{Context, Poll};

type BoxedRouteService<Req, Res> = Box<
  dyn Service<
    Req,
    Response = Res,
    Error = hyper::Error,
    Future = LocalBoxFuture<'static, Result<Res, hyper::Error>>,
  >,
>;

/// Resource route definition
///
/// Route uses builder-like pattern for configuration.
/// If handler is not explicitly set, default *404 Not Found* handler is used.
pub struct Route {
  pub handler: BoxedRouteService<crate::Request, crate::Response>,
}

impl Route {
  pub fn new<F, T, R, U>(handler: F) -> Self
  where
    F: Factory<T, R, U>,
    T: FromRequest + 'static,
    R: Future<Output = U> + 'static,
    U: ToReponse + 'static,
  {
    Route {
      handler: Box::new(RouteService::new(Extract::new(Handler::new(handler)))),
    }
  }
}

struct RouteService<T: Service<crate::Request>> {
  service: T,
}

impl<T> RouteService<T>
where
  T: Service<crate::Request, Response = crate::Response, Error = (hyper::Error, crate::Request)>,
{
  fn new(service: T) -> Self {
    RouteService { service }
  }
}

impl<T> Service<crate::Request> for RouteService<T>
where
  T::Future: 'static,
  T: Service<crate::Request, Response = crate::Response, Error = (hyper::Error, crate::Request)>,
{
  type Response = crate::Response;
  type Error = hyper::Error;
  type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

  fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
    self.service.poll_ready(cx).map_err(|(e, _)| e)
  }

  fn call(&mut self, req: crate::Request) -> Self::Future {
    self
      .service
      .call(req)
      .map(|res| match res {
        Ok(res) => Ok(res),
        Err((_, req)) => Ok(Response::builder().body(req.into_body()).unwrap()),
      })
      .boxed_local()
  }
}
