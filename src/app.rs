use crate::route::Route;
use crate::router::Router;
use crate::{Body, Request, Response};
use futures::future::{ok, BoxFuture};
use http::{header, Method, StatusCode};
use hyper::service::{make_service_fn, Service};
use std::convert::Infallible;
use std::net::SocketAddr;
use std::sync::Arc;
use std::task::{Context, Poll};

pub struct App {
  router: Router<Method, Route>,
  addr: SocketAddr,
}

impl Default for App {
  fn default() -> Self {
    App {
      router: Router::default(),
      addr: ([127, 0, 0, 1], 3000).into(),
    }
  }
}

pub struct AppService(Arc<App>);

impl App {
  pub async fn serve(self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let app = Arc::new(self);
    let addr = app.addr;
    let service = make_service_fn(move |_| {
      let app = app.clone();
      async move { Ok::<_, Infallible>(AppService(app.clone())) }
    });
    hyper::Server::bind(&addr).serve(service).await?;
    Ok(())
  }
}

impl Service<hyper::Request<Body>> for AppService {
  type Response = Response<Body>;
  type Error = hyper::Error;
  type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

  fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
    Poll::Ready(Ok(()))
  }

  fn call(&mut self, mut req: http::Request<Body>) -> Self::Future {
    let method_map = self.0.router.map.get(req.method());

    match method_map {
      Some(method_map) => match method_map.get_value(req.uri().path()) {
        Ok(route) => {
          req.extensions_mut().insert(route.params);
          route.value.handler.call(Request::new(req))
        }
        Err(tsr) => match tsr {
          true => {
            let path = match req.uri().path().len() > 1 && req.uri().path().ends_with('/') {
              true => req.uri().path()[..req.uri().path().len() - 1].to_string(),
              false => req.uri().path().to_string() + "/",
            };
            let res = Response::builder()
              .header(header::LOCATION, path.as_str())
              .status(StatusCode::TEMPORARY_REDIRECT)
              .body(Body::empty())
              .unwrap();
            Box::pin(ok(res))
          }
          false => match method_map.find_case_insensitive_path(req.uri().path(), true) {
            Some(fixed_path) => {
              let res = Response::builder()
                .header(header::LOCATION, fixed_path.as_str())
                .status(StatusCode::TEMPORARY_REDIRECT)
                .body(Body::empty())
                .unwrap();
              Box::pin(ok(res))
            }
            None => {
              let response = Response::builder()
                .status(404)
                .body(Body::default())
                .unwrap();
              Box::pin(ok(response))
            }
          },
        },
      },
      None => {
        let response = Response::builder()
          .status(404)
          .body(Body::default())
          .unwrap();
        Box::pin(ok(response))
      }
    }
  }
}

pub struct AppBuilder {
  addr: Option<SocketAddr>,
}

impl AppBuilder {
  pub fn bind(&mut self, addr: impl Into<SocketAddr>) -> &Self {
    self.addr = Some(addr.into());
    self
  }
}
