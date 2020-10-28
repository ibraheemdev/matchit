use crate::endpoint::Endpoint;
use crate::router::Router;
use crate::{Body, Response, StatusCode};
use futures::future::BoxFuture;
use http::Method;
use hyper::service::{make_service_fn, service_fn};
use std::convert::Infallible;
use std::net::SocketAddr;
use std::sync::Arc;

pub struct App {
  router: Router<Method, Endpoint>,
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

impl App {
  pub async fn serve(self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let app = Arc::new(self);
    let addr = app.addr;
    let service = make_service_fn(move |_| {
      let app = app.clone();
      async move { Ok::<_, Infallible>(service_fn(move |req| app.clone().serve_http(req))) }
    });
    hyper::Server::bind(&addr).serve(service).await?;
    Ok(())
  }

  fn serve_http(
    &self,
    _: http::Request<Body>,
  ) -> BoxFuture<'static, Result<Response<Body>, hyper::Error>> {
    let fut = async move {
      Ok(
        Response::builder()
          .status(StatusCode::IM_USED)
          .body(Body::from("HEY!!!"))
          .unwrap(),
      )
    };

    Box::pin(fut)
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
