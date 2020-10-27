use crate::endpoint::Endpoint;
use crate::router::Router;
use crate::{Body, Response, StatusCode};
use futures::future::BoxFuture;
use http::Method;
use hyper::service::Service;
use std::task::{Context, Poll};

pub struct App {
  pub router: Router<Method, Endpoint>,
}

impl App {
  pub async fn serve(&self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let addr = ([127, 0, 0, 1], 3000).into();
    hyper::Server::bind(&addr).serve(MakeApp()).await?;
    Ok(())
  }
}

impl Service<http::Request<Body>> for App {
  type Response = Response<Body>;
  type Error = hyper::Error;
  type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

  fn poll_ready(&mut self, _: &mut Context) -> Poll<Result<(), Self::Error>> {
    Poll::Ready(Ok(()))
  }

  fn call(&mut self, _: http::Request<Body>) -> Self::Future {
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

struct MakeApp();

impl<T> Service<T> for MakeApp {
  type Response = App;
  type Error = hyper::Error;
  type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

  fn poll_ready(&mut self, _: &mut Context) -> Poll<Result<(), Self::Error>> {
    Poll::Ready(Ok(()))
  }

  fn call(&mut self, _: T) -> Self::Future {
    let fut = async move {
      Ok(App {
        router: Router::default(),
      })
    };
    Box::pin(fut)
  }
}

// use crate::endpoint::Endpoint;
// use crate::router::Router;
// use crate::{Body, Request, Response, StatusCode};
// use futures::future::BoxFuture;
// use http::Method;
// use hyper::service::Service;
// use std::task::{Context, Poll};

// pub struct App {
//   pub router: Router<Method, Endpoint>,
// }

// impl App {
//   pub async fn serve(&self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
//     let addr = ([127, 0, 0, 1], 3000).into();
//     hyper::Server::bind(&addr).serve(MakeApp()).await?;
//     Ok(())
//   }
// }

// impl Service<http::Request<Body>> for App {
//   type Response = Response<Body>;
//   type Error = hyper::Error;
//   type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

//   fn poll_ready(&mut self, _: &mut Context) -> Poll<Result<(), Self::Error>> {
//     Poll::Ready(Ok(()))
//   }

//   fn call(&mut self, _: http::Request<Body>) -> Self::Future {
//     let fut = async move {
//       Ok(
//         Response::builder()
//           .status(StatusCode::IM_USED)
//           .body(Body::from("HEY!!!"))
//           .unwrap(),
//       )
//     };
//     Box::pin(fut)
//   }
// }

// struct MakeApp();

// impl<T> Service<T> for MakeApp {
//   type Response = App;
//   type Error = hyper::Error;
//   type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

//   fn poll_ready(&mut self, _: &mut Context) -> Poll<Result<(), Self::Error>> {
//     Poll::Ready(Ok(()))
//   }

//   fn call(&mut self, _: T) -> Self::Future {
//     let fut = async move {
//       Ok(App {
//         router: Router::default(),
//       })
//     };
//     Box::pin(fut)
//   }
// }
