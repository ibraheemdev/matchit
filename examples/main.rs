use httprouter::{
  router::hyper_server::{BoxedHandler, RouterService},
  Params, Router,
};
use hyper::service::make_service_fn;
use hyper::{Body, Request, Response};
use std::convert::Infallible;
use std::sync::Arc;

async fn index(_: Request<Body>) -> Result<Response<Body>, hyper::Error> {
  Ok(Response::new("Hello, World!".into()))
}

async fn hello(req: Request<Body>) -> Result<Response<Body>, hyper::Error> {
  let params = req.extensions().get::<Params>().unwrap();
  Ok(Response::new(
    format!("Hello, {}", params.by_name("name").unwrap()).into(),
  ))
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
  let mut router = Router::<BoxedHandler>::default();
  router.get("/hello/:name", Box::new(hello));
  router.get("/index", Box::new(index));

  serve(router).await
}

pub async fn serve(
  router: Router<BoxedHandler>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
  let service = RouterService(Arc::new(router));
  let make_svc = make_service_fn(move |_| futures::future::ok::<_, Infallible>(service.clone()));
  hyper::Server::bind(&([127, 0, 0, 1], 3000).into())
    .serve(make_svc)
    .await?;
  Ok(())
}
