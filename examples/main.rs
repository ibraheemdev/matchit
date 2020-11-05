use httprouter::{
  router::hyper_server::{BoxedHandler, HandlerS},
  Params, Router,
};
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response};
use std::convert::Infallible;
use std::sync::Arc;

async fn index(_: Request<Body>) -> Result<Response<Body>, Infallible> {
  Ok(Response::new("Hello, World!".into()))
}

async fn hello(req: Request<Body>) -> Result<Response<Body>, Infallible> {
  let params = req.extensions().get::<Params>().unwrap();
  Ok(Response::new(
    format!("Hello, {}", params.by_name("name").unwrap()).into(),
  ))
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
  let mut router = Router::<BoxedHandler>::default();
  router.get("/hello/:name", Box::new(HandlerS::new(hello)));
  router.get("/index", Box::new(HandlerS::new(index)));

  serve(router).await
}

pub async fn serve(
  router: Router<BoxedHandler>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
  let service = Arc::new(router);
  let make_svc = make_service_fn(move |_| {
    let service = service.clone();
    async move { Ok::<_, Infallible>(service_fn(move |req| service.clone().serve(req))) }
  });
  hyper::Server::bind(&([127, 0, 0, 1], 3000).into())
    .serve(make_svc)
    .await?;
  Ok(())
}
