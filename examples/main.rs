use httprouter::{BoxedHandler, Handler, Params, Router};
use hyper::{Body, Request, Response};

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
  let mut router: Router<BoxedHandler> = Router::default();
  router.get("/hello/:name", Handler::new(hello));
  router.get("/index", Handler::new(index));

  hyper::Server::bind(&([127, 0, 0, 1], 3000).into())
    .serve(router.into_service())
    .await?;
  Ok(())
}
