use crate::endpoint::Endpoint;
use crate::router::Router;
use http::Method;

pub struct App {
  pub router: Router<Method, Endpoint>,
}
