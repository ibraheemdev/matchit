use crate::route::Route;
use crate::router::Router;

pub struct App {
  pub router: Router<Route>,
}
