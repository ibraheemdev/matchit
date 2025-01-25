use std::collections::HashMap;
use std::future;
use std::sync::Arc;

use http_body_util::Full;
use hyper::body::{Bytes, Incoming};
use hyper::server::conn::http1::Builder as ConnectionBuilder;
use hyper::{Method, Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
use tokio::net::TcpListener;
use tower::service_fn;
use tower::Service as _;

type Body = Full<Bytes>;

// GET /
async fn index(_req: Request<Incoming>) -> hyper::Result<Response<Body>> {
    Ok(Response::new(Body::from("Hello, world!")))
}

// GET /blog
async fn blog(_req: Request<Incoming>) -> hyper::Result<Response<Body>> {
    Ok(Response::new(Body::from("...")))
}

// 404 handler
async fn not_found(_req: Request<Incoming>) -> hyper::Result<Response<Body>> {
    Ok(Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(Body::default())
        .unwrap())
}

// We can use `BoxCloneSyncService` to erase the type of each handler service.
type Service = tower::util::BoxCloneSyncService<Request<Incoming>, Response<Body>, hyper::Error>;

// We use a `HashMap` to hold a `Router` for each HTTP method. This allows us
// to register the same route for multiple methods.
type Router = HashMap<Method, matchit::Router<Service>>;

async fn route(router: Arc<Router>, req: Request<Incoming>) -> hyper::Result<Response<Body>> {
    // find the subrouter for this request method
    let Some(router) = router.get(req.method()) else {
        // if there are no routes for this method, respond with 405 Method Not Allowed
        return Ok(Response::builder()
            .status(StatusCode::METHOD_NOT_ALLOWED)
            .body(Body::default())
            .unwrap());
    };

    // find the service for this request path
    let Ok(found) = router.at(req.uri().path()) else {
        // if we there is no matching service, call the 404 handler
        return not_found(req).await;
    };

    let mut service = found.value.clone();

    future::poll_fn(|cx| service.poll_ready(cx)).await?;

    service.call(req).await
}

#[tokio::main]
async fn main() {
    // Create a router and register our routes.
    let mut router = Router::new();

    // GET / => `index`
    router
        .entry(Method::GET)
        .or_default()
        .insert("/", Service::new(service_fn(index)))
        .unwrap();

    // GET /blog => `blog`
    router
        .entry(Method::GET)
        .or_default()
        .insert("/blog", Service::new(service_fn(blog)))
        .unwrap();

    let listener = TcpListener::bind(("127.0.0.1", 3000)).await.unwrap();

    // boilerplate for the hyper service
    let router = Arc::new(router);

    loop {
        let router = router.clone();
        let (tcp, _) = listener.accept().await.unwrap();
        tokio::task::spawn(async move {
            if let Err(err) = ConnectionBuilder::new()
                .serve_connection(
                    TokioIo::new(tcp),
                    hyper::service::service_fn(|request| async {
                        route(router.clone(), request).await
                    }),
                )
                .await
            {
                println!("Error serving connection: {:?}", err);
            }
        });
    }
}
