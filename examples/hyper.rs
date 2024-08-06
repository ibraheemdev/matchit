use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use hyper::body::Incoming;
use hyper::server::conn::http1::Builder as ConnectionBuilder;
use hyper::{Method, Request, Response};
use hyper_util::rt::TokioIo;
use tokio::net::TcpListener;
use tower::service_fn;
use tower::util::BoxCloneService;
use tower::Service as _;

use self::body::Body;

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
    Ok(Response::builder().status(404).body(Body::empty()).unwrap())
}

// We can use `BoxCloneService` to erase the type of each handler service.
//
// We still need a `Mutex` around each service because `BoxCloneService` doesn't
// require the service to implement `Sync`.
type Service = Mutex<BoxCloneService<Request<Incoming>, Response<Body>, hyper::Error>>;

// We use a `HashMap` to hold a `Router` for each HTTP method. This allows us
// to register the same route for multiple methods.
type Router = HashMap<Method, matchit::Router<Service>>;

async fn route(router: Arc<Router>, req: Request<Incoming>) -> hyper::Result<Response<Body>> {
    // find the subrouter for this request method
    let router = match router.get(req.method()) {
        Some(router) => router,
        // if there are no routes for this method, respond with 405 Method Not Allowed
        None => return Ok(Response::builder().status(405).body(Body::empty()).unwrap()),
    };

    // find the service for this request path
    match router.at(req.uri().path()) {
        Ok(found) => {
            // lock the service for a very short time, just to clone the service
            let mut service = found.value.lock().unwrap().clone();
            service.call(req).await
        }
        // if we there is no matching service, call the 404 handler
        Err(_) => not_found(req).await,
    }
}

#[tokio::main]
async fn main() {
    // Create a router and register our routes.
    let mut router = Router::new();

    // GET / => `index`
    router
        .entry(Method::GET)
        .or_default()
        .insert("/", BoxCloneService::new(service_fn(index)).into())
        .unwrap();

    // GET /blog => `blog`
    router
        .entry(Method::GET)
        .or_default()
        .insert("/blog", BoxCloneService::new(service_fn(blog)).into())
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

mod body {
    use std::convert::Infallible;
    use std::pin::Pin;
    use std::task::{Context, Poll};

    use hyper::body::{Body as HttpBody, Bytes, Frame};

    pub enum Body {
        Empty,
        Once(Option<Bytes>),
    }

    impl HttpBody for Body {
        type Data = Bytes;
        type Error = Infallible;

        fn poll_frame(
            mut self: Pin<&mut Self>,
            _cx: &mut Context<'_>,
        ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
            match &mut self.as_mut().get_mut() {
                Self::Empty => Poll::Ready(None),
                Self::Once(val) => Poll::Ready(val.take().map(|bytes| Ok(Frame::data(bytes)))),
            }
        }
    }

    impl Body {
        pub fn empty() -> Self {
            Self::Empty
        }
    }

    impl From<&str> for Body {
        fn from(s: &str) -> Self {
            if s.is_empty() {
                Self::Empty
            } else {
                Self::Once(Some(Bytes::from(s.as_bytes().to_vec())))
            }
        }
    }
}
