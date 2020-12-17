# HttpRouter

HttpRouter is a lightweight high performance HTTP request router.

This router supports variables in the routing pattern and matches against the request method. It also scales better.

The router is optimized for high performance and a small memory footprint. It scales well even with very long paths and a large number of routes. A compressing dynamic trie (radix tree) structure is used for efficient matching.

## Features

**Not tied to any http implementation** HttpRouter is not tied to any http implementation, as the `Router` is generic. It currently has a [`hyper`](https://crates.io/crates/hyper) backend available.

**Only explicit matches:** With other routers, a requested URL path could match multiple patterns. Therefore they have some awkward pattern priority rules, like *longest match* or *first registered, first matched*. By design of this router, a request can only match exactly one or no route. As a result, there are also no unintended matches, which makes it great for SEO and improves the user experience.

**Stop caring about trailing slashes:** Choose the URL style you like, the router automatically redirects the client if a trailing slash is missing or if there is one extra. Of course it only does so, if the new path has a handler. If you don't like it, you can [turn off this behavior](https://docs.rs/httprouter/0.0.0/httprouter/router/struct.Router.html#structfield.redirect_trailing_slash).

**Path auto-correction:** Besides detecting the missing or additional trailing slash at no extra cost, the router can also fix wrong cases and remove superfluous path elements (like `../` or `//`). Is [CAPTAIN CAPS LOCK](http://www.urbandictionary.com/define.php?term=Captain+Caps+Lock) one of your users? HttpRouter can help him by making a case-insensitive look-up and redirecting him to the correct URL.

**Parameters in your routing pattern:** Stop parsing the requested URL path, just give the path segment a name and the router delivers the dynamic value to you. Because of the design of the router, path parameters are very cheap.

**High Performance:** HttpRouter relies on a tree structure which makes heavy use of *common prefixes*, it is basically a [radix tree](https://en.wikipedia.org/wiki/Radix_tree). This makes lookups extremely fast. [See below for technical details](#how-does-it-work).

**Perfect for APIs:** The router design encourages to build sensible, hierarchical RESTful APIs. Moreover it has built-in native support for [OPTIONS requests](http://zacstewart.com/2012/04/14/http-options-method.html) and `405 Method Not Allowed` replies.

Of course you can also set **custom [`NotFound`](https://docs.rs/httprouter/0.0.0/httprouter/router/struct.Router.html#structfield.not_found) and  [`MethodNotAllowed`](https://docs.rs/httprouter/0.0.0/httprouter/router/struct.Router.html#structfield.method_not_allowedd) handlers** and [**serve static files**](https://docs.rs/httprouter/0.0.0/httprouter/router/struct.Router.html#method.serve_files).

## Usage

With the `hyper-server` feature enabled, the `Router` can be used as a router for a hyper server:

```rust,no_run
use httprouter::{Router, HyperRouter, Params, Handler};
use std::convert::Infallible;
use hyper::{Request, Response, Body, Error};

async fn index(_: Request<Body>) -> Result<Response<Body>, Error> {
    Ok(Response::new("Hello, World!".into()))
}

async fn hello(req: Request<Body>) -> Result<Response<Body>, Error> {
    let params = req.extensions().get::<Params>().unwrap();
    Ok(Response::new(format!("Hello, {}", params.by_name("user").unwrap()).into()))
}

#[tokio::main]
async fn main() {
    let mut router: HyperRouter = Router::default();
    router.get("/", Handler::new(index));
    router.get("/hello/:user", Handler::new(hello));

    hyper::Server::bind(&([127, 0, 0, 1], 3000).into())
        .serve(router.into_service())
        .await;
}
```

Because the `Router` is generic, it can be used to store arbitrary values. This makes it flexible enough to be used as a building block for larger framework:

```rust
use httprouter::Router;

fn main() {
    let mut router: Router<usize, String> = Router::default();
    router.handle("/users/:id", 1, "Welcome!".to_string());

    let res = router.lookup(&1, "/users/200").unwrap();
    assert_eq!(res.params.by_name("id"), Some("200"));
    assert_eq!(res.value, &"Welcome!".to_string());
}
```

### Named parameters

As you can see, `:user` is a *named parameter*. The values are accessible via `req.extensions().get::<Params>()`, which is just a vector of keys and values. You can get the value of a parameter either by its index in the vector, or by using the `by_name(name)` method: `:user` can be retrieved by `by_name("user")`.

Named parameters only match a single path segment:

```ignore
Pattern: /user/:user

 /user/gordon              match
 /user/you                 match
 /user/gordon/profile      no match
 /user/                    no match
```

**Note:** Since this router has only explicit matches, you can not register static routes and parameters for the same path segment. For example you can not register the patterns `/user/new` and `/user/:user` for the same request method at the same time. The routing of different request methods is independent from each other.

### Catch-All parameters

The second type are *catch-all* parameters and have the form `*name`. Like the name suggests, they match everything. Therefore they must always be at the **end** of the pattern:

```ignore
Pattern: /src/*filepath

 /src/                     match
 /src/somefile.go          match
 /src/subdir/somefile.go   match
```

## How does it work?

The router relies on a tree structure which makes heavy use of *common prefixes*, it is basically a *compact* [*prefix tree*](https://en.wikipedia.org/wiki/Trie) (or just [*Radix tree*](https://en.wikipedia.org/wiki/Radix_tree)). Nodes with a common prefix also share a common parent. Here is a short example what the routing tree for the `GET` request method could look like:

```ignore,none
Priority   Path             Handle
9          \                *<1>
3          ├s               nil
2          |├earch\         *<2>
1          |└upport\        *<3>
2          ├blog\           *<4>
1          |    └:post      nil
1          |         └\     *<5>
2          ├about-us\       *<6>
1          |        └team\  *<7>
1          └contact\        *<8>
```

Every `*<num>` represents the memory address of a handler function (a pointer). If you follow a path trough the tree from the root to the leaf, you get the complete route path, e.g `\blog\:post\`, where `:post` is just a placeholder ([*parameter*](#named-parameters)) for an actual post name. Unlike hash-maps, a tree structure also allows us to use dynamic parts like the `:post` parameter, since we actually match against the routing patterns instead of just comparing hashes. This works very well and efficiently.

Since URL paths have a hierarchical structure and make use only of a limited set of characters (byte values), it is very likely that there are a lot of common prefixes. This allows us to easily reduce the routing into ever smaller problems. Moreover the router manages a separate tree for every request method. For one thing it is more space efficient than holding a method->handle map in every single node, it also allows us to greatly reduce the routing problem before even starting the look-up in the prefix-tree.

For even better scalability, the child nodes on each tree level are ordered by priority, where the priority is just the number of handles registered in sub nodes (children, grandchildren, and so on..). This helps in two ways:

1. Nodes which are part of the most routing paths are evaluated first. This helps to make as much routes as possible to be reachable as fast as possible.
2. It is some sort of cost compensation. The longest reachable path (highest cost) can always be evaluated first. The following scheme visualizes the tree structure. Nodes are evaluated from top to bottom and from left to right.

```ignore,none
├------------
├---------
├-----
├----
├--
├--
└-
```

## Automatic OPTIONS responses and CORS

One might wish to modify automatic responses to OPTIONS requests, e.g. to support [CORS preflight requests](https://developer.mozilla.org/en-US/docs/Glossary/preflight_request) or to set other headers. This can be achieved using the [`Router.GlobalOPTIONS`](https://docs.rs/httprouter/0.0.0/httprouter/router/struct.Router.html#structfield.global_options) handler:

```rust
use httprouter::{Router, HyperRouter, Handler};
use hyper::{Request, Response, Body, Error};

async fn global_options(_: Request<Body>) -> Result<Response<Body>, Error> {
    Ok(Response::builder()
        .header("Access-Control-Allow-Methods", "Allow")
	.header("Access-Control-Allow-Origin", "*")
        .body(Body::empty())
        .unwrap())
}

fn main() {
  let mut router: HyperRouter = Router::default();
  router.global_options = Some(Handler::new(global_options));
}
```

### Multi-domain / Sub-domains

Here is a quick example: Does your server serve multiple domains / hosts? You want to use sub-domains? Define a router per host!

```rust,no_run
use futures::future::{ok, BoxFuture};
use httprouter::{Handler, HyperRouter, Router};
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response, Result, Server, StatusCode};
use std::collections::HashMap;
use std::convert::Infallible;
use std::sync::Arc;

pub struct HostSwitch(HashMap<String, HyperRouter>);

impl HostSwitch {
    fn serve(&self, req: Request<Body>) -> BoxFuture<'static, Result<Response<Body>>> {
        let forbidden = Response::builder()
            .status(StatusCode::FORBIDDEN)
            .body(Body::empty())
            .unwrap();
        match req.headers().get("host") {
            Some(host) => match self.0.get(host.to_str().unwrap()) {
                Some(router) => router.serve(req),
                None => Box::pin(ok(forbidden)),
            },
            None => Box::pin(ok(forbidden)),
        }
    }
}

async fn hello(_: Request<Body>) -> Result<Response<Body>> {
    Ok(Response::new(Body::default()))
}

#[tokio::main]
async fn main() {
    let mut router: HyperRouter = Router::default();
    router.get("/", Handler::new(hello));

    let mut host_switch: HostSwitch = HostSwitch(HashMap::new());
    host_switch.0.insert("example.com:12345".into(), router);

    let host_switch = Arc::new(host_switch);
    let make_svc = make_service_fn(move |_| {
        let host_switch = host_switch.clone();
        async move {
            Ok::<_, Infallible>(service_fn(move |req: Request<Body>| {
                let fut = host_switch.serve(req);
                async move { fut.await }
            }))
        }
    });

    let server = Server::bind(&([127, 0, 0, 1], 3000).into())
        .serve(make_svc)
        .await;
}
```

### Basic Authentication

Another quick example: Basic Authentication (RFC 2617) for handles:

```rust
// TODO
```

### Not Found Handler

**NOTE: It might be required to set [`Router::method_not_allowed`](https://docs.rs/httprouter/0.0.0/httprouter/router/struct.Router.html#structfield.method_not_allowed) to `None` to avoid problems.**

You can use another handler, to handle requests which could not be matched by this router by using the [`Router::not_found`](https://docs.rs/httprouter/0.0.0/httprouter/router/struct.Router.html#structfield.not_found) handler.

The `not_found` handler can for example be used to return a 404 page:

```rust
use httprouter::{Router, HyperRouter, Handler};
use hyper::{Request, Response, Body};

async fn not_found(_: Request<Body>) -> Result<Response<Body>, hyper::Error> {
  Ok(Response::builder()
    .header("Location", "/404")
    .status(404)
    .body(Body::empty())
    .unwrap())
};

fn main() {
  let mut router: HyperRouter = Router::default();
  router.not_found = Some(Handler::new(not_found));
}
```

### Static files

You can use the router to serve pages from a static file directory:

```rust
// TODO
```
