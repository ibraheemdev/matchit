/*!
A high performance, zero-copy URL router.

```rust
use matchit::Router;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut router = Router::new();
    router.insert("/home", "Welcome!")?;
    router.insert("/users/{id}", "A User")?;

    let matched = router.at("/users/978")?;
    assert_eq!(matched.params.get("id"), Some("978"));
    assert_eq!(*matched.value, "A User");

    Ok(())
}
```

# Parameters

The router supports dynamic route segments. These can either be named or catch-all parameters.

Named parameters like `/{id}` match anything until the next static segment or the end of the path.

```rust
# use matchit::Router;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let mut router = Router::new();
router.insert("/users/{id}", 42)?;

let matched = router.at("/users/1")?;
assert_eq!(matched.params.get("id"), Some("1"));

let matched = router.at("/users/23")?;
assert_eq!(matched.params.get("id"), Some("23"));

assert!(router.at("/users").is_err());
# Ok(())
# }
```

Prefixes and suffixes within a segment are also supported. However, there may only be a single named parameter per route segment.
```rust
# use matchit::Router;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let mut router = Router::new();
router.insert("/images/img-{id}.png", true)?;

let matched = router.at("/images/img-1.png")?;
assert_eq!(matched.params.get("id"), Some("1"));

assert!(router.at("/images/img-1.jpg").is_err());
# Ok(())
# }
```

Catch-all parameters start with a `*` and match anything until the end of the path. They must always be at the *end* of the route.

```rust
# use matchit::Router;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let mut router = Router::new();
router.insert("/{*rest}", true)?;

let matched = router.at("/foo.html")?;
assert_eq!(matched.params.get("rest"), Some("foo.html"));

let matched = router.at("/static/bar.css")?;
assert_eq!(matched.params.get("rest"), Some("static/bar.css"));

// Note that this would lead to an empty parameter value.
assert!(router.at("/").is_err());
# Ok(())
# }
```

The literal characters `{` and `}` may be included in a static route by escaping them with the same character. For example, the `{` character is escaped with `{{`, and the `}` character is escaped with `}}`.

```rust
# use matchit::Router;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let mut router = Router::new();
router.insert("/{{hello}}", true)?;
router.insert("/{hello}", true)?;

// Match the static route.
let matched = router.at("/{hello}")?;
assert!(matched.params.is_empty());

// Match the dynamic route.
let matched = router.at("/hello")?;
assert_eq!(matched.params.get("hello"), Some("hello"));
# Ok(())
# }
```

# Conflict Rules

Static and dynamic route segments are allowed to overlap. If they do, static segments will be given higher priority:

```rust
# use matchit::Router;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let mut router = Router::new();
router.insert("/", "Welcome!").unwrap();       // Priority: 1
router.insert("/about", "About Me").unwrap();  // Priority: 1
router.insert("/{*filepath}", "...").unwrap();  // Priority: 2
# Ok(())
# }
```

Formally, a route consists of a list of segments separated by `/`, with an optional leading and trailing slash: `(/)<segment_1>/.../<segment_n>(/)`.

Given set of routes, their overlapping segments may include, in order of priority:

- Any number of static segments (`/a`, `/b`, ...).
- *One* of the following:
  - Any number of route parameters with a suffix (`/{x}a`, `/{x}b`, ...), prioritizing the longest suffix.
  - Any number of route parameters with a prefix (`/a{x}`, `/b{x}`, ...), prioritizing the longest prefix.
  - A single route parameter with both a prefix and a suffix (`/a{x}b`).
- *One* of the following;
  - A single standalone parameter (`/{x}`).
  - A single standalone catch-all parameter (`/{*rest}`). Note this only applies to the final route segment.

Any other combination of route segments is considered ambiguous, and attempting to insert such a route will result in an error.

The one exception to the above set of rules is that catch-all parameters are always considered to conflict with suffixed route parameters, i.e. that `/{*rest}`
and `/{x}suffix` are overlapping. This is due to an implementation detail of the routing tree that may be relaxed in the future.

# How does it work?

The router takes advantage of the fact that URL routes generally follow a hierarchical structure. Routes are stored them in a radix trie that makes heavy use of common prefixes.

```text
Priority   Path             Value
9          \                1
3          ├s               None
2          |├earch\         2
1          |└upport\        3
2          ├blog\           4
1          |    └{post}     None
1          |          └\    5
2          ├about-us\       6
1          |        └team\  7
1          └contact\        8
```

This allows us to reduce the route search to a small number of branches. Child nodes on the same level of the tree are also prioritized
by the number of children with registered values, increasing the chance of choosing the correct branch of the first try.

As it turns out, this method of routing is extremely fast. See the [benchmark results](https://github.com/ibraheemdev/matchit?tab=readme-ov-file#benchmarks) for details.
*/

#![deny(
    missing_debug_implementations,
    missing_docs,
    dead_code,
    unsafe_op_in_unsafe_fn,
    rustdoc::broken_intra_doc_links
)]

mod error;
mod escape;
mod params;
mod router;
mod tree;

pub use error::{InsertError, MatchError, MergeError};
pub use params::{Params, ParamsIter};
pub use router::{Match, Router};

#[cfg(doctest)]
mod readme {
    #[allow(dead_code)]
    #[doc = include_str!("../README.md")]
    struct Readme;
}
