# MatchIt

[![Documentation](https://img.shields.io/badge/docs-0.4.4-4d76ae?style=for-the-badge)](https://docs.rs/matchit)
[![Version](https://img.shields.io/crates/v/matchit?style=for-the-badge)](https://crates.io/crates/matchit)
[![License](https://img.shields.io/crates/l/matchit?style=for-the-badge)](https://crates.io/crates/matchit)
[![Actions](https://img.shields.io/github/workflow/status/ibraheemdev/matchit/Rust/master?style=for-the-badge)](https://github.com/ibraheemdev/matchit/actions)

A blazing fast URL router and path matcher.

```rust
use matchit::Node;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut matcher = Node::new();
    matcher.insert("/home", "Welcome!")?;
    matcher.insert("/users/:id", "A User")?;

    let matched = matcher.at("/users/978")?;
    assert_eq!(matched.params.get("id"), Some("978"));
    assert_eq!(*matched.value, "A User");

    Ok(())
}
```


## Parameters

The matcher supports dynamic route segments. These are accessible by-name through the [`Params`](https://docs.rs/matchit/*/matchit/struct.Params.html) struct,
which is returned on a successful match attempt.

A registered route can contain named or catch-all parameters.

### Named Parameters

Named parameters like `/:id` match anything until the next `/` or the path end:

```rust,ignore
let mut m = Node::new();
m.insert("/users/:id", true)?;

assert_eq!(m.at("/users/1")?.params.get("id"), Some("1"));
assert_eq!(m.at("/users/23")?.params.get("id"), Some("23"));
assert!(m.at("/users").is_err());
```

### Catch-all Parameters

Catch-all parameters start with `*` and match everything including the trailing slash. They must always be at the **end** of the route:

```rust,ignore
let mut m = Node::new();
m.insert("/*p", true)?;

assert_eq!(m.at("/")?.params.get("p"), Some("/"));
assert_eq!(m.at("/foo.js")?.params.get("p"), Some("/foo.js"));
assert_eq!(m.at("/c/bar.css")?.params.get("p"), Some("/c/bar.css"));
```

## Routing Priority

Static and dynamic route segments are allowed to overlap. If they do, static segments will be given higher priority:
```rust,ignore
let mut m = Node::new();
m.insert("/home", "Welcome!").unwrap();  // priority: 1
m.insert("/about", "About Me").unwrap(); // priority: 1
m.insert("/:other", "...").unwrap();     // priority: 2
```

Catch-all parameters however are not allowed to overlap with other path segments. Attempting to insert a conflicting route will result
in an error:
```rust,ignore
let mut m = Node::new();
m.insert("/home", "Welcome!").unwrap();

assert_eq!(
    m.insert("/*filepath", "..."),
    Err(InsertError::Conflict {
        with: "/home".into()
    })
);
```

## How does it work?

Because URL paths follow a hierarchical structure, the matcher relies on a radix tree structure that makes heavy use of common prefixes:

```text
Priority   Path             Value
9          \                1
3          ├s               None
2          |├earch\         2
1          |└upport\        3
2          ├blog\           4
1          |    └:post      None
1          |         └\     5
2          ├about-us\       6
1          |        └team\  7
1          └contact\        8
```

This allows us to reduce the route search to a small number of branches. Child nodes on the same level of the tree are also prioritized
by the number of children with registered values, increasing the chance of choosing the correct branch of the first try.

# Benchmarks

As it turns out, this method of routing is extremely fast. In a benchmark matching 4 paths against 130 registered routes, `matchit` find the correct routes
in just over 200 nanoseconds, an order of magnitude faster than most other routers. You can view the benchmark code [here](https://github.com/ibraheemdev/matchit/blob/master/benches/bench.rs). 

```text
Compare Routers/matchit 
time:   [203.73 ns 204.07 ns 204.45 ns]

Compare Routers/actix   
time:   [31.629 us 31.664 us 31.701 us]

Compare Routers/regex   
time:   [21.995 us 22.144 us 22.319 us]

Compare Routers/route-recognizer
time:   [4.2389 us 4.2434 us 4.2482 us]
```

# Credits

A lot of the code in this package was based on Julien Schmidt's [`httprouter`](https://github.com/julienschmidt/httprouter).
