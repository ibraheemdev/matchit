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

`matchit` relies on a tree structure which makes heavy use of *common prefixes*, effectively a [radix tree](https://en.wikipedia.org/wiki/Radix_tree). This makes lookups extremely fast. [See below for technical details](#how-does-it-work).

The tree is optimized for high performance and a small memory footprint. It scales well even with very long paths and a large number of routes. A compressing dynamic trie (radix tree) structure is used for efficient matching.

### Parameters

As you can see, `:id` is a *named parameter*. The values are accessible via [`Params`](https://docs.rs/matchit/*/matchit/tree/struct.Params.html), which stores a list of keys and values. You can get the value of a parameter by name, `params.get("id")`, or by iterating through the list.

The registered path can contain two types of parameters:

```text
Syntax    Type
:name     named parameter
*name     catch-all parameter
```

### Named Parameters

Named parameters are dynamic route segments. They match anything until the next `/` or the path end:

```text
Route: /user/:user

 /user/gordon              match: user = "gordon"
 /user/you                 match: user = "you"
 /user/gordon/profile      no match
 /user/                    no match
```

### Catch-All parameters

The second type are *catch-all* parameters and have the form `*name`. Like the name suggests, they match everything. Therefore they must always be at the **end** of the pattern:

```text
Route: /src/*filepath

 /src/                       match: filepath = "/"
 /src/somefile.html          match: filepath = "/somefile.html"
 /src/subdir/somefile.html   match: filepath = "/subdir/somefile.html"
```

### Priority

Static and dynamic route segments are allowed to overlap. If they do, static segments will be given higher priority:
```text
/:page
/posts/:year/:month/:post
/posts/:year/:month/index
/posts/:year/:month
/static/*path
/favicon.ico
```

The following routes will be matched:
```text
/about                => /:page
/posts/2021/01/rust   => /posts/:year/:month/:post
/posts/2021/01/index  => /posts/:year/:month/index
/posts/2021/top       => /posts/:year/top
/static/foo.png       => /static/*path
/favicon.ico          => /favicon.ico
```

## How does it work?

The matcher relies on a tree structure which makes heavy use of *common prefixes*, it is basically a *compact* [*prefix tree*](https://en.wikipedia.org/wiki/Trie) (or [*Radix tree*](https://en.wikipedia.org/wiki/Radix_tree)). Nodes with a common prefix share a parent. Here is a short example what the routing tree for the `GET` request method could look like:

```text
Priority   Path             Handle
9          \                *<1>
3          ├s               None
2          |├earch\         *<2>
1          |└upport\        *<3>
2          ├blog\           *<4>
1          |    └:post      None
1          |         └\     *<5>
2          ├about-us\       *<6>
1          |        └team\  *<7>
1          └contact\        *<8>
```

Every `*<num>` represents the memory address of a handler function (a pointer). If you follow a path trough the tree from the root to the leaf, you get the complete route path, e.g `/blog/:post`, where `:post` is just a placeholder ([*parameter*](#named-parameters)) for an actual post name. Unlike hash-maps, a tree structure also allows us to use dynamic parts like the `:post` parameter, since we actually match against the routing patterns instead of just comparing hashes. This works very efficiently.

Because URL paths have a hierarchical structure and make use only of a limited set of characters (byte values), it is very likely that there are a lot of common prefixes. Storing the routes in this structure allows us to easily reduce the routing into a very small number of branches.

For even better scalability, the child nodes on each tree level are ordered by priority, where the priority is just the number of handles registered in child nodes. This means that nodes that are part of the most routing paths are always evaluated first, increasing the chance of reaching the correct route on our first try.

```test
├------------
├---------
├-----
├----
├--
├--
└-
```

# Benchmarks

As it turns out, this method of routing is extremely fast. In a benchmark matching 4 paths against 130 registered routes, `matchit` find the correct routes in just over 200 nanoseconds, an order of magnitude faster than most other routers. You can view the benchmark code [here](https://github.com/ibraheemdev/matchit/blob/master/benches/bench.rs). 

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
