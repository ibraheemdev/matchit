#![deny(rust_2018_idioms)]

//! [![Documentation](https://img.shields.io/badge/docs-0.3.2-4d76ae?style=for-the-badge)](https://docs.rs/matchit/0.3.2)
//! [![Version](https://img.shields.io/crates/v/matchit?style=for-the-badge)](https://crates.io/crates/matchit)
//! [![License](https://img.shields.io/crates/l/matchit?style=for-the-badge)](https://crates.io/crates/matchit)
//! [![Actions](https://img.shields.io/github/workflow/status/ibraheemdev/matchit/Rust/master?style=for-the-badge)](https://github.com/ibraheemdev/matchit/actions)
//!
//! Matches URL patterns with support for dynamic and wildcard segments.
//!
//! ```rust
//! use matchit::Node;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut matcher = Node::new();
//! matcher.insert("/home", "Welcome!")?;
//! matcher.insert("/users/:id", "A User")?;
//!
//! let matched = matcher.at("/users/1")?;
//! assert_eq!(matched.params.get("id"), Some("1"));
//! assert_eq!(*matched.value, "A User");
//! # Ok(())
//! # }
//! ```
//!
//! `matchit` relies on a tree structure which makes heavy use of *common prefixes*, it is effectively a [radix tree](https://en.wikipedia.org/wiki/Radix_tree). This makes lookups extremely fast. [See below for technical details](#how-does-it-work).
//!
//! The tree is optimized for high performance and a small memory footprint. It scales well even with very long paths and a large number of routes. A compressing dynamic trie (radix tree) structure is used for efficient matching.
//!
//! ### Parameters
//!
//! As you can see, `:id` is a *parameter*. The values are accessible via [`Params`](https://docs.rs/matchit/0.2.0/matchit/tree/struct.Params.html), which stores a vector of keys and values. You can get the value of a parameter either by its index in the vector, or by using the `Params::get(name)` method. For example, `:user` can be retrieved by `params.get("user")`.
//!
//! The registered path can contain two types of parameters:
//! ```text
//! Syntax    Type
//! :name     named parameter
//! *name     catch-all parameter
//! ```
//!
//! ### Named Parameters
//!
//! Named parameters are dynamic path segments. They match anything until the next `/` or the path end:
//!
//! ```text
//! Pattern: /user/:user
//!
//!  /user/gordon              match
//!  /user/you                 match
//!  /user/gordon/profile      no match
//!  /user/                    no match
//! ```
//!
//! **Note:** Since the tree only supports explicit matches, you can not register static routes and parameters for the same path segment. For example you can not register the patterns `/user/new` and `/user/:user` at the same time.
//!
//! ### Catch-All parameters
//!
//! The second type are *catch-all* parameters and have the form `*name`. Like the name suggests, they match everything. Therefore they must always be at the **end** of the pattern:
//!
//! ```text
//! Pattern: /src/*filepath
//!
//!  /src/                     match
//!  /src/somefile.go          match
//!  /src/subdir/somefile.go   match
//! ```
//!
//! ## How does it work?
//!
//! The matcher relies on a tree structure which makes heavy use of *common prefixes*, it is basically a *compact* [*prefix tree*](https://en.wikipedia.org/wiki/Trie) (or [*Radix tree*](https://en.wikipedia.org/wiki/Radix_tree)). Nodes with a common prefix share a parent. Here is a short example what the routing tree for the `GET` request method could look like:
//!
//! ```text
//! Priority   Path             Handle
//! 9          \                *<1>
//! 3          ├s               None
//! 2          |├earch\         *<2>
//! 1          |└upport\        *<3>
//! 2          ├blog\           *<4>
//! 1          |    └:post      None
//! 1          |         └\     *<5>
//! 2          ├about-us\       *<6>
//! 1          |        └team\  *<7>
//! 1          └contact\        *<8>
//! ```
//!
//! Every `*<num>` represents the memory address of a handler function (a pointer). If you follow a path trough the tree from the root to the leaf, you get the complete route path, e.g `/blog/:post`, where `:post` is just a placeholder ([*parameter*](#named-parameters)) for an actual post name. Unlike hash-maps, a tree structure also allows us to use dynamic parts like the `:post` parameter, since we actually match against the routing patterns instead of just comparing hashes. This works very well and efficiently.
//!
//! Because URL paths have a hierarchical structure and make use only of a limited set of characters (byte values), it is very likely that there are a lot of common prefixes. This allows us to easily reduce the routing into ever smaller problems. Moreover the matcher manages a separate tree for every request method. For one thing it is more space efficient than holding a method->handle map in every single node, it also allows us to greatly reduce the routing problem before even starting the look-up in the prefix-tree.
//!
//! For even better scalability, the child nodes on each tree level are ordered by priority, where the priority is just the number of handles registered in sub nodes (children, grandchildren, and so on..). This helps in two ways:
//!
//! 1. Nodes which are part of the most routing paths are evaluated first. This helps make more routes reachable as fast as possible.
//! 2. It acts as a cost compensation. The longest reachable path (highest cost) can always be evaluated first. The following scheme visualizes the tree structure. Nodes are evaluated from top to bottom and from left to right.
//!
//! ```text
//! ├------------
//! ├---------
//! ├-----
//! ├----
//! ├--
//! ├--
//! └-
//! ```
mod error;
mod params;
mod tree;

pub use error::{InsertError, MatchError};
pub use params::{IntoIter, Iter, Params};
pub use tree::{Match, Node};

#[cfg(doctest)]
mod test_readme {
    macro_rules! doc_comment {
        ($x:expr) => {
            #[doc = $x]
            extern "C" {}
        };
    }

    doc_comment!(include_str!("../README.md"));
}
