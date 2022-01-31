//! # MatchIt
//!
//! [![Documentation](https://img.shields.io/badge/docs-0.4.6-4d76ae?style=for-the-badge)](https://docs.rs/matchit)
//! [![Version](https://img.shields.io/crates/v/matchit?style=for-the-badge)](https://crates.io/crates/matchit)
//! [![License](https://img.shields.io/crates/l/matchit?style=for-the-badge)](https://crates.io/crates/matchit)
//! [![Actions](https://img.shields.io/github/workflow/status/ibraheemdev/matchit/Rust/master?style=for-the-badge)](https://github.com/ibraheemdev/matchit/actions)
//!
//! A blazing fast URL router and path matcher.
//!
//! ```rust
//! use matchit::Node;
//!
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let mut matcher = Node::new();
//!     matcher.insert("/home", "Welcome!")?;
//!     matcher.insert("/users/:id", "A User")?;
//!
//!     let matched = matcher.at("/users/978")?;
//!     assert_eq!(matched.params.get("id"), Some("978"));
//!     assert_eq!(*matched.value, "A User");
//!
//!     Ok(())
//! }
//! ```
//!
//!
//! ## Parameters
//!
//! The matcher supports dynamic route segments. These are accessible by-name through the [`Params`](https://docs.rs/matchit/*/matchit/struct.Params.html) struct,
//! which is returned on a successful match attempt.
//!
//! A registered route can contain named or catch-all parameters.
//!
//! ### Named Parameters
//!
//! Named parameters like `/:id` match anything until the next `/` or the path end:
//!
//! ```rust
//! # use matchit::Node;
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut m = Node::new();
//! m.insert("/users/:id", true)?;
//!
//! assert_eq!(m.at("/users/1")?.params.get("id"), Some("1"));
//! assert_eq!(m.at("/users/23")?.params.get("id"), Some("23"));
//! assert!(m.at("/users").is_err());
//!
//! # Ok(())
//! # }
//! ```
//!
//! ### Catch-all Parameters
//!
//! Catch-all parameters start with `*` and match everything including the trailing slash. They must always be at the **end** of the route:
//!
//! ```rust
//! # use matchit::Node;
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut m = Node::new();
//! m.insert("/*p", true)?;
//!
//! assert_eq!(m.at("/")?.params.get("p"), Some("/"));
//! assert_eq!(m.at("/foo.js")?.params.get("p"), Some("/foo.js"));
//! assert_eq!(m.at("/c/bar.css")?.params.get("p"), Some("/c/bar.css"));
//!
//! # Ok(())
//! # }
//! ```
//!
//! ## Routing Priority
//!
//! Static and dynamic route segments are allowed to overlap. If they do, static segments will be given higher priority:
//! ```rust
//! # use matchit::Node;
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut m = Node::new();
//! m.insert("/home", "Welcome!").unwrap();  // priority: 1
//! m.insert("/about", "About Me").unwrap(); // priority: 1
//! m.insert("/:other", "...").unwrap();     // priority: 2
//!
//! # Ok(())
//! # }
//! ```
//!
//! Catch-all parameters however are not allowed to overlap with other path segments. Attempting to insert a conflicting route will result
//! in an error:
//! ```rust
//! # use matchit::{InsertError, Node};
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut m = Node::new();
//! m.insert("/home", "Welcome!").unwrap();
//!
//! assert_eq!(
//!     m.insert("/*filepath", "..."),
//!     Err(InsertError::Conflict {
//!         with: "/home".into()
//!     })
//! );
//!
//! # Ok(())
//! # }
//! ```
//!
//! ## How does it work?
//!
//! Because URL paths follow a hierarchical structure, the matcher relies on a radix tree structure that makes heavy use of common prefixes:
//!
//! ```text
//! Priority   Path             Value
//! 9          \                1
//! 3          ├s               None
//! 2          |├earch\         2
//! 1          |└upport\        3
//! 2          ├blog\           4
//! 1          |    └:post      None
//! 1          |         └\     5
//! 2          ├about-us\       6
//! 1          |        └team\  7
//! 1          └contact\        8
//! ```
//!
//! This allows us to reduce the route search to a small number of branches. Child nodes on the same level of the tree are also prioritized
//! by the number of children with registered values, increasing the chance of choosing the correct branch of the first try.

#![deny(rust_2018_idioms, clippy::all)]

mod error;
mod matcher;
mod params;
mod tree;

pub use error::{InsertError, MatchError};
pub use matcher::{Match, Router};
pub use params::{Params, ParamsIter};

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
