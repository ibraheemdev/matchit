//! Httprouter is a trie based high performance HTTP request router.
//!
//! See [`router`](./router/index.html) for more details
#![deny(clippy::all)]
#![forbid(unsafe_code)]

pub mod path;
pub mod router;
pub(crate) mod tree;

// test the code examples in README.md
#[cfg(doctest)]
mod test_readme {
  macro_rules! doc_comment {
    ($x:expr) => {
        #[doc = $x]
        extern {}
    };
  }

  doc_comment!(include_str!("../README.md"));
}
