pub mod tree;

#[doc(hidden)]
pub use tree::{Node, Match, Params};

#[cfg(doctest)]
mod test_readme {
    macro_rules! doc_comment {
        ($x:expr) => {
            #[doc = $x]
            extern {}
        }
    }

    doc_comment!(include_str!("../README.md"));
}
