use crate::tree::Node;

use std::fmt;

/// Represents errors that can occur when inserting a new route.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum InsertError {
    /// Attempted to insert a path that conflicts with an existing route.
    Conflict {
        /// The existing route that the insertion is conflicting with.
        with: String,
    },
    /// Only one parameter per route segment is allowed.
    TooManyParams,
    /// Parameters must be registered with a name.
    UnnamedParam,
    /// Catch-all parameters are only allowed at the end of a path.
    InvalidCatchAll,
}

impl fmt::Display for InsertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Conflict { with } => {
                write!(
                    f,
                    "insertion failed due to conflict with previously registered route: {}",
                    with
                )
            }
            Self::TooManyParams => write!(f, "only one parameter is allowed per path segment"),
            Self::UnnamedParam => write!(f, "parameters must be registered with a name"),
            Self::InvalidCatchAll => write!(
                f,
                "catch-all parameters are only allowed at the end of a route"
            ),
        }
    }
}

impl std::error::Error for InsertError {}

impl InsertError {
    pub(crate) fn conflict<T>(route: &[u8], prefix: &[u8], current: &Node<T>) -> Self {
        let mut route = route[..route.len() - prefix.len()].to_owned();

        if !route.ends_with(&current.prefix) {
            route.extend_from_slice(&current.prefix);
        }

        let mut current = current.children.first();
        while let Some(node) = current {
            route.extend_from_slice(&node.prefix);
            current = node.children.first();
        }

        InsertError::Conflict {
            with: String::from_utf8(route).unwrap(),
        }
    }
}

/// A failed match attempt.
///
/// ```
/// use matchit::{MatchError, Router};
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let mut router = Router::new();
/// router.insert("/home", "Welcome!")?;
/// router.insert("/blog/", "Our blog.")?;
///
/// // no routes match
/// if let Err(err) = router.at("/foobar") {
///     assert_eq!(err, MatchError::NotFound);
/// }
/// # Ok(())
/// # }
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MatchError {
    /// No matching route was found.
    NotFound,
}

impl fmt::Display for MatchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "matching route not found")
    }
}

impl std::error::Error for MatchError {}
