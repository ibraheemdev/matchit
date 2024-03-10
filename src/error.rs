use crate::escape::{UnescapedRef, UnescapedRoute};
use crate::tree::{denormalize_params, Node};

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
    ///
    /// Prefixes are allowed before a parmeter, but not after it. For example,
    /// `/foo-{bar}` is a valid route, but `/{bar}-foo` is not.
    InvalidParamSegment,
    /// Parameters must be registered with a valid name, and matching braces.
    InvalidParam,
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
            Self::InvalidParamSegment => {
                write!(f, "only one parameter is allowed per path segment")
            }
            Self::InvalidParam => write!(f, "parameters must be registered with a valid name"),
            Self::InvalidCatchAll => write!(
                f,
                "catch-all parameters are only allowed at the end of a route"
            ),
        }
    }
}

impl std::error::Error for InsertError {}

impl InsertError {
    pub(crate) fn conflict<T>(
        route: &UnescapedRoute,
        prefix: UnescapedRef<'_>,
        current: &Node<T>,
    ) -> Self {
        let mut route = route.clone();

        // The new route would have had to replace the current node in the tree.
        if prefix.inner() == current.prefix.inner() {
            denormalize_params(&mut route, &current.param_remapping);
            return InsertError::Conflict {
                with: String::from_utf8(route.into_inner()).unwrap(),
            };
        }

        route.truncate(route.len() - prefix.len());

        if !route.ends_with(&current.prefix) {
            route.append(&current.prefix);
        }

        let mut last = current;
        while let Some(node) = last.children.first() {
            last = node;
        }

        let mut current = current.children.first();
        while let Some(node) = current {
            route.append(&node.prefix);
            current = node.children.first();
        }

        denormalize_params(&mut route, &last.param_remapping);

        InsertError::Conflict {
            with: String::from_utf8(route.into_inner()).unwrap(),
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
