use crate::Node;

use std::fmt;

/// Represents errors that can occur when inserting a new route.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum InsertError {
    /// Attempted to insert a route that conflicts with an existing route.
    Conflict {
        /// The existing route that the insertion is conflicting with.
        with: String,
    },
    /// Only one parameter per path segment is allowed.
    TooManyParams,
    /// Parameters must be registered with a name.
    UnnamedParam,
    /// Catch-all parameters are only allowed at the end of a path.
    InvalidCatchAll,
    /// Invalid tokens in the inserted path.
    MalformedPath,
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
                "catch-all parameters are only allowed at the end of a path"
            ),
            Self::MalformedPath => write!(f, "malformed path"),
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

/// A failed match attempt, with trailing slash redirect information.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct MatchError {
    tsr: bool,
}

impl MatchError {
    /// Indicates whether a route exists at the same path with/without a trailing slash.
    /// ```rust
    /// # use matchit::Node;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/blog/", "Our blog.")?;
    ///
    /// // a route exists without the trailing slash
    /// if let Err(err) = matcher.at("/home/") {
    ///     assert!(err.tsr());
    /// }
    ///
    /// // a route exists with a trailing slash
    /// if let Err(err) = matcher.at("/blog") {
    ///     assert!(err.tsr());
    /// }
    ///
    /// // no routes match, with or without a trailing slash
    /// if let Err(err) = matcher.at("/foobar") {
    ///     assert!(!err.tsr());
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn tsr(self) -> bool {
        self.tsr
    }

    pub(crate) fn new(tsr: bool) -> Self {
        Self { tsr }
    }
}

impl fmt::Display for MatchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "no route registered under the given path")
    }
}

impl std::error::Error for MatchError {}
