use crate::error::MergeError;
use crate::tree::Node;
use crate::{InsertError, MatchError, Params};

/// A zero-copy URL router.
///
/// See [the crate documentation](crate) for details.
#[derive(Clone, Debug)]
pub struct Router<T> {
    root: Node<T>,
}

impl<T> Default for Router<T> {
    fn default() -> Self {
        Self {
            root: Node::default(),
        }
    }
}

impl<T> Router<T> {
    /// Construct a new router.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a route into the router.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use matchit::Router;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut router = Router::new();
    /// router.insert("/home", "Welcome!")?;
    /// router.insert("/users/{id}", "A User")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert(&mut self, route: impl Into<String>, value: T) -> Result<(), InsertError> {
        self.root.insert(route.into(), value)
    }

    /// Tries to find a value in the router matching the given path.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use matchit::Router;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut router = Router::new();
    /// router.insert("/home", "Welcome!")?;
    ///
    /// let matched = router.at("/home").unwrap();
    /// assert_eq!(*matched.value, "Welcome!");
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn at<'path>(&self, path: &'path str) -> Result<Match<'_, 'path, &T>, MatchError> {
        match self.root.at(path.as_bytes()) {
            Ok((value, params)) => Ok(Match {
                // Safety: We only expose `&mut T` through `&mut self`
                value: unsafe { &*value.get() },
                params,
            }),
            Err(e) => Err(e),
        }
    }

    /// Tries to find a value in the router matching the given path,
    /// returning a mutable reference.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use matchit::Router;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut router = Router::new();
    /// router.insert("/", 1)?;
    ///
    /// *router.at_mut("/").unwrap().value += 1;
    /// assert_eq!(*router.at("/").unwrap().value, 2);
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn at_mut<'path>(
        &mut self,
        path: &'path str,
    ) -> Result<Match<'_, 'path, &mut T>, MatchError> {
        match self.root.at(path.as_bytes()) {
            Ok((value, params)) => Ok(Match {
                // Safety: We have `&mut self`
                value: unsafe { &mut *value.get() },
                params,
            }),
            Err(e) => Err(e),
        }
    }

    /// Remove a given route from the router.
    ///
    /// Returns the value stored under the route if it was found.
    /// If the route was not found or invalid, `None` is returned.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use matchit::Router;
    /// let mut router = Router::new();
    ///
    /// router.insert("/home", "Welcome!");
    /// assert_eq!(router.remove("/home"), Some("Welcome!"));
    /// assert_eq!(router.remove("/home"), None);
    ///
    /// router.insert("/home/{id}/", "Hello!");
    /// assert_eq!(router.remove("/home/{id}/"), Some("Hello!"));
    /// assert_eq!(router.remove("/home/{id}/"), None);
    ///
    /// router.insert("/home/{id}/", "Hello!");
    /// // The route does not match.
    /// assert_eq!(router.remove("/home/{user}"), None);
    /// assert_eq!(router.remove("/home/{id}/"), Some("Hello!"));
    ///
    /// router.insert("/home/{id}/", "Hello!");
    /// // Invalid route.
    /// assert_eq!(router.remove("/home/{id"), None);
    /// assert_eq!(router.remove("/home/{id}/"), Some("Hello!"));
    /// ```
    pub fn remove(&mut self, path: impl Into<String>) -> Option<T> {
        self.root.remove(path.into())
    }

    #[cfg(feature = "__test_helpers")]
    pub fn check_priorities(&self) -> Result<u32, (u32, u32)> {
        self.root.check_priorities()
    }

    /// Merge a given router into current one.
    ///
    /// Returns a list of [`InsertError`] for every failed insertion.
    /// Note that this can result in a partially successful merge if
    /// a subset of routes conflict.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use matchit::Router;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut root = Router::new();
    /// root.insert("/home", "Welcome!")?;
    ///
    /// let mut child = Router::new();
    /// child.insert("/users/{id}", "A User")?;
    ///
    /// root.merge(child)?;
    /// assert!(root.at("/users/1").is_ok());
    /// # Ok(())
    /// # }
    /// ```
    pub fn merge(&mut self, other: Self) -> Result<(), MergeError> {
        let mut errors = Vec::new();
        other.root.for_each(|path, value| {
            if let Err(err) = self.insert(path, value) {
                errors.push(err);
            }
        });

        if errors.is_empty() {
            Ok(())
        } else {
            Err(MergeError(errors))
        }
    }
}

/// A successful match consisting of the registered value
/// and URL parameters, returned by [`Router::at`](Router::at).
#[derive(Debug)]
pub struct Match<'k, 'v, V> {
    /// The value stored under the matched node.
    pub value: V,

    /// The route parameters. See [parameters](crate#parameters) for more details.
    pub params: Params<'k, 'v>,
}
