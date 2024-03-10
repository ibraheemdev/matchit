use std::ops::Range;

/// An uescaped route that keeps track of the position of escaped characters ('{{' or '}}').
///
/// Note that this type dereferences to `&[u8]`.
#[derive(Clone, Default)]
pub struct UnscapedRoute {
    inner: Vec<u8>,
    escaped: Vec<usize>,
}

impl UnscapedRoute {
    /// Unescapes escaped brackets ('{{' or '}}') in a route.
    pub fn new(mut inner: Vec<u8>) -> UnscapedRoute {
        let mut escaped = Vec::new();
        let mut i = 0;

        while let Some(&c) = inner.get(i) {
            if (c == b'{' && inner.get(i + 1) == Some(&b'{'))
                || (c == b'}' && inner.get(i + 1) == Some(&b'}'))
            {
                inner.remove(i);
                escaped.push(i);
            }

            i += 1;
        }

        UnscapedRoute { inner, escaped }
    }

    /// Slices the route with `start..`.
    pub fn slice_off(&self, start: usize) -> UnscapedRoute {
        let mut escaped = Vec::new();
        for &i in &self.escaped {
            if i >= start {
                escaped.push(i - start);
            }
        }

        UnscapedRoute {
            inner: self.inner[start..].to_owned(),
            escaped,
        }
    }

    /// Slices the route with `..end`.
    pub fn slice_until(&self, end: usize) -> UnscapedRoute {
        let mut escaped = self.escaped.clone();
        escaped.retain(|&i| i < end);

        UnscapedRoute {
            inner: self.inner[..end].to_owned(),
            escaped,
        }
    }

    /// Replaces the characters in the given range.
    pub fn splice(
        &mut self,
        range: Range<usize>,
        replace: Vec<u8>,
    ) -> impl Iterator<Item = u8> + '_ {
        // update the escaped indices
        let offset = (range.len() as isize) - (replace.len() as isize);
        for i in &mut self.escaped {
            if *i > range.start {
                *i = i.checked_add_signed(offset).unwrap();
            }
        }

        self.inner.splice(range, replace)
    }

    /// Appends another route to the end of this one.
    pub fn append(&mut self, other: &UnscapedRoute) {
        for i in &other.escaped {
            self.escaped.push(self.inner.len() + i);
        }

        self.inner.extend_from_slice(&other.inner);
    }

    /// Truncates the route to the given length.
    pub fn truncate(&mut self, to: usize) {
        self.inner.truncate(to);
    }

    /// Returns a reference to this route.
    pub fn as_ref(&self) -> UnescapedRef<'_> {
        UnescapedRef {
            inner: &self.inner,
            escaped: &self.escaped,
            offset: 0,
        }
    }

    /// Returns a reference to the inner slice.
    pub fn inner(&self) -> &[u8] {
        &self.inner
    }

    /// Returns the inner slice.
    pub fn into_inner(self) -> Vec<u8> {
        self.inner
    }
}

impl std::ops::Deref for UnscapedRoute {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// A reference to an `UnescapedRoute`.
#[derive(Copy, Clone)]
pub struct UnescapedRef<'a> {
    pub inner: &'a [u8],
    escaped: &'a [usize],
    offset: usize,
}

impl<'a> UnescapedRef<'a> {
    /// Converts this reference into an owned route.
    pub fn to_owned(&self) -> UnscapedRoute {
        let mut escaped = Vec::new();
        for &i in self.escaped {
            if i + self.offset < self.inner.len() {
                escaped.push(i);
            }
        }

        UnscapedRoute {
            escaped,
            inner: self.inner.to_owned(),
        }
    }

    /// Returns true if the character at the given index was escaped.
    pub fn is_escaped(&self, i: usize) -> bool {
        self.escaped.contains(&(i + self.offset))
    }

    /// Slices the route with `start..`.
    pub fn slice_off(&self, start: usize) -> UnescapedRef<'a> {
        UnescapedRef {
            inner: &self.inner[start..],
            escaped: &self.escaped,
            offset: self.offset + start,
        }
    }

    /// Slices the route with `..end`.
    pub fn slice_until(&self, end: usize) -> UnescapedRef<'a> {
        UnescapedRef {
            inner: &self.inner[..end],
            escaped: &self.escaped,
            offset: self.offset,
        }
    }

    /// Returns a reference to the inner slice.
    pub fn inner(&self) -> &[u8] {
        &self.inner
    }
}

impl<'a> std::ops::Deref for UnescapedRef<'a> {
    type Target = &'a [u8];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
