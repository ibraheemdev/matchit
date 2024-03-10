use std::{fmt, ops::Range};

/// An uescaped route that keeps track of the position of escaped characters ('{{' or '}}').
///
/// Note that this type dereferences to `&[u8]`.
#[derive(Clone, Default)]
pub struct UnescapedRoute {
    inner: Vec<u8>,
    escaped: Vec<usize>,
}

impl UnescapedRoute {
    /// Unescapes escaped brackets ('{{' or '}}') in a route.
    pub fn new(mut inner: Vec<u8>) -> UnescapedRoute {
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

        UnescapedRoute { inner, escaped }
    }

    /// Returns true if the character at the given index was escaped.
    pub fn is_escaped(&self, i: usize) -> bool {
        self.escaped.contains(&i)
    }

    /// Slices the route with `start..`.
    pub fn slice_off(&self, start: usize) -> UnescapedRoute {
        let mut escaped = Vec::new();
        for &i in &self.escaped {
            if i >= start {
                escaped.push(i - start);
            }
        }

        UnescapedRoute {
            inner: self.inner[start..].to_owned(),
            escaped,
        }
    }

    /// Slices the route with `..end`.
    pub fn slice_until(&self, end: usize) -> UnescapedRoute {
        let mut escaped = self.escaped.clone();
        escaped.retain(|&i| i < end);

        UnescapedRoute {
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
        // ignore any escaped characters in the range being replaced
        self.escaped.retain(|x| !range.contains(x));

        // update the escaped indices
        let offset = (replace.len() as isize) - (range.len() as isize);
        for i in &mut self.escaped {
            if *i > range.end {
                *i = i.checked_add_signed(offset).unwrap();
            }
        }

        self.inner.splice(range, replace)
    }

    /// Appends another route to the end of this one.
    pub fn append(&mut self, other: &UnescapedRoute) {
        for i in &other.escaped {
            self.escaped.push(self.inner.len() + i);
        }

        self.inner.extend_from_slice(&other.inner);
    }

    /// Truncates the route to the given length.
    pub fn truncate(&mut self, to: usize) {
        self.escaped.retain(|&x| x < to);
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

impl std::ops::Deref for UnescapedRoute {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl fmt::Debug for UnescapedRoute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnescapedRoute")
            .field("inner", &std::str::from_utf8(&self.inner))
            .field("escaped", &self.escaped)
            .finish()
    }
}

/// A reference to an `UnescapedRoute`.
#[derive(Copy, Clone)]
pub struct UnescapedRef<'a> {
    pub inner: &'a [u8],
    escaped: &'a [usize],
    offset: isize,
}

impl<'a> UnescapedRef<'a> {
    /// Converts this reference into an owned route.
    pub fn to_owned(self) -> UnescapedRoute {
        let mut escaped = Vec::new();
        for &i in self.escaped {
            let i = i.wrapping_add_signed(-self.offset);
            if i < self.inner.len() {
                escaped.push(i);
            }
        }

        UnescapedRoute {
            escaped,
            inner: self.inner.to_owned(),
        }
    }

    /// Returns true if the character at the given index was escaped.
    pub fn is_escaped(&self, i: usize) -> bool {
        self.escaped.contains(&(i.wrapping_add_signed(self.offset)))
    }

    /// Slices the route with `start..`.
    pub fn slice_off(&self, start: usize) -> UnescapedRef<'a> {
        UnescapedRef {
            inner: &self.inner[start..],
            escaped: self.escaped,
            offset: self.offset + (start as isize),
        }
    }

    /// Slices the route with `..end`.
    pub fn slice_until(&self, end: usize) -> UnescapedRef<'a> {
        UnescapedRef {
            inner: &self.inner[..end],
            escaped: self.escaped,
            offset: self.offset,
        }
    }

    /// Returns a reference to the inner slice.
    pub fn inner(&self) -> &[u8] {
        self.inner
    }
}

impl<'a> std::ops::Deref for UnescapedRef<'a> {
    type Target = &'a [u8];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> fmt::Debug for UnescapedRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnescapedRef")
            .field("inner", &std::str::from_utf8(&self.inner))
            .field("escaped", &self.escaped)
            .field("offset", &self.offset)
            .finish()
    }
}
