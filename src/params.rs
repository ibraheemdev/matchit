use std::{fmt, iter, mem, slice};

/// A single URL parameter, consisting of a key and a value.
#[derive(PartialEq, Eq, Ord, PartialOrd, Default, Copy, Clone)]
pub(crate) struct Param<'k, 'v> {
    // Keys and values are stored as byte slices internally by the router
    // to avoid utf8 checks when slicing. This allows us to perform utf8
    // validation lazily without resorting to unsafe code.
    pub(crate) key: &'k [u8],
    pub(crate) value: &'v [u8],
}

impl<'k, 'v> Param<'k, 'v> {
    const EMPTY: Param<'static, 'static> = Param {
        key: b"",
        value: b"",
    };

    // Returns the parameter key as a string.
    fn key_str(&self) -> &'k str {
        std::str::from_utf8(self.key).unwrap()
    }

    // Returns the parameter value as a string.
    fn value_str(&self) -> &'v str {
        std::str::from_utf8(self.value).unwrap()
    }
}

/// A list of parameters returned by a route match.
///
/// ```rust
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # let mut router = matchit::Router::new();
/// # router.insert("/users/{id}", true).unwrap();
/// let matched = router.at("/users/1")?;
///
/// // Iterate through the keys and values.
/// for (key, value) in matched.params.iter() {
///     println!("key: {}, value: {}", key, value);
/// }
///
/// // Get a specific value by name.
/// let id = matched.params.get("id");
/// assert_eq!(id, Some("1"));
/// # Ok(())
/// # }
/// ```
#[derive(PartialEq, Eq, Ord, PartialOrd, Clone)]
pub struct Params<'k, 'v> {
    kind: ParamsKind<'k, 'v>,
}

impl Default for Params<'_, '_> {
    fn default() -> Self {
        Self::new()
    }
}

// Most routes have a small number of dynamic parameters, so we can avoid
// heap allocations in the common case.
const SMALL: usize = 3;

// A list of parameters, optimized to avoid allocations when possible.
#[derive(PartialEq, Eq, Ord, PartialOrd, Clone)]
enum ParamsKind<'k, 'v> {
    Small([Param<'k, 'v>; SMALL], usize),
    Large(Vec<Param<'k, 'v>>),
}

impl<'k, 'v> Params<'k, 'v> {
    /// Create an empty list of parameters.
    #[inline]
    pub fn new() -> Self {
        Self {
            kind: ParamsKind::Small([Param::EMPTY; SMALL], 0),
        }
    }

    /// Returns the number of parameters.
    pub fn len(&self) -> usize {
        match self.kind {
            ParamsKind::Small(_, len) => len,
            ParamsKind::Large(ref vec) => vec.len(),
        }
    }

    // Truncates the parameter list to the given length.
    pub(crate) fn truncate(&mut self, n: usize) {
        match &mut self.kind {
            ParamsKind::Small(_, len) => *len = n,
            ParamsKind::Large(vec) => vec.truncate(n),
        }
    }

    /// Returns the value of the first parameter registered under the given key.
    pub fn get(&self, key: impl AsRef<str>) -> Option<&'v str> {
        let key = key.as_ref().as_bytes();

        match &self.kind {
            ParamsKind::Small(arr, len) => arr
                .iter()
                .take(*len)
                .find(|param| param.key == key)
                .map(Param::value_str),
            ParamsKind::Large(vec) => vec
                .iter()
                .find(|param| param.key == key)
                .map(Param::value_str),
        }
    }

    /// Returns an iterator over the parameters in the list.
    pub fn iter(&self) -> ParamsIter<'_, 'k, 'v> {
        ParamsIter::new(self)
    }

    /// Returns `true` if there are no parameters in the list.
    pub fn is_empty(&self) -> bool {
        match self.kind {
            ParamsKind::Small(_, len) => len == 0,
            ParamsKind::Large(ref vec) => vec.is_empty(),
        }
    }

    /// Appends a key-value parameter to the list.
    #[inline]
    pub(crate) fn push(&mut self, key: &'k [u8], value: &'v [u8]) {
        #[cold]
        #[inline(never)]
        fn drain_to_vec<T: Default>(len: usize, elem: T, arr: &mut [T; SMALL]) -> Vec<T> {
            let mut vec = Vec::with_capacity(len + 1);
            vec.extend(arr.iter_mut().map(mem::take));
            vec.push(elem);
            vec
        }

        #[cold]
        #[inline(never)]
        fn push_slow<'k, 'v>(vec: &mut Vec<Param<'k, 'v>>, param: Param<'k, 'v>) {
            vec.push(param);
        }

        let param = Param { key, value };
        match &mut self.kind {
            ParamsKind::Small(arr, len) => {
                if *len >= SMALL {
                    self.kind = ParamsKind::Large(drain_to_vec(*len, param, arr));
                    return;
                }

                arr[*len] = param;
                *len += 1;
            }

            ParamsKind::Large(vec) => push_slow(vec, param),
        }
    }

    // Applies a transformation function to each key.
    #[inline]
    pub(crate) fn for_each_key_mut(&mut self, f: impl Fn((usize, &mut Param<'k, 'v>))) {
        match &mut self.kind {
            ParamsKind::Small(arr, len) => arr.iter_mut().take(*len).enumerate().for_each(f),
            ParamsKind::Large(vec) => vec.iter_mut().enumerate().for_each(f),
        }
    }
}

impl fmt::Debug for Params<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

/// An iterator over the keys and values of a route's [parameters](crate::Params).
pub struct ParamsIter<'ps, 'k, 'v> {
    kind: ParamsIterKind<'ps, 'k, 'v>,
}

impl<'ps, 'k, 'v> ParamsIter<'ps, 'k, 'v> {
    fn new(params: &'ps Params<'k, 'v>) -> Self {
        let kind = match &params.kind {
            ParamsKind::Small(arr, len) => ParamsIterKind::Small(arr.iter().take(*len)),
            ParamsKind::Large(vec) => ParamsIterKind::Large(vec.iter()),
        };
        Self { kind }
    }
}

enum ParamsIterKind<'ps, 'k, 'v> {
    Small(iter::Take<slice::Iter<'ps, Param<'k, 'v>>>),
    Large(slice::Iter<'ps, Param<'k, 'v>>),
}

impl<'k, 'v> Iterator for ParamsIter<'_, 'k, 'v> {
    type Item = (&'k str, &'v str);

    fn next(&mut self) -> Option<Self::Item> {
        match self.kind {
            ParamsIterKind::Small(ref mut iter) => {
                iter.next().map(|p| (p.key_str(), p.value_str()))
            }
            ParamsIterKind::Large(ref mut iter) => {
                iter.next().map(|p| (p.key_str(), p.value_str()))
            }
        }
    }
}

impl ExactSizeIterator for ParamsIter<'_, '_, '_> {
    fn len(&self) -> usize {
        match self.kind {
            ParamsIterKind::Small(ref iter) => iter.len(),
            ParamsIterKind::Large(ref iter) => iter.len(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn heap_alloc() {
        let vec = vec![
            ("hello", "hello"),
            ("world", "world"),
            ("foo", "foo"),
            ("bar", "bar"),
            ("baz", "baz"),
        ];

        let mut params = Params::new();
        for (key, value) in vec.clone() {
            params.push(key.as_bytes(), value.as_bytes());
            assert_eq!(params.get(key), Some(value));
        }

        match params.kind {
            ParamsKind::Large(..) => {}
            _ => panic!(),
        }

        assert!(params.iter().eq(vec.clone()));
    }

    #[test]
    fn stack_alloc() {
        let vec = vec![("hello", "hello"), ("world", "world"), ("baz", "baz")];

        let mut params = Params::new();
        for (key, value) in vec.clone() {
            params.push(key.as_bytes(), value.as_bytes());
            assert_eq!(params.get(key), Some(value));
        }

        match params.kind {
            ParamsKind::Small(..) => {}
            _ => panic!(),
        }

        assert!(params.iter().eq(vec.clone()));
    }

    #[test]
    fn ignore_array_default() {
        let params = Params::new();
        assert!(params.get("").is_none());
    }
}
