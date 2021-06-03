use std::iter;
use std::mem;
use std::slice;

/// A single URL parameter, consisting of a key and a value.
#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Default, Copy, Clone)]
struct Param<'k, 'v> {
    key: &'k [u8],
    value: &'v [u8],
}

/// A list of parameters returned by a route match.
///
/// ```rust
/// # use std::str;
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # let mut matcher = matchit::Node::new();
/// # matcher.insert("/users/:id", true).unwrap();
/// let matched = matcher.at(b"/users/978")?;
///
/// // you can iterate through the keys and values
/// for (key, value) in matched.params.iter() {
///     println!("key: {}, value: {}", str::from_utf8(key)?, str::from_utf8(value)?);
/// }
///
/// // or get a specific value by key
/// let id = matched.params.get("id").unwrap();
/// assert_eq!(id, b"978");
/// # Ok(())
/// # }
/// ```
#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
pub struct Params<'k, 'v> {
    kind: ParamsKind<'k, 'v>,
}

// most routes have 1-3 dynamic parameters, so we can avoid a heap allocation in common cases.
const SMALL: usize = 3;

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
enum ParamsKind<'k, 'v> {
    None,
    Small([Param<'k, 'v>; SMALL], usize),
    Large(Vec<Param<'k, 'v>>),
}

impl<'k, 'v> Params<'k, 'v> {
    /// Creates a new list of URL parameters.
    pub(crate) fn new() -> Self {
        let kind = ParamsKind::None;
        Self { kind }
    }

    /// Returns the value of the first parameter registered matched for the given key.
    pub fn get(&self, key: impl AsRef<[u8]>) -> Option<&'v [u8]> {
        match &self.kind {
            ParamsKind::None => None,
            ParamsKind::Small(arr, len) => arr
                .iter()
                .take(*len)
                .find(|param| param.key == key.as_ref())
                .map(|param| param.value),
            ParamsKind::Large(vec) => vec
                .iter()
                .find(|param| param.key == key.as_ref())
                .map(|param| param.value),
        }
    }

    /// Returns an iterator over the parameters in the list.
    pub fn iter(&self) -> ParamsIter<'_, 'k, 'v> {
        ParamsIter::new(self)
    }

    /// Returns `true` if there are no parameters in the list.
    pub fn is_empty(&self) -> bool {
        match &self.kind {
            ParamsKind::None => true,
            ParamsKind::Small(_, len) => *len == 0,
            ParamsKind::Large(vec) => vec.is_empty(),
        }
    }

    /// Inserts a key value parameter pair into the list.
    pub(crate) fn push(&mut self, key: &'k [u8], value: &'v [u8]) {
        #[cold]
        fn drain_to_vec<T: Default>(len: usize, elem: T, arr: &mut [T; SMALL]) -> Vec<T> {
            let mut vec = Vec::with_capacity(len + 1);
            vec.extend(arr.iter_mut().map(mem::take));
            vec.push(elem);
            vec
        }

        let param = Param { key, value };
        match &mut self.kind {
            ParamsKind::None => {
                self.kind = ParamsKind::Small([param, Default::default(), Default::default()], 1);
            }
            ParamsKind::Small(arr, len) => {
                if *len == SMALL {
                    self.kind = ParamsKind::Large(drain_to_vec(*len, param, arr));
                    return;
                }
                arr[*len] = param;
                *len += 1;
            }
            ParamsKind::Large(vec) => vec.push(param),
        }
    }
}

/// An iterator over the keys and values of a route's [parameters](crate::Params).
pub struct ParamsIter<'ps, 'k, 'v> {
    kind: ParamsIterKind<'ps, 'k, 'v>,
}

impl<'ps, 'k, 'v> ParamsIter<'ps, 'k, 'v> {
    fn new(params: &'ps Params<'k, 'v>) -> Self {
        let kind = match &params.kind {
            ParamsKind::None => ParamsIterKind::None,
            ParamsKind::Small(arr, len) => ParamsIterKind::Small(arr.iter().take(*len)),
            ParamsKind::Large(vec) => ParamsIterKind::Large(vec.iter()),
        };
        Self { kind }
    }
}

enum ParamsIterKind<'ps, 'k, 'v> {
    None,
    Small(iter::Take<slice::Iter<'ps, Param<'k, 'v>>>),
    Large(slice::Iter<'ps, Param<'k, 'v>>),
}

impl<'ps, 'k, 'v> Iterator for ParamsIter<'ps, 'k, 'v> {
    type Item = (&'k [u8], &'v [u8]);

    fn next(&mut self) -> Option<Self::Item> {
        match self.kind {
            ParamsIterKind::None => None,
            ParamsIterKind::Small(ref mut iter) => iter.next().map(|p| (p.key, p.value)),
            ParamsIterKind::Large(ref mut iter) => iter.next().map(|p| (p.key, p.value)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_alloc() {
        assert_eq!(Params::new().kind, ParamsKind::None);
    }

    #[test]
    fn heap_alloc() {
        let vec: Vec<(&[u8], &[u8])> = vec![
            (b"hello", b"hello"),
            (b"world", b"world"),
            (b"foo", b"foo"),
            (b"bar", b"bar"),
            (b"baz", b"baz"),
        ];

        let mut params = Params::new();
        for (key, value) in vec.clone() {
            params.push(key, value);
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
        let vec: Vec<(&[u8], &[u8])> =
            vec![(b"hello", b"hello"), (b"world", b"world"), (b"baz", b"baz")];

        let mut params = Params::new();
        for (key, value) in vec.clone() {
            params.push(key, value);
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
