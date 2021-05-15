use std::iter;
use std::mem;
use std::slice;

/// A single URL parameter, consisting of a key and a value.
#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Default)]
struct Param<'k, 'v> {
    key: &'k str,
    value: &'v str,
}

/// A list of parameters returned by a route match.
///
/// ```rust
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # let mut matcher = matchit::Node::new();
/// # matcher.insert("/users/:id", true).unwrap();
/// let matched = matcher.at("/users/1")?;
///
/// // you can iterate through the keys and values
/// for (key, value) in matched.params.iter() {
///     println!("key: {}, value: {}", key, value);
/// }
///
/// // or get a specific value by key
/// let id = matched.params.get("id");
/// assert_eq!(id, Some("1"));
/// # Ok(())
/// # }
/// ```
#[derive(Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Params<'k, 'v> {
    kind: ParamsKind<'k, 'v>,
}

// most routes have 1-3 dynamic parameters, so we can avoid a heap allocation in common cases.
const SMALL: usize = 3;

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd)]
enum ParamsKind<'k, 'v> {
    Small([Param<'k, 'v>; SMALL], usize),
    Large(Vec<Param<'k, 'v>>),
}

impl<'k, 'v> Params<'k, 'v> {
    /// Creates a new list of URL parameters.
    pub(crate) fn new() -> Self {
        let kind = ParamsKind::Small(Default::default(), 0);
        Self { kind }
    }

    /// Returns the value of the first parameter registered matched for the given key.
    pub fn get(&self, key: impl AsRef<str>) -> Option<&'v str> {
        match &self.kind {
            ParamsKind::Small(arr, _) => arr
                .iter()
                .find(|param| param.key == key.as_ref())
                .map(|param| param.value),
            ParamsKind::Large(vec) => vec
                .iter()
                .find(|param| param.key == key.as_ref())
                .map(|param| param.value),
        }
    }

    /// Returns an iterator over the parameters in the list.
    pub fn iter(&self) -> Iter<'_, 'k, 'v> {
        Iter::new(self)
    }

    /// Returns `true` if there are no parameters in the list.
    pub fn is_empty(&self) -> bool {
        match &self.kind {
            ParamsKind::Small(_, len) => *len == 0,
            ParamsKind::Large(vec) => vec.is_empty(),
        }
    }

    /// Inserts a key value parameter pair into the list.
    pub(crate) fn push(&mut self, key: &'k str, value: &'v str) {
        #[cold]
        fn drain_to_vec<T: Default>(len: usize, elem: T, arr: &mut [T; SMALL]) -> Vec<T> {
            let mut vec = Vec::with_capacity(len + 1);
            vec.extend(arr.iter_mut().map(mem::take));
            vec.push(elem);
            vec
        }

        let param = Param { key, value };
        match &mut self.kind {
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

impl<'k, 'v> IntoIterator for Params<'k, 'v> {
    type IntoIter = IntoIter<'k, 'v>;
    type Item = (&'k str, &'v str);

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self)
    }
}

/// An iterator over the keys and values of URL [parameters](crate::Params).
pub struct Iter<'ps, 'k, 'v> {
    kind: IterKind<'ps, 'k, 'v>,
}

impl<'ps, 'k, 'v> Iter<'ps, 'k, 'v> {
    fn new(params: &'ps Params<'k, 'v>) -> Self {
        let kind = match &params.kind {
            ParamsKind::Small(arr, len) => IterKind::Small(arr.iter().take(*len)),
            ParamsKind::Large(vec) => IterKind::Large(vec.iter()),
        };
        Self { kind }
    }
}

enum IterKind<'ps, 'k, 'v> {
    Small(iter::Take<slice::Iter<'ps, Param<'k, 'v>>>),
    Large(slice::Iter<'ps, Param<'k, 'v>>),
}

impl<'ps, 'k, 'v> Iterator for Iter<'ps, 'k, 'v> {
    type Item = (&'k str, &'v str);

    fn next(&mut self) -> Option<Self::Item> {
        match self.kind {
            IterKind::Small(ref mut iter) => iter.next().map(|p| (p.key, p.value)),
            IterKind::Large(ref mut iter) => iter.next().map(|p| (p.key, p.value)),
        }
    }
}

/// An owned iterator over the keys and values of URL [parameters](crate::Params).
pub struct IntoIter<'k, 'v> {
    kind: IntoIterKind<'k, 'v>,
}

impl<'k, 'v> IntoIter<'k, 'v> {
    fn new(params: Params<'k, 'v>) -> Self {
        let kind = match params.kind {
            ParamsKind::Small(arr, len) => {
                IntoIterKind::Small(std::array::IntoIter::new(arr).take(len))
            }
            ParamsKind::Large(vec) => IntoIterKind::Large(vec.into_iter()),
        };
        Self { kind }
    }
}

enum IntoIterKind<'k, 'v> {
    Small(iter::Take<std::array::IntoIter<Param<'k, 'v>, SMALL>>),
    Large(std::vec::IntoIter<Param<'k, 'v>>),
}

impl<'k, 'v> Iterator for IntoIter<'k, 'v> {
    type Item = (&'k str, &'v str);

    fn next(&mut self) -> Option<Self::Item> {
        match self.kind {
            IntoIterKind::Small(ref mut iter) => iter.next().map(|p| (p.key, p.value)),
            IntoIterKind::Large(ref mut iter) => iter.next().map(|p| (p.key, p.value)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn params() {
        // test heap allocated params (4+)
        let vec = vec![
            ("hello", "hello"),
            ("world", "world"),
            ("foo", "foo"),
            ("bar", "bar"),
            ("baz", "baz"),
        ];

        let mut params = Params::new();
        for (key, value) in vec.clone() {
            params.push(key, value);
            assert_eq!(params.get(key), Some(value));
        }

        match params.kind {
            ParamsKind::Small(..) => panic!(),
            ParamsKind::Large(..) => {}
        }

        assert!(params.iter().eq(vec.clone()));
        assert!(params.into_iter().eq(vec.clone()));

        // test stack allocated params (up to 3)
        let vec = vec![("hello", "hello"), ("world", "world"), ("baz", "baz")];

        let mut params = Params::new();
        for (key, value) in vec.clone() {
            params.push(key, value);
            assert_eq!(params.get(key), Some(value));
        }

        match params.kind {
            ParamsKind::Small(..) => {}
            ParamsKind::Large(..) => panic!(),
        }

        assert!(params.iter().eq(vec.clone()));
        assert!(params.into_iter().eq(vec.clone()));
    }
}
