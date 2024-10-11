use matchit::{InsertError, Router};

#[test]
fn merge_ok() {
    let mut root = Router::new();
    assert!(root.insert("/foo", "foo").is_ok());
    assert!(root.insert("/bar/{id}", "bar").is_ok());

    let mut child = Router::new();
    assert!(child.insert("/baz", "baz").is_ok());
    assert!(child.insert("/xyz/{id}", "xyz").is_ok());

    assert!(root.merge(child).is_ok());

    assert_eq!(root.at("/foo").map(|m| *m.value), Ok("foo"));
    assert_eq!(root.at("/bar/1").map(|m| *m.value), Ok("bar"));
    assert_eq!(root.at("/baz").map(|m| *m.value), Ok("baz"));
    assert_eq!(root.at("/xyz/2").map(|m| *m.value), Ok("xyz"));
}

#[test]
fn merge_conflict() {
    let mut root = Router::new();
    assert!(root.insert("/foo", "foo").is_ok());

    let mut child = Router::new();
    assert!(child.insert("/foo", "foo").is_ok());

    assert_eq!(root.merge(child), Err(InsertError::Conflict {with: "/foo".into()}));
}
