use crate::Request;
use futures::future::{ok, Future, Ready};
use hyper::error::Error;

/// Trait implemented by types that can be extracted from request.
///
/// Types that implement this trait can be used with `Route` handlers.
pub trait FromRequest: Sized + Send {
  /// The associated error which can be returned.
  type Error: Into<Error>;

  /// Future that resolves to a Self
  type Future: Future<Output = Result<Self, Self::Error>> + Send;

  /// Convert an http request to `Self`
  fn from_request(req: &Request) -> Self::Future;
}

impl FromRequest for Request {
  type Error = Error;
  type Future = Ready<Result<Request, Error>>;

  #[inline]
  fn from_request(req: &Request) -> Self::Future {
    ok(req.clone())
  }
}

impl FromRequest for () {
  type Error = Error;
  type Future = Ready<Result<(), Error>>;

  fn from_request(_: &Request) -> Self::Future {
    ok(())
  }
}

/// This macro implements the `FromRequest` trait for tuples
/// This allows handler functions to accept multiple extractors as arguments
macro_rules! tuple_from_req ({$fut_type:ident, $(($n:tt, $T:ident)),+} => {
  #[rustfmt::skip]
  #[allow(non_snake_case)]
  #[allow(clippy::type_complexity)]
  #[allow(clippy::unknown_clippy_lints)]
  mod $fut_type {
    use super::*;
    use std::task::{Context, Poll};
    use std::pin::Pin;

    #[pin_project::pin_project]
    struct FutWrapper<$($T: FromRequest),+>($(#[pin] $T::Future),+);

    /// FromRequest implementation for tuple
    #[doc(hidden)]
    impl<$($T: FromRequest + 'static),+> FromRequest for ($($T,)+)
    {
      type Error = Error;
      type Future = $fut_type<$($T),+>;

      fn from_request(req: &Request) -> Self::Future {
        $fut_type {
          items: <($(Option<$T>,)+)>::default(),
          futs: FutWrapper($($T::from_request(req),)+),
        }
      }
    }

    #[doc(hidden)]
    #[pin_project::pin_project]
    pub struct $fut_type<$($T: FromRequest),+> {
      items: ($(Option<$T>,)+),
      #[pin]
      futs: FutWrapper<$($T,)+>,
    }

    impl<$($T: FromRequest),+> Future for $fut_type<$($T),+>
    {
      type Output = Result<($($T,)+), Error>;

      fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();
        let mut ready = true;
        $(if this.items.$n.is_none() {
          match this.futs.as_mut().project().$n.poll(cx) {
            Poll::Ready(Ok(item)) => this.items.$n = Some(item),
            Poll::Pending => ready = false,
            Poll::Ready(Err(e)) => return Poll::Ready(Err(e.into())),
          }
        })+
        match ready {
          true => Poll::Ready(Ok(($(this.items.$n.take().unwrap(),)+))),
          false => Poll::Pending
        }
      }
    }
  }
});

#[rustfmt::skip]
mod m {
  use super::*;

  tuple_from_req!(TA, (0, A));
  tuple_from_req!(TB, (0, A), (1, B));
  tuple_from_req!(TC, (0, A), (1, B), (2, C));
  tuple_from_req!(TD, (0, A), (1, B), (2, C), (3, D));
  tuple_from_req!(TE, (0, A), (1, B), (2, C), (3, D), (4, E));
  tuple_from_req!(TF, (0, A), (1, B), (2, C), (3, D), (4, E), (5, F));
  tuple_from_req!(TG, (0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G));
  tuple_from_req!(TH, (0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G), (7, H));
  tuple_from_req!(TI, (0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G), (7, H), (8, I));
  tuple_from_req!(TJ, (0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G), (7, H), (8, I), (9, J));
  tuple_from_req!(TK, (0, A), (1, B), (2, C), (3, D), (4, E), (5, F), (6, G), (7, H), (8, I), (9, J), (10, K));
}
