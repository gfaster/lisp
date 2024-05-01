use crate::{env::Symbol, Datum, Type};

macro_rules! unit_error {
    ($($(($manual:ident))? $id:ident $(($($tt:tt)+))?),* $(,)?) => {
        #[derive(Debug, Clone)]
        pub enum Error {
            $($id $(($($tt)*))?),*
        }
        $(unit_error!(@decl $($manual)? $id $(($($tt)*))?);)*
        $(unit_error!(@from_impl $id $(($($tt)*))?);)*
    };
    (@decl manual $($tt:tt)*) => {};
    (@from_impl manual $($tt:tt)*) => {};
    (@decl $id:ident $(($($field:ty),*))?) => {
        #[derive(Debug, Clone)]
        pub struct $id $(($(pub $field),*))?;
        impl std::fmt::Display for $id {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                <Self as std::fmt::Debug>::fmt(self, f)
            }
        }
        impl std::error::Error for $id { }
    };
    (@from_impl $id:ident) => {
        impl From<$id> for Error {
            fn from(_value: $id) -> Error {
                Error::$id
            }
        }
    };
    (@from_impl $id:ident ($tt:tt)) => {
        impl From<$id> for Error {
            fn from(value: $id) -> Error {
                Error::$id(value.0)
            }
        }
    };
    (@from_impl $id:ident ($tt1:tt, $tt2:tt)) => {
        impl From<$id> for Error {
            fn from(value: $id) -> Error {
                Error::$id(value.0, value.1)
            }
        }
    };
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Error::TypeError(expected, actual) = self {
            format_args!("expected: {expected:?} actual: {actual:?}").fmt(f)
        } else {
            <Self as std::fmt::Debug>::fmt(self, f)
        }
    }
}

impl std::error::Error for Error {}

unit_error! {
    (manual) TypeError(Type, Type),
    WrongTypeToApply(Datum),
    SymbolNotFound(Symbol),
    NotAPair,
    MismatchedParamNum(u8, u8),
    SyntaxError,
    UnquoteOutOfQuasiquote,
    MalformedArgList,
}

/// (expected, actual)
#[derive(Debug)]
pub struct TypeError(pub Type, pub Type);

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_args!("expected: {:?} actual: {:?}", self.0, self.1).fmt(f)
    }
}


pub fn type_err(expected: impl Into<Type>, actual: impl Into<Type>) -> TypeError {
    TypeError(expected.into(), actual.into())
}
