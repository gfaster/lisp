use crate::env::Symbol;

macro_rules! unit_error {
    ($($id:ident $(($($tt:tt)+))?),* $(,)?) => {
        #[derive(Debug, Clone)]
        pub enum Error {
            $($id $(($($tt)*))?),*
        }
        $(unit_error!(@decl $id $(($($tt)*))?);)*
        $(unit_error!(@from_impl $id $(($($tt)*))?);)*
    };
    (@decl $id:ident $(($($field:ty),*))?) => {
        #[derive(Debug, Clone, Copy)]
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
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl std::error::Error for Error {}

unit_error! {
    TypeError,
    SymbolNotFound(Symbol),
    MismatchedParamNum,
    SyntaxError,
    UnquoteOutOfQuasiquote,
}
