use std::marker::PhantomData;

use super::{Root, RootTruth};

impl<'root, T> From<&Root<'root, T>> for &'root RootRef<'root, T> {
    fn from(value: &Root<'root, T>) -> Self {
        let item: &T = unsafe { &(*value.item.as_ptr()).item };
        unsafe { std::mem::transmute(item) }
    }
}

#[repr(transparent)]
pub struct RootRef<'root, T: ?Sized>{
    _phantom: PhantomData<&'root RootTruth<T>>,
    item: T,
}

macro_rules! impl_trivial_deref {
    ($refty:ident: $($ty:ty),*$(,)?) => {
        $(impl_trivial_deref!(@ $refty $ty);)*
    };
    (@ $refty:ident $ty:ty) => {
        impl std::ops::Deref for $refty<'_, $ty> {
            type Target = $ty;

            fn deref(&self) -> &Self::Target {
                &self.item
            }
        }
    };
}

impl_trivial_deref!(RootRef: u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, f32, f64, (), String, str);
