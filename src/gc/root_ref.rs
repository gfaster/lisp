use std::{marker::PhantomData, pin::Pin, ptr::NonNull};

use super::{Root, RootTruth};

fn mk<'root, T>(item: &'root T) -> RootRef<'root, T> {
    RootRef { _phantom: PhantomData, item: item.into()}
}

impl<'root, T> From<&'root RootTruth<T>> for RootRef<'root, T> {
    fn from(value: &'root RootTruth<T>) -> Self {
        mk(&value.item)
    }
}

impl<'root, T> From<Pin<&'root mut RootTruth<T>>> for RootRef<'root, T> {
    fn from(value: Pin<&'root mut RootTruth<T>>) -> Self {
        RootRef { _phantom: PhantomData, item: (&value.item).into()}
    }
}

impl<'root, T> From<&Root<'root, T>> for RootRef<'root, T> {
    fn from(value: &Root<T>) -> Self {
        let item: &T = unsafe { &value.item.as_ref().item };
        mk(item)
    }
}


impl<'root, T> RootRef<'root, T> { 
    unsafe fn get(self) -> &'root T {
        self.item.as_ref()
    }

}

impl<'root, T: 'root> RootRef<'root, Option<T>> {
    pub fn unwrap_option(self) -> RootRef<'root, T> {
        mk(unsafe { self.get().as_ref().unwrap() })
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct RootRef<'root, T: ?Sized>{
    _phantom: PhantomData<(&'root RootTruth<()>, &'root T)>,
    item: NonNull<T>,
}

impl<T> RootRef<'_, T> {
    pub fn as_ptr(&self) -> *const T {
        self.item.as_ptr().cast_const()
    }
}

macro_rules! impl_trivial_deref {
    ($refty:ident: $($ty:ty),*$(,)?) => {
        $(impl_trivial_deref!(@ $refty $ty);)*
    };
    (@ $refty:ident $ty:ty) => {
        impl std::ops::Deref for $refty<'_, $ty> {
            type Target = $ty;

            fn deref(&self) -> &Self::Target {
                unsafe { self.item.as_ref()}
            }
        }
    };
}

impl_trivial_deref!(RootRef: u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, f32, f64, (), String, str);
