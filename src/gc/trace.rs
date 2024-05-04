use super::{Allocation, Gc};

pub unsafe trait Trace {
    unsafe fn mark(&self, mark: bool);
}

unsafe impl<T: Trace> Trace for Gc<'_, T> {
    unsafe fn mark(&self, mark: bool) {
        if !Gc::is_marked(self, mark) {
            Gc::set_mark(self, mark);
            T::mark(&**self, mark)
        }
    }
}

unsafe impl<T: Trace> Trace for Allocation<T> {
    unsafe fn mark(&self, mark: bool) {
        if self.marked.get() != mark {
            self.marked.set(mark);
            self.item.mark(mark)
        }
    }
}

macro_rules! trivial_trace {
    ($($ty:ty),*) => {
        $(unsafe impl Trace for $ty {
            unsafe fn mark(&self, _mark: bool) { }
        })*
    };
}

trivial_trace!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, f32, f64, (), String, str);

macro_rules! deref_trace {
    ($($ty:ty),*) => {
        $(unsafe impl<T: Trace> Trace for $ty {
            unsafe fn mark(&self, mark: bool) {
                <T as Trace>::mark(&**self, mark);
            }
        })*
    };
}

deref_trace!(Box<T>, std::rc::Rc<T>, std::sync::Arc<T>);


unsafe impl<T: Trace> Trace for Vec<T> {
    unsafe fn mark(&self, mark: bool) {
        for x in self {
            x.mark(mark)
        }
    }
}

unsafe impl<T: Trace> Trace for Option<T> {
    unsafe fn mark(&self, mark: bool) {
        for x in self {
            x.mark(mark)
        }
    }
}

unsafe impl<K: Trace, T: Trace> Trace for std::collections::HashMap<K, T> {
    unsafe fn mark(&self, mark: bool) {
        for x in self {
            x.0.mark(mark);
            x.1.mark(mark);
        }
    }
}
