use super::Gc;

pub unsafe trait Trace {
    unsafe fn mark(&self, mark: bool);
    fn null_gcs(&self);
}

unsafe impl<T: Trace> Trace for Gc<T> {
    unsafe fn mark(&self, mark: bool) {
        if !Gc::is_marked(self, mark) {
            Gc::set_mark(self, mark);
            T::mark(&**self, mark)
        }
    }

    fn null_gcs(&self) {
        self.item.set(None)
    }
}

macro_rules! trivial_trace {
    ($($ty:ty),*) => {
        $(unsafe impl Trace for $ty {
            unsafe fn mark(&self, _mark: bool) { }
            fn null_gcs(&self) { }
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
            fn null_gcs(&self) {
                <T as Trace>::null_gcs(&**self);
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

    fn null_gcs(&self) {
        for x in self {
            x.null_gcs()
        }
    }
}

unsafe impl<T: Trace> Trace for Option<T> {
    unsafe fn mark(&self, mark: bool) {
        for x in self {
            x.mark(mark)
        }
    }

    fn null_gcs(&self) {
        for x in self {
            x.null_gcs()
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

    fn null_gcs(&self) {
        for x in self {
            x.0.null_gcs();
            x.1.null_gcs();
        }
    }
}
