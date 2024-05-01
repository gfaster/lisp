use std::{cell::{Cell, RefCell}, marker::{PhantomData, PhantomPinned}, pin::Pin, ptr::{self, NonNull}};

mod trace;
pub use trace::Trace;

struct Allocation<T: ?Sized> {
    marked: Cell<bool>,
    item: T
}

impl<T> Allocation<T> {
    fn new(item: T) -> NonNull<Self> {
       NonNull::new(Box::into_raw(Box::new(Self { marked: false.into(), item }))).unwrap()
    }
}

pub struct Context {
    dead_roots: RefCell<Vec<*const Allocation<dyn Trace>>>,
    roots: RefCell<Vec<*const Allocation<dyn Trace>>>,
    /// this is a usize so we can declare nested safe points
    safe_point: Cell<usize>,
}

thread_local! { static THREAD_CONTEXT: Context = const { Context::new() } }

impl Context {
    const fn new() -> Self {
        Context { 
            dead_roots: RefCell::new(Vec::new()),
            roots: RefCell::new(Vec::new()),
            safe_point: Cell::new(0),
        }
    }

    unsafe fn push(alloc: *const Allocation<dyn Trace>) -> usize {
        THREAD_CONTEXT.with(|tc| {
            let mut roots = tc.roots.borrow_mut();
            let slots = roots.len();
            roots.push(alloc);
            slots
        })
    }

    unsafe fn set_alloc(slot: usize, alloc: *const Allocation<dyn Trace>) {
        THREAD_CONTEXT.with(|tc| {
            let mut roots = tc.roots.borrow_mut();
            debug_assert_eq!(roots[slot] as *const (), ptr::null());
            roots[slot] = alloc;
        });
    }

    unsafe fn pop() {
        THREAD_CONTEXT.with(|tc| {
            let dead = tc.roots.borrow_mut().pop().unwrap();
            if !dead.is_null() {
                tc.dead_roots.borrow_mut().push(dead);
            }
        });
    }

    pub fn inc_unstable() {
        THREAD_CONTEXT.with(|tc| {
            tc.safe_point.set(tc.safe_point.get() + 1);
        });
    }

    pub unsafe fn dec_unstable() {
        THREAD_CONTEXT.with(|tc| {
            tc.safe_point.set(tc.safe_point.get() - 1);
        });
    }

    pub fn stable_scope<T>(f: impl FnOnce(&StableToken) -> T) -> T {
        let tok = StableToken::new();
        let ret = f(&tok);
        ret
    }

    fn is_empty() -> bool {
        THREAD_CONTEXT.with(|tc: &Context| {
            tc.roots.borrow().len() == 0 && tc.dead_roots.borrow().len() == 0
        })
    }

    fn is_stable() -> bool {
        THREAD_CONTEXT.with(|tc| {
            tc.safe_point.get() == 0
        })
    }

    fn leak_all() {
        assert!(Context::is_stable());
        THREAD_CONTEXT.with(|tc: &Context| {
            assert!(tc.roots.borrow().len() == 0);
            tc.roots.borrow_mut().clear();
            tc.dead_roots.borrow_mut().clear();
        })
    }

    pub fn collect() {
        THREAD_CONTEXT.with(|tc: &Context| {
            if tc.safe_point.get() != 0 {
                return
            }
            for &root in &*tc.roots.borrow() {
                unsafe { (*root).item.mark(true) };
            }
            tc.dead_roots.borrow_mut().retain(|&dead| {
                if unsafe { (*dead).marked.get() } {
                    true
                } else {
                    unsafe { (*dead).item.null_gcs() };
                    let _ = unsafe { Box::from_raw(dead.cast_mut()) };
                    false
                }
            });
            for &root in &*tc.roots.borrow() {
                unsafe { (*root).item.mark(false) };
            }
        });
    }
}

pub struct StableToken {
    _no_construct: (),
}

impl StableToken {
    fn new() -> Self {
        Context::inc_unstable();
        StableToken {_no_construct: ()}
    }
}

impl Drop for StableToken {
    fn drop(&mut self) {
        unsafe { Context::dec_unstable() }
    }
}

pub struct RootTruth<T: ?Sized> {
    _phantom: PhantomData<(PhantomPinned, *mut T)>,
    slot: usize,
}

impl<T: ?Sized> Drop for RootTruth<T> {
    fn drop(&mut self) {
        unsafe { Context::pop() };
    }
}

impl<T: Trace + 'static> RootTruth<T> {
    pub unsafe fn new_empty() -> Self {
        let slot = unsafe { Context::push(ptr::null_mut::<Allocation<T>>()) };
        RootTruth { slot, _phantom: PhantomData }
    }

    unsafe fn register(this: Pin<&mut Self>, item: *mut Allocation<T>) {
        Context::set_alloc(this.slot, item)
    }
}

#[derive(Clone, Copy)]
pub struct Root<'a, T: ?Sized> {
    truth: PhantomData<Pin<&'a mut RootTruth<T>>>,
    item: NonNull<Allocation<T>>,
}

impl<'a, T: Trace + 'static> Root<'a, T> {
    unsafe fn new_unchecked(truth: Pin<&'a mut RootTruth<T>>, item: T) -> Self {
        let item = Allocation::new(item);
        RootTruth::register(truth, item.as_ptr());
        Root { truth: PhantomData, item }
    }

    unsafe fn to_gc(self) -> Gc<T> {
        Gc {
            item: Cell::new(Some(self.item)),
        }
    }
}

impl<T> std::ops::Deref for Root<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.item.as_ptr()).item }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Root<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&**self).fmt(f)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Root<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&**self).fmt(f)
    }
}

impl<T: std::cmp::PartialEq> std::cmp::PartialEq for Root<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(other)
    }
}

impl<T: std::cmp::Eq> std::cmp::Eq for Root<'_, T> { }



pub struct Gc<T: ?Sized> {
    item: Cell<Option<NonNull<Allocation<T>>>>
}

impl<T> Gc<T> {
    pub const fn new() -> Self {
        Self { item: Cell::new(None) }
    }

    /// marks just the immediate pointee
    pub unsafe fn set_mark(this: &Self, mark: bool) {
        if let Some(ptr) = this.item.get() {
            unsafe { (*ptr.as_ptr()).marked.set(mark) }
        }
    }

    pub fn is_marked(this: &Self, with_mark: bool) -> bool {
        if let Some(ptr) = this.item.get() {
            unsafe { (*ptr.as_ptr()).marked.get() == with_mark }
        } else {
            true
        }
    }
}

impl<T> std::ops::Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let ptr = self.item.get().expect("Tried to derefence a Gc pointer out of the heap");
        unsafe { &(*ptr.as_ptr()).item }
    }
}

macro_rules! root {
    ($root:ident, $item:expr) => {
        let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new_empty() });
        let $root = unsafe { crate::gc::Root::new_unchecked($root, $item) };
    };
}

macro_rules! populate_root {
    ($root:ident, $item:ident, $($gc:ident: $from_root:ident),*$(,)?) => {
        let mut $item = $item;
        let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new_empty() });
        let $root = crate::gc::Context::stable_scope(|_| {
            $($item.$gc = unsafe { $from_root.to_gc() };)*
            unsafe { crate::gc::Root::new_unchecked($root, $item) }
        });
    };
}


#[cfg(test)]
mod test {
    use super::*;

    fn assert_clear() {
        if !Context::is_empty() {
            Context::leak_all();
            panic!("context is not empty");
        }
        assert!(Context::is_stable());
    }

    #[test]
    fn can_create_roots() {
        Context::leak_all();
        {
            root!(root, 42);
            assert_eq!(*root, 42);
            assert!(!Context::is_empty());
        }
        Context::collect();
        assert_clear();
    }

    #[test]
    fn can_create_multiple_roots() {
        Context::leak_all();
        {
            root!(root2, 42);
            root!(root1, 39);
            assert_eq!(*root2, 42);
            assert_eq!(*root1, 39);
        }
        Context::collect();
        assert_clear();
    }

    #[test]
    fn populate() {
        struct S {
            x: Gc<i32>,
            y: Gc<i32>,
            z: i32
        }

        unsafe impl Trace for S {
            unsafe fn mark(&self, mark: bool) {
                self.x.mark(mark);
                self.y.mark(mark);
            }

            fn null_gcs(&self) {
                self.x.null_gcs();
                self.y.null_gcs();
            }
        }

        Context::leak_all();
        {
            root!(x, 39);
            root!(y, 40);
            let s = S {
                x: Gc::new(),
                y: Gc::new(),
                z: 41,
            };
            populate_root!(root, s, x: x, y: y);
            assert_eq!(*root.x, 39);
            assert_eq!(*root.y, 40);
            assert_eq!(root.z, 41);
        }
        Context::collect();
        assert_clear();
    }
}
