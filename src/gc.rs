use std::{cell::{Cell, RefCell}, marker::{PhantomData, PhantomPinned}, mem, pin::Pin, ptr::{self, NonNull}};

mod trace;
mod root_ref;
pub use trace::Trace;

use self::root_ref::RootRef;

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
    allocs: RefCell<Vec<*const Allocation<dyn Trace>>>,
    roots: RefCell<Vec<*const Allocation<dyn Trace>>>,
    /// this is a usize so we can declare nested safe points
    safe_point: Cell<usize>,
}

thread_local! { static THREAD_CONTEXT: Context = const { Context::new() } }

impl Context {
    const fn new() -> Self {
        Context { 
            allocs: RefCell::new(Vec::new()),
            roots: RefCell::new(Vec::new()),
            safe_point: Cell::new(0),
        }
    }

    unsafe fn push_alloc(alloc: *const Allocation<dyn Trace>) {
        THREAD_CONTEXT.with(|tc| {
            let mut allocs = tc.allocs.borrow_mut();
            allocs.push(alloc);
        })
    }

    /// this assumes the allocation has already been pushed
    unsafe fn push_root(alloc: *const Allocation<dyn Trace>) -> usize {
        THREAD_CONTEXT.with(|tc| {
            let mut roots = tc.roots.borrow_mut();
            let slots = roots.len();
            roots.push(alloc);
            slots
        })
    }

    /// this assumes the allocation has already been pushed
    unsafe fn set_alloc(slot: usize, alloc: *const Allocation<dyn Trace>) {
        THREAD_CONTEXT.with(|tc| {
            let mut roots = tc.roots.borrow_mut();
            debug_assert_eq!(roots[slot] as *const (), ptr::null());
            roots[slot] = alloc;
        });
    }

    unsafe fn pop() {
        THREAD_CONTEXT.with(|tc: &Context| {
            tc.roots.borrow_mut().pop().unwrap();
        });
    }

    pub fn inc_unstable() {
        THREAD_CONTEXT.with(|tc: &Context| {
            tc.safe_point.set(tc.safe_point.get() + 1);
        });
    }

    pub unsafe fn dec_unstable() {
        THREAD_CONTEXT.with(|tc: &Context| {
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
            tc.roots.borrow().len() == 0 && tc.allocs.borrow().len() == 0
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
            tc.allocs.borrow_mut().clear();
        })
    }

    pub fn collect() {
        IS_COLLECTION.set(true);
        THREAD_CONTEXT.with(|tc: &Context| {
            if tc.safe_point.get() != 0 {
                return
            }
            for &root in &*tc.roots.borrow() {
                unsafe { (*root).item.mark(true) };
            }
            tc.allocs.borrow_mut().retain(|&dead| {
                if unsafe { (*dead).marked.get() } {
                    true
                } else {
                    let _ = unsafe { Box::from_raw(dead.cast_mut()) };
                    false
                }
            });
            for &root in &*tc.roots.borrow() {
                unsafe { (*root).item.mark(false) };
            }
        });
        IS_COLLECTION.set(false);
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

impl<'a, T: Trace + 'a> RootTruth<T> {
    pub unsafe fn new_empty() -> Self {
        // this is to remove the liftime of the pointer
        let ptr = ptr::null_mut::<Allocation<T>>() as *const Allocation<dyn Trace>;
        type P = *const Allocation<dyn Trace>;
        let ptr = unsafe { std::mem::transmute::<P, P>(ptr) };
        let slot = unsafe { Context::push_root(ptr) };
        RootTruth { slot, _phantom: PhantomData }
    }

    unsafe fn register(this: Pin<&'a mut Self>, item: *mut Allocation<T>) {
        // need to remove the lifetime of item since we can only set alloc once
        let ptr = item.cast_const() as *const Allocation<dyn Trace>;
        type P = *const Allocation<dyn Trace>;
        let ptr = unsafe { std::mem::transmute::<P, P>(ptr) };
        Context::set_alloc(this.slot, ptr)
    }
}

#[derive(Clone, Copy)]
pub struct Root<'a, T> {
    truth: PhantomData<Pin<&'a mut RootTruth<T>>>,
    item: NonNull<Allocation<T>>,
}

impl<'root, T: Trace + 'root> Root<'root, T> {
    unsafe fn new_unchecked(truth: Pin<&'root mut RootTruth<T>>, item: T) -> Self {
        let item = Allocation::new(item);
        RootTruth::register(truth, item.as_ptr());
        Root { truth: PhantomData, item }
    }


    fn to_gc(self) -> Gc<'root, T> {
        Gc {
            item: self.item,
            _lifetime: PhantomData,
            _aliased: PhantomPinned,
        }
    }
}

impl<'root, T: Trace + 'root> From<Root<'root, T>> for Gc<'root, T> {
    fn from(value: Root<'root, T>) -> Self {
        value.to_gc()
    }
}

impl<'root, 'old, T> Root<'root, T> 
where T: WithGcLt<'root, To = T> + Trace,
{
    unsafe fn from_gc_unchecked(truth: Pin<&'root mut RootTruth<T>>, item: Gc<'old, T>) -> Root<'root, T::To>
    {
        let item = item.item;
        RootTruth::register(truth, item.as_ptr());
        Root { truth: PhantomData, item }
    }
}

impl<'root, T> std::ops::Deref for Root<'root, T> {
    type Target = RootRef<'root, T>;

    fn deref(&self) -> &Self::Target {
        self.into()
    }
}

pub struct RootEmpty<'root, T> {
    truth: Pin<&'root mut RootTruth<T>>,
}

impl<'root, T: 'root> RootEmpty<'root, T> {
    unsafe fn new_unchecked(truth: Pin<&'root mut RootTruth<T>>) -> Self {
        Self { truth }
    }

    fn set(self, item: T) -> Root<'root, T> where T: Trace {
        unsafe { Root::new_unchecked(self.truth, item) }
    }
}

thread_local! { static IS_COLLECTION: Cell<bool> = const { Cell::new(false) } }

#[repr(transparent)]
#[derive(Clone)]
pub struct Gc<'root, T> {
    item: NonNull<Allocation<T>>,
    _lifetime: PhantomData<&'root RootTruth<T>>,
    /// need to be able to alias because we need to be able to trace even if there is a mutable
    /// reference to this. Note that that will only be possible through interior mutability.
    _aliased: PhantomPinned
}

impl<T> Gc<'_, T> {
    /// marks just the immediate pointee
    pub unsafe fn set_mark(this: &Self, mark: bool) {
        let ptr = this.item;
        unsafe { (*ptr.as_ptr()).marked.set(mark) }
    }

    pub fn is_marked(this: &Self, with_mark: bool) -> bool {
        let ptr = this.item;
        unsafe { (*ptr.as_ptr()).marked.get() == with_mark }
    }
}

impl<T> std::ops::Deref for Gc<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        if IS_COLLECTION.get() {
            panic!("cannot dereference Gc pointer while collecting")
        }
        let ptr = self.item;
        unsafe { &(*ptr.as_ptr()).item }
    }
}

pub unsafe trait WithGcLt<'to> {
    type To: 'to;

    unsafe fn extend_lifetime(from: Self) -> Self::To;
}

unsafe impl<'to, T: WithGcLt<'to>> WithGcLt<'to> for Gc<'_, T> {
    type To = Gc<'to, <T as WithGcLt<'to>>::To>;

    unsafe fn extend_lifetime(from: Self) -> Self::To {
        mem::transmute(from)
    }
}

macro_rules! root {
    ($root:ident) => {
        let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new_empty() });
        let $root = unsafe { crate::gc::RootEmpty::new_unchecked($root) };
    };
    ($root:ident, $item:expr) => {
        let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new_empty() });
        let $root = unsafe { crate::gc::Root::new_unchecked($root, $item) };
    };
}

macro_rules! reroot {
    ($root:ident, $gc:expr) => {
        let gc: crate::gc::Gc<_> = $gc;
        let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new_empty() });
        let $root = unsafe { crate::gc::Root::from_gc_unchecked($root, gc) };
    };
}

// macro_rules! populate_root {
//     ($root:ident, $item:ident, $($gc:ident: $from_root:ident),*$(,)?) => {
//         let mut $item = $item;
//         let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new_empty() });
//         let $root = crate::gc::Context::stable_scope(|_| {
//             $($item.$gc = unsafe { $from_root.to_gc() };)*
//             unsafe { crate::gc::Root::new_unchecked($root, $item) }
//         });
//     };
// }


#[cfg(test)]
mod test {
    // use super::*;

    // fn assert_clear() {
    //     if !Context::is_empty() {
    //         Context::leak_all();
    //         panic!("context is not empty");
    //     }
    //     assert!(Context::is_stable());
    // }
    //
    // #[test]
    // fn can_create_roots() {
    //     Context::leak_all();
    //     {
    //         root!(root, 42);
    //         assert_eq!(*root, 42);
    //         assert!(!Context::is_empty());
    //     }
    //     Context::collect();
    //     assert_clear();
    // }
    //
    // #[test]
    // fn can_create_multiple_roots() {
    //     Context::leak_all();
    //     {
    //         root!(root2, 42);
    //         root!(root1, 39);
    //         assert_eq!(*root2, 42);
    //         assert_eq!(*root1, 39);
    //     }
    //     Context::collect();
    //     assert_clear();
    // }
    //
    // #[test]
    // fn populate() {
    //     struct S {
    //         x: Gc<'static, i32>,
    //         y: Gc<'static, i32>,
    //         z: i32
    //     }
    //
    //     unsafe impl Trace for S {
    //         unsafe fn mark(&self, mark: bool) {
    //             self.x.mark(mark);
    //             self.y.mark(mark);
    //         }
    //
    //         fn null_gcs(&self) {
    //             self.x.null_gcs();
    //             self.y.null_gcs();
    //         }
    //     }
    //
    //     Context::leak_all();
    //     {
    //         root!(x, 39);
    //         root!(y, 40);
    //         let s = S {
    //             x: Gc::new(),
    //             y: Gc::new(),
    //             z: 41,
    //         };
    //         populate_root!(root, s, x: x, y: y);
    //         assert_eq!(*root.x, 39);
    //         assert_eq!(*root.y, 40);
    //         assert_eq!(root.z, 41);
    //     }
    //     Context::collect();
    //     assert_clear();
    // }
}
