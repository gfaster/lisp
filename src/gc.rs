use std::{cell::{Cell, RefCell}, marker::{PhantomData, PhantomPinned}, mem::{self, MaybeUninit}, pin::Pin, ptr::{self, NonNull}};

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

    unsafe fn to_raw(this: NonNull<Self>) -> NonNull<T> {
        unsafe { (&this.as_ref().item).into()}
    }

    unsafe fn from_raw(this: NonNull<T>) -> NonNull<Self> {
        todo!()
    }
}

pub struct Context {
    allocs: RefCell<Vec<*const Allocation<dyn Trace>>>,
    roots: RefCell<Vec<*const dyn Trace>>,
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

    unsafe fn push_alloc<'a, T: Trace + 'a>(alloc: *const Allocation<T>) {
        THREAD_CONTEXT.with(|tc| {
            let mut allocs = tc.allocs.borrow_mut();
            type R<'a> = &'a Allocation<dyn Trace>;
            type P = *const Allocation<dyn Trace>;
            let ptr: P = unsafe { mem::transmute::<P, P>(alloc as *const _) };
            let ptr = unsafe { std::mem::transmute::<P, P>(ptr) };
            allocs.push(ptr);
        })
    }

    unsafe fn push_root<T: Trace>() -> usize {
        THREAD_CONTEXT.with(|tc| {
            let mut roots = tc.roots.borrow_mut();
            let slots = roots.len();
            type P = *const dyn Trace;
            let ptr = ptr::null::<T>() as P;
            let ptr = unsafe { std::mem::transmute::<P, P>(ptr) };
            roots.push(ptr);
            slots
        })
    }

    unsafe fn set_root<T: Trace>(slot: usize, root: NonNull<T>) {
        THREAD_CONTEXT.with(|tc| {
            let mut roots = tc.roots.borrow_mut();
            // debug_assert_eq!(roots[slot] as *const (), ptr::null());
            type P = *const dyn Trace;
            let ptr = root.as_ptr() as P;
            let ptr = unsafe { std::mem::transmute::<P, P>(ptr) };
            roots[slot] = ptr;
        })
    }

    // /// this assumes the allocation has already been pushed
    // unsafe fn set_alloc(slot: usize, alloc: *const dyn Trace) {
    //     THREAD_CONTEXT.with(|tc| {
    //         let mut roots = tc.roots.borrow_mut();
    //         debug_assert_eq!(roots[slot] as *const (), ptr::null());
    //         roots[slot] = alloc;
    //     });
    // }

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
                unsafe { (*root).mark(true) };
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
                unsafe { (*root).mark(false) };
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

/// we can't use MaybeUninit with RootTruth due to the lack of Trace implementation.
pub struct RootTruth<T: ?Sized> {
    _phantom: PhantomData<PhantomPinned>,
    slot: usize,
    item: T,
}

impl<T: ?Sized> Drop for RootTruth<T> {
    fn drop(&mut self) {
        unsafe { Context::pop() };
    }
}

impl<'a, T: Trace + 'a> RootTruth<T> {
    pub unsafe fn new(item: T) -> Self {
        let slot = Context::push_root::<T>();
        RootTruth { _phantom: PhantomData, item, slot }
    }

    unsafe fn lock(this: Pin<&'a mut Self>, slot: usize) -> RootRef<'a, T> {
        // need to remove the lifetime of item since we can only set alloc once
        Context::set_root(slot, (&this.item).into());
        this.into()
    }
}

impl<'a, T: Trace + 'a> RootTruth<Gc<'a, T>> {
    pub unsafe fn new_heap(item: T) -> Self {
        let slot = Context::push_root::<Gc<'_, T>>();
        let item = Gc::new_unbounded(item);
        RootTruth { _phantom: PhantomData, item, slot}
    }

    unsafe fn lock_heap(this: Pin<&'a mut Self>, slot: usize) -> Root<'a, T> {
        Context::set_root(slot, (&*this.item).into());
        Root { truth: PhantomData, item: this.item.item }
    }
}

/// rooted pointer to the heap
#[derive(Clone, Copy)]
pub struct Root<'a, T> {
    truth: PhantomData<Pin<&'a mut RootTruth<Allocation<T>>>>,
    item: NonNull<Allocation<T>>,
}

impl<'root, T: Trace + 'root> Root<'root, T> {
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
    unsafe fn from_gc_unchecked(empty: RootEmpty<'root, Gc<'root, T>>, item: Gc<'old, T>) -> Root<'root, T::To>
    {
        let ret = Root {
            truth: PhantomData,
            item: item.item,
        };
        empty.set(unsafe { WithGcLt::extend_lifetime(item) });
        ret
    }
}

pub struct RootEmpty<'root, T> {
    truth: Pin<&'root mut RootTruth<Option<T>>>,
}

impl<'root, T: 'root> RootEmpty<'root, T> {
    unsafe fn new_unchecked(truth: Pin<&'root mut RootTruth<Option<T>>>) -> Self {
        Self { truth }
    }

    fn set(mut self, item: T) -> RootRef<'root, T> where T: Trace {
        unsafe { self.truth.as_mut().get_unchecked_mut().item = Some(item) };
        let slot = self.truth.slot;
        let locked = unsafe { RootTruth::lock(self.truth, slot)};
        locked.unwrap_option()
    }
}

thread_local! { static IS_COLLECTION: Cell<bool> = const { Cell::new(false) } }

/// I want Gc to have the same repr as RootRef
#[repr(transparent)]
pub struct Gc<'root, T> {
    item: NonNull<T>,
    _lifetime: PhantomData<&'root RootTruth<T>>,
    /// need to be able to alias because we need to be able to trace even if there is a mutable
    /// reference to this. Note that that will only be possible through interior mutability.
    _aliased: PhantomPinned
}

impl<T> Clone for Gc<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Gc<'_, T> {}


impl<'root, T> Gc<'root, T> {
    unsafe fn new_unbounded(item: T) -> Self where T: Trace {
        let item = Allocation::new(item);
        Context::push_alloc(item.as_ref());
        Self { item, _lifetime: PhantomData, _aliased: PhantomPinned }
    }

    /// marks just the immediate pointee
    pub unsafe fn set_mark(this: &Self, mark: bool) {
        let ptr = this.item;
        unsafe { (*ptr.as_ptr()).marked.set(mark) }
    }

    pub fn is_marked(this: &Self, with_mark: bool) -> bool {
        let ptr = this.item;
        unsafe { (*ptr.as_ptr()).marked.get() == with_mark }
    }

    pub unsafe fn assume_rooted(self) -> RootRef<'root, T> {
        mem::transmute(self)
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
        let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new() });
        let $root = unsafe { crate::gc::RootEmpty::new_unchecked($root) };
    };
    ($root:ident, $item:expr) => {
        let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new() });
        let $root = unsafe { crate::gc::Root::new_unchecked($root, $item) };
    };
}
pub(crate) use root;

macro_rules! reroot {
    ($root:ident, $gc:expr) => {
        let gc: crate::gc::Gc<_> = $gc;
        let $root = std::pin::pin!( unsafe { crate::gc::RootTruth::new_empty() });
        let $root = unsafe { crate::gc::Root::from_gc_unchecked($root, gc) };
    };
}
pub(crate) use reroot;

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
