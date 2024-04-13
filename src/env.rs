use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::Datum;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(u32);

impl Symbol {
    /// looks up the name and returns the symbol associated with it, creating if it doesn't exist
    pub fn new(name: Rc<str>) -> Self {
        SymTable::get(name)
    }

    /// gets the symbol, but will be slower if it doesn't exist
    pub fn get_borrowed(name: impl AsRef<str>) -> Self {
        SymTable::get_borrowed(name)
    }

    pub fn gensym(prefix: &str) -> Self {
        SymTable::gensym(prefix)
    }
}

// macro_rules! static_gensym {
//     () => {{
//         thread_local! { static SYM: Symbol = Symbol::gensym("static_symbol") };
//         SYM
//     }};
// }

macro_rules! static_sym {
    ($sym:literal) => {{
        thread_local! { static SYM: std::cell::Cell<Symbol> = std::cell::Cell::new(Symbol::get_borrowed($sym)) };
        let sym: Symbol = SYM.get();
        sym
    }};
}
pub(crate) use static_sym;

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        SymTable::get_name(*self).fmt(f)
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}

pub struct SymTable {
    syms: Cell<Option<HashMap<Rc<str>, Symbol>>>,
    sym_names: Cell<Option<HashMap<Symbol, Rc<str>>>>,
}

thread_local! { static SYMS: SymTable = const { SymTable::new() } }

impl SymTable {
    const fn new() -> Self {
        Self {
            syms: Cell::new(None),
            sym_names: Cell::new(None),
        }
    }

    pub fn get_name(sym: Symbol) -> Rc<str> {
        SYMS.with(|t: &SymTable| {
            let table = t.sym_names.take().expect("symbol doesn't exist");
            let name = Rc::clone(table.get(&sym).expect("symbol doesn't exist"));
            t.sym_names.set(Some(table));
            name
        })
    }

    fn gensym(prefix: &str) -> Symbol {
        let sym = SymTable::new_symbol_no_insert();
        let name: Rc<str> = format!("__{prefix}{}__", sym.0).into();
        let ret = SYMS.with(|t: &SymTable| {
            let mut table = t.syms.take().unwrap_or_else(|| HashMap::new());
            let mut names = t.sym_names.take().unwrap_or_else(|| HashMap::new());
            table.insert(Rc::clone(&name), sym);
            names.insert(sym, Rc::clone(&name));
            t.syms.set(Some(table));
            t.sym_names.set(Some(names));
            sym
        });
        ret
    }

    fn new_symbol_no_insert() -> Symbol {
        use std::sync::atomic::*;
        static CNT: AtomicU32 = AtomicU32::new(0);
        Symbol(CNT.fetch_add(1, Ordering::Relaxed))
    }

    pub fn get_borrowed(s: impl AsRef<str>) -> Symbol {
        let s = s.as_ref();
        let ret = SYMS.with(|t: &SymTable| {
            let table = t.syms.take().unwrap_or_else(|| HashMap::new());
            let Some(&sym) = table.get(s) else {
                t.syms.set(Some(table));
                return SymTable::get(s.into());
            };
            t.syms.set(Some(table));
            sym
        });
        ret
    }

    pub fn get(s: Rc<str>) -> Symbol {
        let ret = SYMS.with(|t: &SymTable| {
            let mut table = t.syms.take().unwrap_or_else(|| HashMap::new());
            let sym = *table.entry(Rc::clone(&s)).or_insert_with(|| {
                let new_sym = SymTable::new_symbol_no_insert();
                let mut names = t.sym_names.take().unwrap_or_else(|| HashMap::new());
                names.insert(new_sym, s);
                t.sym_names.set(Some(names));
                new_sym
            });
            t.syms.set(Some(table));
            sym
        });
        ret
    }
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        Symbol::new(value.into())
    }
}

pub struct Env(RefCell<EnvInner>);

#[derive(Default)]
struct EnvInner {
    up: Option<Rc<Env>>,
    syms: HashMap<Symbol, Datum>,
}

impl Env {
    pub fn new() -> Rc<Self> {
        Rc::new(Env(EnvInner::default().into()))
    }

    pub fn get(&self, sym: Symbol) -> Option<Datum> {
        let guard = self.0.borrow();
        guard
            .syms
            .get(&sym)
            .cloned()
            .or_else(|| guard.up.as_deref().and_then(|x| x.get(sym)))
    }

    #[allow(dead_code)]
    pub fn has(&self, sym: Symbol) -> bool {
        let guard = self.0.borrow();
        guard.syms.contains_key(&sym) || guard.up.as_deref().is_some_and(|x| x.has(sym))
    }

    pub fn declare(&self, sym: Symbol, val: Datum) {
        self.0.borrow_mut().syms.insert(sym, val);
    }

    fn set_inner(&self, sym: Symbol, val: Datum) -> bool {
        let mut guard = self.0.borrow_mut();
        let set = |map: &mut HashMap<_, _>| {
            let entry = map.entry(sym).and_modify(|k| *k = val.clone());
            match entry {
                std::collections::hash_map::Entry::Occupied(_) => true,
                std::collections::hash_map::Entry::Vacant(_) => false,
            }
        };
        set(&mut guard.syms) || guard.up.as_deref().is_some_and(|x| x.set_inner(sym, val))
    }

    pub fn set(&self, sym: Symbol, val: Datum) -> Result<(), ()> {
        self.set_inner(sym, val).then_some(()).ok_or(())
    }

    pub fn extend(self: Rc<Self>) -> Rc<Self> {
        Rc::new(Env(EnvInner {
            up: Some(self),
            syms: HashMap::new(),
        }
        .into()))
    }
}
