use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{Datum, Symbol};


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

    pub fn get(&self, sym: impl AsRef<str>) -> Option<Datum> {
        let sym = sym.as_ref();
        let guard = self.0.borrow();
        guard.syms.get(sym).cloned().or_else(|| guard.up.as_deref().and_then(|x| x.get(sym)))
    }

    pub fn has(&self, sym: impl AsRef<str>) -> bool {
        let sym = sym.as_ref();
        let guard = self.0.borrow();
        guard.syms.contains_key(sym) || guard.up.as_deref().is_some_and(|x| x.has(sym))
    }

    pub fn declare(&self, sym: Symbol, val: Datum) {
        self.0.borrow_mut().syms.insert(sym, val);
    }

    fn set_inner(&self, sym: Symbol, val: Datum) -> bool {
        let mut guard = self.0.borrow_mut();
        let set = |map: &mut HashMap<_, _>| {
            let entry = map.entry(sym.clone()).and_modify(|k| *k = val.clone());
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
        }.into()))
    }
}
