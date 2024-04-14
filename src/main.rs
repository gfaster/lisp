use crate::env::{static_sym, Symbol};
use std::any::Any;
use std::cell::Cell;
use std::io::prelude::{Read, Write};
use std::rc::Rc;

use env::Env;

mod env;
mod intrinsics;
mod error;
use error::*;

type LispResult = Result<Datum, Error>;



#[derive(Clone, Default)]
enum Datum {
    Symbol(Symbol),
    Atom(i64),
    Cons(Rc<Cons>),
    Func(fn(List, Rc<Env>) -> LispResult),
    Lambda(Rc<Lambda>),
    Obj(Rc<dyn Any>),
    Macro(Rc<Macro>),

    #[allow(dead_code)]
    DynamicExtent(Rc<Env>),

    #[default]
    Nil,
}

impl std::fmt::Debug for Datum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}

impl FromIterator<Datum> for Datum {
    fn from_iter<T: IntoIterator<Item = Datum>>(iter: T) -> Self {
        Datum::from_cdr(Cons::from_datums(iter))
    }
}

impl Datum {
    fn from_cdr(cdr: Option<Rc<Cons>>) -> Self {
        match cdr {
            Some(cdr) => Datum::Cons(cdr),
            None => Datum::Nil,
        }
    }

    fn as_atom(&self) -> Result<i64, TypeError> {
        if let Self::Atom(a) = self {
            Ok(*a)
        } else {
            Err(TypeError)
        }
    }

    fn as_list(&self) -> Result<List, TypeError> {
        match self {
            Datum::Cons(c) => Ok(List(Some(Rc::clone(c)))),
            Datum::Nil => Ok(List(None)),
            _ => Err(TypeError),
        }
    }

    fn as_sym(&self) -> Result<Symbol, TypeError> {
        if let Self::Symbol(s) = self {
            Ok(*s)
        } else {
            Err(TypeError)
        }
    }

    /// See [`List::stream_map_symbols`] - Note that this won't modify the cons if this belongs to
    /// one
    #[allow(dead_code)]
    fn stream_map_symbols(self, mut f: impl FnMut(Symbol) -> Symbol) -> Self {
        if let Self::Symbol(s) = self {
            return Self::Symbol(f(s))
        } 
        if let Self::Cons(c) = &self {
            List(Some(c.clone())).stream_map_symbols(f)
        }
        self
    }

    /// clones all nested lists, but keeps everything else aliased
    #[allow(dead_code)]
    fn list_clone(self) -> Self {
        if let Ok(l) = self.as_list() {
            return l.into_iter().map(|x| x.list_clone()).collect()
        }
        self
    }

    fn eval(&self, env: Rc<Env>) -> LispResult {
        match self {
            Datum::Symbol(s) => env.get(*s).ok_or(Error::SymbolNotFound(*s)),
            Datum::Atom(i) => Ok(Datum::Atom(*i)),
            Datum::Cons(c) => c.eval(env),
            Datum::Func(f) => Ok(Datum::Func(*f)),
            Datum::Lambda(l) => Ok(Datum::Lambda(l.clone())),
            Datum::Obj(o) => Ok(Datum::Obj(o.clone())),
            Datum::Nil => Ok(Datum::Nil),
            Datum::Macro(m) => Ok(Datum::Macro(m.clone())),
            Datum::DynamicExtent(e) => Ok(Datum::DynamicExtent(e.clone())),
        }
    }
}

impl<const LEN: usize> From<[Datum; LEN]> for Datum {
    fn from(value: [Datum; LEN]) -> Self {
        value.into_iter().collect()
    }
}

struct Lambda {
    params: Box<[Symbol]>,
    body: List,
}

impl Lambda {
    fn eval(&self, params: Option<Rc<Cons>>, env: Rc<Env>) -> LispResult {
        let param_vals: Vec<_> = Cons::flat_iter(params).collect();
        if param_vals.len() != self.params.len() {
            return Err(Error::MismatchedParamNum);
        }

        let env = env.extend();
        for (&sym, val) in self.params.iter().zip(param_vals) {
            env.declare(sym, val);
        }
        intrinsics::progn(self.body.clone(), env)
    }
}

struct Macro {
    /// these symbols should be unique
    args: Vec<Symbol>,
    body: Datum,
}

impl Macro {
    fn expand(&self, l: List, env: Rc<Env>) -> LispResult {
        let env = env.extend();
        let mut it = l.into_iter();
        for &sym in &self.args {
            let val = it.next().ok_or(MismatchedParamNum)?;
            env.declare(sym, val);
        }
        if it.next().is_some() {
            return Err(Error::MismatchedParamNum);
        }
        self.body.eval(env)
    }
}

struct Cons {
    car: Cell<Datum>,
    cdr: Cell<Option<Rc<Cons>>>,
}

impl Cons {
    fn new_pair(car: Datum, cdr: Option<Rc<Cons>>) -> Self {
        Cons {
            car: car.into(),
            cdr: cdr.into(),
        }
    }

    fn as_pair(&self) -> Result<(Datum, Datum), MismatchedParamNum> {
        let left = self.get_car();
        let right = self.get_cdr().ok_or(MismatchedParamNum)?;
        if right.get_cdr().is_some() {
            return Err(MismatchedParamNum)
        }
        Ok((left, right.get_car()))
    }

    fn new(car: Datum) -> Self {
        Cons {
            car: car.into(),
            cdr: None.into(),
        }
    }

    fn from_datums(iter: impl IntoIterator<Item = Datum>) -> Option<Rc<Self>> {
        let mut ret = None;
        let mut tail = None;
        for datum in iter {
            let new = Rc::new(Cons::new(datum));
            if matches!(ret, None) {
                ret = Some(new);
                tail = ret.clone();
            } else {
                let Some(prev) = tail else { unreachable!() };
                prev.cdr.set(Some(new));
                tail = prev.get_cdr();
            }
        }
        ret
    }

    fn set_car(&self, car: Datum) {
        self.car.set(car)
    }

    fn get_car(&self) -> Datum {
        let car = self.car.replace(Datum::Nil);
        self.car.set(car.clone());
        car
    }

    fn get_cdr(&self) -> Option<Rc<Cons>> {
        let cdr = self.cdr.take();
        self.cdr.set(cdr.clone());
        cdr
    }

    #[allow(dead_code)]
    fn get_cadr(&self) -> Option<Datum> {
        self.get_cdr().map(|x| x.get_car())
    }

    #[allow(dead_code)]
    fn get_caadr(&self) -> Option<Datum> {
        self.get_cdr().and_then(|x| x.get_cdr()).map(|x| x.get_car())
    }

    #[allow(dead_code)]
    fn iter(self: Rc<Self>) -> ConsIter {
        ConsIter { cons: Some(self) }
    }

    fn flat_iter(cons: Option<Rc<Self>>) -> ConsIter {
        ConsIter { cons }
    }

    fn eval(&self, env: Rc<Env>) -> LispResult {
        let op = match self.get_car() {
            Datum::Atom(_) | Datum::Obj(_) | Datum::Nil => return Err(Error::TypeError),
            Datum::Cons(c) => c.eval(env.clone())?,
            Datum::Symbol(s) => env.get(s).ok_or(SymbolNotFound(s))?,
            op => op,
        };

        match op {
            Datum::Atom(_) | Datum::Cons(_) | Datum::Obj(_) | Datum::Nil | Datum::Symbol(_) => {
                Err(Error::TypeError)
            },
            Datum::Macro(m) => m.expand(List(self.get_cdr()), env.clone())?.eval(env),
            Datum::Func(f) => f(List(self.get_cdr()), env),
            Datum::Lambda(l) => l.eval(self.get_cdr(), env),
            Datum::DynamicExtent(e) => {
                let (_, inner) = self.as_pair()?;
                inner.eval(env.extend_with(&e))
            },
        }
    }
}

struct ConsIter {
    cons: Option<Rc<Cons>>,
}

impl ConsIter {
    fn rem(self) -> List {
        List(self.cons)
    }
}

impl Iterator for ConsIter {
    type Item = Datum;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.cons.take()?;
        self.cons = curr.get_cdr();
        Some(curr.get_car())
    }
}

impl std::fmt::Display for Datum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Datum::Symbol(s) => write!(f, "{s}"),
            Datum::Atom(a) => write!(f, "{a}"),
            Datum::Cons(cons) => write!(f, "{cons}"),
            Datum::Func(func) => write!(f, "[function at {:p}]", func),
            Datum::Nil => write!(f, "()"),
            Datum::Lambda(l) => write!(f, "[lambda at {l:p}]"),
            Datum::Obj(o) => write!(f, "[object at {o:p}]"),
            Datum::Macro(m) => write!(f, "[macro at {m:p}]"),
            Datum::DynamicExtent(e) => write!(f, "[dynamic extent at {e:p}]"),
        }
    }
}

impl std::fmt::Display for Cons {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}", self.get_car())?;
        let mut cdr = self.get_cdr();
        while let Some(cons) = cdr {
            write!(f, " {}", cons.get_car())?;
            cdr = cons.get_cdr();
        }
        write!(f, ")")
    }
}

#[derive(Clone)]
struct List(Option<Rc<Cons>>);

// const NIL: List = List(None);

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Some(c) => c.fmt(f),
            None => "()".fmt(f),
        }
    }
}

impl std::fmt::Debug for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}

impl List {
    fn to_datum(self) -> Datum {
        match self.0 {
            Some(a) => Datum::Cons(a),
            None => Datum::Nil,
        }
    }

    fn as_args<const N: usize>(self) -> Result<[Datum; N], MismatchedParamNum> {
        // TODO: avoid alloc
        let v: Vec<_> = self.into_iter().collect();
        v.try_into().map_err(|_| MismatchedParamNum)
    }

    /// mutates symbols recursively, fails on cycles, does not modify dynamic extents (for now). We
    /// don't modify lambdas either, since they're already compiled if we encounter them, but we
    /// will modify lambda definitions.
    ///
    /// `f` should return its passed symbol if it doesn't want to modify it.
    fn stream_map_symbols(self, f: impl FnMut(Symbol) -> Symbol) {
        let mut next = self.0.clone();
        let mut f: Box<dyn FnMut(Symbol) -> Symbol> = Box::new(f);
        while let Some(curr) = next {
            let car = curr.get_car();
            if let Ok(sym) = car.as_sym() {
                curr.set_car(Datum::Symbol(f(sym)));
            } else {
                match car {
                    Datum::Symbol(_) => unreachable!(),
                    Datum::Atom(_) => (),
                    Datum::Cons(c) => List(Some(c)).stream_map_symbols(&mut f),
                    Datum::Func(_) => (),
                    Datum::Lambda(_) => (),
                    Datum::Obj(_) => (),
                    Datum::Macro(_) => (),
                    Datum::DynamicExtent(_) => (),
                    Datum::Nil => (),
                }
            }
            next = curr.get_cdr();
        }
    }
}

impl IntoIterator for List {
    type Item = Datum;

    type IntoIter = ConsIter;

    fn into_iter(self) -> Self::IntoIter {
        ConsIter { cons: self.0 }
    }
}

impl FromIterator<Datum> for List {
    fn from_iter<T: IntoIterator<Item = Datum>>(iter: T) -> Self {
        List(Cons::from_datums(iter))
    }
}

fn read(buf: &str) -> Datum {
    fn read_list(mut s: &str) -> (Datum, &str) {
        s = s.trim_start();
        let Some(first_c) = s.chars().nth(0) else {
            panic!("No closing paren")
        };
        if first_c == ')' {
            return (Datum::Nil, &s[1..]);
        }
        let (car, rem) = read_s(s);
        let (cdr, rem) = read_list(rem);
        let cdr = match cdr {
            Datum::Cons(cons) => Some(cons),
            Datum::Nil => None,
            _ => unreachable!("read_list returns a list"),
        };
        (Datum::Cons(Cons::new_pair(car, cdr).into()), rem)
    }

    fn read_str(s: &str) -> (Datum, &str) {
        let mut it = s.char_indices();
        let mut buf = String::new();
        if it.next() != Some((0, '"')) {
            unreachable!()
        }
        while let Some((i, c)) = it.next() {
            if c == '"' {
                let ret: Rc<dyn Any> = Rc::new(buf);
                return (Datum::Obj(ret), &s[(i + 1)..]);
            }
            if c == '\\' {
                match it.next() {
                    Some((_, 'n')) => buf.push('\n'),
                    Some((_, 't')) => buf.push('\t'),
                    Some((_, '\\')) => buf.push('\\'),
                    Some((_, '"')) => buf.push('"'),
                    None => panic!("No close quote"),
                    Some((_, c)) => panic!("Unknown escaped character: {c}"),
                }
                continue;
            }
            buf.push(c);
        }
        panic!("No close quote")
    }

    fn read_s(s: &str) -> (Datum, &str) {
        let s = s.trim_start();
        let mut it = s.chars();
        let Some(first_c) = it.next() else {
            return (Datum::Nil, s);
        };
        match first_c {
            ')' => panic!("Extra closing paren"),
            '(' => read_list(&s[1..]),
            '\'' => {
                let (expr, rem) = read_s(&s[1..]);
                let sym = static_sym!("quote");
                ([Datum::Symbol(sym), expr].into(), rem)
            }
            '`' => {
                let (expr, rem) = read_s(&s[1..]);
                let sym = static_sym!("quasiquote");
                ([Datum::Symbol(sym), expr].into(), rem)
            }
            ',' => {
                if it.next() == Some('@') {
                    let (expr, rem) = read_s(&s[2..]);
                    let sym = static_sym!("unquotesplice");
                    ([Datum::Symbol(sym), expr].into(), rem)
                } else {
                    let (expr, rem) = read_s(&s[1..]);
                    let sym = static_sym!("unquote");
                    ([Datum::Symbol(sym), expr].into(), rem)
                }
            }
            '"' => read_str(s),
            c => {
                let len = 1 + it
                    .take_while(|&c| !c.is_ascii_whitespace() && c != '(' && c != ')')
                    .count();
                let dat = &s[..len];
                let dat = if dat == "nil" {
                    Datum::Nil
                } else if c.is_ascii_digit() {
                    Datum::Atom(dat.parse().unwrap())
                } else {
                    Datum::Symbol(dat.into())
                };
                (dat, &s[len..])
            }
        }
    }

    read_s(buf).0
}

fn read_file(p: impl AsRef<std::path::Path>, env: Rc<Env>) -> std::io::Result<()> {
    let mut f = std::fs::File::open(p)?;
    let mut buf = String::new();
    f.read_to_string(&mut buf)?;
    read(&buf).eval(env).unwrap();
    Ok(())
}

fn main() {
    let root_env = Env::new();
    intrinsics::declare_intrinsics(&root_env);
    read_file("stdlib.cl", root_env.clone()).unwrap();
    let mut buf = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut buf).unwrap();
        match read(&buf).eval(Rc::clone(&root_env)) {
            Ok(dat) => {
                println!("{dat}");
            }
            Err(e) => {
                println!("ERROR: {e}");
            }
        }
        buf.clear();
    }
}
