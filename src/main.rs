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
mod gc;

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
    Goto(Symbol),

    #[allow(dead_code)]
    DynamicExtent(Rc<Env>),

    #[default]
    Nil,
}

#[derive(Debug, Clone, Copy)]
enum Type {
    Symbol,
    Atom,
    Cons,
    Func,
    Lambda,
    Function,
    Obj,
    String,
    List,
    Macro,
    Goto,
    DynamicExtent,
    Nil
}

impl From<&Datum> for Type {
    fn from(value: &Datum) -> Self {
        match value {
            Datum::Symbol(_) => Type::Symbol,
            Datum::Atom(_) => Type::Atom,
            Datum::Cons(_) => Type::Cons,
            Datum::Func(_) => Type::Func,
            Datum::Lambda(_) => Type::Lambda,
            Datum::Obj(_) => Type::Obj,
            Datum::Macro(_) => Type::Macro,
            Datum::Goto(_) => Type::Goto,
            Datum::DynamicExtent(_) => Type::DynamicExtent,
            Datum::Nil => Type::Nil,
        }
    }
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
            Err(type_err(Type::Atom, self))
        }
    }

    fn as_list(&self) -> Result<List, TypeError> {
        match self {
            Datum::Cons(c) => Ok(List(Some(Rc::clone(c)))),
            Datum::Nil => Ok(List(None)),
            _ => Err(type_err(Type::Cons, self)),
        }
    }

    fn as_sym(&self) -> Result<Symbol, TypeError> {
        if let Self::Symbol(s) = self {
            Ok(*s)
        } else {
            Err(type_err(Type::Symbol, self))
        }
    }

    /// See [`List::stream_map_symbols`] - Note that this won't modify the cons if this belongs to
    /// one
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
            Datum::Symbol(s) => Ok(env.get(*s)?),
            Datum::Atom(i) => Ok(Datum::Atom(*i)),
            Datum::Cons(c) => c.eval(env),
            Datum::Func(f) => Ok(Datum::Func(*f)),
            Datum::Lambda(l) => Ok(Datum::Lambda(l.clone())),
            Datum::Obj(o) => Ok(Datum::Obj(o.clone())),
            Datum::Nil => Ok(Datum::Nil),
            Datum::Macro(m) => Ok(Datum::Macro(m.clone())),
            Datum::DynamicExtent(e) => Ok(Datum::DynamicExtent(e.clone())),
            Datum::Goto(label) => Ok(Datum::Goto(*label)),
        }
    }

    // fn resolve(self, env: Rc<Env>) -> LispResult {
    //     match self {
    //         Datum::Symbol(s) => Ok(env.get(s)?),
    //         Datum::Cons(c) => c.resolve(env),
    //         other => Ok(other)
    //     }
    // }

    fn try_as<T: 'static>(&self) -> Result<Rc<T>, TypeError> {
        if let Datum::Obj(o) = self {
            if let Ok(ret) = Rc::clone(o).downcast() {
                return Ok(ret)
            }
        }
        Err(type_err(Type::String, self))
    }
}

impl<const LEN: usize> From<[Datum; LEN]> for Datum {
    fn from(value: [Datum; LEN]) -> Self {
        value.into_iter().collect()
    }
}

struct Lambda {
    params: ArgList,
    body: List,
}

impl Lambda {
    fn eval(&self, l: List, env: Rc<Env>) -> LispResult {
        let l = l.into_iter().map(|x| x.eval(env.clone())).collect::<Result<List, _>>()?;
        let env = self.params.enter_env(l, env)?;
        intrinsics::progn(self.body.clone(), env)
    }
}

struct Macro {
    /// these symbols should be unique
    args: ArgList,
    body: Datum,
}

impl Macro {
    fn expand(&self, l: List, env: Rc<Env>) -> LispResult {
        // dbg!(&l);
        let env = self.args.enter_env(l, env)?;
        // Ok(Datum::from_iter([Datum::DynamicExtent(env), self.body.clone()]))
        self.body.eval(env).inspect_err(|e| eprintln!("expand error {e}"))
    }
}

struct Cons {
    car: Cell<Datum>,
    cdr: Cell<Option<Rc<Cons>>>,
}

impl Cons {
    fn new_pair(car: Datum, cdr: List) -> Self {
        Cons {
            car: car.into(),
            cdr: cdr.0.into(),
        }
    }

    fn as_pair(&self) -> Result<(Datum, Datum), NotAPair> {
        let left = self.get_car();
        let right = self.get_cdr().ok_or(NotAPair)?;
        if right.get_cdr().is_some() {
            return Err(NotAPair)
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

    fn set_cdr(&self, List(cdr): List) {
        self.cdr.set(cdr)
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
    fn iter(self: Rc<Self>) -> DatumIter {
        DatumIter { cons: Some(self) }
    }

    // fn resolve(self: Rc<Self>, env: Rc<Env>) {
    // }


    fn eval(&self, env: Rc<Env>) -> LispResult {
        // eprintln!("car before eval {}", self.get_car());
        // let op = match self.get_car() {
        //     fail @ Datum::Atom(_) | fail @ Datum::Obj(_) | fail @ Datum::Nil => return Err(WrongTypeToApply(fail).into()),
        //     Datum::Cons(c) => c.eval(env.clone())?,
        //     Datum::Symbol(s) => env.get(s)?,
        //     op => op,
        // };
        let op = self.get_car().eval(env.clone())?;

        match op {
            Datum::Atom(_) | Datum::Cons(_) | Datum::Obj(_) | Datum::Nil | Datum::Symbol(_) => {
                Err(WrongTypeToApply(op).into())
            },
            Datum::Macro(m) => m.expand(List(self.get_cdr()), env.clone())?.eval(env),
            Datum::Func(f) => f(List(self.get_cdr()), env),
            Datum::Lambda(l) => l.eval(List(self.get_cdr()), env),
            Datum::DynamicExtent(e) => {
                let (_, inner) = self.as_pair()?;
                inner.eval(env.extend_with(&e)).inspect_err(|e| eprintln!("dynamic extent error {e}"))
            },
            Datum::Goto(l) => Ok(Datum::Goto(l)),
        }
    }
}

struct DatumIter {
    cons: Option<Rc<Cons>>,
}

impl DatumIter {
    fn rem(self) -> List {
        List(self.cons)
    }
}

impl Iterator for DatumIter {
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
            Datum::Goto(label) => write!(f, "[goto {label}]"),
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
        v.try_into().map_err(|_| MismatchedParamNum(0, 0))
    }

    fn as_args_and_rem<const N: usize>(self) -> Result<([Datum; N], Self), MismatchedParamNum> {
        // TODO: avoid alloc
        let mut v = Vec::with_capacity(N);
        let mut it = self.into_iter();
        for _ in 0..N {
            v.push(it.next().ok_or(MismatchedParamNum(v.capacity() as u8, v.len() as u8))?)
        }
        let arr = v.try_into().expect("v has correct size");
        Ok((arr, it.rem()))
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
            curr.set_car(car.stream_map_symbols(&mut f));
            next = curr.get_cdr();
        }
    }
}

impl IntoIterator for List {
    type Item = Datum;

    type IntoIter = DatumIter;

    fn into_iter(self) -> Self::IntoIter {
        DatumIter { cons: self.0 }
    }
}

impl FromIterator<Datum> for List {
    fn from_iter<T: IntoIterator<Item = Datum>>(iter: T) -> Self {
        List(Cons::from_datums(iter))
    }
}

struct ArgList {
    args: Box<[Symbol]>,
    last_is_rest: bool,
}

impl ArgList {
    fn new(l: List) -> Result<Self, Error> {
        let rest = static_sym!("&rest");
        let body = static_sym!("&body");
        let mut args = Vec::new();
        let mut last_is_rest = false;
        let mut complete = false;
        for s in l {
            if complete {
                return Err(Error::MalformedArgList);
            }
            let sym = s.as_sym()?;
            if sym == rest || sym == body {
                if last_is_rest {
                    return Err(Error::MalformedArgList);
                }
                last_is_rest = true;
                continue;
            }
            args.push(sym);
            if last_is_rest {
                complete = true;
            }
        }
        if last_is_rest && !complete {
            return Err(Error::MalformedArgList);
        }
        Ok(ArgList {
            args: args.into(),
            last_is_rest,
        })
    }

    fn enter_env(&self, args: List, env: Rc<Env>) -> Result<Rc<Env>, Error> {
        let env = env.extend();
        if self.last_is_rest {
            let mut it = args.into_iter();
            if self.args.len() < 1 {
                unreachable!("creation of ArgList should not let this happen")
            }
            for &sym in &self.args[..(self.args.len() - 1)] {
                env.declare(sym, it.next().ok_or(MismatchedParamNum(0, 0))?);
            }
            env.declare(*self.args.last().expect("nonempty"), it.rem().to_datum());
        } else {
            let mut it = args.into_iter();
            for &sym in &*self.args {
                env.declare(sym, it.next().ok_or(MismatchedParamNum(0,0))?);
            }
            if it.next().is_some() {
                return Err(MismatchedParamNum(0,0).into())
            }
        }
        Ok(env)
    }
}

fn read(buf: &str) -> Datum {
    fn trim_comments(mut s: &str) -> &str {
        loop {
            s = s.trim_start();
            if s.starts_with(';') {
                let idx = s.find('\n').unwrap_or(s.len()); 
                s = &s[idx..]
            } else {
                return s;
            }
        }
    }
    fn read_list(mut s: &str) -> (Datum, &str) {
        s = trim_comments(s);
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
        (Datum::Cons(Cons::new_pair(car, List(cdr)).into()), rem)
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
        let s = trim_comments(s);
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
            '#' => {
                if it.next() == Some('\'') {
                    let (expr, rem) = read_s(&s[2..]);
                    let sym = static_sym!("function");
                    ([Datum::Symbol(sym), expr].into(), rem)
                } else {
                    panic!("idk yet")
                }
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
    let mut buf = String::from("(list ");
    f.read_to_string(&mut buf)?;
    buf.push_str(" )");
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
