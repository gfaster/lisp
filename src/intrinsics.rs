use crate::{type_err, ArgList, Cons, Error, Macro, Symbol, Type, UnquoteOutOfQuasiquote};
use std::{collections::HashMap, rc::Rc};

use crate::{
    env::{static_sym, Env},
    Datum, Lambda, LispResult, List, SymbolNotFound, SyntaxError
};

pub fn declare_intrinsics(env: &Env) {
    env.declare("car".into(), Datum::Func(car));
    env.declare("cdr".into(), Datum::Func(cdr));
    env.declare("setcar".into(), Datum::Func(setcar));
    env.declare("setcdr".into(), Datum::Func(setcdr));
    env.declare("cons".into(), Datum::Func(cons));
    env.declare("apply".into(), Datum::Func(apply));
    env.declare("+".into(), Datum::Func(add));
    env.declare("-".into(), Datum::Func(sub));
    env.declare("*".into(), Datum::Func(mul));
    env.declare("list".into(), Datum::Func(list));
    env.declare("lambda".into(), Datum::Func(lambda));
    env.declare("defun".into(), Datum::Func(defun));
    env.declare("defvar".into(), Datum::Func(defvar));
    env.declare("set".into(), Datum::Func(set));
    env.declare("display".into(), Datum::Func(display));
    env.declare("if".into(), Datum::Func(if_expr));
    env.declare("function".into(), Datum::Func(function));
    env.declare("quote".into(), Datum::Func(quote));
    env.declare("unquote".into(), Datum::Func(unquote));
    env.declare("quasiquote".into(), Datum::Func(quasiquote));
    env.declare("progn".into(), Datum::Func(progn));
    env.declare("expandmacro".into(), Datum::Func(expandmacro));
    env.declare("defmacro".into(), Datum::Func(defmacro));
    env.declare("tagbody".into(), Datum::Func(tagbody));
    env.declare("go".into(), Datum::Func(go));
    env.declare("gensym".into(), Datum::Func(gensym));
}

fn car(l: List, env: Rc<Env>) -> LispResult {
    let [list] = l.as_args()?;
    let list = list.eval(env)?;
    Ok(list.as_list()?.0.ok_or(type_err(Type::List, &list))?.get_car())
}

fn setcar(l: List, env: Rc<Env>) -> LispResult {
    let [target, val] = l.as_args()?;
    let Datum::Cons(c) = target.eval(env.clone())? else { return Err(type_err(Type::Cons, &target).into())};
    let val = val.eval(env)?;
    c.set_car(val.clone());
    Ok(val)
}

fn setcdr(l: List, env: Rc<Env>) -> LispResult {
    let [target, val] = l.as_args()?;
    let Datum::Cons(c) = target.eval(env.clone())? else { return Err(type_err(Type::Cons, &target).into())};
    let val = val.eval(env)?;
    let list = val.as_list()?;
    c.set_cdr(list.clone());
    Ok(val)
}

fn cdr(l: List, env: Rc<Env>) -> LispResult {
    let [list] = l.as_args()?;
    let list = list.eval(env)?;
    Ok(List(list.as_list()?.0.ok_or(type_err(Type::List, &list))?.get_cdr()).to_datum())
}

fn cons(l: List, env: Rc<Env>) -> LispResult {
    let [car, cdr] = l.as_args()?;
    let car = car.eval(env.clone())?;
    let cdr = cdr.eval(env)?.as_list()?;
    Ok(Datum::Cons(Cons::new_pair(car, cdr).into()))
}

fn apply(l: List, env: Rc<Env>) -> LispResult {
    let func = cons(l, env.clone())?.eval(env)?;
    Ok(func)
}


fn add(l: List, env: Rc<Env>) -> LispResult {
    l.into_iter()
        .map(|x| {
            x.eval(env.clone())
                .and_then(|x| x.as_atom().map_err(Into::into))
        })
        .try_fold(0, |acc, val| val.map(|v| v + acc))
        .map(|val| crate::Datum::Atom(val))
}

fn sub(l: List, env: Rc<Env>) -> LispResult {
    l.into_iter()
        .map(|x| {
            x.eval(env.clone())
                .and_then(|x| x.as_atom().map_err(Into::into))
        })
        .try_fold(0, |acc, val| val.map(|v| acc - v))
        .map(|val| crate::Datum::Atom(val))
}

fn mul(l: List, env: Rc<Env>) -> LispResult {
    l.into_iter()
        .map(|x| {
            x.eval(env.clone())
                .and_then(|x| x.as_atom().map_err(Into::into))
        })
        .try_fold(0, |acc, val| val.map(|v| acc * v))
        .map(|val| crate::Datum::Atom(val))
}

fn list(l: List, env: Rc<Env>) -> LispResult {
    l.into_iter().map(|x| x.eval(env.clone())).collect()
}

fn lambda(l: List, _env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let args = it.next().ok_or(SyntaxError)?.as_list()?;
    let args = ArgList::new(args)?;
    let body = it.rem();
    let l = Lambda { params: args, body };
    let dat = Datum::Lambda(Rc::new(l));
    Ok(dat)
}

fn defun(l: List, env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let sym = it.next().ok_or(SyntaxError)?.as_sym()?;
    let dat = lambda(it.rem(), env.clone())?;
    env.declare(sym, dat.clone());
    Ok(dat)
}

fn defvar(l: List, env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let sym = it.next().ok_or(SyntaxError)?.as_sym()?;
    let val = it.next().ok_or(SyntaxError)?;
    if it.next().is_some() {
        Err(SyntaxError)?
    }
    let dat = val.eval(Rc::clone(&env))?;
    env.declare(sym, dat.clone());
    Ok(dat)
}

pub fn quote(l: List, _env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let val = it.next().ok_or(SyntaxError)?;
    if it.next().is_some() {
        return Err(SyntaxError.into());
    }
    Ok(val)
}

pub fn unquote(_l: List, _env: Rc<Env>) -> LispResult {
    Err(UnquoteOutOfQuasiquote.into())
}

pub fn quasiquote(l: List, env: Rc<Env>) -> LispResult {
    fn unquote_valid(l: List, env: Rc<Env>) -> LispResult {
        let mut it = l.into_iter();
        let Some(body) = it.next() else {
            return Err(SyntaxError.into());
        };
        if it.next().is_some() {
            return Err(SyntaxError.into());
        }
        body.eval(env)
    }

    fn unquote_splice_valid(l: List, env: Rc<Env>) -> Result<List, Error> {
        let mut it = l.into_iter();
        let Some(body) = it.next() else {
            return Err(SyntaxError.into());
        };
        if it.next().is_some() {
            return Err(SyntaxError.into());
        }

        return Ok(body.eval(env)?.as_list()?)
    }

    fn quasiquote_list(l: List, env: Rc<Env>) -> Result<List, Error> {
        let unquote = static_sym!("unquote");
        let unquotesplice = static_sym!("unquotesplice");

        l.into_iter()
            .map(|x| {
                if let Ok(l) = x.as_list() {
                    let mut it = l.clone().into_iter();
                    match it.next() {
                        Some(Datum::Symbol(sym)) if sym == unquote => return Ok(List::from_iter([unquote_valid(it.rem(), env.clone())?])),
                        Some(Datum::Symbol(sym)) if sym == unquotesplice => return unquote_splice_valid(it.rem(), env.clone()),
                        _ => (),
                    }
                    return Ok(List::from_iter([quasiquote_list(l, env.clone())?.to_datum()]))
                }
                return Ok(List::from_iter([quasiquote_datum(x, env.clone())?]));
            })
            .collect::<Result<Vec<List>, Error>>()?
            .into_iter()
            .flatten()
            .map(|x| Ok(x))
            .collect()
    }

    fn quasiquote_datum(val: Datum, env: Rc<Env>) -> LispResult {
        let unquote = static_sym!("unquote");
        let Ok(l) = val.as_list() else { return Ok(val) };
        let mut it = l.clone().into_iter();
        match it.next() {
            Some(Datum::Symbol(sym)) if sym == unquote => return unquote_valid(it.rem(), env),
            Some(Datum::Cons(_)) => return Ok(quasiquote_list(l, env)?.to_datum()),
            None => return Ok(Datum::Nil),
            Some(_) => (),
        };
        quasiquote_list(l, env).map(|x| x.to_datum())
    }

    let mut it = l.into_iter();
    let val = it.next().ok_or(SyntaxError)?;
    if it.next().is_some() {
        return Err(SyntaxError.into());
    }
    quasiquote_datum(val, env)
}

fn set(l: List, env: Rc<Env>) -> LispResult {
    let [sym, val] = l.as_args()?;
    let sym = sym.eval(env.clone())?.as_sym()?;
    let val = val.eval(env.clone())?;
    env.set(sym, val.clone()).ok().ok_or(SymbolNotFound(sym))?;
    Ok(val)
}

fn display(l: List, env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let val = it.next().ok_or(SyntaxError)?.eval(env)?;
    if it.next().is_some() {
        return Err(SyntaxError.into());
    }
    let s: Rc<String> = val.try_as()?;
    println!("{s}");
    Ok(Datum::Nil)
}

fn if_expr(l: List, env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let cond = it.next().ok_or(SyntaxError)?.eval(Rc::clone(&env))?;
    let if_block = it.next().ok_or(SyntaxError)?;
    let else_block = it.next();
    if it.next().is_some() {
        return Err(SyntaxError.into());
    }
    if !matches!(cond, Datum::Nil) {
        return if_block.eval(env);
    }
    if let Some(else_block) = else_block {
        return else_block.eval(env);
    }
    Ok(Datum::Nil)
}

pub fn progn(l: List, env: Rc<Env>) -> LispResult {
    l.into_iter()
        .try_fold(Datum::Nil, |_acc, x| x.eval(Rc::clone(&env)))
}

fn expandmacro(l: List, env: Rc<Env>) -> LispResult {
    let [sym, args] = l.as_args()?;
    let m = sym.eval(env.clone())?;
    let Datum::Macro(m) = m else { return Err(type_err(Type::Macro, &m).into()) };
    let args = args.eval(env.clone()).inspect_err(|e| eprintln!("{e}"))?;
    m.expand(args.as_list()?, env)
}

fn defmacro(l: List, env: Rc<Env>) -> LispResult {
    let ([sym, args], body) = l.as_args_and_rem()?;
    let sym = sym.as_sym()?;
    let mut args = ArgList::new(args.as_list()?)?;
    let arg_s = &*args.args;
    let mut transformed_args = Vec::with_capacity(arg_s.len());
    for &arg in arg_s.iter() {
        transformed_args.push(Symbol::gensym(arg))
    }
    body.clone().stream_map_symbols(|sym| arg_s.iter().position(|&x| x == sym).map_or(sym, |i| transformed_args[i]));
    args.args = transformed_args.into();
    let body = Datum::Cons(Cons::new_pair(Datum::Func(progn), body).into());
    let m = Macro {
        args,
        body,
    };
    let ret = Datum::Macro(Rc::new(m));
    env.declare(sym, ret.clone());
    Ok(ret)
}

fn tagbody(l: List, env: Rc<Env>) -> LispResult {
    let Some(head) = l.clone().0 else {
        return Ok(Datum::Nil)
    };
    let mut labels = HashMap::new();
    let mut next = head.get_cdr();
    if let Ok(sym) = head.get_car().as_sym() {
        labels.insert(sym, head.clone());
    }
    while let Some(cons) = next {
        let car = cons.get_car();
        if let Ok(label) = car.as_sym() {
            labels.insert(label, cons.clone());
        } else {
            car.as_list()?;
        }
        next = cons.get_cdr()
    }
    let mut ip = head;
    loop {
        let res = ip.get_car().eval(env.clone())?;
        if let Datum::Goto(label) = res {
            ip = labels.get(&label).cloned().ok_or(SymbolNotFound(label))?;
            continue
        } 
        if let Some(next) = ip.get_cdr() {
            ip = next;
            continue
        }
        return Ok(res)
    }
}

fn go(l: List, _env: Rc<Env>) -> LispResult {
    let [sym] = l.as_args()?;
    let sym = sym.as_sym()?;
    Ok(Datum::Goto(sym))
}

fn gensym(l: List, _env: Rc<Env>) -> LispResult {
    if let Ok([name]) = l.as_args() {
        let name: Rc<String> = name.try_as()?;
        Ok(Datum::Symbol(Symbol::gensym(name)))
    } else {
        Ok(Datum::Symbol(Symbol::gensym("")))
    }
}

/// this is kinda redundant for now since I don't have separate namespaces for functions and
/// variables
fn function(l: List, env: Rc<Env>) -> LispResult {
    let [fun] = l.as_args()?;
    let fun = fun.eval(env.clone())?;
    if !matches!(fun, Datum::Func(_) | Datum::Lambda(_)) {
        Err(type_err(Type::Function, &fun))?
    }
    Ok(fun)
}
