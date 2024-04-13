use crate::{Error, Symbol, UnquoteOutOfQuasiquote};
use std::rc::Rc;

use crate::{
    env::{static_sym, Env},
    Datum, Lambda, LispResult, List, SymbolNotFound, SyntaxError, TypeError,
};

pub fn declare_intrinsics(env: &Env) {
    env.declare("+".into(), Datum::Func(add));
    env.declare("-".into(), Datum::Func(sub));
    env.declare("*".into(), Datum::Func(mul));
    env.declare("lambda".into(), Datum::Func(lambda));
    env.declare("defun".into(), Datum::Func(defun));
    env.declare("defvar".into(), Datum::Func(defvar));
    env.declare("setq".into(), Datum::Func(setq));
    env.declare("display".into(), Datum::Func(display));
    env.declare("if".into(), Datum::Func(if_expr));
    env.declare("quote".into(), Datum::Func(quote));
    env.declare("unquote".into(), Datum::Func(unquote));
    env.declare("quasiquote".into(), Datum::Func(quasiquote));
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

fn lambda(l: List, _env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let args = it.next().ok_or(SyntaxError)?.as_list()?;
    let args = args
        .into_iter()
        .map(|a| a.as_sym())
        .collect::<Result<Box<[_]>, _>>()?;
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
                    return quasiquote_list(l, env.clone())
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

fn setq(l: List, env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let sym = it.next().ok_or(SyntaxError)?.as_sym()?;
    let val = it.next().ok_or(SyntaxError)?.eval(env.clone())?;
    env.set(sym, val.clone()).ok().ok_or(SymbolNotFound)?;
    Ok(val)
}

fn display(l: List, env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let val = it.next().ok_or(SyntaxError)?.eval(env)?;
    if it.next().is_some() {
        return Err(SyntaxError.into());
    }
    let Datum::Obj(val) = val else {
        return Err(TypeError.into());
    };
    let s: &String = val.downcast_ref().ok_or(TypeError)?;
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
