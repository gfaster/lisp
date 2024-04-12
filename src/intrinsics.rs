use std::rc::Rc;

use crate::{env::Env, Datum, Lambda, LispResult, List, SyntaxError};

pub fn declare_intrinsics(env: &Env) {
    env.declare("+".into(), Datum::Func(add));
    env.declare("-".into(), Datum::Func(sub));
    env.declare("*".into(), Datum::Func(mul));
    env.declare("lambda".into(), Datum::Func(lambda));
    env.declare("defun".into(), Datum::Func(defun));
    env.declare("defvar".into(), Datum::Func(defvar));
}

fn add(l: List, env: Rc<Env>) -> LispResult {
    l.into_iter().map(|x| x.eval(env.clone()).and_then(|x| x.as_atom().map_err(Into::into))).try_fold(0, |acc, val| val.map(|v| v + acc)).map(|val| crate::Datum::Atom(val))
}

fn sub(l: List, env: Rc<Env>) -> LispResult {
    l.into_iter().map(|x| x.eval(env.clone()).and_then(|x| x.as_atom().map_err(Into::into))).try_fold(0, |acc, val| val.map(|v| acc - v)).map(|val| crate::Datum::Atom(val))
}

fn mul(l: List, env: Rc<Env>) -> LispResult {
    l.into_iter().map(|x| x.eval(env.clone()).and_then(|x| x.as_atom().map_err(Into::into))).try_fold(0, |acc, val| val.map(|v| acc * v)).map(|val| crate::Datum::Atom(val))
}

fn lambda(l: List, _env: Rc<Env>) -> LispResult {
    let mut it = l.into_iter();
    let args = it.next().ok_or(SyntaxError)?.as_list()?;
    let args = args.into_iter().map(|a| a.as_sym()).collect::<Result<Rc<[_]>, _>>()?;
    let body = it.rem();
    let l = Lambda {
        params: args,
        body,
    };
    let dat = Datum::Lambda(l);
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
