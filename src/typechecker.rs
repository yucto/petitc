use beans::location::Span;

use std::collections::HashMap;

use super::{
    ast::*,
    error::{Error, Result},
    parsing::{SpanAnnotation, WithSpan},
};

pub struct TypeAnnotation;
pub struct TypedInstr<T> {
    pub instr: T,
    pub span: Span,
    pub loop_level: usize,
    pub expected_return_type: Type,
}
pub struct WithType<T> {
    pub inner: T,
    pub ty: Type,
    pub span: Span,
}

impl<T> WithType<T> {
    pub fn new(inner: T, ty: Type, span: Span) -> Self {
        Self { inner, ty, span }
    }
}

impl Annotation for TypeAnnotation {
    type Ident = WithSpan<Ident>;
    type Type = WithSpan<Type>;
    type WrapExpr<T> = WithType<T>;
    type WrapInstr<T> = TypedInstr<T>;
    type WrapFunDecl<T> = WithSpan<T>;
    type WrapVarDecl<T> = WithSpan<T>;
    type WrapElseBranch<T> = Option<TypedInstr<T>>;
}

pub type TypedExpr =
    <TypeAnnotation as Annotation>::WrapExpr<Expr<TypeAnnotation>>;

enum Binding {
    Var(Type),
    Fun((Type, Vec<Type>)),
}

fn get_fun(
    env: &HashMap<Ident, Binding>,
    name: Ident,
) -> Result<&(Type, Vec<Type>)> {
    if let Some(Binding::Fun(res)) = env.get(&name) {
        Ok(res)
    } else {
        // TODO : no function named name in current scope
        Err(Error::NameError)
    }
}

fn get_var(env: &HashMap<Ident, Binding>, name: Ident) -> Result<Type> {
    if let Some(Binding::Var(res)) = env.get(&name) {
        Ok(*res)
    } else {
        // TODO : no variable named name in current scope
        Err(Error::NameError)
    }
}

fn type_expr(
    e: WithSpan<Expr<SpanAnnotation>>,
    env: &HashMap<Ident, Binding>,
) -> Result<TypedExpr> {
    match e.inner {
        Expr::True => Ok(WithType::new(Expr::True, Type::BOOL, e.span)),
        Expr::False => Ok(WithType::new(Expr::False, Type::BOOL, e.span)),
        Expr::Null => Ok(WithType::new(Expr::Null, Type::VOID_PTR, e.span)),
        Expr::Int(n) => Ok(WithType::new(Expr::Int(n), Type::INT, e.span)),
        Expr::Ident(name) => Ok(WithType::new(
            Expr::Ident(name),
            get_var(env, name)?,
            e.span,
        )),
        Expr::SizeOf(ty) => {
            if !ty.inner.is_eq(&Type::VOID) {
                Ok(WithType::new(Expr::SizeOf(ty.clone()), Type::INT, ty.span))
            } else {
                Err(Error::TypeError("void type has no size".into()))
            }
        }
        Expr::Addr(inner_e) => {
            if inner_e.inner.is_lvalue() {
                type_expr(*inner_e, env).map(|inner_e| {
                    let ty = inner_e.ty.ptr();
                    WithType::new(Expr::Addr(Box::new(inner_e)), ty, e.span)
                })
            } else {
                Err(Error::TypeError(
                    "Can't take the address of a rvalue".into(),
                ))
            }
        }
        Expr::Deref(inner_e) => type_expr(*inner_e, env).and_then(|inner_e| {
            let mut ty = inner_e.ty;
            if ty.is_ptr() {
                ty.indirection_count -= 1;
                Ok(WithType::new(Expr::Deref(Box::new(inner_e)), ty, e.span))
            } else {
                Err(Error::TypeError(format!(
                    "Can't deref non-ptr type {}",
                    ty
                )))
            }
        }),
        Expr::Assign { lhs, rhs } => {
            if !lhs.inner.is_lvalue() {
                return Err(Error::TypeError("Can't assign to rvalue".into()));
            }
            let lhs = type_expr(*lhs, env)?;
            let rhs = type_expr(*rhs, env)?;
            let ty1 = lhs.ty;
            let ty2 = rhs.ty;

            if !ty1.is_eq(&ty2) {
                return Err(Error::TypeError(format!(
                    "Type mismatch on assignation : {} != {}",
                    ty1, ty2
                )));
            }

            Ok(WithType::new(
                Expr::Assign {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ty1,
                e.span,
            ))
        }
        Expr::PrefixIncr(inner_e) => {
            if inner_e.inner.is_lvalue() {
                let inner_e = type_expr(*inner_e, env)?;
                let ty = inner_e.ty;
                Ok(WithType::new(
                    Expr::PrefixIncr(Box::new(inner_e)),
                    ty,
                    e.span,
                ))
            } else {
                Err(Error::TypeError(
                    "Can't use a prefix operation on a rvalue".into(),
                ))
            }
        }
        // The code here should be the same of the one at the previous branch
        Expr::PrefixDecr(inner_e) => {
            if inner_e.inner.is_lvalue() {
                let inner_e = type_expr(*inner_e, env)?;
                let ty = inner_e.ty;
                Ok(WithType::new(
                    Expr::PrefixDecr(Box::new(inner_e)),
                    ty,
                    e.span,
                ))
            } else {
                Err(Error::TypeError(
                    "Can't use a prefix operation on a rvalue".into(),
                ))
            }
        }
        // The code here should be the same of the one at the previous branch
        Expr::PostfixIncr(inner_e) => {
            if inner_e.inner.is_lvalue() {
                let inner_e = type_expr(*inner_e, env)?;
                let ty = inner_e.ty;
                Ok(WithType::new(
                    Expr::PostfixIncr(Box::new(inner_e)),
                    ty,
                    e.span,
                ))
            } else {
                Err(Error::TypeError(
                    "Can't use a postfix operation on a rvalue".into(),
                ))
            }
        }
        // The code here should be the same of the one at the previous branch
        Expr::PostfixDecr(inner_e) => {
            if inner_e.inner.is_lvalue() {
                let inner_e = type_expr(*inner_e, env)?;
                let ty = inner_e.ty;
                Ok(WithType::new(
                    Expr::PostfixDecr(Box::new(inner_e)),
                    ty,
                    e.span,
                ))
            } else {
                Err(Error::TypeError(
                    "Can't use a postfix operation on a rvalue".into(),
                ))
            }
        }
        Expr::Pos(inner_e) => {
            let inner_e = type_expr(*inner_e, env)?;
            let ty = inner_e.ty;

            if !ty.is_eq(&Type::INT) {
                return Err(Error::TypeError(format!(
                    "Can't use unary operation + on a non-int of type {}",
                    ty
                )));
            }
            Ok(WithType::new(
                Expr::Pos(Box::new(inner_e)),
                Type::INT,
                e.span,
            ))
        }
        // The code here should be the same of the one at the previous branch
        Expr::Neg(inner_e) => {
            let inner_e = type_expr(*inner_e, env)?;
            let ty = inner_e.ty;

            if !ty.is_eq(&Type::INT) {
                return Err(Error::TypeError(format!(
                    "Can't use unary operation - on a non-int of type {}",
                    ty
                )));
            }
            Ok(WithType::new(
                Expr::Neg(Box::new(inner_e)),
                Type::INT,
                e.span,
            ))
        }
        Expr::Not(inner_e) => {
            let inner_e = type_expr(*inner_e, env)?;
            if inner_e.ty.is_eq(&Type::VOID) {
                return Err(Error::TypeError(
                    "Can't use unary operation ! on void".into(),
                ));
            }
            Ok(WithType::new(
                Expr::Not(Box::new(inner_e)),
                Type::INT,
                e.span,
            ))
        }
        Expr::Op {
            op:
                op @ (BinOp::Eq
                | BinOp::NEq
                | BinOp::Lt
                | BinOp::Le
                | BinOp::Gt
                | BinOp::Ge),
            lhs,
            rhs,
        } => {
            let lhs = type_expr(*lhs, env)?;
            let rhs = type_expr(*rhs, env)?;
            let ty1 = lhs.ty;
            let ty2 = rhs.ty;
            if !ty1.is_eq(&ty2) {
                return Err(Error::TypeError(format!(
                    "Type mismatch on comparison : {} != {}",
                    ty1, ty2
                )));
            }
            Ok(WithType::new(
                Expr::Op {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Type::INT,
                e.span,
            ))
        }
        Expr::Op {
            op:
                op @ (BinOp::Mul
                | BinOp::Div
                | BinOp::Mod
                | BinOp::BOr
                | BinOp::BAnd),
            lhs,
            rhs,
        } => {
            let lhs = type_expr(*lhs, env)?;
            let rhs = type_expr(*rhs, env)?;
            let ty1 = lhs.ty;
            let ty2 = rhs.ty;
            if !ty1.is_eq(&Type::INT) || !ty1.is_eq(&ty2) {
                return Err(Error::TypeError(format!(
                    "Can't use arithmetic operation on non-int of type {} and {}",
                    ty1, ty2
                )));
            }
            Ok(WithType::new(
                Expr::Op {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Type::INT,
                e.span,
            ))
        }
        Expr::Op {
            op: BinOp::Add,
            lhs,
            rhs,
        } => {
            let lhs = type_expr(*lhs, env)?;
            let rhs = type_expr(*rhs, env)?;
            let mut ty1 = lhs.ty;
            let mut ty2 = rhs.ty;
            let new_e = Expr::Op {
                op: BinOp::Add,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
            if ty1.is_ptr() && ty2.is_ptr() {
                return Err(Error::TypeError("Can't add ptr together".into()));
            }

            if ty2.is_ptr() {
                std::mem::swap(&mut ty1, &mut ty2);
            }

            if ty1.is_ptr() {
                if !ty2.is_eq(&Type::INT) {
                    return Err(Error::TypeError(
                        "Can't add together a pointer and a non-int".into(),
                    ));
                }

                Ok(WithType::new(new_e, ty1, e.span))
            } else {
                if !ty1.is_eq(&ty2) || !ty1.is_eq(&Type::INT) {
                    return Err(Error::TypeError(
                        "Can't use addition on non-integers or non-pointers"
                            .into(),
                    ));
                }

                Ok(WithType::new(new_e, Type::INT, e.span))
            }
        }
        Expr::Op {
            op: BinOp::Sub,
            lhs,
            rhs,
        } => {
            let lhs = type_expr(*lhs, env)?;
            let rhs = type_expr(*rhs, env)?;
            let ty1 = lhs.ty;
            let ty2 = rhs.ty;
            let new_e = Expr::Op {
                op: BinOp::Sub,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };

            if ty1.is_ptr() {
                if ty2.is_ptr() {
                    if ty1 != ty2 {
                        return Err(Error::TypeError(format!(
                            "Can't subtract pointers of different types : {} != {}",
                            ty1, ty2
                        )));
                    }

                    return Ok(WithType::new(new_e, Type::INT, e.span));
                }

                if !ty2.is_eq(&Type::INT) {
                    return Err(Error::TypeError(
                        "Can't subtract a non-int to a pointer".into(),
                    ));
                }

                Ok(WithType::new(new_e, ty1, e.span))
            } else {
                if !ty1.is_eq(&Type::INT) || !ty1.is_eq(&ty2) {
                    return Err(Error::TypeError(format!(
                        "Can't subtract {} to {}",
                        ty1, ty2
                    )));
                }

                Ok(WithType::new(new_e, Type::INT, e.span))
            }
        }
        Expr::Call { name, args } => {
            let (ret_ty, args_ty) = get_fun(env, name.inner)?.clone();
            if args.len() != args_ty.len() {
                return Err(Error::TypeError(format!(
                    "Arguments' count mismatch : expected {}, got {}",
                    args_ty.len(),
                    args.len()
                )));
            }

            let mut typed_args = Vec::new();

            for (arg, ty) in args.into_iter().zip(args_ty.iter()) {
                let arg = type_expr(arg, env)?;
                let arg_ty = arg.ty;

                if !arg_ty.is_eq(ty) {
                    return Err(Error::TypeError(format!(
                        "Argument type mismatch, expected {}, got {}",
                        arg_ty, ty
                    )));
                }

                typed_args.push(arg);
            }

            Ok(WithType::new(
                Expr::Call {
                    name,
                    args: typed_args,
                },
                ret_ty,
                e.span,
            ))
        }
    }
}

fn typecheck_instr(
    instr: WithSpan<Instr<SpanAnnotation>>,
    loop_level: usize,
    expected_return_type: Type,
    env: &mut HashMap<Ident, Binding>,
) -> Result<TypedInstr<Instr<TypeAnnotation>>> {
    match instr.inner {
        Instr::EmptyInstr => Ok(TypedInstr {
            instr: Instr::EmptyInstr,
            span: instr.span,
            loop_level,
            expected_return_type,
        }),
        Instr::ExprInstr(e) => Ok(TypedInstr {
            instr: Instr::ExprInstr(type_expr(e, env)?),
            span: instr.span,
            loop_level,
            expected_return_type,
        }),
        Instr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond = type_expr(cond, env)?;
            let then_branch = typecheck_instr(
                *then_branch,
                loop_level,
                expected_return_type,
                env,
            )?;
            let else_branch = if let Some(else_branch) = *else_branch {
                Some(typecheck_instr(
                    else_branch,
                    loop_level,
                    expected_return_type,
                    env,
                )?)
            } else {
                None
            };
            if cond.ty.is_eq(&Type::VOID) {
                return Err(Error::TypeError(
                    "The condition in an if can't be of type void".into(),
                ));
            }
            if let Some(ref else_branch) = else_branch {
                if then_branch.expected_return_type
                    != else_branch.expected_return_type
                {
                    return Err(Error::TypeError(format!(
                    "if and else branch have different return types : {} != {}",
                    then_branch.expected_return_type,
                    else_branch.expected_return_type
                )));
                }
            }
            if then_branch.expected_return_type != expected_return_type {
                return Err(Error::TypeError(format!(
                    "if statement don't return the right type, expected {}, got {}",
                    expected_return_type,
                    then_branch.expected_return_type
                )));
            }

            Ok(TypedInstr {
                instr: Instr::If {
                    cond,
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
                span: instr.span,
                loop_level,
                expected_return_type,
            })
        }
        Instr::While { cond, body } => {
            let cond = type_expr(cond, env)?;
            let body = typecheck_instr(
                *body,
                loop_level + 1,
                expected_return_type,
                env,
            )?;
            if cond.ty.is_eq(&Type::VOID) {
                return Err(Error::TypeError(
                    "The condition in a while can't be of type void".into(),
                ));
            }
            if body.expected_return_type != expected_return_type {
                return Err(Error::TypeError(format!(
                    "while body don't return the right type, expected {}, got {}",
                    expected_return_type,
                    body.expected_return_type,
                )));
            }
            Ok(TypedInstr {
                instr: Instr::While {
                    cond,
                    body: Box::new(body),
                },
                span: instr.span,
                loop_level,
                expected_return_type,
            })
        }
        Instr::For {
            loop_var: None,
            cond,
            incr,
            body,
        } => {
            let cond = cond.map(|cond| type_expr(cond, env)).transpose()?;
            let incr = incr
                .into_iter()
                .map(|incr| type_expr(incr, env))
                .collect::<Result<Vec<_>>>()?;
            let body = Box::new(typecheck_instr(
                *body,
                loop_level + 1,
                expected_return_type,
                env,
            )?);

            cond.as_ref()
                .map(|cond| {
                    if cond.ty.is_eq(&Type::VOID) {
                        return Err(Error::TypeError(
                            "The condition in an if can't be of type void"
                                .into(),
                        ));
                    } else {
                        Ok(())
                    }
                })
                .transpose()?;

            if body.expected_return_type != expected_return_type {
                return Err(Error::TypeError(format!(
                    "for body don't return the right type, expected {}, got {}",
                    expected_return_type, body.expected_return_type,
                )));
            }
            Ok(TypedInstr {
                instr: Instr::For {
                    loop_var: None,
                    cond,
                    incr,
                    body,
                },
                span: instr.span,
                loop_level,
                expected_return_type,
            })
        }
        Instr::For {
            loop_var: Some(decl),
            cond,
            incr,
            body,
        } => typecheck_block(
            WithSpan::new(
                vec![
                    DeclOrInstr::Var(decl),
                    DeclOrInstr::Instr(WithSpan::new(
                        Instr::For {
                            loop_var: None,
                            cond,
                            incr,
                            body,
                        },
                        instr.span.clone(),
                    )),
                ],
                instr.span,
            ),
            loop_level,
            expected_return_type,
            env,
        ),
        Instr::Block(block) => typecheck_block(
            WithSpan::new(block, instr.span),
            loop_level,
            expected_return_type,
            env,
        ),
        Instr::Return(None) => {
            if expected_return_type != Type::VOID {
                return Err(Error::TypeError(format!(
                    "empty return only valid when void is expected, not {}",
                    expected_return_type,
                )));
            }
            Ok(TypedInstr {
                instr: Instr::Return(None),
                span: instr.span,
                loop_level,
                expected_return_type,
            })
        }
        Instr::Return(Some(e)) => {
            let e = type_expr(e, env)?;
            if !e.ty.is_eq(&expected_return_type) {
                return Err(Error::TypeError(format!(
                    "return type mismatch, expected {}, got {}",
                    expected_return_type, e.ty,
                )));
            }
            Ok(TypedInstr {
                instr: Instr::Return(Some(e)),
                span: instr.span,
                loop_level,
                expected_return_type,
            })
        }
        Instr::Break | Instr::Continue => {
            if loop_level == 0 {
                return Err(Error::BreakContinueOutsideLoop);
            }

            Ok(TypedInstr {
                instr: Instr::Break,
                span: instr.span,
                loop_level,
                expected_return_type,
            })
        }
    }
}

/// On returning an instr, always returns a block
fn typecheck_block(
    block: WithSpan<Vec<DeclOrInstr<SpanAnnotation>>>,
    loop_level: usize,
    expected_return_type: Type,
    env: &mut HashMap<usize, Binding>,
) -> Result<TypedInstr<Instr<TypeAnnotation>>> {
    let mut new_bindings: Vec<(Ident, Option<Binding>)> = Vec::new();
    let mut ret = Vec::new();

    fn assert_var_is_not_reused(
        var_name: Ident,
        new_bindings: &[(usize, Option<Binding>)],
    ) -> Result<()> {
        if new_bindings
            .iter()
            .map(|(name, _)| *name)
            .find(|name| *name == var_name)
            .is_some()
        {
            // TODO : add the name in the error message
            return Err(Error::TypeError(
                "can't assign the same name twice in the same block".into(),
            ));
        } else {
            Ok(())
        }
    }

    for decl_or_instr in block.inner {
        match decl_or_instr {
            DeclOrInstr::Fun(fun_decl) => {
                assert_var_is_not_reused(
                    fun_decl.inner.name.inner,
                    &new_bindings,
                )?;
                let fun_decl = typecheck_fun(fun_decl, env)?;
                new_bindings.push((
                    fun_decl.inner.name.inner,
                    env.remove(&fun_decl.inner.name.inner),
                ));
                ret.push(DeclOrInstr::Fun(fun_decl));
            }
            DeclOrInstr::Var(var_decl) => {
                if var_decl.inner.ty.inner.is_eq(&Type::VOID) {
                    // TODO : add the name in the error message
                    return Err(Error::TypeError(
                        "can't assign variable of void type".into(),
                    ));
                }
                assert_var_is_not_reused(
                    var_decl.inner.name.inner,
                    &new_bindings,
                )?;
                new_bindings.push((
                    var_decl.inner.name.inner,
                    env.remove(&var_decl.inner.name.inner),
                ));
                env.insert(
                    var_decl.inner.name.inner,
                    Binding::Var(var_decl.inner.ty.inner),
                );
                let value = var_decl
                    .inner
                    .value
                    .map(|value| type_expr(value, env))
                    .transpose()?;

                if value
                    .as_ref()
                    .map(|val| !val.ty.is_eq(&var_decl.inner.ty.inner))
                    .unwrap_or(false)
                {
                    return Err(Error::TypeError(format!(
                        "Mismatch type on assignation, expected {}, got {}",
                        var_decl.inner.ty.inner,
                        value.unwrap().ty,
                    )));
                }

                ret.push(DeclOrInstr::Var(WithSpan::new(
                    VarDecl {
                        ty: var_decl.inner.ty,
                        name: var_decl.inner.name,
                        value,
                    },
                    var_decl.span,
                )));
            }
            DeclOrInstr::Instr(instr) => ret.push(DeclOrInstr::Instr(
                typecheck_instr(instr, loop_level, expected_return_type, env)?,
            )),
        }
    }
    for (name, old_binding) in new_bindings {
        if let Some(binding) = old_binding {
            env.insert(name, binding);
        } else {
            env.remove(&name);
        }
    }

    Ok(TypedInstr {
        instr: Instr::Block(ret),
        span: block.span,
        loop_level,
        expected_return_type,
    })
}

/// Insert the function in fun_env
/// Caller should remove it later if needed,
/// and saved previous value
fn typecheck_fun(
    decl: WithSpan<FunDecl<SpanAnnotation>>,
    env: &mut HashMap<usize, Binding>,
) -> Result<WithSpan<FunDecl<TypeAnnotation>>> {
    let code_span = decl.inner.code.span.clone();
    let code = decl
        .inner
        .params
        .iter()
        .map(|(ty, name)| {
            DeclOrInstr::Var(WithSpan::new(
                VarDecl {
                    ty: ty.clone(),
                    name: name.clone(),
                    value: None,
                },
                name.span.clone(),
            ))
        })
        .chain(decl.inner.code.inner.into_iter())
        .collect::<Vec<_>>();
    env.insert(
        decl.inner.name.inner,
        Binding::Fun((
            decl.inner.ty.inner,
            decl.inner.params.iter().map(|(ty, _)| ty.inner).collect(),
        )),
    );

    let typed_instr = typecheck_block(
        WithSpan::new(code, decl.inner.code.span),
        0,
        decl.inner.ty.inner,
        env,
    )?;

    let Instr::Block(mut code) =
        typed_instr.instr
    else { unreachable!("Internal error") };

    code = code.into_iter().skip(decl.inner.params.len()).collect();

    let typed_code = TypedInstr {
        instr: code,
        span: code_span,
        loop_level: typed_instr.loop_level,
        expected_return_type: typed_instr.expected_return_type,
    };

    Ok(WithSpan::new(
        FunDecl {
            ty: decl.inner.ty,
            name: decl.inner.name,
            params: decl.inner.params,
            code: typed_code,
            toplevel: decl.inner.toplevel,
        },
        decl.span,
    ))
}

pub fn typecheck(
    file: File<SpanAnnotation>,
    string_store: &[String],
) -> Result<File<TypeAnnotation>> {
    let main_decl = &file
        .fun_decls
        .iter()
        .find(|decl| string_store[decl.inner.name.inner] == "main")
        .ok_or(Error::NoMainFunction)?
        .inner;
    if main_decl.ty.inner != Type::INT || main_decl.params.len() != 0 {
        return Err(Error::IncorrectMainFunctionType {
            ty: main_decl.ty.inner,
            params: main_decl.params.iter().map(|(ty, _)| ty.inner).collect(),
        });
    }

    let mut env = HashMap::new();
    env.insert(0, Binding::Fun((Type::VOID_PTR, vec![Type::INT])));
    env.insert(1, Binding::Fun((Type::INT, vec![Type::INT])));
    let mut fun_decls = Vec::new();

    for decl in file.fun_decls {
        if get_fun(&env, decl.inner.name.inner).is_ok() {
            // TODO : Add the function name
            return Err(Error::TypeError("Function is already defined".into()));
        }
        fun_decls.push(typecheck_fun(decl, &mut env)?);
    }

    Ok(File { fun_decls })
}
