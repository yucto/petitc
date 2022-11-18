use beans::span::Span;
use colored::Colorize;

use std::collections::HashMap;

use crate::error::ErrorKind;

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

type Environment = HashMap<Ident, (Binding, Option<Span>)>;

fn get_fun<'env>(
    env: &'env Environment,
    ident: WithSpan<Ident>,
    name_of: &'_ [String],
) -> Result<(&'env (Type, Vec<Type>), &'env Option<Span>)> {
    if let Some((Binding::Fun(res), span)) = env.get(&ident.inner) {
        Ok((res, span))
    } else {
        Err(Error::new(ErrorKind::NameError {
            name: name_of[ident.inner].clone(),
            span: ident.span,
        }))
    }
}

fn get_var(
    env: &Environment,
    ident: WithSpan<Ident>,
    name_of: &[String],
) -> Result<Type> {
    if let Some((Binding::Var(res), _)) = env.get(&ident.inner) {
        Ok(*res)
    } else {
        Err(Error::new(ErrorKind::NameError {
            name: name_of[ident.inner].clone(),
            span: ident.span,
        }))
    }
}

fn type_expr(
    e: WithSpan<Expr<SpanAnnotation>>,
    env: &Environment,
    name_of: &[String],
) -> Result<TypedExpr> {
    match e.inner {
        Expr::True => Ok(WithType::new(Expr::True, Type::BOOL, e.span)),
        Expr::False => Ok(WithType::new(Expr::False, Type::BOOL, e.span)),
        Expr::Null => Ok(WithType::new(Expr::Null, Type::VOID_PTR, e.span)),
        Expr::Int(n) => Ok(WithType::new(Expr::Int(n), Type::INT, e.span)),
        Expr::Ident(name) => Ok(WithType::new(
            Expr::Ident(name),
            get_var(
                env,
                WithSpan {
                    inner: name,
                    span: e.span.clone(),
                },
                name_of,
            )?,
            e.span,
        )),
        Expr::SizeOf(ty) => {
            if !ty.inner.is_eq(&Type::VOID) {
                Ok(WithType::new(Expr::SizeOf(ty.clone()), Type::INT, ty.span))
            } else {
                Err(Error::new(ErrorKind::SizeofVoid { span: e.span }))
            }
        }
        Expr::Addr(inner_e) => {
            if inner_e.inner.is_lvalue() {
                type_expr(*inner_e, env, name_of).map(|inner_e| {
                    let ty = inner_e.ty.ptr();
                    WithType::new(Expr::Addr(Box::new(inner_e)), ty, e.span)
                })
            } else {
                Err(Error::new(ErrorKind::AddressOfRvalue {
		    span: e.span,
		    expression_span: inner_e.span,
		})
                    .add_help(String::from("you could allocate this expression, by binding it to a variable")))
            }
        }
        Expr::Deref(inner_e) => {
            type_expr(*inner_e, env, name_of).and_then(|inner_e| {
                let mut ty = inner_e.ty;
                if ty.is_ptr() {
                    ty.indirection_count -= 1;
		    if ty.is_eq(&Type::VOID) {
			Err(Error::new(ErrorKind::DerefVoidPointer {
			    span: inner_e.span,
			}))
		    } else {
			Ok(WithType::new(
                            Expr::Deref(Box::new(inner_e)),
                            ty,
                            e.span,
			))
		    }
                } else {
                    Err(Error::new(ErrorKind::DerefNonPointer {
                        ty,
                        span: inner_e.span,
                    }))
                }
            })
        }
        Expr::Assign { lhs, rhs } => {
            if !lhs.inner.is_lvalue() {
                return Err(Error::new(ErrorKind::RvalueAssignment {
                    span: lhs.span,
                }));
            }
            let lhs = type_expr(*lhs, env, name_of)?;
            let rhs = type_expr(*rhs, env, name_of)?;
            let ty1 = lhs.ty;
            let ty2 = rhs.ty;

            if !ty1.is_eq(&ty2) {
                return Err(Error::new(ErrorKind::TypeMismatch {
                    span: e.span,
                    expected_type: ty1,
                    found_type: ty2,
                }));
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
        Expr::PrefixIncr(inner_e)
        | Expr::PrefixDecr(inner_e)
        | Expr::PostfixIncr(inner_e)
        | Expr::PostfixDecr(inner_e) => {
            if inner_e.inner.is_lvalue() {
                let inner_e = type_expr(*inner_e, env, name_of)?;
                let ty = inner_e.ty;
                Ok(WithType::new(
                    Expr::PrefixIncr(Box::new(inner_e)),
                    ty,
                    e.span,
                ))
            } else {
                Err(Error::new(ErrorKind::IncrOrDecrRvalue {
                    span: e.span,
                    expression_span: inner_e.span,
                }))
            }
        }
        Expr::Pos(inner_e) => {
            let inner_e = type_expr(*inner_e, env, name_of)?;
            let ty = inner_e.ty;

            if !ty.is_eq(&Type::INT) {
                return Err(Error::new(ErrorKind::TypeMismatch {
                    expected_type: Type::INT,
                    found_type: ty,
                    span: inner_e.span,
                }));
            }
            Ok(WithType::new(
                Expr::Pos(Box::new(inner_e)),
                Type::INT,
                e.span,
            ))
        }
        // The code here should be the same of the one at the previous branch
        Expr::Neg(inner_e) => {
            let inner_e = type_expr(*inner_e, env, name_of)?;
            let ty = inner_e.ty;

            if !ty.is_eq(&Type::INT) {
                Err(Error::new(ErrorKind::TypeMismatch {
                    expected_type: Type::INT,
                    found_type: ty,
                    span: inner_e.span,
                }))
            } else {
                Ok(WithType::new(
                    Expr::Neg(Box::new(inner_e)),
                    Type::INT,
                    e.span,
                ))
            }
        }
        Expr::Not(inner_e) => {
            let inner_e = type_expr(*inner_e, env, name_of)?;
            if inner_e.ty.is_eq(&Type::VOID) {
                Err(Error::new(ErrorKind::VoidExpression {
                    span: inner_e.span,
                }))
            } else {
                Ok(WithType::new(
                    Expr::Not(Box::new(inner_e)),
                    Type::INT,
                    e.span,
                ))
            }
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
            let lhs = type_expr(*lhs, env, name_of)?;
            let rhs = type_expr(*rhs, env, name_of)?;
            let ty1 = lhs.ty;
            let ty2 = rhs.ty;
	    if ty1.is_eq(&Type::VOID) {
		Err(Error::new(ErrorKind::VoidExpression { span: lhs.span, }))
	    } else if ty2.is_eq(&Type::VOID) {
		Err(Error::new(ErrorKind::VoidExpression { span: rhs.span, }))
            } else if !ty1.is_eq(&ty2) {
                Err(Error::new(ErrorKind::TypeMismatch {
                    span: rhs.span,
                    expected_type: ty1,
                    found_type: ty2,
                })
                .add_help(format!(
                    "Type `{}` was expected because the expression ",
                    format!("{}", ty1).bold()
                )))
            } else {
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
            let lhs = type_expr(*lhs, env, name_of)?;
            let rhs = type_expr(*rhs, env, name_of)?;
            let ty1 = lhs.ty;
            let ty2 = rhs.ty;
            if !ty1.is_eq(&Type::INT) {
                return Err(Error::new(ErrorKind::TypeMismatch {
                    span: lhs.span,
                    expected_type: Type::INT,
                    found_type: ty1,
                }));
            }
            if !ty2.is_eq(&Type::INT) {
                return Err(Error::new(ErrorKind::TypeMismatch {
                    span: rhs.span,
                    expected_type: Type::INT,
                    found_type: ty2,
                }));
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
            let lhs = type_expr(*lhs, env, name_of)?;
            let rhs = type_expr(*rhs, env, name_of)?;
            let mut ty1 = lhs.ty;
            let mut ty2 = rhs.ty;
            let new_e = Expr::Op {
                op: BinOp::Add,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
            if ty1.is_ptr() && ty2.is_ptr() {
                return Err(Error::new(ErrorKind::BuiltinBinopTypeMismatch {
                    left_type: ty1,
                    right_type: ty2,
                    span: e.span,
                    op: "+",
                })
			   .reason(String::from("pointers cannot be added."))
			   .add_help(String::from(
			       "maybe you meant to subtract the pointers?",
			   )));
            }

            if ty2.is_ptr() {
                std::mem::swap(&mut ty1, &mut ty2);
            }

            if ty1.is_ptr() {
                if !ty2.is_eq(&Type::INT) {
                    return Err(Error::new(
                        ErrorKind::BuiltinBinopTypeMismatch {
                            left_type: ty1,
                            right_type: ty2,
                            span: e.span,
                            op: "+",
                        },
                    ));
                }

                Ok(WithType::new(new_e, ty1, e.span))
            } else if !ty1.is_eq(&ty2) {
                Err(Error::new(ErrorKind::BuiltinBinopTypeMismatch {
                    left_type: ty1,
                    right_type: ty2,
                    span: e.span,
                    op: "+",
                })
                    .reason(format!(
                        "casting between {ty1} and {ty2} is undefined"
                    )))
            } else if !ty1.is_eq(&Type::INT) {
                Err(Error::new(ErrorKind::BuiltinBinopTypeMismatch {
                    left_type: ty1,
                    right_type: ty2,
                    span: e.span,
                    op: "+",
                })
                    .reason(format!("addition over `{ty1}` is undefined")))
            } else {
                Ok(WithType::new(new_e, Type::INT, e.span))
            }
            
        }
        Expr::Op {
            op: BinOp::Sub,
            lhs,
            rhs,
        } => {
            let lhs = type_expr(*lhs, env, name_of)?;
            let rhs = type_expr(*rhs, env, name_of)?;
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
                        Err(Error::new(ErrorKind::BuiltinBinopTypeMismatch {
                            left_type: ty1,
                            right_type: ty2,
                            span: e.span,
                            op: "-",
                        })
                        .reason(String::from(
                            "heterogeneous pointers cannot be subtracted",
                        )))
                    } else {
                        Ok(WithType::new(new_e, Type::INT, e.span))
                    }
                } else if !ty2.is_eq(&Type::INT) {
                    Err(Error::new(ErrorKind::BuiltinBinopTypeMismatch {
                        left_type: ty1,
                        right_type: ty2,
                        span: e.span,
                        op: "-",
                    }))
                } else {
                    Ok(WithType::new(new_e, ty1, e.span))
                }
            } else if !ty1.is_eq(&Type::INT) || !ty1.is_eq(&ty2) {
                let mut error =
                    Error::new(ErrorKind::BuiltinBinopTypeMismatch {
                        left_type: ty1,
                        right_type: ty2,
                        span: e.span,
                        op: "-",
                    });
                if ty1.is_eq(&Type::INT) && ty2.is_ptr() {
                    error = error
                        .add_help(String::from("maybe you meant to have the operands the other way around"));
                }
                Err(error)
            } else {
                Ok(WithType::new(new_e, Type::INT, e.span))
            }
        }
        Expr::Call { name, args } => {
            let ((ret_ty, args_ty), fun_span) =
                get_fun(env, name.clone(), name_of)?;
            if args.len() != args_ty.len() {
                return Err(Error::new(ErrorKind::ArityMismatch {
                    found_arity: args.len(),
                    expected_arity: args_ty.len(),
                    span: e.span,
                    definition_span: fun_span.clone(),
                    function_name: name_of[name.inner].clone(),
                }));
            }

            let mut typed_args = Vec::new();

            for (arg, ty) in args.into_iter().zip(args_ty.iter()) {
                let arg = type_expr(arg, env, name_of)?;
                let arg_ty = arg.ty;

                if !arg_ty.is_eq(ty) {
                    return Err(Error::new(ErrorKind::TypeMismatch {
                        expected_type: *ty,
                        found_type: arg_ty,
                        span: arg.span,
                    }));
                }

                typed_args.push(arg);
            }

            Ok(WithType::new(
                Expr::Call {
                    name,
                    args: typed_args,
                },
                *ret_ty,
                e.span,
            ))
        }
    }
}

fn typecheck_instr(
    instr: WithSpan<Instr<SpanAnnotation>>,
    loop_level: usize,
    expected_return_type: Type,
    env: &mut Environment,
    name_of: &[String],
) -> Result<TypedInstr<Instr<TypeAnnotation>>> {
    match instr.inner {
        Instr::EmptyInstr => Ok(TypedInstr {
            instr: Instr::EmptyInstr,
            span: instr.span,
            loop_level,
            expected_return_type,
        }),
        Instr::ExprInstr(e) => Ok(TypedInstr {
            instr: Instr::ExprInstr(type_expr(e, env, name_of)?),
            span: instr.span,
            loop_level,
            expected_return_type,
        }),
        Instr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond = type_expr(cond, env, name_of)?;
            let then_branch = typecheck_instr(
                *then_branch,
                loop_level,
                expected_return_type,
                env,
                name_of,
            )?;
            let else_branch = if let Some(else_branch) = *else_branch {
                Some(typecheck_instr(
                    else_branch,
                    loop_level,
                    expected_return_type,
                    env,
                    name_of,
                )?)
            } else {
                None
            };
            if cond.ty.is_eq(&Type::VOID) {
                return Err(Error::new(ErrorKind::VoidExpression {
                    span: cond.span,
                }));
            }
            if then_branch.expected_return_type != expected_return_type {
                return Err(Error::new(ErrorKind::TypeMismatch {
                    span: then_branch.span,
                    expected_type: expected_return_type,
                    found_type: then_branch.expected_return_type,
                })
                .add_help(String::from("this should not happen")));
            }
            if let Some(ref else_branch) = else_branch {
                if else_branch.expected_return_type != expected_return_type {
                    return Err(Error::new(ErrorKind::TypeMismatch {
                        span: else_branch.span.clone(),
                        expected_type: expected_return_type,
                        found_type: else_branch.expected_return_type,
                    })
                    .add_help(String::from("this should not happen")));
                }
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
            let cond = type_expr(cond, env, name_of)?;
            let body = typecheck_instr(
                *body,
                loop_level + 1,
                expected_return_type,
                env,
                name_of,
            )?;
            if cond.ty.is_eq(&Type::VOID) {
                return Err(Error::new(ErrorKind::VoidExpression {
                    span: cond.span,
                }));
            }
            if body.expected_return_type != expected_return_type {
                return Err(Error::new(ErrorKind::TypeMismatch {
                    expected_type: expected_return_type,
                    found_type: body.expected_return_type,
                    span: body.span,
                })
                .add_help(String::from("this should not happen")));
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
            let cond =
                cond.map(|cond| type_expr(cond, env, name_of)).transpose()?;
            let incr = incr
                .into_iter()
                .map(|incr| type_expr(incr, env, name_of))
                .collect::<Result<Vec<_>>>()?;
            let body = Box::new(typecheck_instr(
                *body,
                loop_level + 1,
                expected_return_type,
                env,
                name_of,
            )?);

            cond.as_ref()
                .map(|cond| {
                    if cond.ty.is_eq(&Type::VOID) {
                        Err(Error::new(ErrorKind::VoidExpression {
                            span: cond.span.clone(),
                        }))
                    } else {
                        Ok(())
                    }
                })
                .transpose()?;

            if body.expected_return_type != expected_return_type {
                Err(Error::new(ErrorKind::TypeMismatch {
                    expected_type: expected_return_type,
                    found_type: body.expected_return_type,
                    span: body.span,
                })
                .add_help(String::from("this should not happen")))
            } else {
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
            name_of,
        ),
        Instr::Block(block) => typecheck_block(
            WithSpan::new(block, instr.span),
            loop_level,
            expected_return_type,
            env,
            name_of,
        ),
        Instr::Return(None) => {
            if expected_return_type != Type::VOID {
                Err(Error::new(ErrorKind::TypeMismatch {
                    span: instr.span,
                    expected_type: expected_return_type,
                    found_type: Type::VOID,
                })
                    .reason(String::from(
			"a `return` statement without arguments requires the current function to have a return type `void`"
		    ))
                    .add_help(format!(
			"try adding an argument `{}`",
			format!("return /* {expected_return_type} */;").bold())
		    ))
            } else {
                Ok(TypedInstr {
                    instr: Instr::Return(None),
                    span: instr.span,
                    loop_level,
                    expected_return_type,
                })
            }
        }
        Instr::Return(Some(e)) => {
            let e = type_expr(e, env, name_of)?;
            if !e.ty.is_eq(&expected_return_type) {
                Err(Error::new(ErrorKind::TypeMismatch {
                    span: instr.span,
                    expected_type: expected_return_type,
                    found_type: e.ty,
                }))
            } else {
                Ok(TypedInstr {
                    instr: Instr::Return(Some(e)),
                    span: instr.span,
                    loop_level,
                    expected_return_type,
                })
            }
        }
        Instr::Break | Instr::Continue => {
            if loop_level == 0 {
                Err(Error::new(ErrorKind::BreakContinueOutsideLoop {
                    span: instr.span,
                }))
            } else {
                Ok(TypedInstr {
                    instr: Instr::Break,
                    span: instr.span,
                    loop_level,
                    expected_return_type,
                })
            }
        }
    }
}

/// On returning an instr, always returns a block
fn typecheck_block(
    block: WithSpan<Vec<DeclOrInstr<SpanAnnotation>>>,
    loop_level: usize,
    expected_return_type: Type,
    env: &mut Environment,
    name_of: &[String],
) -> Result<TypedInstr<Instr<TypeAnnotation>>> {
    let mut new_bindings: Vec<(WithSpan<Ident>, Option<Binding>)> = Vec::new();
    let mut ret = Vec::new();

    fn assert_var_is_not_reused(
        var_name: WithSpan<Ident>,
        new_bindings: &[(WithSpan<Ident>, Option<Binding>)],
        name_of: &[String],
    ) -> Result<()> {
        if let Some((_, first_definition_span)) = new_bindings
            .iter()
            .map(|(WithSpan { inner, span }, _)| (*inner, span))
            .find(|(name, _)| *name == var_name.inner)
        {
            Err(Error::new(ErrorKind::SymbolDefinedTwice {
                first_definition: first_definition_span.clone(),
                second_definition: var_name.span,
                name: name_of[var_name.inner].clone(),
            }))
        } else {
            Ok(())
        }
    }

    for decl_or_instr in block.inner {
        match decl_or_instr {
            DeclOrInstr::Fun(fun_decl) => {
                assert_var_is_not_reused(
                    fun_decl
                        .inner
                        .name
                        .clone()
                        .with_span(fun_decl.span.clone()),
                    &new_bindings,
                    name_of,
                )?;
                let fun_decl = typecheck_fun(fun_decl, env, name_of)?;
                new_bindings.push((
                    fun_decl
                        .inner
                        .name
                        .clone()
                        .with_span(fun_decl.span.clone()),
                    env.remove(&fun_decl.inner.name.inner).map(|x| x.0),
                ));
                env.insert(
                    fun_decl.inner.name.inner,
                    (
                        Binding::Fun((
                            fun_decl.inner.ty.inner,
                            fun_decl
                                .inner
                                .params
                                .iter()
                                .map(|(ty, _)| ty.inner)
                                .collect(),
                        )),
                        Some(fun_decl.span.clone()),
                    ),
                );
                ret.push(DeclOrInstr::Fun(fun_decl));
            }
            DeclOrInstr::Var(var_decl) => {
                if var_decl.inner.ty.inner.is_eq(&Type::VOID) {
                    return Err(Error::new(ErrorKind::VoidVariable {
                        span: var_decl.span,
                        name: name_of[var_decl.inner.name.inner].clone(),
                    }));
                }
                assert_var_is_not_reused(
                    var_decl
                        .inner
                        .name
                        .clone()
                        .with_span(var_decl.span.clone()),
                    &new_bindings,
                    name_of,
                )?;
                new_bindings.push((
                    var_decl
                        .inner
                        .name
                        .clone()
                        .with_span(var_decl.span.clone()),
                    env.remove(&var_decl.inner.name.inner).map(|x| x.0),
                ));
                env.insert(
                    var_decl.inner.name.inner,
                    (
                        Binding::Var(var_decl.inner.ty.inner),
                        Some(var_decl.span.clone()),
                    ),
                );
                let value = var_decl
                    .inner
                    .value
                    .map(|value| type_expr(value, env, name_of))
                    .transpose()?;

                if let Some(ref val) = value {
                    if !val.ty.is_eq(&var_decl.inner.ty.inner) {
                        return Err(Error::new(
                            ErrorKind::VariableTypeMismatch {
                                expected_type: var_decl.inner.ty.inner,
                                found_type: val.ty,
                                span: val.span.clone(),
                                definition_span: var_decl
                                    .inner
                                    .ty
                                    .span
                                    .sup(&var_decl.inner.name.span),
                                variable_name: name_of
                                    [var_decl.inner.name.inner]
                                    .clone(),
                            },
                        ));
                    }
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
            DeclOrInstr::Instr(instr) => {
                ret.push(DeclOrInstr::Instr(typecheck_instr(
                    instr,
                    loop_level,
                    expected_return_type,
                    env,
                    name_of,
                )?))
            }
        }
    }
    for (name, old_binding) in new_bindings {
        if let Some(binding) = old_binding {
            env.insert(name.inner, (binding, Some(name.span)));
        } else {
            env.remove(&name.inner);
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
    env: &mut Environment,
    name_of: &[String],
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
                ty.span.sup(&name.span),
            ))
        })
        .chain(decl.inner.code.inner.into_iter())
        .collect::<Vec<_>>();
    env.insert(
        decl.inner.name.inner,
        (
            Binding::Fun((
                decl.inner.ty.inner,
                decl.inner.params.iter().map(|(ty, _)| ty.inner).collect(),
            )),
            Some(decl.span.clone()),
        ),
    );

    let typed_instr = typecheck_block(
        WithSpan::new(code, decl.inner.code.span),
        0,
        decl.inner.ty.inner,
        env,
        name_of,
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
    name_of: &[String],
) -> Result<File<TypeAnnotation>> {
    let WithSpan {
        inner: main_decl,
        span: main_span,
    } = &file
        .fun_decls
        .iter()
        .find(|decl| name_of[decl.inner.name.inner] == "main")
        .ok_or_else(|| Error::new(ErrorKind::NoMainFunction))?;
    if main_decl.ty.inner != Type::INT || !main_decl.params.is_empty() {
        return Err(Error::new(ErrorKind::IncorrectMainFunctionType {
            ty: main_decl.ty.inner,
            params: main_decl.params.iter().map(|(ty, _)| ty.inner).collect(),
            span: main_span.clone(),
        }));
    }

    let mut env = HashMap::new();
    env.insert(0, (Binding::Fun((Type::VOID_PTR, vec![Type::INT])), None));
    env.insert(1, (Binding::Fun((Type::INT, vec![Type::INT])), None));
    let mut fun_decls = Vec::new();

    for decl in file.fun_decls {
        if let Ok((_, first_definition)) = get_fun(
            &env,
            decl.inner.name.clone().with_span(decl.span.clone()),
            name_of,
        ) {
            return Err(Error::new(ErrorKind::FunctionDefinedTwice {
                first_definition: first_definition.clone(),
                second_definition: decl.span,
                name: name_of[decl.inner.name.inner].clone(),
            }));
        }
        fun_decls.push(typecheck_fun(decl, &mut env, name_of)?);
    }

    Ok(File { fun_decls })
}
