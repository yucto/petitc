use super::{
    ast,
    error::{Error, Result},
};

use std::collections::HashMap;

pub struct TypeAnnotion;
pub struct TypedInstr<T> {
    pub instr: T,
    pub loop_level: usize,
    pub expected_return_type: ast::Type,
}

impl ast::Annotation for TypeAnnotion {
    type Ident = ast::Ident;
    type Type = ast::Type;
    type WrapExpr<T> = (T, ast::Type);
    type WrapInstr<T> = TypedInstr<T>;
    type WrapFunDecl<T> = T;
    type WrapVarDecl<T> = T;
}

pub type TypedExpr =
    <TypeAnnotion as ast::Annotation>::WrapExpr<ast::Expr<TypeAnnotion>>;

enum Binding {
    Var(ast::Type),
    Fun((ast::Type, Vec<ast::Type>)),
}

fn get_fun(
    env: &HashMap<ast::Ident, Binding>,
    name: ast::Ident,
) -> Result<&(ast::Type, Vec<ast::Type>)> {
    if let Some(Binding::Fun(res)) = env.get(&name) {
        Ok(res)
    } else {
        // TODO : no function named name in current scope
        Err(Error::NameError)
    }
}

fn get_var(
    env: &HashMap<ast::Ident, Binding>,
    name: ast::Ident,
) -> Result<ast::Type> {
    if let Some(Binding::Var(res)) = env.get(&name) {
        Ok(*res)
    } else {
        // TODO : no variable named name in current scope
        Err(Error::NameError)
    }
}

fn type_expr(
    e: ast::Expr,
    env: &HashMap<ast::Ident, Binding>,
) -> Result<TypedExpr> {
    match e {
        ast::Expr::True => Ok((ast::Expr::True, ast::Type::BOOL)),
        ast::Expr::False => Ok((ast::Expr::False, ast::Type::BOOL)),
        ast::Expr::Null => Ok((ast::Expr::Null, ast::Type::VOID_PTR)),
        ast::Expr::Int(n) => Ok((ast::Expr::Int(n), ast::Type::INT)),
        ast::Expr::Ident(name) => {
            Ok((ast::Expr::Ident(name), get_var(env, name)?))
        }
        ast::Expr::SizeOf(ty) => {
            if !ty.is_eq(&ast::Type::VOID) {
                Ok((ast::Expr::SizeOf(ty), ast::Type::INT))
            } else {
                Err(Error::TypeError("void type has no size".into()))
            }
        }
        ast::Expr::Addr(e) => {
            if e.is_lvalue() {
                type_expr(*e, env).map(|(e, ty)| {
                    (ast::Expr::Addr(Box::new((e, ty))), ty.ptr())
                })
            } else {
                Err(Error::TypeError(
                    "Can't take the address of a rvalue".into(),
                ))
            }
        }
        ast::Expr::Deref(e) => {
            type_expr(*e, env).and_then(|inner_e @ (_, mut ty)| {
                if ty.is_ptr() {
                    ty.indirection_count -= 1;
                    Ok((ast::Expr::Deref(Box::new(inner_e)), ty))
                } else {
                    Err(Error::TypeError(format!(
                        "Can't deref non-ptr type {}",
                        ty
                    )))
                }
            })
        }
        ast::Expr::Assign { lhs, rhs } => {
            if !lhs.is_lvalue() {
                return Err(Error::TypeError("Can't assign to rvalue".into()));
            }
            let lhs @ (_, ty1) = type_expr(*lhs, env)?;
            let rhs @ (_, ty2) = type_expr(*rhs, env)?;

            if !ty1.is_eq(&ty2) {
                return Err(Error::TypeError(format!(
                    "Type mismatch on assignation : {} != {}",
                    ty1, ty2
                )));
            }

            Ok((
                ast::Expr::Assign {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ty1,
            ))
        }
        ast::Expr::PrefixIncr(e) => {
            if e.is_lvalue() {
                let e @ (_, ty) = type_expr(*e, env)?;
                Ok((ast::Expr::PrefixIncr(Box::new(e)), ty))
            } else {
                Err(Error::TypeError(
                    "Can't use a prefix operation on a rvalue".into(),
                ))
            }
        }
        // The code here should be the same of the one at the previous branch
        ast::Expr::PrefixDecr(e) => {
            if e.is_lvalue() {
                let e @ (_, ty) = type_expr(*e, env)?;
                Ok((ast::Expr::PrefixDecr(Box::new(e)), ty))
            } else {
                Err(Error::TypeError(
                    "Can't use a prefix operation on a rvalue".into(),
                ))
            }
        }
        // The code here should be the same of the one at the previous branch
        ast::Expr::PostfixIncr(e) => {
            if e.is_lvalue() {
                let e @ (_, ty) = type_expr(*e, env)?;
                Ok((ast::Expr::PostfixIncr(Box::new(e)), ty))
            } else {
                Err(Error::TypeError(
                    "Can't use a postfix operation on a rvalue".into(),
                ))
            }
        }
        // The code here should be the same of the one at the previous branch
        ast::Expr::PostfixDecr(e) => {
            if e.is_lvalue() {
                let e @ (_, ty) = type_expr(*e, env)?;
                Ok((ast::Expr::PostfixDecr(Box::new(e)), ty))
            } else {
                Err(Error::TypeError(
                    "Can't use a postfix operation on a rvalue".into(),
                ))
            }
        }
        ast::Expr::Pos(e) => {
            let e @ (_, ty) = type_expr(*e, env)?;

            if !ty.is_eq(&ast::Type::INT) {
                return Err(Error::TypeError(format!(
                    "Can't use unary operation + on a non-int of type {}",
                    ty
                )));
            }
            Ok((ast::Expr::Pos(Box::new(e)), ast::Type::INT))
        }
        // The code here should be the same of the one at the previous branch
        ast::Expr::Neg(e) => {
            let e @ (_, ty) = type_expr(*e, env)?;

            if !ty.is_eq(&ast::Type::INT) {
                return Err(Error::TypeError(format!(
                    "Can't use unary operation - on a non-int of type {}",
                    ty
                )));
            }
            Ok((ast::Expr::Neg(Box::new(e)), ast::Type::INT))
        }
        ast::Expr::Not(e) => {
            let e @ (_, ty) = type_expr(*e, env)?;
            if ty.is_eq(&ast::Type::VOID) {
                return Err(Error::TypeError(
                    "Can't use unary operation ! on void".into(),
                ));
            }
            Ok((ast::Expr::Not(Box::new(e)), ty))
        }
        ast::Expr::Op {
            op:
                op @ (ast::BinOp::Eq
                | ast::BinOp::NEq
                | ast::BinOp::Lt
                | ast::BinOp::Le
                | ast::BinOp::Gt
                | ast::BinOp::Ge),
            lhs,
            rhs,
        } => {
            let lhs @ (_, ty1) = type_expr(*lhs, env)?;
            let rhs @ (_, ty2) = type_expr(*rhs, env)?;
            if !ty1.is_eq(&ty2) {
                return Err(Error::TypeError(format!(
                    "Type mismatch on comparison : {} != {}",
                    ty1, ty2
                )));
            }
            Ok((
                ast::Expr::Op {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ast::Type::INT,
            ))
        }
        ast::Expr::Op {
            op:
                op @ (ast::BinOp::Mul
                | ast::BinOp::Div
                | ast::BinOp::Mod
                | ast::BinOp::BOr
                | ast::BinOp::BAnd),
            lhs,
            rhs,
        } => {
            let lhs @ (_, ty1) = type_expr(*lhs, env)?;
            let rhs @ (_, ty2) = type_expr(*rhs, env)?;
            if !ty1.is_eq(&ast::Type::INT) || !ty1.is_eq(&ty2) {
                return Err(Error::TypeError(format!(
                    "Can't use arithmetic operation on non-int of type {} and {}",
                    ty1, ty2
                )));
            }
            Ok((
                ast::Expr::Op {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ast::Type::INT,
            ))
        }
        ast::Expr::Op {
            op: ast::BinOp::Add,
            lhs,
            rhs,
        } => {
            let lhs @ (_, mut ty1) = type_expr(*lhs, env)?;
            let rhs @ (_, mut ty2) = type_expr(*rhs, env)?;
            let e = ast::Expr::Op {
                op: ast::BinOp::Add,
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
                if !ty2.is_eq(&ast::Type::INT) {
                    return Err(Error::TypeError(
                        "Can't add together a pointer and a non-int".into(),
                    ));
                }

                Ok((e, ty1))
            } else {
                if !ty1.is_eq(&ty2) || !ty1.is_eq(&ast::Type::INT) {
                    return Err(Error::TypeError(
                        "Can't use addition on non-integers or non-pointers"
                            .into(),
                    ));
                }

                Ok((e, ast::Type::INT))
            }
        }
        ast::Expr::Op {
            op: ast::BinOp::Sub,
            lhs,
            rhs,
        } => {
            let lhs @ (_, ty1) = type_expr(*lhs, env)?;
            let rhs @ (_, ty2) = type_expr(*rhs, env)?;
            let e = ast::Expr::Op {
                op: ast::BinOp::Sub,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
            if ty1.is_ptr() && ty2.is_ptr() {
                if ty1 != ty2 {
                    return Err(Error::TypeError(format!(
                        "Can't subtract pointers of different types : {} != {}",
                        ty1, ty2
                    )));
                }

                return Ok((e, ast::Type::INT));
            }

            if ty1.is_ptr() {
                if ty2.is_ptr() {
                    return Ok((e, ast::Type::INT));
                }

                if !ty2.is_eq(&ast::Type::INT) {
                    return Err(Error::TypeError(
                        "Can't subtract a non-int to a pointer".into(),
                    ));
                }

                Ok((e, ty1))
            } else {
                if !ty1.is_eq(&ty2) || !ty1.is_eq(&ast::Type::INT) {
                    return Err(Error::TypeError(format!(
                        "Can't subtract {} to {}",
                        ty1, ty2
                    )));
                }

                Ok((e, ast::Type::INT))
            }
        }
        ast::Expr::Call { name, args } => {
            let (ret_ty, args_ty) = get_fun(env, name)?.clone();
            if args.len() != args_ty.len() {
                return Err(Error::TypeError(format!(
                    "Arguments' count mismatch : expected {}, got {}",
                    args_ty.len(),
                    args.len()
                )));
            }

            let mut typed_args = Vec::new();

            for (arg, ty) in args.into_iter().zip(args_ty.iter()) {
                let arg @ (_, arg_ty) = type_expr(arg, env)?;

                if !arg_ty.is_eq(ty) {
                    return Err(Error::TypeError(format!(
                        "Argument type mismatch, expected {}, got {}",
                        arg_ty, ty
                    )));
                }

                typed_args.push(arg);
            }

            Ok((
                ast::Expr::Call {
                    name,
                    args: typed_args,
                },
                ret_ty,
            ))
        }
    }
}

fn typecheck_instr(
    instr: ast::Instr,
    loop_level: usize,
    expected_return_type: ast::Type,
    env: &mut HashMap<ast::Ident, Binding>,
) -> Result<TypedInstr<ast::Instr<TypeAnnotion>>> {
    match instr {
        ast::Instr::EmptyInstr => Ok(TypedInstr {
            instr: ast::Instr::EmptyInstr,
            loop_level,
            expected_return_type,
        }),
        ast::Instr::ExprInstr(e) => Ok(TypedInstr {
            instr: ast::Instr::ExprInstr(type_expr(e, env)?),
            loop_level,
            expected_return_type,
        }),
        ast::Instr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond @ (_, ty) = type_expr(cond, env)?;
            let then_branch = typecheck_instr(
                *then_branch,
                loop_level,
                expected_return_type,
                env,
            )?;
            let else_branch = typecheck_instr(
                *else_branch,
                loop_level,
                expected_return_type,
                env,
            )?;
            if ty.is_eq(&ast::Type::VOID) {
                return Err(Error::TypeError(
                    "The condition in an if can't be of type void".into(),
                ));
            }
            if then_branch.expected_return_type
                != else_branch.expected_return_type
            {
                return Err(Error::TypeError(format!(
                    "if and else branch have different return types : {} != {}",
                    then_branch.expected_return_type,
                    else_branch.expected_return_type
                )));
            }
            if then_branch.expected_return_type != expected_return_type {
                return Err(Error::TypeError(format!(
                    "if statement don't return the right type, expected {}, got {}",
                    expected_return_type,
                    then_branch.expected_return_type
                )));
            }

            Ok(TypedInstr {
                instr: ast::Instr::If {
                    cond,
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
                loop_level,
                expected_return_type,
            })
        }
        ast::Instr::While { cond, body } => {
            let cond @ (_, ty) = type_expr(cond, env)?;
            let body = typecheck_instr(
                *body,
                loop_level + 1,
                expected_return_type,
                env,
            )?;
            if ty.is_eq(&ast::Type::VOID) {
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
                instr: ast::Instr::While {
                    cond,
                    body: Box::new(body),
                },
                loop_level,
                expected_return_type,
            })
        }
        ast::Instr::For {
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
                .map(|(_, ty)| {
                    if ty.is_eq(&ast::Type::VOID) {
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
                instr: ast::Instr::For {
                    loop_var: None,
                    cond,
                    incr,
                    body,
                },
                loop_level,
                expected_return_type,
            })
        }
        ast::Instr::For {
            loop_var: Some(decl),
            cond,
            incr,
            body,
        } => typecheck_block(
            vec![
                ast::DeclOrInstr::Var(decl),
                ast::DeclOrInstr::Instr(ast::Instr::For {
                    loop_var: None,
                    cond,
                    incr,
                    body,
                }),
            ],
            loop_level,
            expected_return_type,
            env,
        ),
        ast::Instr::Block(block) => {
            typecheck_block(block, loop_level, expected_return_type, env)
        }
        ast::Instr::Return(None) => {
            if expected_return_type != ast::Type::VOID {
                return Err(Error::TypeError(format!(
                    "empty return only valid when void is expected, not {}",
                    expected_return_type,
                )));
            }
            Ok(TypedInstr {
                instr: ast::Instr::Return(None),
                loop_level,
                expected_return_type,
            })
        }
        ast::Instr::Return(Some(e)) => {
            let e @ (_, ty) = type_expr(e, env)?;
            if !ty.is_eq(&expected_return_type) {
                return Err(Error::TypeError(format!(
                    "return type mismatch, expected {}, got {}",
                    expected_return_type, ty,
                )));
            }
            Ok(TypedInstr {
                instr: ast::Instr::Return(Some(e)),
                loop_level,
                expected_return_type,
            })
        }
        ast::Instr::Break | ast::Instr::Continue => {
            if loop_level == 0 {
                return Err(Error::BreakContinueOutsideLoop);
            }

            Ok(TypedInstr {
                instr: ast::Instr::Break,
                loop_level,
                expected_return_type,
            })
        }
    }
}

/// On returning an instr, always returns a block
fn typecheck_block(
    block: Vec<ast::DeclOrInstr>,
    loop_level: usize,
    expected_return_type: ast::Type,
    env: &mut HashMap<usize, Binding>,
) -> Result<TypedInstr<ast::Instr<TypeAnnotion>>> {
    let mut new_bindings: Vec<(ast::Ident, Option<Binding>)> = Vec::new();
    let mut ret = Vec::new();

    fn assert_var_is_not_reused(
        var_name: ast::Ident,
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

    for decl_or_instr in block {
        match decl_or_instr {
            ast::DeclOrInstr::Fun(fun_decl) => {
                assert_var_is_not_reused(fun_decl.name, &new_bindings)?;
                let fun_decl = typecheck_fun(fun_decl, env)?;
                new_bindings.push((fun_decl.name, env.remove(&fun_decl.name)));
                ret.push(ast::DeclOrInstr::Fun(fun_decl));
            }
            ast::DeclOrInstr::Var(var_decl) => {
                if var_decl.ty.is_eq(&ast::Type::VOID) {
                    // TODO : add the name in the error message
                    return Err(Error::TypeError(
                        "can't assign variable of void type".into(),
                    ));
                }
                assert_var_is_not_reused(var_decl.name, &new_bindings)?;
                new_bindings.push((var_decl.name, env.remove(&var_decl.name)));
                env.insert(var_decl.name, Binding::Var(var_decl.ty));
                let value = var_decl
                    .value
                    .map(|value| type_expr(value, env))
                    .transpose()?;

                if value
                    .as_ref()
                    .map(|(_, ty)| !ty.is_eq(&var_decl.ty))
                    .unwrap_or(false)
                {
                    return Err(Error::TypeError(format!(
                        "Mismatch type on assignation, expected {}, got {}",
                        var_decl.ty,
                        value.unwrap().1,
                    )));
                }

                ret.push(ast::DeclOrInstr::Var(ast::VarDecl {
                    ty: var_decl.ty,
                    name: var_decl.name,
                    value,
                }));
            }
            ast::DeclOrInstr::Instr(instr) => {
                ret.push(ast::DeclOrInstr::Instr(typecheck_instr(
                    instr,
                    loop_level,
                    expected_return_type,
                    env,
                )?))
            }
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
        instr: ast::Instr::Block(ret),
        loop_level,
        expected_return_type,
    })
}

/// Insert the function in fun_env
/// Caller should remove it later if needed,
/// and saved previous value
fn typecheck_fun(
    decl: ast::FunDecl,
    env: &mut HashMap<usize, Binding>,
) -> Result<ast::FunDecl<TypeAnnotion>> {
    let code = decl
        .params
        .iter()
        .map(|(ty, name)| {
            ast::DeclOrInstr::Var(ast::VarDecl {
                ty: *ty,
                name: *name,
                value: None,
            })
        })
        .chain(decl.code.into_iter())
        .collect::<Vec<_>>();
    env.insert(
        decl.name,
        Binding::Fun((
            decl.ty,
            decl.params.iter().map(|(ty, _)| ty).cloned().collect(),
        )),
    );

    let typed_instr = typecheck_block(code, 0, decl.ty, env)?;

    let ast::Instr::Block(mut code) =
        typed_instr.instr
    else { unreachable!("Internal error") };

    code = code.into_iter().skip(decl.params.len()).collect();

    let typed_code = TypedInstr {
        instr: code,
        loop_level: typed_instr.loop_level,
        expected_return_type: typed_instr.expected_return_type,
    };

    Ok(ast::FunDecl {
        ty: decl.ty,
        name: decl.name,
        params: decl.params,
        code: typed_code,
        toplevel: decl.toplevel,
    })
}

pub fn typecheck(
    file: ast::File,
    string_store: &[String],
) -> Result<ast::File<TypeAnnotion>> {
    let main_decl = file
        .fun_decls
        .iter()
        .find(|decl| string_store[decl.name] == "main")
        .ok_or(Error::NoMainFunction)?;
    if main_decl.ty != ast::Type::INT || main_decl.params.len() != 0 {
        return Err(Error::IncorrectMainFunctionType {
            ty: main_decl.ty,
            params: main_decl
                .params
                .iter()
                .map(|(ty, _)| ty)
                .cloned()
                .collect(),
        });
    }

    let mut env = HashMap::new();
    env.insert(0, Binding::Fun((ast::Type::VOID_PTR, vec![ast::Type::INT])));
    env.insert(1, Binding::Fun((ast::Type::INT, vec![ast::Type::INT])));
    let mut fun_decls = Vec::new();

    for decl in file.fun_decls {
        if get_fun(&env, decl.name).is_ok() {
            // TODO : Add the function name
            return Err(Error::TypeError("Function is already defined".into()));
        }

        fun_decls.push(typecheck_fun(decl, &mut env)?);
    }

    Ok(ast::File { fun_decls })
}
