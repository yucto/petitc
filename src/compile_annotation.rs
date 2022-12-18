use crate::parsing::WithSpan;

use super::{
    ast::*,
    environment::Environment,
    tree::{Id, Tree},
    typechecker::*,
    typing::{BasisType, Type},
};

#[derive(Clone)]
pub struct DepthIdent {
    pub depth: usize,
    pub offset: i64,
}

/// unique among all function names
pub type FunIdent = Ident;

pub struct CBlock {
    pub block: Vec<CInstr>,
    // Maximal stack size that the block will use
    pub size_of: usize,
}

#[derive(Clone, Copy)]
pub enum CType {
    Void,
    Int,
    Bool,
    Ptr,
}

impl From<Type> for CType {
    fn from(ty: Type) -> Self {
        if ty.is_ptr() {
            CType::Ptr
        } else {
            match ty.basis {
                BasisType::Int => CType::Int,
                BasisType::Void => CType::Void,
                BasisType::Bool => CType::Bool,
            }
        }
    }
}

pub struct CFile {
    /// List of all functions, not only the toplevel ones
    pub fun_decls: Vec<CFunDecl>,
    /// A tree whose nodes are functions, and
    /// a finction is parent of another one if
    /// the other one is declared in itself
    pub function_dependencies: Tree,
}

pub struct CFunDecl {
    pub ty: CType,
    pub name: Ident,
    pub params: Vec<Ident>,
    pub code: CBlock,
    /// Store the depth of the function declaration
    pub depth: usize,
    /// Store the id in the dependency tree
    pub id: Id,
}

pub enum CExpr {
    Int(i64),
    Ident(DepthIdent),
    Deref(Box<CExpr>),
    Assign {
        lhs: Box<CExpr>,
        rhs: Box<CExpr>,
    },
    Call {
        name: FunIdent,
        args: Vec<Box<CExpr>>,
    },
    Addr(Box<CExpr>),
    Not(Box<CExpr>),
    Neg(Box<CExpr>),
    Op {
        op: BinOp,
        lhs: Box<CExpr>,
        rhs: Box<CExpr>,
    },
}

pub enum CInstr {
    EmptyInstr,
    ExprInstr(CExpr),
    If {
        cond: CExpr,
        then_branch: Box<CInstr>,
        else_branch: Option<Box<CInstr>>,
        unique_id: String,
    },
    While {
        cond: CExpr,
        body: Box<CInstr>,
        unique_id: String,
    },
    For {
        cond: Option<CExpr>,
        incr: Vec<CExpr>,
        body: Box<CInstr>,
        unique_id: String,
    },
    Block(CBlock),
    Return(Option<CExpr>),
    Break,
    Continue,
}

/// We get rid of the pre/postfix operation by transforming
/// ++i in (i += 1) and i++ in (i += 1) - 1
fn annotate_expr(
    e: TypedExpr,
    env: &Environment<Ident, DepthIdent>,
) -> (CExpr, CType) {
    match e.inner {
        Expr::Int(n) => (CExpr::Int(n), CType::Int),
        Expr::True => (CExpr::Int(1), CType::Bool),
        Expr::False => (CExpr::Int(0), CType::Bool),
        Expr::Null => (CExpr::Int(1), CType::Ptr),
        Expr::Ident(id) => {
            let id = env.get(&id.inner).unwrap().1;
            (CExpr::Ident(id.clone()), e.ty.into())
        }
        Expr::Deref(new_e) => {
            let ty = e.ty;
            let (new_e, _) = annotate_expr(*new_e, env);
            (CExpr::Deref(Box::new(new_e)), ty.into())
        }
        Expr::Assign { lhs, rhs } => {
            let (lhs, ty) = annotate_expr(*lhs, env);
            let (rhs, _) = annotate_expr(*rhs, env);
            (
                CExpr::Assign {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ty,
            )
        }
        Expr::Call { name, args } => {
            let args = args
                .into_iter()
                .map(|e| Box::new(annotate_expr(e, env).0))
                .collect();
            (
                CExpr::Call {
                    name: name.inner,
                    args,
                },
                e.ty.into(),
            )
        }
        Expr::PrefixIncr(_)
        | Expr::PrefixDecr(_)
        | Expr::PostfixIncr(_)
        | Expr::PostfixDecr(_) => unreachable!("invariant of typechecker"),
        Expr::Addr(e) => {
            let (e, _) = annotate_expr(*e, env);
            (CExpr::Addr(Box::new(e)), CType::Ptr)
        }
        Expr::Not(new_e) => {
            let (new_e, _) = annotate_expr(*new_e, env);
            (CExpr::Not(Box::new(new_e)), e.ty.into())
        }
        Expr::Neg(new_e) => {
            let (new_e, _) = annotate_expr(*new_e, env);
            (CExpr::Neg(Box::new(new_e)), e.ty.into())
        }
        Expr::Pos(new_e) => annotate_expr(*new_e, env),
        Expr::Op { op, lhs, rhs } => {
            let (lhs, ty1) = annotate_expr(*lhs, env);
            let (rhs, ty2) = annotate_expr(*rhs, env);
            match (op, ty1, ty2) {
                (BinOp::Sub, CType::Ptr, CType::Ptr) => (
                    CExpr::Op {
                        op: BinOp::Div,
                        lhs: Box::new(CExpr::Op {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }),
                        rhs: Box::new(CExpr::Int(8)),
                    },
                    e.ty.into(),
                ),
                (BinOp::Add, CType::Ptr, CType::Ptr)
                | (BinOp::Add | BinOp::Sub, _, CType::Ptr) => {
                    unreachable!()
                }
                (BinOp::Add | BinOp::Sub, CType::Ptr, _) => (
                    CExpr::Op {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(CExpr::Op {
                            op: BinOp::Mul,
                            lhs: Box::new(CExpr::Int(8)),
                            rhs: Box::new(rhs),
                        }),
                    },
                    e.ty.into(),
                ),
                _ => (
                    CExpr::Op {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    e.ty.into(),
                ),
            }
        }
        Expr::SizeOf(_) => (CExpr::Int(8), CType::Int),
    }
}

/// Return the instruction, annotate for compilation
/// and the size we need to allocate in the stack
/// for all variables
fn annotate_instr(
    instr: TypedInstr<Instr<TypeAnnotation>>,
    env: &mut Environment<Ident, DepthIdent>,
    // size of the stack of the function we're in
    stack: usize,
    // depth in function nesting
    depth: usize,
    // dependencies of each functions
    // where is each function declared ?
    deps: &mut Tree,
    parent_fun: Id,
    functions: &mut Vec<CFunDecl>,
) -> (CInstr, usize) {
    match instr.instr {
        Instr::EmptyInstr => (CInstr::EmptyInstr, 0),
        Instr::ExprInstr(e) => (CInstr::ExprInstr(annotate_expr(e, env).0), 0),
        Instr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let (then_branch, mut s) = annotate_instr(
                *then_branch,
                env,
                stack,
                depth,
                deps,
                parent_fun,
                functions,
            );
            let else_branch = if let Some(else_branch) = *else_branch {
                let else_branch = annotate_instr(
                    else_branch,
                    env,
                    stack,
                    depth,
                    deps,
                    parent_fun,
                    functions,
                );
                s = s.max(else_branch.1);
                Some(Box::new(else_branch.0))
            } else {
                None
            };
            (
                CInstr::If {
                    cond: annotate_expr(cond, env).0,
                    then_branch: Box::new(then_branch),
                    else_branch,
                    unique_id: format_span(&instr.span),
                },
                s,
            )
        }
        Instr::While { cond, body } => {
            let body = annotate_instr(
                *body, env, stack, depth, deps, parent_fun, functions,
            );
            (
                CInstr::While {
                    cond: annotate_expr(cond, env).0,
                    body: Box::new(body.0),
                    unique_id: format_span(&instr.span),
                },
                body.1,
            )
        }
        Instr::For {
            loop_var: Some(var_decl),
            cond,
            incr,
            body,
        } => {
            let span = instr.span.clone();
            let block = annotate_block(
                WithSpan {
                    inner: vec![
                        DeclOrInstr::Var(var_decl),
                        DeclOrInstr::Instr(TypedInstr {
                            instr: Instr::For {
                                loop_var: None,
                                cond,
                                incr,
                                body,
                            },
                            ..instr
                        }),
                    ],
                    span,
                },
                env,
		stack,
                depth,
                deps,
                parent_fun,
                functions,
            );
            let new_stack = block.size_of;
            (CInstr::Block(block), new_stack)
        }
        Instr::For {
            loop_var: None,
            cond,
            incr,
            body,
        } => {
            let body = annotate_instr(
                *body, env, stack, depth, deps, parent_fun, functions,
            );
            (
                CInstr::For {
                    cond: cond.map(|cond| annotate_expr(cond, env).0),
                    incr: incr
                        .into_iter()
                        .map(|incr| annotate_expr(incr, env).0)
                        .collect(),
                    body: Box::new(body.0),
                    unique_id: format_span(&instr.span),
                },
                body.1,
            )
        }
        Instr::Block(block) => {
            let block =
                annotate_block(block, env, stack, depth, deps, parent_fun, functions);
            let new_stack = block.size_of;
            (CInstr::Block(block), new_stack)
        }
        Instr::Return(e) => {
            (CInstr::Return(e.map(|e| annotate_expr(e, env).0)), 0)
        }
        Instr::Break => (CInstr::Break, 0),
        Instr::Continue => (CInstr::Continue, 0),
    }
}

fn annotate_block(
    block: WithSpan<Vec<DeclOrInstr<TypeAnnotation>>>,
    env: &mut Environment<Ident, DepthIdent>,
    previous_stack: usize,
    depth: usize,
    deps: &mut Tree,
    parent_fun: Id,
    functions: &mut Vec<CFunDecl>,
) -> CBlock {
    env.begin_frame();
    let mut instructions = Vec::new();
    let mut max_stack_size = 0;

    let mut stack = 0;

    for instr_or_decl in block.inner {
        match instr_or_decl {
            DeclOrInstr::Instr(instr) => {
                let (instr, new_stack) = annotate_instr(
                    instr, env, previous_stack+stack, depth, deps, parent_fun, functions,
                );
                instructions.push(instr);
                max_stack_size = max_stack_size.max(new_stack);
            }
            DeclOrInstr::Var(var_decl) => {
                stack += 8;
                env.insert(
                    var_decl.inner.name.inner,
                    DepthIdent {
                        depth,
                        offset: -((previous_stack + stack) as i64),
                    },
                );
                if let Some(value) = var_decl.inner.value {
                    // we know it allocates 0 extra space because it is an assignation
                    let ty = var_decl.inner.ty.inner;
                    let assign_instr = TypedInstr {
                        instr: Instr::ExprInstr(WithType {
                            inner: Expr::Assign {
                                lhs: Box::new(WithType {
                                    inner: Expr::Ident(
                                        var_decl.inner.name.clone(),
                                    ),
                                    ty,
                                    span: var_decl.inner.name.span.clone(),
                                }),
                                rhs: Box::new(value),
                            },
                            span: var_decl.span.clone(),
                            ty,
                        }),
                        span: var_decl.span,
                        loop_level: 0,
                        expected_return_type: var_decl.inner.ty.inner,
                    };
                    let (instr, _) = annotate_instr(
                        assign_instr,
                        env,
                        previous_stack+stack,
                        depth,
                        deps,
                        parent_fun,
                        functions,
                    );
                    instructions.push(instr);
                }
            }
            DeclOrInstr::Fun(fun_decl) => {
                annotate_fun(fun_decl, env, deps, parent_fun, functions);
            }
        }
    }

    env.end_frame();

    CBlock {
        block: instructions,
        size_of: stack + max_stack_size,
    }
}

fn annotate_fun(
    WithSpan {
        inner: fun_decl, ..
    }: WithSpan<FunDecl<TypeAnnotation>>,
    env: &mut Environment<Ident, DepthIdent>,
    deps: &mut Tree,
    parent_fun: Id,
    functions: &mut Vec<CFunDecl>,
) {
    let id = deps.add_child(parent_fun, fun_decl.name.inner);
    let mut params = Vec::new();
    env.begin_frame();
    for (_, name) in fun_decl.params.into_iter() {
        env.insert(
            name.inner,
            DepthIdent {
                depth: fun_decl.depth,
                offset: 8 * (3 + env.size_current_frame()) as i64,
            },
        );
        params.push(name.inner);
    }

    let code =
        annotate_block(fun_decl.code, env, 0, fun_decl.depth, deps, id, functions);
    env.end_frame();
    functions.push(CFunDecl {
        ty: fun_decl.ty.inner.into(),
        name: fun_decl.name.inner,
        params,
        code,
        depth: fun_decl.depth,
        id,
    })
}

pub fn annotate(file: TypedFile) -> CFile {
    let mut deps = Tree::new();
    let root = deps.root();
    // malloc
    deps.add_child(root, 0);
    // putchar
    deps.add_child(root, 1);

    let mut fun_decls = Vec::new();
    let mut env = Environment::new();

    for fun_decl in file.fun_decls {
        annotate_fun(fun_decl, &mut env, &mut deps, root, &mut fun_decls);
    }

    CFile {
        fun_decls,
        function_dependencies: deps,
    }
}
