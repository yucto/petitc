use beans::span::Span;

use super::{
    ast::*,
    environment::Environment,
    parsing::WithSpan,
    tree::{Id, Tree},
    typechecker::*,
    typing::Type,
};

#[derive(Clone)]
pub struct DepthIdent {
    pub depth: usize,
    pub offset: i64,
}

/// unique among all function names
pub type FunIdent = Ident;

pub struct ABlock {
    pub block: Vec<AInstr>,
    // Maximal stack size that the block will use
    pub size_of: usize,
}

#[derive(Clone, Copy)]
enum AType {
    Ptr,
    Other,
}

impl From<Type> for AType {
    fn from(ty: Type) -> Self {
        if ty.is_ptr() {
            AType::Ptr
        } else {
            AType::Other
        }
    }
}

pub struct AFile {
    /// List of all functions, not only the toplevel ones
    pub fun_decls: Vec<AFunDecl>,
    /// A tree whose nodes are functions, and
    /// a finction is parent of another one if
    /// the other one is declared in itself
    pub function_dependencies: Tree,
}

pub struct AFunDecl {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub code: ABlock,
    /// Store the depth of the function declaration
    pub depth: usize,
    /// Store the id in the dependency tree
    pub id: Id,
}

pub enum AExpr {
    Int(i64),
    Ident(DepthIdent),
    Deref(Box<AExpr>),
    Assign {
        lhs: Box<AExpr>,
        rhs: Box<AExpr>,
    },
    Call {
        name: FunIdent,
        args: Vec<Box<AExpr>>,
    },
    Addr(Box<AExpr>),
    Not(Box<AExpr>),
    Neg(Box<AExpr>),
    BinOp {
        op: ABinOp,
        lhs: Box<AExpr>,
        rhs: Box<AExpr>,
    },
    PrefixOp {
        op: AUnOp,
        e: Box<AExpr>,
        arg: i64,
    },
    PostfixOp {
        op: AUnOp,
        e: Box<AExpr>,
        arg: i64,
    },
}

pub enum AUnOp {
    Add,
    Sub,
}

#[rustfmt::skip]
pub enum ABinOp {
    Eq, NEq,
    Lt, Le, Gt, Ge,
    Add, Sub, Mul, Div, Mod,
    // unique identifiers for jump
    LAnd(String), LOr(String),
}

impl From<(BinOp, Span)> for ABinOp {
    fn from((op, span): (BinOp, Span)) -> Self {
        match op {
            BinOp::Eq => ABinOp::Eq,
            BinOp::NEq => ABinOp::NEq,
            BinOp::Lt => ABinOp::Lt,
            BinOp::Le => ABinOp::Le,
            BinOp::Gt => ABinOp::Gt,
            BinOp::Ge => ABinOp::Ge,
            BinOp::Add => ABinOp::Add,
            BinOp::Sub => ABinOp::Sub,
            BinOp::Mul => ABinOp::Mul,
            BinOp::Div => ABinOp::Div,
            BinOp::Mod => ABinOp::Mod,
            BinOp::LAnd => ABinOp::LAnd(format_loc(span.end())),
            BinOp::LOr => ABinOp::LOr(format_loc(span.end())),
        }
    }
}

pub enum AInstr {
    EmptyInstr,
    ExprInstr(AExpr),
    If {
        cond: AExpr,
        then_branch: Box<AInstr>,
        else_branch: Option<Box<AInstr>>,
        unique_id: String,
    },
    While {
        cond: AExpr,
        body: Box<AInstr>,
        unique_id: String,
    },
    For {
        cond: Option<AExpr>,
        incr: Vec<AExpr>,
        body: Box<AInstr>,
        unique_id: String,
    },
    Block(ABlock),
    Return(Option<AExpr>),
    Break,
    Continue,
}

fn annotate_expr(
    e: TypedExpr,
    env: &Environment<Ident, DepthIdent>,
) -> (AExpr, AType) {
    match e.inner {
        Expr::Int(n) => (AExpr::Int(n), AType::Other),
        Expr::True => (AExpr::Int(1), AType::Other),
        Expr::False => (AExpr::Int(0), AType::Other),
        Expr::Null => (AExpr::Int(1), AType::Ptr),
        Expr::Ident(id) => {
            let id = env.get(&id.inner).unwrap().1;
            (AExpr::Ident(id.clone()), e.ty.into())
        }
        Expr::Deref(new_e) => {
            let ty = e.ty;
            let (new_e, _) = annotate_expr(*new_e, env);
            (AExpr::Deref(Box::new(new_e)), ty.into())
        }
        Expr::Assign { lhs, rhs } => {
            let (lhs, ty) = annotate_expr(*lhs, env);
            let (rhs, _) = annotate_expr(*rhs, env);
            (
                AExpr::Assign {
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
                AExpr::Call {
                    name: name.inner,
                    args,
                },
                e.ty.into(),
            )
        }
        Expr::PrefixIncr(e) => {
            let (e, ty) = annotate_expr(*e, env);
            let arg = if let AType::Ptr = ty { 8 } else { 1 };
            (
                AExpr::PrefixOp {
                    op: AUnOp::Add,
                    e: Box::new(e),
                    arg,
                },
                ty,
            )
        }
        Expr::PrefixDecr(e) => {
            let (e, ty) = annotate_expr(*e, env);
            let arg = if let AType::Ptr = ty { 8 } else { 1 };
            (
                AExpr::PrefixOp {
                    op: AUnOp::Sub,
                    e: Box::new(e),
                    arg,
                },
                ty,
            )
        }
        Expr::PostfixIncr(e) => {
            let (e, ty) = annotate_expr(*e, env);
            let arg = if let AType::Ptr = ty { 8 } else { 1 };
            (
                AExpr::PostfixOp {
                    op: AUnOp::Add,
                    e: Box::new(e),
                    arg,
                },
                ty,
            )
        }
        Expr::PostfixDecr(e) => {
            let (e, ty) = annotate_expr(*e, env);
            let arg = if let AType::Ptr = ty { 8 } else { 1 };
            (
                AExpr::PostfixOp {
                    op: AUnOp::Sub,
                    e: Box::new(e),
                    arg,
                },
                ty,
            )
        }
        Expr::Addr(e) => {
            let (e, _) = annotate_expr(*e, env);
            (AExpr::Addr(Box::new(e)), AType::Ptr)
        }
        Expr::Not(new_e) => {
            let (new_e, _) = annotate_expr(*new_e, env);
            (AExpr::Not(Box::new(new_e)), e.ty.into())
        }
        Expr::Neg(new_e) => {
            let (new_e, _) = annotate_expr(*new_e, env);
            (AExpr::Neg(Box::new(new_e)), e.ty.into())
        }
        Expr::Pos(new_e) => annotate_expr(*new_e, env),
        Expr::Op { op, lhs, rhs } => {
            let span = lhs.span.clone();
            let (lhs, ty1) = annotate_expr(*lhs, env);
            let (rhs, ty2) = annotate_expr(*rhs, env);
            match (op, ty1, ty2) {
                (BinOp::Sub, AType::Ptr, AType::Ptr) => (
                    AExpr::BinOp {
                        op: ABinOp::Div,
                        lhs: Box::new(AExpr::BinOp {
                            op: (op, span).into(),
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }),
                        rhs: Box::new(AExpr::Int(8)),
                    },
                    e.ty.into(),
                ),
                (BinOp::Add, AType::Ptr, AType::Ptr)
                | (BinOp::Add | BinOp::Sub, _, AType::Ptr) => {
                    unreachable!()
                }
                (BinOp::Add | BinOp::Sub, AType::Ptr, _) => (
                    AExpr::BinOp {
                        op: (op, span).into(),
                        lhs: Box::new(lhs),
                        rhs: Box::new(AExpr::BinOp {
                            op: ABinOp::Mul,
                            lhs: Box::new(AExpr::Int(8)),
                            rhs: Box::new(rhs),
                        }),
                    },
                    e.ty.into(),
                ),
                _ => (
                    AExpr::BinOp {
                        op: (op, span).into(),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    e.ty.into(),
                ),
            }
        }
        Expr::SizeOf(_) => (AExpr::Int(8), AType::Other),
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
    functions: &mut Vec<AFunDecl>,
) -> (AInstr, usize) {
    match instr.instr {
        Instr::EmptyInstr => (AInstr::EmptyInstr, 0),
        Instr::ExprInstr(e) => (AInstr::ExprInstr(annotate_expr(e, env).0), 0),
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
                AInstr::If {
                    cond: annotate_expr(cond, env).0,
                    then_branch: Box::new(then_branch),
                    else_branch,
                    unique_id: format_loc(instr.span.start()),
                },
                s,
            )
        }
        Instr::While { cond, body } => {
            let body = annotate_instr(
                *body, env, stack, depth, deps, parent_fun, functions,
            );
            (
                AInstr::While {
                    cond: annotate_expr(cond, env).0,
                    body: Box::new(body.0),
                    unique_id: format_loc(instr.span.start()),
                },
                body.1,
            )
        }
        Instr::For {
            loop_var: Some(_), ..
        } => unreachable!(),
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
                AInstr::For {
                    cond: cond.map(|cond| annotate_expr(cond, env).0),
                    incr: incr
                        .into_iter()
                        .map(|incr| annotate_expr(incr, env).0)
                        .collect(),
                    body: Box::new(body.0),
                    unique_id: format_loc(instr.span.start()),
                },
                body.1,
            )
        }
        Instr::Block(block) => {
            let block = annotate_block(
                block, env, stack, depth, deps, parent_fun, functions,
            );
            let new_stack = block.size_of;
            (AInstr::Block(block), new_stack)
        }
        Instr::Return(e) => {
            (AInstr::Return(e.map(|e| annotate_expr(e, env).0)), 0)
        }
        Instr::Break => (AInstr::Break, 0),
        Instr::Continue => (AInstr::Continue, 0),
    }
}

fn annotate_block(
    block: WithSpan<Vec<DeclOrInstr<TypeAnnotation>>>,
    env: &mut Environment<Ident, DepthIdent>,
    previous_stack: usize,
    depth: usize,
    deps: &mut Tree,
    parent_fun: Id,
    functions: &mut Vec<AFunDecl>,
) -> ABlock {
    env.begin_frame();
    let mut instructions = Vec::new();
    let mut max_stack_size = 0;

    let mut stack = 0;

    for instr_or_decl in block.inner {
        match instr_or_decl {
            DeclOrInstr::Instr(instr) => {
                let (instr, new_stack) = annotate_instr(
                    instr,
                    env,
                    previous_stack + stack,
                    depth,
                    deps,
                    parent_fun,
                    functions,
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
                if let Some(_) = var_decl.inner.value {
                    unreachable!()
                }
            }
            DeclOrInstr::Fun(fun_decl) => {
                annotate_fun(fun_decl, env, deps, parent_fun, functions);
            }
        }
    }

    env.end_frame();

    ABlock {
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
    functions: &mut Vec<AFunDecl>,
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

    let code = annotate_block(
        fun_decl.code,
        env,
        0,
        fun_decl.depth,
        deps,
        id,
        functions,
    );
    env.end_frame();
    functions.push(AFunDecl {
        name: fun_decl.name.inner,
        params,
        code,
        depth: fun_decl.depth,
        id,
    })
}

pub fn annotate(file: TypedFile) -> AFile {
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

    AFile {
        fun_decls,
        function_dependencies: deps,
    }
}
