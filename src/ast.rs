use crate::typing::Type;

/// All strings are represented by an index in a Vec<String>
/// which should be passed along with the AST
pub type Ident = usize;

/// Annotate the AST
pub trait Annotation {
    type Ident;
    type Type;
    type WrapExpr<T>;
    type WrapInstr<T>;
    type WrapBlock<T>;
    type WrapFunDecl<T>;
    type WrapVarDecl<T>;
    type WrapElseBranch<T>;
}

struct DummyAnnotation;

impl Annotation for DummyAnnotation {
    type Ident = Ident;
    type Type = Type;
    type WrapExpr<T> = T;
    type WrapInstr<T> = T;
    type WrapBlock<T> = T;
    type WrapFunDecl<T> = T;
    type WrapVarDecl<T> = T;
    type WrapElseBranch<T> = Option<T>;
}

pub struct File<A: Annotation> {
    pub fun_decls: Vec<A::WrapFunDecl<FunDecl<A>>>,
}

pub struct FunDecl<A: Annotation> {
    pub ty: A::Type,
    pub name: A::Ident,
    pub params: Vec<(A::Type, A::Ident)>,
    /// Behave like an Instr::Block
    pub code: Block<A>,
    /// Store the depth of the function declaration
    pub depth: usize,
}

pub enum DeclOrInstr<A: Annotation> {
    Fun(A::WrapFunDecl<FunDecl<A>>),
    Var(A::WrapVarDecl<VarDecl<A>>),
    Instr(A::WrapInstr<Instr<A>>),
}

pub struct VarDecl<A: Annotation> {
    pub ty: A::Type,
    pub name: A::Ident,
    pub value: Option<A::WrapExpr<Expr<A>>>,
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Eq, NEq,
    Lt, Le, Gt, Ge,
    Add, Sub, Mul, Div, Mod,
    LAnd, LOr,
}

#[rustfmt::skip]
#[derive(Clone, Debug)]
pub enum Expr<A: Annotation> {
    Int(i64), True, False, Null,
    Ident(A::Ident),
    Deref(Box<A::WrapExpr<Expr<A>>>),
    // Assign { lhs, rhs } represents lhs = rhs
    Assign { lhs: Box<A::WrapExpr<Expr<A>>>, rhs: Box<A::WrapExpr<Expr<A>>> },
    Call { name: A::Ident, args: Vec<A::WrapExpr<Expr<A>>> },
    PrefixIncr(Box<A::WrapExpr<Expr<A>>>), PrefixDecr(Box<A::WrapExpr<Expr<A>>>),
    PostfixIncr(Box<A::WrapExpr<Expr<A>>>), PostfixDecr(Box<A::WrapExpr<Expr<A>>>),
    Addr(Box<A::WrapExpr<Expr<A>>>), Not(Box<A::WrapExpr<Expr<A>>>),
    Neg(Box<A::WrapExpr<Expr<A>>>), Pos(Box<A::WrapExpr<Expr<A>>>),
    // Op { op, lhs, rhs } represents lhs op rhs
    Op { op: BinOp, lhs: Box<A::WrapExpr<Expr<A>>>, rhs: Box<A::WrapExpr<Expr<A>>> },
    SizeOf(A::Type),
}

impl<A: Annotation> Expr<A> {
    pub const fn is_lvalue(&self) -> bool {
        matches!(self, Expr::Ident(_) | Expr::Deref(_))
    }
}

#[allow(type_alias_bounds)]
pub type Block<A: Annotation> = A::WrapBlock<Vec<DeclOrInstr<A>>>;

pub enum Instr<A: Annotation> {
    /// ;
    EmptyInstr,
    /// expr;
    ExprInstr(A::WrapExpr<Expr<A>>),
    /// if (cond) then_branch else else_branch
    If {
        cond: A::WrapExpr<Expr<A>>,
        then_branch: Box<A::WrapInstr<Instr<A>>>,
        /// Default to EmptyInstr if not present
        else_branch: Box<A::WrapElseBranch<Instr<A>>>,
    },
    /// while (cond) body
    While {
        cond: A::WrapExpr<Expr<A>>,
        body: Box<A::WrapInstr<Instr<A>>>,
    },
    /// for (loop_var; cond; incr) body
    For {
        loop_var: Option<A::WrapVarDecl<VarDecl<A>>>,
        cond: Option<A::WrapExpr<Expr<A>>>,
        incr: Vec<A::WrapExpr<Expr<A>>>,
        body: Box<A::WrapInstr<Instr<A>>>,
    },
    /// { body }
    Block(Block<A>),
    /// return (expr)?;
    Return(Option<A::WrapExpr<Expr<A>>>),
    /// break;
    Break,
    /// continue;
    Continue,
}
