/// All strings are represented by an index in a Vec<String>
/// which should be passed along with the AST
pub type Ident = usize;

/// Annotate the AST
pub trait Annotation {
    type Ident;
    type Type;
    type WrapExpr<T>;
    type WrapInstr<T>;
    type WrapFunDecl<T>;
    type WrapVarDecl<T>;
    type WrapElseBranch<T>;
}

/// For developing purpose
/// All bounds shall
/// be replaced by a SpanAnnotation later
struct DummyAnnotation;

impl Annotation for DummyAnnotation {
    type Ident = Ident;
    type Type = Type;
    type WrapExpr<T> = T;
    type WrapInstr<T> = T;
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
    pub code: A::WrapInstr<Vec<DeclOrInstr<A>>>,
    /// Store wether it is declared at the toplevel or not
    pub toplevel: bool,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Type {
    pub basis: BasisType,
    pub indirection_count: usize,
}

impl Type {
    pub const fn ptr(self) -> Type {
        Type {
            indirection_count: self.indirection_count + 1,
            ..self
        }
    }

    pub const fn is_ptr(&self) -> bool {
        self.indirection_count > 0
    }

    pub const VOID: Type = Type {
        basis: BasisType::Void,
        indirection_count: 0,
    };

    pub const INT: Type = Type {
        basis: BasisType::Int,
        indirection_count: 0,
    };

    pub const BOOL: Type = Type {
        basis: BasisType::Bool,
        indirection_count: 0,
    };

    pub const VOID_PTR: Type = Type::VOID.ptr();

    pub fn is_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Type {
                    basis: basis1,
                    indirection_count: cnt1,
                },
                Type {
                    basis: basis2,
                    indirection_count: cnt2,
                },
            ) if basis1 == basis2 && cnt1 == cnt2 => true,
            (
                Type {
                    basis: BasisType::Bool | BasisType::Int,
                    indirection_count: 0,
                },
                Type {
                    basis: BasisType::Bool | BasisType::Int,
                    indirection_count: 0,
                },
            ) => true,
            (
                Type {
                    indirection_count: cnt1,
                    ..
                },
                Type {
                    basis,
                    indirection_count: cnt2,
                },
            )
            | (
                Type {
                    basis,
                    indirection_count: cnt1,
                },
                Type {
                    indirection_count: cnt2,
                    ..
                },
            ) if *basis == BasisType::Void && *cnt1 > 0 && *cnt2 > 0 => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.basis {
            BasisType::Int => write!(f, "int")?,
            BasisType::Bool => write!(f, "bool")?,
            BasisType::Void => write!(f, "void")?,
        }
        for _ in 0..self.indirection_count {
            write!(f, "*")?;
        }
        write!(f, "")
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BasisType {
    Void,
    Int,
    Bool,
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
#[derive(Clone, Copy)]
pub enum BinOp {
    Eq, NEq,
    Lt, Le, Gt, Ge,
    Add, Sub, Mul, Div, Mod,
    BAnd, BOr,
}

#[rustfmt::skip]
pub enum Expr<A: Annotation> {
    Int(i64), True, False, Null,
    Ident(Ident),
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
        match self {
            Expr::Ident(_) | Expr::Deref(_) => true,
            _ => false,
        }
    }
}

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
    Block(Vec<DeclOrInstr<A>>),
    /// return (expr)?;
    Return(Option<A::WrapExpr<Expr<A>>>),
    /// break;
    Break,
    /// continue;
    Continue,
}
