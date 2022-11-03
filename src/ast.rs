/// All strings are represented by an index in a Vec<String>
/// which should be passed along with the AST
pub type Ident = usize;

pub struct File {
    pub fun_decls: Vec<FunDecl>,
}

pub struct FunDecl {
    pub ty: Type,
    pub name: Ident,
    pub params: Vec<(Type, Ident)>,
    pub code: Vec<DeclOrInstr>,
}

#[derive(PartialEq, Eq, Clone, Copy)]
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
        basis: BasisType::Void,
        indirection_count: 0,
    };

    pub const BOOL: Type = Type {
        basis: BasisType::Void,
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

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum BasisType {
    Void,
    Int,
    Bool,
}

pub enum DeclOrInstr {
    Fun(FunDecl),
    Var(VarDecl),
    Instr(Instr),
}

pub struct VarDecl {
    pub ty: Type,
    pub name: Ident,
    pub value: Option<Expr>,
}

#[rustfmt::skip]
pub enum BinOp {
    Eq, NEq,
    Lt, Le, Gt, Ge,
    Add, Sub, Mul, Div, Mod,
    BAnd, BOr,
}

#[rustfmt::skip]
pub enum Expr {
    Int(i64), True, False, Null,
    Ident(Ident),
    Deref(Box<Expr>),
    // Assign { lhs, rhs } represents lhs = rhs
    Assign { lhs: Box<Expr>, rhs: Box<Expr> },
    Call { name: Ident, args: Vec<Expr> },
    PrefixIncr(Box<Expr>), PrefixDecr(Box<Expr>),
    PostfixIncr(Box<Expr>), PostfixDecr(Box<Expr>),
    Addr(Box<Expr>), Not(Box<Expr>),
    Neg(Box<Expr>), Pos(Box<Expr>),
    // Op { op, lhs, rhs } represents lhs op rhs
    Op { op: BinOp, lhs: Box<Expr>, rhs: Box<Expr> },
    SizeOf(Type),
}

impl Expr {
    pub const fn is_lvalue(&self) -> bool {
        match self {
            Expr::Ident(_) | Expr::Deref(_) => true,
            _ => false,
        }
    }
}

pub enum Instr {
    /// ;
    EmptyInstr,
    /// expr;
    ExprInstr(Expr),
    /// if (cond) then_branch (else else_branch)?
    If {
        cond: Expr,
        then_branch: Box<Instr>,
        else_branch: Option<Box<Instr>>,
    },
    /// while (cond) body
    While { cond: Expr, body: Box<Instr> },
    /// for (loop_var; cond; incr) body
    For {
        loop_var: Option<VarDecl>,
        cond: Option<Expr>,
        incr: Vec<Expr>,
        body: Box<Instr>,
    },
    /// { body }
    Block(Vec<DeclOrInstr>),
    /// return (expr)?;
    Return(Option<Expr>),
    /// break;
    Break,
    /// continue;
    Continue,
}
