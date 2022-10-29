pub struct FunDecl {
    pub ty: Type,
    pub name: String,
    pub params: Vec<(Type, String)>,
    pub code: Vec<DeclOrInstr>,
}

pub struct Type {
    pub basis: BasisType,
    pub indirection_count: usize,
}

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
    pub name: String,
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
    Ident,
    Deref(Box<Expr>),
    // Index { lhs, rhs } represents lhs[rhs]
    Index { lsh: Box<Expr>, rhs: Box<Expr> },
    // Assign { lsh, rhs } represents lhs = rhs
    Assign { lsh: Box<Expr>, rhs: Box<Expr> },
    Call { name: String, args: Vec<Expr> },
    PrefixIncr(Box<Expr>), PreficDecr(Box<Expr>),
    PostfixIncr(Box<Expr>), PostfixDecr(Box<Expr>),
    Addr(Box<Expr>), Not(Box<Expr>),
    Neg(Box<Expr>), Pos(Box<Expr>),
    // Op { op, lhs, rhs } represents lhs op rhs
    Op { op: BinOp, lhs: Box<Expr>, rhs: Box<Expr> },
    SizeOf(Box<Expr>),
}

pub enum Instr {
    /// ;
    EmptyInstr,
    /// expr;
    ExprInstr,
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
