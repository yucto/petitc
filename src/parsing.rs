use std::{collections::HashMap, path::Path};

use crate::ast::{
    BasisType, BinOp, DeclOrInstr, Expr, File, FunDecl, Instr, Type, VarDecl,
};

use crate::error::Result;
use beans::error::WarningSet;
use beans::include_parser;
use beans::parser::{Parser, Value, AST};
use beans::stream::StringStream;

macro_rules! error {
    ($($tok:tt)*) => {{
	eprintln!(
	    "File \"{}\", line {}, character {}:\nInternal error:",
	    std::file!(),
	    std::line!(),
	    std::column!(),
	);
	eprintln!($($tok)*);
	::std::process::exit(2);
    }};
}

macro_rules! get {
    ($node:expr, $key:literal) => {
        $node.remove($key).unwrap_or_else(|| {
            error!("expected to find child {}, got\n{:?}", $key, $node)
        })
    };
}

macro_rules! node {
    ($node:expr) => {
        if let AST::Node { attributes, .. } = $node {
            attributes
        } else {
            error!("expected to find node");
        }
    };
}

macro_rules! value {
    ($node:expr, $key:literal) => {
        if let AST::Literal(Value::Str(result)) = get!($node, $key) {
            result
        } else {
            error!("expected to find value, got\n{:?}", $node);
        }
    };
}

macro_rules! match_variant {
    (($node:expr) {
	$($variant:literal => $code:expr),* $(,)?
    }) => {
	let variant = value!($node, "variant");
	match &*variant {
	    $($variant => $code,)*
	    found_variant => error!("Unexpected variant {}", found_variant),
	}
    };
}

fn read_ident(
    ident: String,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> usize {
    *string_assoc.entry(ident.clone()).or_insert_with(|| {
        string_store.push(ident);
        string_store.len()
    })
}

fn read_nonempty_list<T>(
    f: impl Fn(AST, &mut Vec<String>, &mut HashMap<String, usize>) -> T,
    mut ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Vec<T> {
    let mut result = Vec::new();
    loop {
        let mut node = node!(ast);
        match_variant! {(node) {
            "Cons" => {
            result.push(f(get!(node, "head"), string_store, string_assoc));
            ast = get!(node, "tail");
            },
            "Nil" => {
            result.push(f(get!(node, "head"), string_store, string_assoc));
            break;
            },
        }}
    }
    result
}

fn read_option<T>(
    f: impl Fn(AST, &mut Vec<String>, &mut HashMap<String, usize>) -> T,
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Option<T> {
    let mut node = node!(ast);
    match_variant! {(node) {
    "None" => None,
    "Some" => Some(f(get!(node, "value"), string_store, string_assoc)),
    }}
}

fn read_list<T>(
    f: impl Fn(AST, &mut Vec<String>, &mut HashMap<String, usize>) -> T,
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Vec<T> {
    let mut node = node!(ast);
    read_option(
        |ast, string_store, string_assoc| {
            read_nonempty_list(&f, ast, string_store, string_assoc)
        },
        get!(node, "value"),
        string_store,
        string_assoc,
    )
    .unwrap_or_default()
}

fn read_else(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Instr {
    let mut node = node!(ast);
    read_statement(get!(node, "else"), string_store, string_assoc)
}

fn read_statement(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Instr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "None" => Instr::EmptyInstr,
    "Regular" => Instr::ExprInstr(read_expr(get!(node, "stmt"),string_store, string_assoc)),
    "If" => Instr::If {
        cond: read_expr(get!(node, "condition"),string_store, string_assoc),
        then_branch: Box::new(read_statement(get!(node, "then"), string_store, string_assoc)),
        else_branch: read_option(
            |ast, string_store, string_assoc| {
                Box::new(read_else(ast, string_store, string_assoc))
            },
            get!(node, "else"),
            string_store,
            string_assoc
        ),
    },
    "While" => Instr::While {
        cond: read_expr(get!(node, "condition"),string_store, string_assoc),
        body: Box::new(read_statement(get!(node, "body"), string_store, string_assoc)),
    },
    "For" => Instr::For {
        loop_var: read_option(read_variable_declaration, get!(node, "init"), string_store, string_assoc),
        cond: read_option(read_expr, get!(node, "test"), string_store, string_assoc),
        incr: read_list(read_expr, get!(node, "step"), string_store, string_assoc),
        body: Box::new(read_statement(get!(node, "body"), string_store, string_assoc)),
    },
    "Block" => Instr::Block(read_block(get!(node, "stmts"),string_store, string_assoc)),
    "Return" => Instr::Return(read_option(read_expr, get!(node, "value"),string_store, string_assoc)),
    "Break" => Instr::Break,
    "Continue" => Instr::Continue,
    }}
}

fn read_int(ast: AST) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Int" => Expr::Int(value!(node, "value").parse().unwrap()),
    "Char" => Expr::Int(value!(node, "value").as_bytes()[0].into()),
    }}
}

fn read_bool(ast: AST) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "True" => Expr::True,
    "False" => Expr::False,
    }}
}

fn read_literal_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Int" => read_int(get!(node, "value")),
    "Bool" => read_bool(get!(node, "value")),
    "Null" => Expr::Null,
    "Through" => read_expr(get!(node, "this"),string_store, string_assoc),
    }}
}

fn read_unary_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Not" => Expr::Not(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Incrl" => Expr::PrefixIncr(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Incrr" => Expr::PostfixIncr(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Decrl" => Expr::PrefixDecr(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Decrr" => Expr::PostfixDecr(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Borrow" => Expr::Addr(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Deref" => Expr::Deref(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Plus" => Expr::Pos(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Minus" => Expr::Neg(Box::new(read_unary_expr(get!(node, "value"),string_store, string_assoc))),
    "Ident" => Expr::Ident(read_ident(value!(node, "value").to_string(), string_store, string_assoc)),
    "Sizeof" => Expr::SizeOf(read_type(get!(node, "type"))),
    "Call" => Expr::Call {
            name: read_ident(value!(node, "name").to_string(), string_store, string_assoc),
            args: read_list(read_expr, get!(node, "args"), string_store, string_assoc),
    },
    "Index" => Expr::Deref(Box::new(Expr::Op {
        op: BinOp::Add,
        lhs: Box::new(read_unary_expr(get!(node, "array"), string_store, string_assoc)),
        rhs: Box::new(read_expr(get!(node, "index"), string_store, string_assoc)),
    })),
    "Through" => read_literal_expr(get!(node, "this"), string_store, string_assoc),
    }}
}

fn read_mdm_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Mul" => Expr::Op {
        op: BinOp::Mul,
        lhs: Box::new(read_unary_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_mdm_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Div" => Expr::Op {
        op: BinOp::Div,
        lhs: Box::new(read_unary_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_mdm_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Mod" => Expr::Op {
        op: BinOp::Mod,
        lhs: Box::new(read_unary_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_mdm_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Through" => read_unary_expr(get!(node, "this"), string_store, string_assoc),
    }}
}

fn read_pm_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Add" => Expr::Op {
        op: BinOp::Add,
        lhs: Box::new(read_mdm_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_pm_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Sub" => Expr::Op {
        op: BinOp::Sub,
        lhs: Box::new(read_mdm_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_pm_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Through" => read_mdm_expr(get!(node, "this"), string_store, string_assoc),
    }}
}

fn read_cmp_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Lt" => Expr::Op {
        op: BinOp::Lt,
        lhs: Box::new(read_pm_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_cmp_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Gt" => Expr::Op {
        op: BinOp::Gt,
        lhs: Box::new(read_pm_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_cmp_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Geq" => Expr::Op {
        op: BinOp::Ge,
        lhs: Box::new(read_pm_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_cmp_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Leq" => Expr::Op {
        op: BinOp::Le,
        lhs: Box::new(read_pm_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_cmp_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Through" => read_pm_expr(get!(node, "this"), string_store, string_assoc),
    }}
}

fn read_eq_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Equal" => Expr::Op {
        op: BinOp::Eq,
        lhs: Box::new(read_cmp_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_eq_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "NotEqual" => Expr::Op {
        op: BinOp::NEq,
        lhs: Box::new(read_cmp_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_eq_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Through" => read_cmp_expr(get!(node, "this"), string_store, string_assoc),
    }}
}

fn read_and_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "And" => Expr::Op {
        op: BinOp::BAnd,
        lhs: Box::new(read_eq_expr(get!(node, "left"), string_store, string_assoc)),
        rhs: Box::new(read_and_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Through" => read_eq_expr(get!(node, "this"), string_store, string_assoc),
    }}
}

fn read_or_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Or" => Expr::Op {
            op: BinOp::BOr,
            lhs: Box::new(read_and_expr(get!(node, "left"), string_store, string_assoc)),
            rhs: Box::new(read_or_expr(get!(node, "right"), string_store, string_assoc)),
    },
    "Through" => read_and_expr(get!(node, "this"), string_store, string_assoc),
    }}
}

fn read_equal_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Assign" => Expr::Assign {
        lhs: Box::new(read_or_expr(
                get!(node, "key"), string_store, string_assoc
            )),
            rhs: Box::new(read_equal_expr(
                get!(node, "value"), string_store, string_assoc
            )),
    },
    "Through" => read_or_expr(get!(node, "this"), string_store, string_assoc),
    }}
}

fn read_expr(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    if let AST::Node { mut attributes, .. } = ast {
        read_equal_expr(
            attributes.remove("this").unwrap(),
            string_store,
            string_assoc,
        )
    } else {
        panic!();
    }
}

fn read_definition(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Expr {
    let mut node = node!(ast);
    read_expr(get!(node, "value"), string_store, string_assoc)
}

fn read_variable_declaration(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> VarDecl {
    let mut node = node!(ast);
    VarDecl {
        ty: read_type(get!(node, "type")),
        name: read_ident(
            value!(node, "name").to_string(),
            string_store,
            string_assoc,
        ),
        value: read_option(
            read_definition,
            get!(node, "value"),
            string_store,
            string_assoc,
        ),
    }
}

fn read_declaration_or_statement(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> DeclOrInstr {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Function" => DeclOrInstr::Fun(read_fun_decl(
        get!(node, "declaration"), string_store, string_assoc
    )),
    "Declaration" => DeclOrInstr::Var(read_variable_declaration(
        get!(node, "declaration"),
        string_store,
        string_assoc,
    )),
    "Statement" => DeclOrInstr::Instr(read_statement(
        get!(node, "stmt"), string_store, string_assoc
    )),
    }}
}

fn read_block(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Vec<DeclOrInstr> {
    let mut node = node!(ast);
    read_list(
        read_declaration_or_statement,
        get!(node, "stmts"),
        string_store,
        string_assoc,
    )
}

fn read_type(ast: AST) -> Type {
    enum TransparentType {
        Void,
        Int,
        Bool,
        Pointer(Box<TransparentType>),
    }

    impl From<TransparentType> for Type {
        fn from(ty: TransparentType) -> Type {
            match ty {
                TransparentType::Void => Type {
                    basis: BasisType::Void,
                    indirection_count: 0,
                },
                TransparentType::Bool => Type {
                    basis: BasisType::Bool,
                    indirection_count: 0,
                },
                TransparentType::Int => Type {
                    basis: BasisType::Int,
                    indirection_count: 0,
                },
                TransparentType::Pointer(ty) => {
                    let mut a: Type = Type::from(*ty);
                    a.indirection_count += 1;
                    a
                }
            }
        }
    }

    fn aux(ast: AST) -> TransparentType {
        let mut node = node!(ast);
        match_variant! {(node) {
            "Void" => TransparentType::Void,
                "Int" => TransparentType::Int,
                "Bool" => TransparentType::Bool,
                "Pointer" => TransparentType::Pointer(Box::new(aux(
                    get!(node, "pointed"),
                ))),
        }}
    }
    aux(ast).into()
}

fn read_typed_param(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> (Type, usize) {
    let mut node = node!(ast);
    (
        read_type(get!(node, "type")),
        read_ident(
            value!(node, "name").to_string(),
            string_store,
            string_assoc,
        ),
    )
}

fn read_fun_decl(
    ast: AST,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> FunDecl {
    let mut node = node!(ast);
    FunDecl {
        ty: read_type(get!(node, "rettype")),
        name: read_ident(
            value!(node, "name").to_string(),
            string_store,
            string_assoc,
        ),
        params: read_list(
            read_typed_param,
            get!(node, "args"),
            string_store,
            string_assoc,
        ),
        code: read_block(get!(node, "block"), string_store, string_assoc),
    }
}

fn read_file(ast: AST) -> (File, Vec<String>) {
    let mut node = node!(ast);
    let mut string_store = Vec::new();
    let mut string_assoc = HashMap::new();
    (
        File {
            fun_decls: read_list(
                read_fun_decl,
                get!(node, "decls"),
                &mut string_store,
                &mut string_assoc,
            ),
        },
        Vec::new(),
    )
}

pub(crate) fn parse_to_ast(source: &Path) -> Result<(File, Vec<String>)> {
    let mut warnings = WarningSet::empty();
    let (lexer, parser) = include_parser!(
    lexer => compiled "../gmrs/petitc.clx",
    parser => compiled "../gmrs/petitc.cgr",
    )?
    .unpack_into(&mut warnings);
    let mut input = StringStream::from_file(source)?.unpack_into(&mut warnings);
    let ast = parser
        .parse(&mut lexer.lex(&mut input))?
        .unpack_into(&mut warnings);
    Ok(read_file(ast.tree))
}
