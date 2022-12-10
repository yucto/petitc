use std::rc::Rc;
use std::{collections::HashMap, path::Path};

use crate::ast::{
    Annotation, BinOp, Block, DeclOrInstr, Expr, File, FunDecl, Ident, Instr,
    VarDecl,
};
use crate::error::Result;
use crate::typechecker::PartialType;
use crate::typing::Type;

use beans::error::WarningSet;
use beans::include_parser;
use beans::parser::{Parser, Value, AST};
use beans::span::Span;
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
        $node.attributes.remove($key).unwrap_or_else(|| {
            error!("expected to find child {}, got\n{:?}", $key, $node)
        })
    };
}

macro_rules! node {
    ($node:expr) => {
        if let AST::Node {
            attributes, span, ..
        } = $node
        {
            AstNode { attributes, span }
        } else {
            error!("expected to find node");
        }
    };
}

macro_rules! value {
    ($node:expr => $key:literal) => {
        if let AST::Literal {
            value: Value::Str(result),
            ..
        } = get!($node, $key)
        {
            result
        } else {
            error!("expected to find value, got\n{:?}", $node);
        }
    };
    ($node:expr => .$key:literal) => {
        if let AST::Literal {
            value: Value::Str(result),
            span: Some(span),
        } = get!($node, $key)
        {
            (result, span)
        } else {
            error!("expected to find value, got\n{:?}", $node);
        }
    };
}

macro_rules! match_variant {
    (($node:expr) {
	$($variant:literal => $code:expr),* $(,)?
    }) => {
	let variant = value!($node => "variant");
	match &*variant {
	    $($variant => $code,)*
	    found_variant => error!("Unexpected variant {}", found_variant),
	}
    };
    (($node:expr) .{
	$($variant:literal => $code:expr),* $(,)?
    }) => {
	let variant = value!($node => "variant");
	let inner = match &*variant {
	    $($variant => $code,)*
	    found_variant => error!("Unexpected variant {}", found_variant),
	};
	WithSpan {
	    inner,
	    span: $node.span,
	}
    };
}

pub struct SpanAnnotation;

impl Annotation for SpanAnnotation {
    type Ident = WithSpan<Ident>;
    type Type = WithSpan<Type>;
    type WrapExpr<T> = WithSpan<T>;
    type WrapInstr<T> = WithSpan<T>;
    type WrapBlock<T> = WithSpan<T>;
    type WrapFunDecl<T> = WithSpan<T>;
    type WrapVarDecl<T> = WithSpan<T>;
    type WrapElseBranch<T> = Option<WithSpan<T>>;
}

#[derive(Clone, Debug)]
pub struct WithSpan<T> {
    pub inner: T,
    pub span: Span,
}

impl From<WithSpan<Type>> for WithSpan<PartialType> {
    fn from(WithSpan { inner, span }: WithSpan<Type>) -> Self {
        WithSpan {
            inner: inner.from_basic(),
            span,
        }
    }
}

impl<T> WithSpan<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self { inner, span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithSpan<U> {
        WithSpan {
            inner: f(self.inner),
            span: self.span,
        }
    }

    pub fn map_opt<U>(
        self,
        f: impl FnOnce(T) -> Option<U>,
    ) -> Option<WithSpan<U>> {
        Some(WithSpan {
            inner: f(self.inner)?,
            span: self.span,
        })
    }

    pub fn with_span(self, span: Span) -> Self {
        Self { span, ..self }
    }
}

#[derive(Debug)]
struct AstNode {
    attributes: HashMap<Rc<str>, AST>,
    span: Span,
}

fn read_spanned_ident(
    (ident, ident_span): (Rc<str>, Span),
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> WithSpan<Ident> {
    let actual_ident =
        *string_assoc.entry(ident.to_string()).or_insert_with(|| {
            string_store.push(ident.to_string());
            string_store.len() - 1
        });
    WithSpan {
        inner: actual_ident,
        span: ident_span,
    }
}

fn read_nonempty_list<T>(
    f: impl Fn(AST, usize, &mut Vec<String>, &mut HashMap<String, usize>) -> T,
    mut ast: AST,
    depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Vec<T> {
    let mut result = Vec::new();
    loop {
        let mut node = node!(ast);
        match_variant! {(node) {
            "Cons" => {
            result.push(f(get!(node, "head"), depth, string_store, string_assoc));
            ast = get!(node, "tail");
            },
            "Nil" => {
            result.push(f(get!(node, "head"), depth, string_store, string_assoc));
            break;
            },
        }}
    }
    result
}

fn read_option<T>(
    f: impl Fn(AST, usize, &mut Vec<String>, &mut HashMap<String, usize>) -> T,
    ast: AST,
    depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Option<T> {
    let mut node = node!(ast);
    match_variant! {(node) {
    "None" => None,
    "Some" => Some(f(get!(node, "value"), depth, string_store, string_assoc)),
    }}
}

fn read_list<T>(
    f: impl Fn(AST, usize, &mut Vec<String>, &mut HashMap<String, usize>) -> T,
    ast: AST,
    depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Vec<T> {
    let mut node = node!(ast);
    read_option(
        |ast, depth, string_store, string_assoc| {
            read_nonempty_list(&f, ast, depth, string_store, string_assoc)
        },
        get!(node, "value"),
        depth,
        string_store,
        string_assoc,
    )
    .unwrap_or_default()
}

fn read_else(
    ast: AST,
    depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> WithSpan<Instr<SpanAnnotation>> {
    let mut node = node!(ast);
    read_statement(get!(node, "else"), depth, string_store, string_assoc)
        .with_span(node.span)
}

fn read_statement(
    ast: AST,
    depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> WithSpan<Instr<SpanAnnotation>> {
    let mut node = node!(ast);
    match_variant! {(node) .{
    "None" => Instr::EmptyInstr,
    "Regular" => Instr::ExprInstr(read_expr(get!(node, "stmt"), depth, string_store, string_assoc)),
    "If" => Instr::If {
        cond: read_expr(get!(node, "condition"), depth, string_store, string_assoc),
        then_branch: Box::new(read_statement(get!(node, "then"), depth, string_store, string_assoc)),
        else_branch: Box::new(read_option(
            read_else,
            get!(node, "else"),
            depth,
            string_store,
            string_assoc
        )),
    },
    "While" => Instr::While {
        cond: read_expr(get!(node, "condition"), depth, string_store, string_assoc),
        body: Box::new(read_statement(get!(node, "body"), depth, string_store, string_assoc)),
    },
    "For" => Instr::For {
        loop_var: read_option(read_variable_declaration, get!(node, "init"), depth, string_store, string_assoc),
        cond: read_option(read_expr, get!(node, "test"), depth, string_store, string_assoc),
        incr: read_list(read_expr, get!(node, "step"), depth, string_store, string_assoc),
        body: Box::new(read_statement(get!(node, "body"), depth, string_store, string_assoc)),
    },
    "Block" => Instr::Block(read_block(get!(node, "stmts"), depth, string_store, string_assoc)),
    "Return" => Instr::Return(read_option(read_expr, get!(node, "value"), depth, string_store, string_assoc)),
    "Break" => Instr::Break,
    "Continue" => Instr::Continue,
    }}
}

fn read_int(ast: AST) -> Expr<SpanAnnotation> {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Int" => Expr::Int(value!(node => "value").parse().unwrap()),
    "Char" => Expr::Int(value!(node => "value").as_bytes()[0].into()),
    }}
}

fn read_bool(ast: AST) -> Expr<SpanAnnotation> {
    let mut node = node!(ast);
    match_variant! {(node) {
    "True" => Expr::True,
    "False" => Expr::False,
    }}
}

fn read_op(ast: AST) -> BinOp {
    use BinOp::*;
    let mut node = node!(ast);
    match_variant! {(node) {
    "Mul" => Mul,
    "Div" => Div,
    "Mod" => Mod,
    "Add" => Add,
    "Sub" => Sub,
    "Lt" => Lt,
    "Gt" => Gt,
    "Geq" => Ge,
    "Leq" => Le,
    "Equal" => Eq,
    "NotEqual" => NEq,
    "And" => LAnd,
    "Or" => LOr,
    }}
}

fn read_expr(
    ast: AST,
    _depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, Ident>,
) -> WithSpan<Expr<SpanAnnotation>> {
    let mut node = node!(ast);
    match_variant! {(node) .{
    "Int" => read_int(get!(node, "value")),
    "Bool" => read_bool(get!(node, "value")),
    "Null" => Expr::Null,
    "Through" => return read_expr(get!(node, "this"), _depth, string_store, string_assoc),
    "Not" => Expr::Not(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Incrl" => Expr::PrefixIncr(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Incrr" => Expr::PostfixIncr(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Decrl" => Expr::PrefixDecr(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Decrr" => Expr::PostfixDecr(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Borrow" => Expr::Addr(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Deref" => Expr::Deref(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Plus" => Expr::Pos(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Minus" => Expr::Neg(Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
    ))),
    "Ident" => Expr::Ident(read_spanned_ident(
        value!(node => ."value"),
        string_store,
        string_assoc,
    )),
    "Sizeof" => Expr::SizeOf(read_type(get!(node, "type"))),
    "Call" => Expr::Call {
        name: read_spanned_ident(
        value!(node => ."name"),
        string_store,
        string_assoc,
    ),
        args: read_list(
        read_expr,
        get!(node, "args"),
        _depth,
        string_store,
        string_assoc,
        ),
    },
    "BinOp" => Expr::Op {
        op: read_op(get!(node, "op")),
        lhs: Box::new(read_expr(
        get!(node, "left"),
        _depth,
        string_store,
        string_assoc,
        )),
        rhs: Box::new(read_expr(
        get!(node, "right"),
        _depth,
        string_store,
        string_assoc,
        )),
    },
    "Assign" => Expr::Assign {
        lhs: Box::new(read_expr(
        get!(node, "key"),
        _depth,
        string_store,
        string_assoc,
        )),
        rhs: Box::new(read_expr(
        get!(node, "value"),
        _depth,
        string_store,
        string_assoc,
        )),
    },
    }}
}

fn read_definition(
    ast: AST,
    _depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> WithSpan<Expr<SpanAnnotation>> {
    let mut node = node!(ast);
    read_expr(get!(node, "value"), _depth, string_store, string_assoc)
    // .with_span(node.span)
}

fn read_variable_declaration(
    ast: AST,
    _depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> WithSpan<VarDecl<SpanAnnotation>> {
    let mut node = node!(ast);
    let var_decl = VarDecl {
        ty: read_type(get!(node, "type")),
        name: read_spanned_ident(
            value!(node => ."name"),
            string_store,
            string_assoc,
        ),
        value: read_option(
            read_definition,
            get!(node, "value"),
            _depth,
            string_store,
            string_assoc,
        ),
    };
    WithSpan {
        inner: var_decl,
        span: node.span,
    }
}

fn read_declaration_or_statement(
    ast: AST,
    depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> DeclOrInstr<SpanAnnotation> {
    let mut node = node!(ast);
    match_variant! {(node) {
    "Function" => DeclOrInstr::Fun(read_fun_decl(
        get!(node, "declaration"), depth + 1, string_store, string_assoc
    )),
    "Declaration" => DeclOrInstr::Var(read_variable_declaration(
        get!(node, "declaration"),
        depth,
        string_store,
        string_assoc,
    )),
    "Statement" => DeclOrInstr::Instr(read_statement(
        get!(node, "stmt"), depth, string_store, string_assoc
    )),
    }}
}

fn read_block(
    ast: AST,
    depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> Block<SpanAnnotation> {
    let mut node = node!(ast);
    let block = read_list(
        read_declaration_or_statement,
        get!(node, "stmts"),
        depth,
        string_store,
        string_assoc,
    );
    WithSpan {
        inner: block,
        span: node.span,
    }
}

fn read_type(ast: AST) -> WithSpan<Type> {
    let mut node = node!(ast);
    match_variant! {(node) .{
        "Void" => Type::VOID,
        "Int" => Type::INT,
        "Bool" => Type::BOOL,
        "Pointer" => read_type(get!(node, "pointed")).inner.ptr()
    }}
}

fn read_typed_param(
    ast: AST,
    _depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> (WithSpan<Type>, WithSpan<Ident>) {
    let mut node = node!(ast);
    (
        read_type(get!(node, "type")),
        read_spanned_ident(value!(node => ."name"), string_store, string_assoc),
    )
}

fn read_fun_decl(
    ast: AST,
    depth: usize,
    string_store: &mut Vec<String>,
    string_assoc: &mut HashMap<String, usize>,
) -> WithSpan<FunDecl<SpanAnnotation>> {
    let mut node = node!(ast);
    let fun_decl = FunDecl {
        ty: read_type(get!(node, "rettype")),
        name: read_spanned_ident(
            value!(node => ."name"),
            string_store,
            string_assoc,
        ),
        params: read_list(
            read_typed_param,
            get!(node, "args"),
            depth,
            string_store,
            string_assoc,
        ),
        code: read_block(
            get!(node, "block"),
            depth,
            string_store,
            string_assoc,
        ),
        depth,
    };
    WithSpan {
        inner: fun_decl,
        span: node.span,
    }
}

fn read_file(ast: AST) -> (File<SpanAnnotation>, Vec<String>) {
    let mut node = node!(ast);
    let mut string_store = vec!["malloc".into(), "putchar".into()];
    let mut string_assoc = HashMap::new();
    string_assoc.insert("malloc".into(), 0);
    string_assoc.insert("putchar".into(), 1);
    (
        File {
            fun_decls: read_list(
                read_fun_decl,
                get!(node, "decls"),
                0,
                &mut string_store,
                &mut string_assoc,
            ),
        },
        string_store,
    )
}

/// Return a file and a string vector v such that
/// v[id] represent the ident id
/// v[0] is guaranteed to be malloc
/// and v[1] to be putchar
pub(crate) fn parse_to_ast(
    source: &Path,
) -> Result<(File<SpanAnnotation>, Vec<String>)> {
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
