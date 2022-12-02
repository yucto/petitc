use write_x86_64::{traits::Writable, *};

use std::{collections::HashMap, fs::File as StdFile, io::Write, path::Path};

use super::{
    ast::*,
    error::Result,
    tree::{Id, Tree},
    typechecker::*,
};

/// Push the addr of the given lvalue in %rax
fn push_addr(
    e: TypedExpr,
    asm: &mut Text,
    variables: &mut HashMap<usize, i64>,
    name_of: &[String],
    deps: &Tree,
    fun_id: Id,
) {
    match e.inner {
        Expr::Ident(name) => {
            *asm += movq(reg!(RBP), reg!(RAX));
            for _ in name.depth..deps.depth(fun_id) {
                *asm += movq(addr!(16, RAX), reg!(RAX));
            }
            *asm += addq(immq(variables[&name.inner]), reg!(RAX));
        }
        Expr::Deref(e) => {
            compile_expr(*e, asm, variables, name_of, deps, fun_id)
        }
        _ => unreachable!(),
    }
}

/// Push the expression in %rax
fn compile_expr(
    e: TypedExpr,
    asm: &mut Text,
    variables: &mut HashMap<usize, i64>,
    name_of: &[String],
    deps: &Tree,
    fun_id: Id,
) {
    match e.inner {
        Expr::Int(x) => *asm += movq(immq(x), reg!(RAX)),
        Expr::True => *asm += movq(immq(1), reg!(RAX)),
        Expr::False | Expr::Null => *asm += movq(immq(0), reg!(RAX)),
        Expr::Ident(name) => {
            *asm += movq(addr!(variables[&name.inner], RBP), reg!(RAX))
        }
        Expr::Deref(e) => {
            compile_expr(*e, asm, variables, name_of, deps, fun_id);
            *asm += movq(addr!(RAX), reg!(RAX));
        }
        Expr::Assign { lhs, rhs } => {
            push_addr(*lhs, asm, variables, name_of, deps, fun_id);
            *asm += pushq(reg!(RAX));
            compile_expr(*rhs, asm, variables, name_of, deps, fun_id);
            *asm += popq(RBX);
            *asm += movq(reg!(RAX), addr!(RBX));
        }
        Expr::Call { name, mut args } => {
            fn call_extern(asm: &mut Text, label: reg::Label) {
                *asm += movq(reg!(RSP), reg!(RBX)); // Saving the stack
                *asm += andq(immq(-16), reg!(RSP)); // Align the stack
                *asm += movq(reg!(RAX), reg!(RDI)); // Move the argument

                *asm += call(label);

                *asm += movq(reg!(RBX), reg!(RSP)); // Restoring the stack
            }

            // malloc
            if name.inner == 0 {
                compile_expr(
                    args.remove(0),
                    asm,
                    variables,
                    name_of,
                    deps,
                    fun_id,
                );

                call_extern(asm, reg::Label::from_str("malloc".to_string()));
            }
            // putchar
            else if name.inner == 1 {
                compile_expr(
                    args.remove(0),
                    asm,
                    variables,
                    name_of,
                    deps,
                    fun_id,
                );

                call_extern(asm, reg::Label::from_str("putchar".to_string()));
            } else {
                let arity = args.len();
                for arg in args.into_iter().rev() {
                    compile_expr(arg, asm, variables, name_of, deps, fun_id);
                    *asm += pushq(reg!(RAX));
                }
                // Safe because of the typechecking
                // TODO: there is currently a bug here
                let height =
                    deps.lca(fun_id, deps.find_by_name(name.inner).unwrap());
                *asm += movq(reg!(RBP), reg!(RAX));
                for _ in 0..height {
                    *asm += movq(addr!(16, RAX), reg!(RAX));
                }
                *asm += pushq(reg!(RAX));
                // return is in %rax
                *asm += call(reg::Label::from_str(name_of[name.inner].clone()));
                // pop the parent %rbp
                *asm += popq(RBX);
                // pop args
                for _ in 0..arity {
                    *asm += pushq(reg!(RBX));
                }
            }
        }
        Expr::PrefixIncr(_)
        | Expr::PrefixDecr(_)
        | Expr::PostfixIncr(_)
        | Expr::PostfixDecr(_) => unreachable!("postcondition of typechecker"),
        Expr::Addr(e) => push_addr(*e, asm, variables, name_of, deps, fun_id),
        Expr::Not(e) => {
            compile_expr(*e, asm, variables, name_of, deps, fun_id);
            *asm += movq(reg!(RAX), reg!(RBX));
            *asm += movq(immq(0), reg!(RAX));
            *asm += cmpq(reg!(RBX), immq(0));
            *asm += set(instr::Cond::Z, reg!(AL));
        }
        Expr::Neg(e) => {
            compile_expr(*e, asm, variables, name_of, deps, fun_id);
            *asm += negq(reg!(RAX));
        }
        Expr::Pos(e) => {
            compile_expr(*e, asm, variables, name_of, deps, fun_id);
        }
        Expr::Op { op, lhs, rhs } => {
            compile_expr(*lhs, asm, variables, name_of, deps, fun_id);
            *asm += pushq(reg!(RAX));
            compile_expr(*rhs, asm, variables, name_of, deps, fun_id);
            *asm += movq(reg!(RAX), reg!(RBX));
            *asm += popq(RAX);
            match op {
                BinOp::Eq => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += set(instr::Cond::Z, reg!(AL));
		    *asm += movzbq(reg!(AL), RAX);
                }
                BinOp::NEq => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += set(instr::Cond::Z, reg!(AL));
		    *asm += movzbq(reg!(AL), RAX);
                    *asm += notq(reg!(RAX));
                }
                BinOp::Lt => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += set(instr::Cond::L, reg!(AL));
		    *asm += movzbq(reg!(AL), RAX);
                }
                BinOp::Le => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += set(instr::Cond::LE, reg!(AL));
		    *asm += movzbq(reg!(AL), RAX);
                }
                BinOp::Gt => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += set(instr::Cond::G, reg!(AL));
		    *asm += movzbq(reg!(AL), RAX);
                }
                BinOp::Ge => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += set(instr::Cond::GE, reg!(AL));
		    *asm += movzbq(reg!(AL), RAX);
                }
                BinOp::Add => {
                    *asm += addq(reg!(RBX), reg!(RAX));
                }
                BinOp::Sub => {
                    *asm += subq(reg!(RBX), reg!(RAX));
                }
                BinOp::Mul => {
                    *asm += imulq(reg!(RBX), reg!(RAX));
                }
                BinOp::Div => {
		    *asm += movq(immq(0), reg!(RDX));
                    *asm += divq(reg!(RBX));
                }
                BinOp::Mod => {
		    *asm += movq(immq(0), reg!(RDX));
                    *asm += divq(reg!(RBX));
                    *asm += movq(reg!(RDX), reg!(RAX));
                }
                BinOp::BAnd => {
                    *asm += andq(reg!(RBX), reg!(RAX));
                }
                BinOp::BOr => {
                    *asm += orq(reg!(RBX), reg!(RAX));
                }
            }
        }
        Expr::SizeOf(_) => *asm += movq(immq(8), reg!(RAX)),
    }
}

/// Compile an instruction
/// Should leave the stack pointer unchanged at the end of the instruction
fn compile_instr(
    instr: TypedInstr<Instr<TypeAnnotation>>,
    asm: &mut Text,
    // loop start/end
    current_loop: Option<(&reg::Label, &reg::Label)>,
    variables: &mut HashMap<usize, i64>,
    name_of: &[String],
    deps: &Tree,
    fun_id: Id,
    fun_stack: &mut Vec<TypedFunDecl<FunDecl<TypeAnnotation>>>,
) {
    match instr.instr {
        Instr::EmptyInstr => (),
        Instr::ExprInstr(e) => {
            compile_expr(e, asm, variables, name_of, deps, fun_id)
        }
        Instr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            compile_expr(cond, asm, variables, name_of, deps, fun_id);
            let else_label =
                new_label(&format!("else{}", format_span(&instr.span)));
            let endif_label =
                new_label(&format!("endif{}", format_span(&instr.span)));
            if else_branch.is_some() {
                *asm += testq(reg!(RAX), reg!(RAX));
                *asm += jz(else_label.clone());
            }
            compile_instr(
                *then_branch,
                asm,
                current_loop,
                variables,
                name_of,
                deps,
                fun_id,
                fun_stack,
            );
            *asm += jmp(endif_label.clone());
            if let Some(else_branch) = *else_branch {
                *asm += Text::label(else_label);
                compile_instr(
                    else_branch,
                    asm,
                    current_loop,
                    variables,
                    name_of,
                    deps,
                    fun_id,
                    fun_stack,
                );
            }
            *asm += Text::label(endif_label);
        }
        Instr::While { cond, body } => {
            let loop_start =
                new_label(&format!("loop{}", format_span(&instr.span)));
            let loop_exit =
                new_label(&format!("loop_exit{}", format_span(&instr.span)));
            *asm += Text::label(loop_start.clone());
            compile_expr(cond, asm, variables, name_of, deps, fun_id);
            *asm += testq(reg!(RAX), reg!(RAX));
            *asm += jz(loop_exit.clone());
            compile_instr(
                *body,
                asm,
                Some((&loop_start, &loop_exit)),
                variables,
                name_of,
                deps,
                fun_id,
                fun_stack,
            );
            *asm += jmp(loop_start);
            *asm += Text::label(loop_exit);
        }
        Instr::For {
            loop_var: Some(_), ..
        } => unreachable!("invariant of typechecker"),
        Instr::For {
            loop_var: None,
            cond,
            incr,
            body,
        } => {
            let for_start =
                new_label(&format!("for{}", format_span(&instr.span)));
            let for_exit =
                new_label(&format!("for_exit{}", format_span(&instr.span)));
            *asm += Text::label(for_start.clone());
            if let Some(cond) = cond {
                compile_expr(cond, asm, variables, name_of, deps, fun_id);
                *asm += testq(reg!(RAX), reg!(RAX));
                *asm += jz(for_exit.clone());
            }
            compile_instr(
                *body,
                asm,
                Some((&for_start, &for_exit)),
                variables,
                name_of,
                deps,
                fun_id,
                fun_stack,
            );
            for incr_expr in incr {
                compile_expr(incr_expr, asm, variables, name_of, deps, fun_id);
            }
            *asm += jmp(for_start);
            *asm += Text::label(for_exit);
        }
        Instr::Block(block) => compile_block(
            block,
            asm,
            variables,
            current_loop,
            name_of,
            deps,
            fun_id,
            fun_stack,
        ),
        Instr::Return(opt_e) => {
            if let Some(e) = opt_e {
                compile_expr(e, asm, variables, name_of, deps, fun_id);
            } else {
                *asm += movq(immq(0), reg!(RAX));
            }
            *asm += leave();
            *asm += ret()
        }
        Instr::Break => *asm += jmp(current_loop.unwrap().1.clone()),
        Instr::Continue => *asm += jmp(current_loop.unwrap().0.clone()),
    }
}

fn compile_block(
    block: Block<TypeAnnotation>,
    asm: &mut Text,
    variables: &mut HashMap<usize, i64>,
    current_loop: Option<(&reg::Label, &reg::Label)>,
    name_of: &[String],
    deps: &Tree,
    fun_id: Id,
    fun_stack: &mut Vec<TypedFunDecl<FunDecl<TypeAnnotation>>>,
) {
    let mut variable_names = Vec::new();
    let mut old_variables = HashMap::new();

    for new_var in block.declared_vars {
        if let Some(offset) = variables.remove(&new_var) {
            old_variables.insert(new_var, offset);
        }
        variable_names.push(new_var);
        variables.insert(new_var, -8 * (variables.len() + 1) as i64);
    }
    *asm += subq(immq(8 * variables.len() as i64), reg!(RSP));

    for decl_or_instr in block.inner {
        // We don't have to handle var_decl with value since the typechecker remove them
        if let DeclOrInstr::Instr(instr) = decl_or_instr {
            compile_instr(
                instr,
                asm,
                current_loop,
                variables,
                name_of,
                deps,
                fun_id,
                fun_stack,
            );
        } else if let DeclOrInstr::Fun(fun_decl) = decl_or_instr {
            fun_stack.push(fun_decl);
        }
    }

    for name in variable_names {
        variables.remove(&name);
        if let Some(var) = old_variables.remove(&name) {
            variables.insert(name, var);
        }
    }

    *asm += addq(immq(variables.len() as i64), reg!(RSP));
}

fn compile_fun(
    fun_decl: TypedFunDecl<FunDecl<TypeAnnotation>>,
    asm: &mut Text,
    variables: &mut HashMap<usize, i64>,
    name_of: &[String],
    deps: &Tree,
    fun_stack: &mut Vec<TypedFunDecl<FunDecl<TypeAnnotation>>>,
) {
    let fun_id = fun_decl.id;

    *asm += Text::label(reg::Label::from_str(
        name_of[fun_decl.inner.name.inner].clone(),
    ));
    *asm += pushq(reg!(RBP));
    *asm += movq(reg!(RSP), reg!(RBP));

    let mut old_variables = Vec::new();
    for (nb, (_, arg)) in fun_decl.inner.params.iter().enumerate() {
        let old = variables.insert(arg.inner, (3 + nb as i64) * 8);
        old_variables.push((arg.inner, old));
    }

    compile_block(
        fun_decl.inner.code,
        asm,
        variables,
        None,
        name_of,
        deps,
        fun_id,
        fun_stack,
    );
    *asm += movq(immq(0), reg!(RAX));
    *asm += leave();
    *asm += ret();

    for (id, old) in old_variables {
        variables.remove(&id);
        if let Some(old) = old {
            variables.insert(id, old);
        }
    }
}

pub fn compile(
    path: impl AsRef<Path>,
    file: TypedFile,
    name_of: &[String],
) -> Result<()> {
    let mut asm = Text::empty();
    let mut variables = HashMap::new();
    let mut fun_stack = file.inner.fun_decls;
    while let Some(fun_decl) = fun_stack.pop() {
        compile_fun(
            fun_decl,
            &mut asm,
            &mut variables,
            name_of,
            &file.function_dependencies,
            &mut fun_stack,
        );
    }
    let mut f = StdFile::create(path.as_ref().with_extension("s"))?;
    f.write_all(b"\t.text\n")?;
    f.write_all(b"\t.globl main\n")?;

    asm.write_in(&mut f)?;

    Ok(())
}
