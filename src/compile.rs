use write_x86_64::*;

use std::collections::HashMap;

use super::{ast::*, typechecker::*};

/// Push the addr of the given lvalue in %rax
fn push_addr(
    e: TypedExpr,
    asm: &mut Text,
    variables: &mut HashMap<usize, i64>,
) {
    match e.inner {
        Expr::Ident(name) => {
            *asm += movq(reg!(RAX), reg!(RBP));
            *asm += addq(reg!(RAX), immq(variables[&name]));
        }
        Expr::Deref(e) => compile_expr(*e, asm, variables),
        _ => (),
    }
}

fn format_span(span: &beans::span::Span) -> String {
    let start_loc = span.start();
    let end_loc = span.end();
    format!(
        "{}.{}-{}.{}",
        start_loc.0, start_loc.1, end_loc.0, end_loc.1
    )
}

/// Push the expression in %rax
fn compile_expr(
    e: TypedExpr,
    asm: &mut Text,
    variables: &mut HashMap<usize, i64>,
) {
    match e.inner {
        Expr::Int(x) => *asm += movq(reg!(RAX), immq(x)),
        Expr::True => *asm += movq(reg!(RAX), immq(1)),
        Expr::False | Expr::Null => *asm += movq(reg!(RAX), immq(0)),
        Expr::Ident(name) => {
            *asm += movq(reg!(RAX), addr!(variables[&name], RBP))
        }
        Expr::Deref(e) => {
            compile_expr(*e, asm, variables);
            *asm += movq(reg!(RAX), addr!(RAX));
        }
        Expr::Assign { lhs, rhs } => {
            push_addr(*lhs, asm, variables);
            *asm += pushq(reg!(RAX));
            compile_expr(*rhs, asm, variables);
            *asm += popq(RBX);
            *asm += movq(addr!(RBX), reg!(RAX));
        }
        // TODO: call expression
        Expr::Call { .. } => (),
        Expr::PrefixIncr(e) => {
            push_addr(*e, asm, variables);
            *asm += incq(addr!(RAX));
            *asm += movq(reg!(RAX), addr!(RAX));
        }
        Expr::PrefixDecr(e) => {
            push_addr(*e, asm, variables);
            *asm += decq(addr!(RAX));
            *asm += movq(reg!(RAX), addr!(RAX));
        }
        Expr::PostfixIncr(e) => {
            push_addr(*e, asm, variables);
            *asm += movq(reg!(RBX), reg!(RAX));
            *asm += movq(reg!(RAX), addr!(RAX));
            *asm += incq(addr!(RBX));
        }
        Expr::PostfixDecr(e) => {
            push_addr(*e, asm, variables);
            *asm += movq(reg!(RBX), reg!(RAX));
            *asm += movq(reg!(RAX), addr!(RAX));
            *asm += decq(addr!(RBX));
        }
        Expr::Addr(e) => push_addr(*e, asm, variables),
        Expr::Not(e) => {
            compile_expr(*e, asm, variables);
            *asm += movq(reg!(RBX), reg!(RAX));
            *asm += xorq(reg!(RAX), reg!(RAX));
            *asm += cmpq(reg!(RBX), immq(0));
            *asm += set(instr::Cond::Z, reg!(AL));
        }
        Expr::Neg(e) => {
            compile_expr(*e, asm, variables);
            *asm += negq(reg!(RAX));
        }
        Expr::Pos(e) => {
            compile_expr(*e, asm, variables);
        }
        Expr::Op { op, lhs, rhs } => {
            compile_expr(*lhs, asm, variables);
            *asm += pushq(reg!(RAX));
            compile_expr(*rhs, asm, variables);
            *asm += movq(reg!(RBX), reg!(RAX));
            *asm += popq(RAX);
            match op {
                BinOp::Eq => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += xorq(reg!(RAX), reg!(RAX));
                    *asm += set(instr::Cond::Z, reg!(AL));
                }
                BinOp::NEq => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += xorq(reg!(RAX), reg!(RAX));
                    *asm += set(instr::Cond::Z, reg!(AL));
                    *asm += notq(reg!(RAX));
                }
                BinOp::Lt => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += xorq(reg!(RAX), reg!(RAX));
                    *asm += set(instr::Cond::L, reg!(AL));
                }
                BinOp::Le => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += xorq(reg!(RAX), reg!(RAX));
                    *asm += set(instr::Cond::LE, reg!(AL));
                }
                BinOp::Gt => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += xorq(reg!(RAX), reg!(RAX));
                    *asm += set(instr::Cond::G, reg!(AL));
                }
                BinOp::Ge => {
                    *asm += cmpq(reg!(RAX), reg!(RBX));
                    *asm += xorq(reg!(RAX), reg!(RAX));
                    *asm += set(instr::Cond::GE, reg!(AL));
                }
                BinOp::Add => {
                    *asm += addq(reg!(RAX), reg!(RBX));
                }
                BinOp::Sub => {
                    *asm += subq(reg!(RAX), reg!(RBX));
                }
                BinOp::Mul => {
                    *asm += imulq(reg!(RAX), reg!(RBX));
                }
                BinOp::Div => {
                    *asm += divq(reg!(RBX));
                }
                BinOp::Mod => {
                    *asm += divq(reg!(RBX));
                    *asm += movq(reg!(RAX), reg!(RDX));
                }
                BinOp::BAnd => {
                    *asm += andq(reg!(RAX), reg!(RBX));
                }
                BinOp::BOr => {
                    *asm += orq(reg!(RAX), reg!(RBX));
                }
            }
        }
        Expr::SizeOf(_) => *asm += movq(reg!(RAX), immq(8)),
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
) {
    match instr.instr {
        Instr::EmptyInstr => (),
        Instr::ExprInstr(e) => compile_expr(e, asm, variables),
        Instr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            compile_expr(cond, asm, variables);
            let else_label =
                new_label(&format!("@else{}", format_span(&instr.span)));
            let endif_label =
                new_label(&format!("@endif{}", format_span(&instr.span)));
            if else_branch.is_some() {
                *asm += jz(else_label.clone());
            }
            compile_instr(*then_branch, asm, current_loop, variables, name_of);
            *asm += jz(endif_label.clone());
            if let Some(else_branch) = *else_branch {
                *asm += Text::label(else_label);
                compile_instr(
                    else_branch,
                    asm,
                    current_loop,
                    variables,
                    name_of,
                );
            }
            *asm += Text::label(endif_label)
        }
        Instr::While { cond, body } => {
            let loop_start =
                new_label(&format!("@loop{}", format_span(&instr.span)));
            let loop_exit =
                new_label(&format!("@loop_exit{}", format_span(&instr.span)));
            *asm += Text::label(loop_start.clone());
            compile_expr(cond, asm, variables);
            *asm += jz(loop_exit.clone());
            compile_instr(
                *body,
                asm,
                Some((&loop_start, &loop_exit)),
                variables,
                name_of,
            );
            *asm += jmp(loop_start);
            *asm += Text::label(loop_exit)
        }
        /*For {
            loop_var,
            cond,
            incr,
            body,
        } => (),*/
        Instr::Block(block) => {
            compile_block(block, asm, variables, current_loop, name_of)
        }
        Instr::Return(opt_e) => {
            if let Some(e) = opt_e {
                compile_expr(e, asm, variables);
            }
            *asm += ret()
        }
        Instr::Break => *asm += jmp(current_loop.unwrap().1.clone()),
        Instr::Continue => *asm += jmp(current_loop.unwrap().0.clone()),
        _ => (),
    }
}

fn compile_block(
    block: Vec<DeclOrInstr<TypeAnnotation>>,
    asm: &mut Text,
    variables: &mut HashMap<usize, i64>,
    current_loop: Option<(&reg::Label, &reg::Label)>,
    name_of: &[String],
) {
    let mut variable_names = Vec::new();
    let mut old_variables = HashMap::new();

    for instr_or_decl in &block {
        match instr_or_decl {
            // TODO: Fun in block
            DeclOrInstr::Fun(_) => (),
            DeclOrInstr::Var(var) => {
                let var_decl = &var.inner;
                if let Some(var) = variables.remove(&var_decl.name.inner) {
                    old_variables.insert(var_decl.name.inner, var);
                }
                variable_names.push(var_decl.name.inner);
                variables.insert(
                    var_decl.name.inner,
                    -8 * (variables.len() + 1) as i64,
                );
                *asm += subq(reg!(RSP), immq(1));
            }
            DeclOrInstr::Instr(_) => (),
        }
    }

    for instr_or_decl in block {
        if let DeclOrInstr::Instr(instr) = instr_or_decl {
            compile_instr(instr, asm, current_loop, variables, name_of);
        }
    }

    for name in variable_names {
        variables.remove(&name);
        if let Some(var) = old_variables.remove(&name) {
            variables.insert(name, var);
        }
    }

    *asm += addq(reg!(RSP), immq(variables.len() as i64));
}
