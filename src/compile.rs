use write_x86_64::{traits::Writable, *};

use std::{fs::File as StdFile, io::Write, path::Path};

use super::{
    compile_annotation::*,
    error::Result,
    tree::{Id, Tree},
    typechecker::*,
};

/// Push the addr of the given lvalue in %rax
fn push_addr(
    e: CExpr,
    asm: &mut Text,
    name_of: &[String],
    deps: &Tree,
    fun_id: Id,
) {
    match e {
        CExpr::Ident(name) => {
            let relative_depth = deps.depth(fun_id) - name.depth;
            *asm += movq(reg!(RBP), reg!(RAX));
            for _ in 0..relative_depth {
                *asm += movq(addr!(16, RAX), reg!(RAX));
            }
            *asm += addq(immq(name.offset), reg!(RAX));
        }
        CExpr::Deref(e) => compile_expr(*e, asm, name_of, deps, fun_id),
        _ => unreachable!(),
    }
}

/// Push the expression in %rax
fn compile_expr(
    e: CExpr,
    asm: &mut Text,
    name_of: &[String],
    deps: &Tree,
    fun_id: Id,
) {
    match e {
        CExpr::Int(x) => *asm += movq(immq(x), reg!(RAX)),
        CExpr::Ident(_) => {
            push_addr(e, asm, name_of, deps, fun_id);
            *asm += movq(addr!(RAX), reg!(RAX));
        }
        CExpr::Deref(e) => {
            compile_expr(*e, asm, name_of, deps, fun_id);
            *asm += movq(addr!(RAX), reg!(RAX));
        }
        CExpr::Assign { lhs, rhs } => {
            push_addr(*lhs, asm, name_of, deps, fun_id);
            *asm += pushq(reg!(RAX));
            compile_expr(*rhs, asm, name_of, deps, fun_id);
            *asm += popq(RBX);
            *asm += movq(reg!(RAX), addr!(RBX));
        }
        CExpr::Call { name, mut args } => {
            fn call_extern(asm: &mut Text, label: reg::Label) {
                *asm += movq(reg!(RSP), reg!(RBX)); // Saving the stack
                *asm += andq(immq(-16), reg!(RSP)); // Align the stack
                *asm += movq(reg!(RAX), reg!(RDI)); // Move the argument

                *asm += call(label);

                *asm += movq(reg!(RBX), reg!(RSP)); // Restoring the stack
            }

            // malloc
            if name == 0 {
                compile_expr(*args.remove(0), asm, name_of, deps, fun_id);

                call_extern(asm, reg::Label::from_str("malloc".to_string()));
            }
            // putchar
            else if name == 1 {
                compile_expr(*args.remove(0), asm, name_of, deps, fun_id);

                call_extern(asm, reg::Label::from_str("putchar".to_string()));
            } else {
                let arity = args.len();
                for arg in args.into_iter().rev() {
                    compile_expr(*arg, asm, name_of, deps, fun_id);
                    *asm += pushq(reg!(RAX));
                }
                // safe because of the typechecking
                let height = deps
                    .relative_height(fun_id, deps.find_by_name(name).unwrap());
                *asm += movq(reg!(RBP), reg!(RAX));
                // we retrieve the %rbp of the parent of the function
                for _ in 0..height {
                    *asm += movq(addr!(16, RAX), reg!(RAX));
                }
                *asm += pushq(reg!(RAX));
                // return is in %rax
                *asm += call(reg::Label::from_str(name_of[name].clone()));

                // TODO: Removing the arguments can be done by adding to %rsp
                //       addq (arity+1)*8, %rsp
                // pop the parent %rbp
                *asm += popq(RBX);
                // pop args
                for _ in 0..arity {
                    *asm += popq(RBX);
                }
            }
        }
        CExpr::Addr(e) => push_addr(*e, asm, name_of, deps, fun_id),
        CExpr::Not(e) => {
            compile_expr(*e, asm, name_of, deps, fun_id);
            *asm += movq(reg!(RAX), reg!(RBX));
            *asm += movq(immq(0), reg!(RAX));
            *asm += cmpq(immq(0), reg!(RBX));
            *asm += set(instr::Cond::Z, reg!(AL));
            // We zero the rest of %rax
            *asm += movzbq(reg!(AL), RAX);
        }
        CExpr::Neg(e) => {
            compile_expr(*e, asm, name_of, deps, fun_id);
            *asm += negq(reg!(RAX));
        }
        CExpr::BinOp { op, lhs, rhs }
            if !matches!(op, CBinOp::LAnd(_) | CBinOp::LOr(_)) =>
        {
            compile_expr(*lhs, asm, name_of, deps, fun_id);
            *asm += pushq(reg!(RAX));
            compile_expr(*rhs, asm, name_of, deps, fun_id);
            *asm += movq(reg!(RAX), reg!(RBX));
            *asm += popq(RAX);
            match op {
                CBinOp::Eq => {
                    *asm += cmpq(reg!(RBX), reg!(RAX));
                    *asm += set(instr::Cond::Z, reg!(AL));
                    *asm += movzbq(reg!(AL), RAX);
                }
                CBinOp::NEq => {
                    *asm += cmpq(reg!(RBX), reg!(RAX));
                    *asm += set(instr::Cond::NZ, reg!(AL));
                    *asm += movzbq(reg!(AL), RAX);
                }
                CBinOp::Lt => {
                    *asm += cmpq(reg!(RBX), reg!(RAX));
                    *asm += set(instr::Cond::L, reg!(AL));
                    *asm += movzbq(reg!(AL), RAX);
                }
                CBinOp::Le => {
                    *asm += cmpq(reg!(RBX), reg!(RAX));
                    *asm += set(instr::Cond::LE, reg!(AL));
                    *asm += movzbq(reg!(AL), RAX);
                }
                CBinOp::Gt => {
                    *asm += cmpq(reg!(RBX), reg!(RAX));
                    *asm += set(instr::Cond::G, reg!(AL));
                    *asm += movzbq(reg!(AL), RAX);
                }
                CBinOp::Ge => {
                    *asm += cmpq(reg!(RBX), reg!(RAX));
                    *asm += set(instr::Cond::GE, reg!(AL));
                    *asm += movzbq(reg!(AL), RAX);
                }
                CBinOp::Add => {
                    *asm += addq(reg!(RBX), reg!(RAX));
                }
                CBinOp::Sub => {
                    *asm += subq(reg!(RBX), reg!(RAX));
                }
                CBinOp::Mul => {
                    *asm += imulq(reg!(RBX), reg!(RAX));
                }
                CBinOp::Div => {
                    *asm += cqto();
                    *asm += idivq(reg!(RBX));
                }
                CBinOp::Mod => {
                    *asm += movq(immq(0), reg!(RDX));
                    *asm += idivq(reg!(RBX));
                    *asm += movq(reg!(RDX), reg!(RAX));
                }
                CBinOp::LAnd(_) | CBinOp::LOr(_) => unreachable!(),
            }
        }
        CExpr::BinOp { op, lhs, rhs } => {
            compile_expr(*lhs, asm, name_of, deps, fun_id);
            *asm += testq(reg!(RAX), reg!(RAX));
            let label = if let CBinOp::LAnd(unique_id) = op {
                let label =
                    reg::Label::from_str(format!("endboolexpr{}", unique_id));
                *asm += jz(label.clone());
                label
            } else if let CBinOp::LOr(unique_id) = op {
                let label =
                    reg::Label::from_str(format!("endboolexpr{}", unique_id));
                *asm += jnz(label.clone());
                label
            } else {
                unreachable!()
            };
            compile_expr(*rhs, asm, name_of, deps, fun_id);
            *asm += testq(reg!(RAX), reg!(RAX));
            *asm += Text::label(label);
            *asm += set(instr::Cond::NZ, reg!(AL));
            *asm += movzbq(reg!(AL), RAX);
        }
        CExpr::PrefixOp { op, e, arg } => {
            push_addr(*e, asm, name_of, deps, fun_id);
            *asm += if let CUnOp::Add = op { addq } else { subq }(
                immq(arg),
                addr!(RAX),
            );
            *asm += movq(addr!(RAX), reg!(RAX));
        }
        CExpr::PostfixOp { op, e, arg } => {
            push_addr(*e, asm, name_of, deps, fun_id);
            *asm += movq(addr!(RAX), reg!(RBX));
            *asm += if let CUnOp::Add = op { addq } else { subq }(
                immq(arg),
                addr!(RAX),
            );
            *asm += movq(reg!(RBX), reg!(RAX));
        }
    }
}

/// Compile an instruction
/// Should leave the stack pointer unchanged at the end of the instruction
fn compile_instr(
    instr: CInstr,
    asm: &mut Text,
    // loop start/end
    current_loop: Option<(&reg::Label, &reg::Label)>,
    name_of: &[String],
    deps: &Tree,
    fun_id: Id,
) {
    match instr {
        CInstr::EmptyInstr => (),
        CInstr::ExprInstr(e) => compile_expr(e, asm, name_of, deps, fun_id),
        CInstr::If {
            cond,
            then_branch,
            else_branch,
            unique_id,
        } => {
            compile_expr(cond, asm, name_of, deps, fun_id);
            let else_label = new_label(&format!("else{}", unique_id));
            let endif_label = new_label(&format!("endif{}", unique_id));
            *asm += testq(reg!(RAX), reg!(RAX));
            if else_branch.is_some() {
                *asm += jz(else_label.clone());
            } else {
                *asm += jz(endif_label.clone());
            }
            compile_instr(
                *then_branch,
                asm,
                current_loop,
                name_of,
                deps,
                fun_id,
            );
            *asm += jmp(endif_label.clone());
            if let Some(else_branch) = else_branch {
                *asm += Text::label(else_label);
                compile_instr(
                    *else_branch,
                    asm,
                    current_loop,
                    name_of,
                    deps,
                    fun_id,
                );
            }
            *asm += Text::label(endif_label);
        }
        CInstr::While {
            cond,
            body,
            unique_id,
        } => {
            let loop_start = new_label(&format!("loop{}", unique_id));
            let loop_exit = new_label(&format!("loop_exit{}", unique_id));
            *asm += Text::label(loop_start.clone());
            compile_expr(cond, asm, name_of, deps, fun_id);
            *asm += testq(reg!(RAX), reg!(RAX));
            *asm += jz(loop_exit.clone());
            compile_instr(
                *body,
                asm,
                Some((&loop_start, &loop_exit)),
                name_of,
                deps,
                fun_id,
            );
            *asm += jmp(loop_start);
            *asm += Text::label(loop_exit);
        }
        CInstr::For {
            cond,
            incr,
            body,
            unique_id,
        } => {
            let for_start = new_label(&format!("for{}", unique_id));
            let for_incr = new_label(&format!("for_incr{}", unique_id));
            let for_exit = new_label(&format!("for_exit{}", unique_id));
            *asm += Text::label(for_start.clone());
            if let Some(cond) = cond {
                compile_expr(cond, asm, name_of, deps, fun_id);
                *asm += testq(reg!(RAX), reg!(RAX));
                *asm += jz(for_exit.clone());
            }
            compile_instr(
                *body,
                asm,
                Some((&for_incr, &for_exit)),
                name_of,
                deps,
                fun_id,
            );
            *asm += Text::label(for_incr);
            for incr_expr in incr {
                compile_expr(incr_expr, asm, name_of, deps, fun_id);
            }
            *asm += jmp(for_start);
            *asm += Text::label(for_exit);
        }
        CInstr::Block(block) => {
            compile_block(block, asm, current_loop, name_of, deps, fun_id)
        }
        CInstr::Return(opt_e) => {
            if let Some(e) = opt_e {
                compile_expr(e, asm, name_of, deps, fun_id);
            } else {
                *asm += movq(immq(0), reg!(RAX));
            }
            *asm += leave();
            *asm += ret()
        }
        CInstr::Break => *asm += jmp(current_loop.unwrap().1.clone()),
        CInstr::Continue => *asm += jmp(current_loop.unwrap().0.clone()),
    }
}

fn compile_block(
    block: CBlock,
    asm: &mut Text,
    current_loop: Option<(&reg::Label, &reg::Label)>,
    name_of: &[String],
    deps: &Tree,
    fun_id: Id,
) {
    for instr in block.block {
        compile_instr(instr, asm, current_loop, name_of, deps, fun_id);
    }
}

fn compile_fun(
    fun_decl: CFunDecl,
    asm: &mut Text,
    name_of: &[String],
    deps: &Tree,
) {
    let fun_id = fun_decl.id;

    *asm += Text::label(reg::Label::from_str(name_of[fun_decl.name].clone()));

    *asm += pushq(reg!(RBP));
    *asm += movq(reg!(RSP), reg!(RBP));

    if fun_decl.code.size_of > 0 {
        // allocate stack space for the function
        *asm += subq(immq(fun_decl.code.size_of as i64), reg!(RSP));
    }
    compile_block(fun_decl.code, asm, None, name_of, deps, fun_id);

    *asm += movq(immq(0), reg!(RAX));
    *asm += leave();
    *asm += ret();
}

pub fn compile(
    path: impl AsRef<Path>,
    file: TypedFile,
    name_of: &[String],
) -> Result<()> {
    let file = annotate(file);

    let mut asm = Text::empty();
    for fun_decl in file.fun_decls.into_iter() {
        compile_fun(fun_decl, &mut asm, name_of, &file.function_dependencies);
    }
    let mut f = StdFile::create(path.as_ref().with_extension("s"))?;
    f.write_all(b"\t.text\n")?;
    f.write_all(b"\t.globl main\n")?;

    asm.write_in(&mut f)?;

    Ok(())
}
