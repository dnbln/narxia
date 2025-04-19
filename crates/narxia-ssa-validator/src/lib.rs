use std::collections::BTreeMap;
use std::fmt;

use narxia_ssa::BlockRef;
use narxia_ssa::EndInstr;
use narxia_ssa::EndInstrKind;
use narxia_ssa::Function;
use narxia_ssa::FunctionRef;
use narxia_ssa::IValue;
use narxia_ssa::LocalRef;
use narxia_ssa::Module;
use narxia_ssa::PhiInstr;
use narxia_ssa::Value;

#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("block doesn't end (has no end instruction): {0:?}")]
    BlockDoesNotEnd(BlockRef),
    #[error("undeclared local ref: {0:?} (used in block {1:?} at {2:?})")]
    UndeclaredLocalRef(LocalRef, BlockRef, LocalRef),
    #[error(
        "local ref {0:?} is declared in block {1:?} but used in block {2:?}, outside of a phi node (at {3:?})"
    )]
    LocalRefDeclaredInBlockButUsedInAnother(LocalRef, BlockRef, BlockRef, LocalRef),
    #[error(
        "local ref {0:?} is declared in block {1:?} but used as phi value in block link {2:?} -> {3:?} (at {4:?})"
    )]
    PhiLinksToLocalRefOutsideOfPhiBlock(LocalRef, BlockRef, BlockRef, BlockRef, LocalRef),
    #[error("pred phi link missing for block link {0:?} -> {1:?} (at {2:?})")]
    PredPhiLinkMissing(BlockRef, BlockRef, LocalRef),
    #[error("pred phi linked twice for block link {0:?} -> {1:?} (at {2:?})")]
    PredPhiLinkTwice(BlockRef, BlockRef, LocalRef),
}

fn check_local_ref_links(f: &Function, validation_errors: &mut Vec<ValidationError>) {
    let mut lref_declarations = BTreeMap::new();
    for block in &f.blocks {
        for phi in &block.phi {
            lref_declarations.insert(phi.lhs, block.id);
        }
        for vphi in &block.var_phi {
            lref_declarations.insert(vphi.lhs, block.id);
        }
        for instr in &block.instrs {
            lref_declarations.insert(instr.lhs, block.id);
        }
        if let Some(EndInstr { local_ref, .. }) = &block.end {
            lref_declarations.insert(*local_ref, block.id);
        }
    }

    for block in &f.blocks {
        for phi_link in block.phi.iter().chain(block.var_phi.iter()) {
            phi_link.rhs.iter().for_each(|(b, l)| {
                if !lref_declarations.contains_key(l) {
                    validation_errors.push(ValidationError::UndeclaredLocalRef(
                        *l,
                        block.id,
                        phi_link.lhs,
                    ));
                } else if lref_declarations[l] != *b {
                    validation_errors.push(ValidationError::PhiLinksToLocalRefOutsideOfPhiBlock(
                        *l,
                        lref_declarations[l],
                        *b,
                        block.id,
                        phi_link.lhs,
                    ))
                }
            });
        }

        for instr in &block.instrs {
            let mut check_lref = |l: LocalRef| {
                if !lref_declarations.contains_key(&l) {
                    validation_errors
                        .push(ValidationError::UndeclaredLocalRef(l, block.id, instr.lhs));
                } else if lref_declarations[&l] != block.id {
                    validation_errors.push(
                        ValidationError::LocalRefDeclaredInBlockButUsedInAnother(
                            l,
                            lref_declarations[&l],
                            block.id,
                            instr.lhs,
                        ),
                    );
                }
            };

            match &instr.rhs {
                IValue::Param(_) => {}
                IValue::BinaryExpr(b) => {
                    if let Value::Local(l) = b.lhs {
                        check_lref(l);
                    }
                    if let Value::Local(l) = b.rhs {
                        check_lref(l);
                    }
                }
                IValue::Call(call) => {
                    for arg in &call.args {
                        if let Value::Local(l) = arg {
                            check_lref(*l);
                        }
                    }
                }
                IValue::Debug(l) | IValue::Display(l) => {
                    check_lref(*l);
                }
                IValue::SConcat(l) => {
                    for arg in l {
                        check_lref(*arg);
                    }
                }
                IValue::DoNothing => {}
                IValue::Value(v) => {
                    if let Value::Local(l) = v {
                        check_lref(*l);
                    }
                }
            }
        }

        if let Some(EndInstr {
            local_ref,
            kind: EndInstrKind::Return(v) | EndInstrKind::ConditionalBranch(v, ..),
        }) = &block.end
        {
            if let Value::Local(l) = v {
                if !lref_declarations.contains_key(l) {
                    validation_errors.push(ValidationError::UndeclaredLocalRef(
                        *l, block.id, *local_ref,
                    ));
                } else if lref_declarations[l] != block.id {
                    validation_errors.push(
                        ValidationError::LocalRefDeclaredInBlockButUsedInAnother(
                            *l,
                            lref_declarations[l],
                            block.id,
                            *local_ref,
                        ),
                    );
                }
            }
        }
    }
}

fn check_phi_links(f: &Function, validation_errors: &mut Vec<ValidationError>) {
    for block in &f.blocks {
        for phi_link in block.phi.iter().chain(block.var_phi.iter()) {
            let mut phis_remaining = block.preds.clone();
            for (b, l) in &phi_link.rhs {
                let Some(p) = phis_remaining.iter().position(|i| i == b) else {
                    validation_errors.push(ValidationError::PredPhiLinkTwice(
                        *b,
                        block.id,
                        phi_link.lhs,
                    ));
                    continue;
                };
                phis_remaining.remove(p);
            }

            for b in phis_remaining {
                validation_errors.push(ValidationError::PredPhiLinkMissing(
                    b,
                    block.id,
                    phi_link.lhs,
                ));
            }
        }
    }
}

fn check_blocks_end(f: &Function, validation_error: &mut Vec<ValidationError>) {
    for block in &f.blocks {
        if block.end.is_none() {
            validation_error.push(ValidationError::BlockDoesNotEnd(block.id));
        }
    }
}

fn validate_function(f: &Function, validation_errors: &mut Vec<ValidationError>) {
    check_blocks_end(f, validation_errors);
    check_phi_links(f, validation_errors);
    check_local_ref_links(f, validation_errors);
}

pub fn validate(m: &Module) -> Result<(), BTreeMap<FunctionRef, Vec<ValidationError>>> {
    let mut root_validation_errors = BTreeMap::new();
    for f in &m.functions {
        let mut validation_errors = Vec::new();
        validate_function(f, &mut validation_errors);
        if !validation_errors.is_empty() {
            root_validation_errors.insert(f.fn_id, validation_errors);
        }
    }

    if root_validation_errors.is_empty() {
        Ok(())
    } else {
        Err(root_validation_errors)
    }
}

pub fn present_validation_errors(
    m: &Module,
    validation_errors: &BTreeMap<FunctionRef, Vec<ValidationError>>,
) {
    use std::fmt::Write;
    let mut buffer = String::new();

    for (fn_ref, errors) in validation_errors {
        let f = m.functions.iter().find(|it| it.fn_id == *fn_ref).unwrap();
        writeln!(buffer, "Errors in function:\n{:?}\n", f).unwrap();

        let lookup_block =
            |block_ref: BlockRef| f.blocks.iter().find(|it| it.id == block_ref).unwrap();

        enum Instr<'a> {
            Phi(&'a PhiInstr),
            VarPhi(&'a PhiInstr),
            Normal(&'a narxia_ssa::Instr),
            End(&'a EndInstr),
        }

        impl<'a> Instr<'a> {
            fn local_ref(&self) -> LocalRef {
                match self {
                    Instr::Phi(it) => it.lhs,
                    Instr::VarPhi(it) => it.lhs,
                    Instr::Normal(it) => it.lhs,
                    Instr::End(it) => it.local_ref,
                }
            }
        }

        impl<'a> fmt::Debug for Instr<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Instr::Phi(it) => it.fmt(f),
                    Instr::VarPhi(it) => it.fmt(f),
                    Instr::Normal(it) => it.fmt(f),
                    Instr::End(it) => it.fmt(f),
                }
            }
        }

        let lookup_instr = |local_ref: LocalRef| {
            f.blocks
                .iter()
                .flat_map(|b| {
                    b.phi
                        .iter()
                        .map(Instr::Phi)
                        .chain(b.var_phi.iter().map(Instr::VarPhi))
                        .chain(b.instrs.iter().map(Instr::Normal))
                        .chain(b.end.as_ref().map(Instr::End))
                })
                .find(|it| it.local_ref() == local_ref)
                .unwrap()
        };

        for error in errors {
            writeln!(buffer, "Error:\n{}\n", error).unwrap();

            match error {
                ValidationError::BlockDoesNotEnd(b) => {
                    writeln!(buffer, "Block:\n{:?}\n", lookup_block(*b)).unwrap();
                }
                ValidationError::UndeclaredLocalRef(undeclared, used_in, at) => {
                    writeln!(buffer, "Undeclared local ref:\n{:?}\n", undeclared).unwrap();
                    writeln!(buffer, "Used in block:\n{:?}\n", lookup_block(*used_in)).unwrap();
                    writeln!(buffer, "At:\n{:?}\n", lookup_instr(*at)).unwrap();
                }
                ValidationError::LocalRefDeclaredInBlockButUsedInAnother(l, declared, used, at) => {
                    writeln!(
                        buffer,
                        "Local ref declared in block:\n{:?}\n",
                        lookup_block(*declared)
                    )
                    .unwrap();
                    writeln!(buffer, "Used in block:\n{:?}\n", lookup_block(*used)).unwrap();
                    writeln!(buffer, "At:\n{:?}\n", lookup_instr(*at)).unwrap();
                }
                ValidationError::PhiLinksToLocalRefOutsideOfPhiBlock(
                    v,
                    declared_in,
                    phi_pre,
                    phi_post,
                    used_at,
                ) => {
                    writeln!(
                        buffer,
                        "Local ref declared in block:\n{:?}\n",
                        lookup_block(*declared_in)
                    )
                    .unwrap();
                    writeln!(
                        buffer,
                        "Used in phi link:\n{:?}\n\nto\n\n{:?}\n",
                        lookup_block(*phi_pre),
                        lookup_block(*phi_post)
                    )
                    .unwrap();
                    writeln!(buffer, "At:\n{:?}\n", lookup_instr(*used_at)).unwrap();
                }
                ValidationError::PredPhiLinkMissing(phi_pre, phi_post, at) => {
                    writeln!(
                        buffer,
                        "Pred phi link missing:\n{:?}\n\nto\n\n{:?}\n",
                        lookup_block(*phi_pre),
                        lookup_block(*phi_post)
                    )
                    .unwrap();
                    writeln!(buffer, "At:\n{:?}\n", lookup_instr(*at)).unwrap();
                }
                ValidationError::PredPhiLinkTwice(phi_pre, phi_post, at) => {
                    writeln!(
                        buffer,
                        "Pred phi link twice:\n{:?}\n\nto\n\n{:?}\n",
                        lookup_block(*phi_pre),
                        lookup_block(*phi_post)
                    )
                    .unwrap();
                    writeln!(buffer, "At:\n{:?}\n", lookup_instr(*at)).unwrap();
                }
            }
        }
    }
    
    narxia_log::error!("{}", buffer);
}
