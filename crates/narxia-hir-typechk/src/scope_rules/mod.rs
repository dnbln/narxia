use narxia_hir::hir::HirIdNewtype;
use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::{self, HirVisitor};
use narxia_hir::{hir, HirId};

pub struct ScopeRuleViolation {
    hir_id: HirId,
    kind: ViolationKind,
}

enum ScopeStackFrame {
    Module(hir::ModId),
    Stmt(hir::StmtId),
    Function(hir::FnId),
}

impl ScopeStackFrame {
    fn non_module_item(&self) -> Option<NonModuleItem> {
        match self {
            ScopeStackFrame::Module(_) => None,
            ScopeStackFrame::Function(fn_id) => Some(NonModuleItem::Fn(*fn_id)),
            ScopeStackFrame::Stmt(stmt_id) => Some(NonModuleItem::Stmt(*stmt_id)),
        }
    }
}

pub enum NonModuleItem {
    Item(hir::Item),
    Fn(hir::FnId),
    Stmt(hir::StmtId),
}

pub enum ViolationKind {
    ModuleInNonModuleItem(NonModuleItem),
}

struct ScopeRulesVisitor<'hir> {
    hir_map: &'hir HirMap,
    violations: Vec<ScopeRuleViolation>,

    stack: Vec<ScopeStackFrame>,
}

/// Returns the last element of the iterator that satisfies the predicate.
///
/// This is similar to `Iterator::find`, but it returns the last element
/// that satisfies the predicate, rather than the first.
///
/// If the iterator is empty or no elements satisfy the predicate, `None` is returned.
fn last_filter_map<I, T, U, F>(iter: I, f: F) -> Option<U>
where
    F: FnMut(T) -> Option<U>,
    I: Iterator<Item = T> + DoubleEndedIterator,
{
    iter.rev().filter_map(f).next()
}

impl<'hir> HirVisitor<'hir> for ScopeRulesVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    fn visit_mod_def(
        &mut self,
        mod_id: narxia_hir::hir::ModId,
        mod_def: &'hir narxia_hir::hir::ModDef,
    ) {
        if let Some(non_module_item) =
            last_filter_map(self.stack.iter(), |frame| frame.non_module_item())
        {
            self.violations.push(ScopeRuleViolation {
                hir_id: mod_id.hir_id(),
                kind: ViolationKind::ModuleInNonModuleItem(non_module_item),
            });
        }

        self.stack.push(ScopeStackFrame::Module(mod_id));
        visitor::walk_mod_def(self, mod_def);
        self.stack.pop();
    }

    fn visit_fn_def(&mut self, fn_id: hir::FnId, fn_def: &'hir hir::FnDef) {
        self.stack.push(ScopeStackFrame::Function(fn_id));
        visitor::walk_fn_def(self, fn_def);
        self.stack.pop();
    }

    fn visit_stmt(&mut self, stmt_id: hir::StmtId, stmt: &'hir hir::Stmt) {
        self.stack.push(ScopeStackFrame::Stmt(stmt_id));
        visitor::walk_stmt(self, stmt_id, stmt);
        self.stack.pop();
    }
}

pub fn collect_scope_violations<'hir>(
    hir_map: &'hir HirMap,
    mod_id: narxia_hir::hir::ModId,
) -> Vec<ScopeRuleViolation> {
    let mut visitor = ScopeRulesVisitor {
        hir_map,
        violations: Vec::new(),
        stack: Vec::new(),
    };
    visitor.visit_mod_def(mod_id, hir_map.get_mod(mod_id));
    visitor.violations
}

#[cfg(test)]
mod tests {
    #[test]
    fn last_filter_map() {
        let v = vec![1, 2, 3, 4, 5];
        assert_eq!(super::last_filter_map(v.iter(), |&x| Some(x)), Some(5));
        assert_eq!(
            super::last_filter_map(v.iter(), |&x| (x == 3).then_some(x)).unwrap(),
            3
        );
        assert_eq!(
            super::last_filter_map(v.iter(), |&x| (x == 6).then_some(x)),
            None
        );
        assert_eq!(
            super::last_filter_map(v.iter(), |&x| (x % 2 == 0).then_some(x)),
            Some(4)
        );
    }
}
