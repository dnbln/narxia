use narxia_hir::HirId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefId {
    pub(crate) hir: HirId,
}
