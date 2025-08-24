use std::sync::Arc;
use std::sync::Weak;

use dir_structure::DirStructureItem;
use dir_structure::FsVfs;
use miette::IntoDiagnostic;
use narxia_dir_structures::Workspace;
use narxia_dir_structures::ws_root;
use prodash::tree::Item;
use prodash::tree::Root as Tree;
use prodash::tree::root::Options;

use crate::LLVMManager;
use crate::NexusOutputGroups;
use crate::NexusR;

struct Shell {
    pub tree: Arc<Tree>,
}

pub struct NexusContext<'vfs> {
    shell: Shell,
    root: Item,
    groups: Option<NexusOutputGroups>,
    pub llvm_manager: LLVMManager,
    pub ws: Workspace<'vfs, FsVfs>,
}

impl<'vfs> NexusContext<'vfs> {
    pub fn new(groups: Option<NexusOutputGroups>) -> NexusR<(Self, Weak<Tree>)> {
        let tree = Arc::new(
            Options {
                message_buffer_capacity: 30,
                ..Default::default()
            }
            .create(),
        );
        let root = tree.add_child("Nexus");
        let ctxt = Self {
            shell: Shell { tree },
            root,
            groups,
            llvm_manager: LLVMManager::make_from_target(),
            ws: Workspace::read(ws_root()).into_diagnostic()?,
        };
        let tree = Arc::downgrade(&ctxt.shell.tree);
        Ok((ctxt, tree))
    }

    pub fn new_child(&mut self, label: &str) -> Item {
        self.root.add_child(label)
    }

    pub fn done(&mut self, message: impl Into<String>) {
        let message = message.into();
        self.root.done(message);
    }

    pub fn info(&mut self, message: impl Into<String>) {
        let message = message.into();
        self.root.info(message);
    }

    pub fn fail(&mut self, message: impl Into<String>) {
        let message = message.into();
        self.root.fail(message);
    }

    pub fn groups(&self) -> Option<&NexusOutputGroups> {
        self.groups.as_ref()
    }
}
