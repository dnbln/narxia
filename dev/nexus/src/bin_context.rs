use std::sync::{Arc, Weak};

use prodash::tree::root::Options;
use prodash::tree::{Item, Root as Tree};

use crate::{LLVMManager, NexusOutputGroups};

struct Shell {
    pub tree: Arc<Tree>,
}

pub struct NexusContext {
    shell: Shell,
    root: Item,
    groups: Option<NexusOutputGroups>,
    pub llvm_manager: LLVMManager,
}

impl NexusContext {
    pub fn new(groups: Option<NexusOutputGroups>) -> (Self, Weak<Tree>) {
        let tree = Arc::new(
            Options {
                message_buffer_capacity: 300,
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
        };
        let tree = Arc::downgrade(&ctxt.shell.tree);
        (ctxt, tree)
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
