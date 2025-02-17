use llvm_sys::prelude::*;
use llvm_sys::*;
use narxia_codegen::CodegenBackend;

pub struct Backend {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
}

impl Backend {
    pub fn new() -> Self {
        let context = unsafe { core::LLVMContextCreate() };
        let module = unsafe {
            core::LLVMModuleCreateWithNameInContext(b"narxia\0".as_ptr() as *const _, context)
        };
        let builder = unsafe { core::LLVMCreateBuilderInContext(context) };

        Self { context, module, builder }
    }
}

impl Drop for Backend {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeBuilder(self.builder);
            core::LLVMDisposeModule(self.module);
            core::LLVMContextDispose(self.context);
        }
    }
}

impl CodegenBackend for Backend {
    type Error = ();

    fn generate_code(
        &self,
        ir: &narxia_codegen::ir::Mod,
        out: &narxia_codegen::Out,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}
