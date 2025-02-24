use std::fs;

use llvm_api::Builder;
use llvm_api::Ctxt;
use llvm_api::Module;
use llvm_api::StandardTypes;
use llvm_api::Ty;
use narxia_codegen::ir;
use narxia_codegen::CodegenBackend;

mod llvm_api;

pub struct Backend {
    standard_ty: StandardTypes,

    // keep them ordered like this for drop order
    builder: Builder,
    module: Module,
    context: Ctxt,
}

impl Default for Backend {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend {
    pub fn new() -> Self {
        let context = Ctxt::new();
        let module = context.create_module_with_name("narxia");
        let builder = context.builder();
        let standard_ty = StandardTypes::get_from_context(&context);

        Self {
            context,
            module,
            builder,

            standard_ty,
        }
    }
}

impl CodegenBackend for Backend {
    type Error = ();

    fn generate_code(
        &mut self,
        tcx: &narxia_codegen::TyCtxt,
        ir: &ir::Mod,
        out: narxia_codegen::Out,
    ) -> Result<(), Self::Error> {
        let void_ty = self.standard_ty.void_ty();
        let i32_ty = self.standard_ty.i32_ty();
        let main = self
            .module
            .add_function("main", Ty::function(&mut [], i32_ty, false));

        let bb = self.builder.make_block(&self.context, main, "entry");
        let one = i32_ty.const_int(1, false);
        let two = i32_ty.const_int(2, false);
        let sum = bb.add(one, two);
        let _ = bb.ret(sum);

        self.module.verify();

        let s = self.module.print_to_string();

        match out {
            narxia_codegen::Out::File(path_buf) => {
                fs::write(path_buf, s.string().to_bytes()).unwrap();
            }
            narxia_codegen::Out::ToWrite(w) => {
                w.write_all(s.string().to_bytes()).unwrap();
            }
        }

        Ok(())
    }
}
