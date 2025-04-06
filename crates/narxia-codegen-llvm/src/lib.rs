use std::fs;

use llvm_api::Builder;
use llvm_api::Ctxt;
use llvm_api::IntCmp;
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

        let factorial_ty = Ty::function(&mut [i32_ty], i32_ty, false);
        let factorial = self.module.add_function("factorial", factorial_ty);

        let entry = self.builder.create_block(&self.context, main, "entry");
        let entry_bb = self.builder.build_block(entry);
        // let one = i32_ty.const_int(1, false);
        // let two = i32_ty.const_int(2, false);
        let sum = entry_bb.call(factorial, factorial_ty, &mut [i32_ty.const_int(10, false)]);
        let _ = entry_bb.ret(sum);

        let entry = self.builder.create_block(&self.context, factorial, "entry");
        let recursive_case = self
            .builder
            .create_block(&self.context, factorial, "recursive_case");
        let end = self.builder.create_block(&self.context, factorial, "end");

        let entry_bb = self.builder.build_block(entry);
        let n = factorial.get_param(0);
        let eq_zero = entry_bb.int_cmp(IntCmp::EQ, n, i32_ty.const_int(0, false));

        entry_bb.cond_br(eq_zero, end, recursive_case);

        let recursive_case_bb = self.builder.build_block(recursive_case);

        let n_minus_one = recursive_case_bb.sub(n, i32_ty.const_int(1, false));
        let n_minus_one_fact = recursive_case_bb.call(factorial, factorial_ty, &mut [n_minus_one]);

        let result = recursive_case_bb.mul(n, n_minus_one_fact);

        recursive_case_bb.br(end);

        let end_bb = self.builder.build_block(end);

        let final_result = end_bb.phi(i32_ty, |phi| {
            phi.branch(entry, i32_ty.const_int(1, false));
            phi.branch(recursive_case, result);
        });
        let _ = end_bb.ret(final_result);

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
