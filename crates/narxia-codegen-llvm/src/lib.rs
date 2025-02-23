#![allow(unsafe_code)]

use std::ffi::{CStr, CString};
use std::{fs, ptr};

use llvm_sys::prelude::*;
use llvm_sys::*;
use narxia_codegen::{ir, CodegenBackend};

pub struct Backend {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
}

impl Default for Backend {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend {
    pub fn new() -> Self {
        let context = unsafe { core::LLVMContextCreate() };
        let module = unsafe {
            core::LLVMModuleCreateWithNameInContext(b"narxia\0".as_ptr() as *const _, context)
        };
        let builder = unsafe { core::LLVMCreateBuilderInContext(context) };

        Self {
            context,
            module,
            builder,
        }
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

pub(crate) struct BlockBuilder {
    builder: LLVMBuilderRef,
    block: LLVMBasicBlockRef,
}

impl BlockBuilder {
    pub fn new(
        context: LLVMContextRef,
        builder: LLVMBuilderRef,
        func: LLVMValueRef,
        name: &str,
    ) -> Self {
        let s = CString::new(name).unwrap();
        let block = unsafe {
            let block = core::LLVMAppendBasicBlockInContext(context, func, s.as_ptr());
            core::LLVMPositionBuilderAtEnd(builder, block);
            block
        };
        Self { builder, block }
    }

    pub fn add(&self, left: LLVMValueRef, right: LLVMValueRef) -> LLVMValueRef {
        unsafe { core::LLVMBuildAdd(self.builder, left, right, b"add\0".as_ptr() as *const _) }
    }

    pub fn ret_void(&self) -> LLVMValueRef {
        unsafe { core::LLVMBuildRetVoid(self.builder) }
    }

    pub fn ret(&self, value: LLVMValueRef) -> LLVMValueRef {
        unsafe { core::LLVMBuildRet(self.builder, value) }
    }
}

pub(crate) struct LLVMMessage {
    ptr: *const i8,
}

impl LLVMMessage {
    pub fn new(ptr: *const i8) -> Self {
        Self { ptr }
    }

    pub fn string(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.ptr) }
    }
}

impl Drop for LLVMMessage {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeMessage(self.ptr as *mut _);
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
        let void_ty = unsafe { core::LLVMVoidTypeInContext(self.context) };
        let int_ty = unsafe { core::LLVMInt32TypeInContext(self.context) };
        let main = unsafe {
            core::LLVMAddFunction(
                self.module,
                b"main\0".as_ptr() as *const _,
                core::LLVMFunctionType(int_ty, ptr::null_mut(), 0, 0),
            )
        };

        let entry = BlockBuilder::new(self.context, self.builder, main, "entry");
        let one = unsafe { core::LLVMConstInt(int_ty, 1, 0) };
        let two = unsafe { core::LLVMConstInt(int_ty, 2, 0) };
        let sum = entry.add(one, two);
        let _ = entry.ret(sum);

        unsafe {
            analysis::LLVMVerifyModule(
                self.module,
                analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
                ptr::null_mut(),
            );
        }

        unsafe {
            let s = LLVMMessage::new(core::LLVMPrintModuleToString(self.module));

            match out {
                narxia_codegen::Out::File(path_buf) => {
                    fs::write(path_buf, s.string().to_bytes()).unwrap();
                }
                narxia_codegen::Out::ToWrite(w) => {
                    w.write_all(s.string().to_bytes()).unwrap();
                }
            }
        }

        Ok(())
    }
}
