#![allow(unsafe_code)]

use std::ffi;
use std::iter;
use std::ptr;
use std::slice;

use llvm_sys::analysis;
use llvm_sys::core;
use llvm_sys::prelude::*;

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ty {
    ty: LLVMTypeRef,
}

impl Ty {
    pub fn function(params: &mut [Ty], ret: Ty, is_var_arg: bool) -> Self {
        let p = params.as_mut_ptr();
        let params_len = params.len();
        let params_p = unsafe { slice::from_raw_parts_mut(p as *mut LLVMTypeRef, params_len) };
        Self {
            ty: unsafe {
                core::LLVMFunctionType(
                    ret.ty,
                    params_p.as_mut_ptr(),
                    params_p.len() as u32,
                    is_var_arg as LLVMBool,
                )
            },
        }
    }

    pub fn const_int(&self, value: u64, sign_extend: bool) -> VRef {
        VRef {
            value: unsafe { core::LLVMConstInt(self.ty, value, sign_extend as LLVMBool) },
        }
    }
}

#[repr(transparent)]
pub struct Ctxt {
    context: LLVMContextRef,
}

impl Ctxt {
    pub fn new() -> Self {
        let context = unsafe { core::LLVMContextCreate() };
        Self { context }
    }

    pub fn create_module_with_name(&self, name: &str) -> Module {
        with_string(name, |name| unsafe {
            Module {
                module: core::LLVMModuleCreateWithNameInContext(name, self.context),
            }
        })
    }

    pub fn builder(&self) -> Builder {
        Builder::new_in_context(self)
    }
}

impl Drop for Ctxt {
    fn drop(&mut self) {
        unsafe {
            core::LLVMContextDispose(self.context);
        }
    }
}

#[repr(transparent)]
pub struct Module {
    module: LLVMModuleRef,
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeModule(self.module);
        }
    }
}

impl Module {
    pub fn add_function(&self, name: &str, ty: Ty) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMAddFunction(self.module, name, ty.ty),
            }
        })
    }

    pub fn verify(&self) {
        unsafe {
            analysis::LLVMVerifyModule(
                self.module,
                analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
                ptr::null_mut(),
            );
        }
    }

    pub fn print_to_string(&self) -> LLVMMessage {
        LLVMMessage::new(unsafe { core::LLVMPrintModuleToString(self.module) })
    }
}

#[repr(transparent)]
pub struct Builder {
    builder: LLVMBuilderRef,
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeBuilder(self.builder);
        }
    }
}

impl Builder {
    fn new_in_context(context: &Ctxt) -> Self {
        let builder = unsafe { core::LLVMCreateBuilderInContext(context.context) };
        Self { builder }
    }

    pub fn make_block(&self, context: &Ctxt, func: VRef, name: &str) -> BlockBuilder {
        BlockBuilder::new(context, self, func, name)
    }

    fn build_add(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildAdd(self.builder, left.value, right.value, name),
            }
        })
    }

    fn build_sub(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildSub(self.builder, left.value, right.value, name),
            }
        })
    }

    fn build_mul(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildMul(self.builder, left.value, right.value, name),
            }
        })
    }

    fn build_idiv(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildSDiv(self.builder, left.value, right.value, name),
            }
        })
    }

    fn build_imod(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildSRem(self.builder, left.value, right.value, name),
            }
        })
    }

    fn build_udiv(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildUDiv(self.builder, left.value, right.value, name),
            }
        })
    }

    fn build_umod(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildURem(self.builder, left.value, right.value, name),
            }
        })
    }

    fn build_ret(&self, value: VRef) -> VRef {
        VRef {
            value: unsafe { core::LLVMBuildRet(self.builder, value.value) },
        }
    }

    fn build_ret_void(&self) -> VRef {
        VRef {
            value: unsafe { core::LLVMBuildRetVoid(self.builder) },
        }
    }
}

pub(crate) struct BlockBuilder<'b> {
    builder: &'b Builder,
    block: LLVMBasicBlockRef,
}

impl<'b> BlockBuilder<'b> {
    pub fn new(context: &Ctxt, builder: &'b Builder, func: VRef, name: &str) -> Self {
        let s = ffi::CString::new(name).unwrap();
        let block = unsafe {
            let block =
                core::LLVMAppendBasicBlockInContext(context.context, func.value, s.as_ptr());
            core::LLVMPositionBuilderAtEnd(builder.builder, block);
            block
        };

        Self { builder, block }
    }

    pub fn add(&self, left: VRef, right: VRef) -> VRef {
        self.builder.build_add(left, right, "add")
    }

    pub fn ret_void(&self) -> VRef {
        self.builder.build_ret_void()
    }

    pub fn ret(&self, value: VRef) -> VRef {
        self.builder.build_ret(value)
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct VRef {
    value: LLVMValueRef,
}

pub struct StandardTypes {
    void: LLVMTypeRef,
    i8: LLVMTypeRef,
    i16: LLVMTypeRef,
    i32: LLVMTypeRef,
    i64: LLVMTypeRef,
    i128: LLVMTypeRef,
    u8: LLVMTypeRef,
    u16: LLVMTypeRef,
    u32: LLVMTypeRef,
    u64: LLVMTypeRef,
    u128: LLVMTypeRef,
    f16: LLVMTypeRef,
    f32: LLVMTypeRef,
    f64: LLVMTypeRef,
    bool: LLVMTypeRef,
}

impl StandardTypes {
    pub fn get_from_context(context: &Ctxt) -> Self {
        let void = unsafe { core::LLVMVoidTypeInContext(context.context) };
        let i8 = unsafe { core::LLVMInt8TypeInContext(context.context) };
        let i16 = unsafe { core::LLVMInt16TypeInContext(context.context) };
        let i32 = unsafe { core::LLVMInt32TypeInContext(context.context) };
        let i64 = unsafe { core::LLVMInt64TypeInContext(context.context) };
        let i128 = unsafe { core::LLVMInt128TypeInContext(context.context) };
        let u8 = unsafe { core::LLVMInt8TypeInContext(context.context) };
        let u16 = unsafe { core::LLVMInt16TypeInContext(context.context) };
        let u32 = unsafe { core::LLVMInt32TypeInContext(context.context) };
        let u64 = unsafe { core::LLVMInt64TypeInContext(context.context) };
        let u128 = unsafe { core::LLVMInt128TypeInContext(context.context) };
        let f16 = unsafe { core::LLVMHalfTypeInContext(context.context) };
        let f32 = unsafe { core::LLVMFloatTypeInContext(context.context) };
        let f64 = unsafe { core::LLVMDoubleTypeInContext(context.context) };
        let bool = unsafe { core::LLVMInt1TypeInContext(context.context) };

        Self {
            void,
            i8,
            i16,
            i32,
            i64,
            i128,
            u8,
            u16,
            u32,
            u64,
            u128,
            f16,
            f32,
            f64,
            bool,
        }
    }

    pub fn void_ty(&self) -> Ty {
        Ty { ty: self.void }
    }

    pub fn i32_ty(&self) -> Ty {
        Ty { ty: self.i8 }
    }
}

fn with_string<T>(s: &str, f: impl FnOnce(*const i8) -> T) -> T {
    let s = ffi::CString::new(s).unwrap();
    f(s.as_ptr())
}

fn with_strings<
    T: StringTuple,
    R,
    F: FnOnce(<T::OwnedStringsTuple as OwnedStringsTuple>::PtrTuple) -> R,
>(
    t: T,
    f: F,
) -> R {
    let o = t.owned_strings();
    f(o.as_ptr_tuple())
}

trait StringTuple {
    type OwnedStringsTuple: OwnedStringsTuple;
    fn owned_strings(self) -> Self::OwnedStringsTuple;
}

trait OwnedStringsTuple {
    type PtrTuple;
    fn as_ptr_tuple(&self) -> Self::PtrTuple;
}

struct SBuffer {
    buf: Vec<ffi::c_char>,
}

impl SBuffer {
    fn new(buf: Vec<i8>) -> Self {
        Self { buf }
    }

    fn push_str(&mut self, s: &str) -> *const ffi::c_char {
        if s.len() + 1 + self.buf.len() > self.buf.capacity() {
            panic!("buffer overflow");
        }

        let current_offset = self.buf.len();

        self.buf
            .extend(s.bytes().chain(iter::once(0)).map(|b| b as ffi::c_char));

        self.buf[current_offset..].as_ptr()
    }
}

macro_rules! impl_string_tuple {
    ($($name:ident: $n:ty => $o:ty => $o2:ty),* $(,)?) => {
        impl StringTuple for ($($n,)*) {
            type OwnedStringsTuple = (SBuffer, $($o,)*);
            fn owned_strings(self) -> Self::OwnedStringsTuple {
                let ($($name,)*) = self;
                let mut sbuffer = SBuffer::new(Vec::with_capacity($($name.len()+1+)* 0));
                $(let $name = sbuffer.push_str($name);)*
                (sbuffer, $($name,)*)
            }
        }

        impl OwnedStringsTuple for (SBuffer, $($o,)*) {
            type PtrTuple = ($($o2,)*);
            fn as_ptr_tuple(&self) -> Self::PtrTuple {
                let (_sbuffer, $($name,)*) = self;
                ($(*$name,)*)
            }
        }
    };
}

impl_string_tuple!(
    a: &str => *const i8 => *const i8,
);

impl_string_tuple!(
    a: &str => *const i8 => *const i8,
    b: &str => *const i8 => *const i8,
);

impl_string_tuple!(
    a: &str => *const i8 => *const i8,
    b: &str => *const i8 => *const i8,
    c: &str => *const i8 => *const i8,
);

impl_string_tuple!(
    a: &str => *const i8 => *const i8,
    b: &str => *const i8 => *const i8,
    c: &str => *const i8 => *const i8,
    d: &str => *const i8 => *const i8,
);

#[repr(transparent)]
pub(crate) struct LLVMMessage {
    ptr: *const i8,
}

impl LLVMMessage {
    pub fn new(ptr: *const i8) -> Self {
        Self { ptr }
    }

    pub fn string(&self) -> &ffi::CStr {
        unsafe { ffi::CStr::from_ptr(self.ptr) }
    }
}

impl Drop for LLVMMessage {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeMessage(self.ptr as *mut _);
        }
    }
}
