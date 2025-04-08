#![expect(unsafe_code, reason = "This module is a wrapper around LLVM's C API.")]

use std::ffi;
use std::fmt;
use std::iter;
use std::ptr;
use std::slice;

use llvm_sys::analysis;
use llvm_sys::core;
use llvm_sys::error;
use llvm_sys::prelude::*;
use llvm_sys::target;
use llvm_sys::target_machine;
use llvm_sys::transforms::pass_builder;
use llvm_sys::LLVMIntPredicate;

fn transparent_downcast_slice<T, U>(t: &[T]) -> &[U]
where
    T: TransparentDowncast<Inner = U>,
{
    let p = t.as_ptr();
    let len = t.len();

    unsafe { slice::from_raw_parts(p as *const U, len) }
}

fn transparent_downcast<T, U>(t: &[T]) -> (*const U, u32)
where
    T: TransparentDowncast<Inner = U>,
{
    let downcasted = transparent_downcast_slice(t);

    (downcasted.as_ptr(), downcasted.len() as u32)
}

fn transparent_downcast_to_slice_mut<T, U>(t: &mut [T]) -> &mut [U]
where
    T: TransparentDowncast<Inner = U>,
{
    let p = t.as_mut_ptr();
    let len = t.len();

    unsafe { slice::from_raw_parts_mut(p as *mut U, len) }
}

fn transparent_downcast_mut<T, U>(t: &mut [T]) -> (*mut U, u32)
where
    T: TransparentDowncast<Inner = U>,
{
    let downcasted = transparent_downcast_to_slice_mut(t);

    (downcasted.as_mut_ptr(), downcasted.len() as u32)
}

/// # Safety
/// `Self` should be a `#[repr(transparent)]` wrapper around `Self::Inner`.
unsafe trait TransparentDowncast {
    type Inner;
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ty {
    ty: LLVMTypeRef,
}

unsafe impl TransparentDowncast for Ty {
    type Inner = LLVMTypeRef;
}

impl Ty {
    pub fn function(params: &mut [Ty], ret: Ty, is_var_arg: bool) -> Self {
        let (params, params_len) = transparent_downcast_mut(params);
        Self {
            ty: unsafe {
                core::LLVMFunctionType(ret.ty, params, params_len, is_var_arg as LLVMBool)
            },
        }
    }

    pub fn const_int(&self, value: u64, sign_extend: bool) -> VRef {
        VRef {
            value: unsafe { core::LLVMConstInt(self.ty, value, sign_extend as LLVMBool) },
        }
    }

    pub fn true_value(&self) -> VRef {
        self.const_int(1, false)
    }

    pub fn false_value(&self) -> VRef {
        self.const_int(0, false)
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
                module: core::LLVMModuleCreateWithNameInContext(*name, self.context),
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
                value: core::LLVMAddFunction(self.module, *name, ty.ty),
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

    pub fn optimize(&mut self, optimization_level: OptLevel, tm: &TargetMachine) {
        let pm =
            PassManager::new(unsafe { core::LLVMCreateFunctionPassManagerForModule(self.module) });
        pm.optimize_module_with_profile(self, optimization_level, tm, PassBuilderOptions::new())
            .cant_fail();
    }

    pub fn print_to_string(&self) -> LLVMMessage {
        LLVMMessage::new(unsafe { core::LLVMPrintModuleToString(self.module) })
    }
}

struct PassManager {
    pm: LLVMPassManagerRef,
}

impl PassManager {
    fn new(pm: LLVMPassManagerRef) -> Self {
        Self { pm }
    }

    fn optimize_module_with_profile(
        &self,
        m: &mut Module,
        optimization_level: OptLevel,
        tm: &TargetMachine,
        options: PassBuilderOptions,
    ) -> LLVMError {
        let passes = match optimization_level {
            OptLevel::O0 => "default<O0>",
            OptLevel::O1 => "default<O1>",
            OptLevel::O2 => "default<O2>",
            OptLevel::O3 => "default<O3>",
            OptLevel::Os => "default<Os>",
            OptLevel::Oz => "default<Oz>",
        };
        with_string(passes, |passes| unsafe {
            LLVMError::new(pass_builder::LLVMRunPasses(
                m.module,
                *passes,
                tm.target_machine,
                options.pass_builder_options,
            ))
        })
    }
}

impl Drop for PassManager {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposePassManager(self.pm);
        }
    }
}

struct PassBuilderOptions {
    pass_builder_options: pass_builder::LLVMPassBuilderOptionsRef,
}

impl PassBuilderOptions {
    fn new() -> Self {
        let pass_builder_options = unsafe { pass_builder::LLVMCreatePassBuilderOptions() };
        Self {
            pass_builder_options,
        }
    }
}

impl Drop for PassBuilderOptions {
    fn drop(&mut self) {
        unsafe {
            pass_builder::LLVMDisposePassBuilderOptions(self.pass_builder_options);
        }
    }
}

pub enum OptLevel {
    O0,
    O1,
    O2,
    O3,
    Os,
    Oz,
}

pub struct LLVMTarget {
    target: target_machine::LLVMTargetRef,
}

impl LLVMTarget {
    pub(crate) fn init() {
        unsafe {
            target::LLVM_InitializeAllTargets();
            target::LLVM_InitializeAllTargetInfos();
            target::LLVM_InitializeAllTargetMCs();
            target::LLVM_InitializeAllAsmPrinters();
            target::LLVM_InitializeAllAsmParsers();
            target::LLVM_InitializeAllDisassemblers();
        }
    }

    pub fn new_default() -> Self {
        Self::new_from_triple_internal(unsafe { target_machine::LLVMGetDefaultTargetTriple() })
            .expect("Default target triple doesn't exist")
    }

    pub fn new_from_triple(triple: &str) -> Result<Self, LLVMMessage> {
        with_string(triple, |triple| Self::new_from_triple_internal(*triple))
    }

    fn new_from_triple_internal(triple: *const libc::c_char) -> Result<Self, LLVMMessage> {
        let mut target_ref = ptr::null_mut();
        let mut error = ptr::null_mut();
        let target_found = unsafe {
            target_machine::LLVMGetTargetFromTriple(
                triple,
                &mut target_ref as *mut _,
                &mut error as *mut _,
            )
        };
        if target_found == 1 {
            return Err(LLVMMessage::new(error));
        }
        Ok(Self { target: target_ref })
    }

    pub fn get_name(&self) -> &str {
        unsafe { ffi::CStr::from_ptr(target_machine::LLVMGetTargetName(self.target)) }
            .to_str()
            .unwrap()
    }
}

pub struct TargetMachine {
    target_machine: target_machine::LLVMTargetMachineRef,
}

impl TargetMachine {
    pub fn new_from_triple(target_triple: &str) -> Self {
        with_string(target_triple, |target_triple| {
            Self::new_with_triple(
                LLVMTarget::new_from_triple_internal(*target_triple).expect("Unknown target"),
                *target_triple,
            )
        })
    }

    pub fn new_with_triple(target: LLVMTarget, triple: *const libc::c_char) -> Self {
        let target_machine_options = unsafe { target_machine::LLVMCreateTargetMachineOptions() };
        let machine = unsafe {
            target_machine::LLVMCreateTargetMachineWithOptions(
                target.target,
                triple,
                target_machine_options,
            )
        };
        Self {
            target_machine: machine,
        }
    }
}

struct TargetMachineOptions {
    options: target_machine::LLVMTargetMachineOptionsRef,
}

impl TargetMachineOptions {
    fn new() -> Self {
        let options = unsafe { target_machine::LLVMCreateTargetMachineOptions() };
        Self { options }
    }
}

impl Drop for TargetMachineOptions {
    fn drop(&mut self) {
        unsafe {
            target_machine::LLVMDisposeTargetMachineOptions(self.options);
        }
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

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct BRef {
    block: LLVMBasicBlockRef,
}

unsafe impl TransparentDowncast for BRef {
    type Inner = LLVMBasicBlockRef;
}

impl Builder {
    fn new_in_context(context: &Ctxt) -> Self {
        let builder = unsafe { core::LLVMCreateBuilderInContext(context.context) };
        Self { builder }
    }

    pub fn build_block(&self, block: BRef) -> BlockBuilder {
        BlockBuilder::build_block(self, block)
    }

    pub fn create_block(&self, context: &Ctxt, func: VRef, name: &str) -> BRef {
        BlockBuilder::create_block(context, func, name)
    }

    fn build_add(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildAdd(self.builder, left.value, right.value, *name),
            }
        })
    }

    fn build_sub(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildSub(self.builder, left.value, right.value, *name),
            }
        })
    }

    fn build_mul(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildMul(self.builder, left.value, right.value, *name),
            }
        })
    }

    fn build_idiv(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildSDiv(self.builder, left.value, right.value, *name),
            }
        })
    }

    fn build_imod(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildSRem(self.builder, left.value, right.value, *name),
            }
        })
    }

    fn build_udiv(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildUDiv(self.builder, left.value, right.value, *name),
            }
        })
    }

    fn build_umod(&self, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildURem(self.builder, left.value, right.value, *name),
            }
        })
    }

    fn build_phi(&self, ty: Ty, phi: impl FnOnce(&mut PhiBuilder), name: &str) -> VRef {
        let phi_v = with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildPhi(self.builder, ty.ty, *name),
            }
        });
        let mut phi_builder = PhiBuilder { phi: phi_v };
        phi(&mut phi_builder);

        phi_v
    }

    fn build_int_cmp(&self, op: IntCmp, left: VRef, right: VRef, name: &str) -> VRef {
        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildICmp(
                    self.builder,
                    op.to_llvm_int_predicate(),
                    left.value,
                    right.value,
                    *name,
                ),
            }
        })
    }

    fn build_br(&self, block: BRef) {
        unsafe {
            core::LLVMBuildBr(self.builder, block.block);
        }
    }

    fn build_cond_br(&self, cond: VRef, then_block: BRef, else_block: BRef) {
        unsafe {
            core::LLVMBuildCondBr(self.builder, cond.value, then_block.block, else_block.block);
        }
    }

    fn build_call(&self, func: VRef, func_ty: Ty, args: &mut [VRef], name: &str) -> VRef {
        let (args, args_count) = transparent_downcast_mut(args);

        with_string(name, |name| unsafe {
            VRef {
                value: core::LLVMBuildCall2(
                    self.builder,
                    func_ty.ty,
                    func.value,
                    args,
                    args_count,
                    *name,
                ),
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

pub(crate) struct PhiBuilder {
    phi: VRef,
}

impl PhiBuilder {
    pub fn branch(&mut self, block: BRef, value: VRef) {
        unsafe {
            core::LLVMAddIncoming(
                self.phi.value,
                &mut [value.value] as *mut LLVMValueRef,
                &mut [block.block] as *mut LLVMBasicBlockRef,
                1,
            );
        }
    }
}

pub(crate) struct BlockBuilder<'b> {
    builder: &'b Builder,
    block: BRef,
}

impl<'b> BlockBuilder<'b> {
    pub fn build_block(builder: &'b Builder, block: BRef) -> Self {
        unsafe {
            core::LLVMPositionBuilderAtEnd(builder.builder, block.block);
        }

        Self { builder, block }
    }

    pub fn create_block(context: &Ctxt, func: VRef, name: &str) -> BRef {
        with_string(name, |name| unsafe {
            BRef {
                block: core::LLVMAppendBasicBlockInContext(context.context, func.value, *name),
            }
        })
    }

    pub fn add(&self, left: VRef, right: VRef) -> VRef {
        self.builder.build_add(left, right, "add")
    }

    pub fn sub(&self, left: VRef, right: VRef) -> VRef {
        self.builder.build_sub(left, right, "sub")
    }

    pub fn mul(&self, left: VRef, right: VRef) -> VRef {
        self.builder.build_mul(left, right, "mul")
    }

    pub fn idiv(&self, left: VRef, right: VRef) -> VRef {
        self.builder.build_idiv(left, right, "idiv")
    }

    pub fn udiv(&self, left: VRef, right: VRef) -> VRef {
        self.builder.build_udiv(left, right, "udiv")
    }

    pub fn imod(&self, left: VRef, right: VRef) -> VRef {
        self.builder.build_imod(left, right, "imod")
    }

    pub fn umod(&self, left: VRef, right: VRef) -> VRef {
        self.builder.build_umod(left, right, "umod")
    }

    pub fn phi(&self, ty: Ty, phi: impl FnOnce(&mut PhiBuilder)) -> VRef {
        self.builder.build_phi(ty, phi, "phi")
    }

    pub fn int_cmp(&self, op: IntCmp, left: VRef, right: VRef) -> VRef {
        self.builder.build_int_cmp(op, left, right, "cmp")
    }

    pub fn br(&self, block: BRef) {
        self.builder.build_br(block);
    }

    pub fn cond_br(&self, cond: VRef, then_block: BRef, else_block: BRef) {
        self.builder.build_cond_br(cond, then_block, else_block);
    }

    pub fn call(&self, func: VRef, func_ty: Ty, args: &mut [VRef]) -> VRef {
        self.builder.build_call(func, func_ty, args, "call")
    }

    pub fn ret_void(&self) -> VRef {
        self.builder.build_ret_void()
    }

    pub fn ret(&self, value: VRef) -> VRef {
        self.builder.build_ret(value)
    }
}

#[derive(Clone, Copy)]
#[expect(
    clippy::upper_case_acronyms,
    reason = "LLVM uses acronyms for this, it's easier."
)]
pub enum IntCmp {
    EQ,
    NE,
    UGT,
    UGE,
    ULT,
    ULE,
    SGT,
    SGE,
    SLT,
    SLE,
}

impl IntCmp {
    fn to_llvm_int_predicate(self) -> LLVMIntPredicate {
        match self {
            IntCmp::EQ => LLVMIntPredicate::LLVMIntEQ,
            IntCmp::NE => LLVMIntPredicate::LLVMIntNE,
            IntCmp::UGT => LLVMIntPredicate::LLVMIntUGT,
            IntCmp::UGE => LLVMIntPredicate::LLVMIntUGE,
            IntCmp::ULT => LLVMIntPredicate::LLVMIntULT,
            IntCmp::ULE => LLVMIntPredicate::LLVMIntULE,
            IntCmp::SGT => LLVMIntPredicate::LLVMIntSGT,
            IntCmp::SGE => LLVMIntPredicate::LLVMIntSGE,
            IntCmp::SLT => LLVMIntPredicate::LLVMIntSLT,
            IntCmp::SLE => LLVMIntPredicate::LLVMIntSLE,
        }
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct VRef {
    value: LLVMValueRef,
}

unsafe impl TransparentDowncast for VRef {
    type Inner = LLVMValueRef;
}

impl VRef {
    pub fn get_param(&self, param_id: u32) -> VRef {
        VRef {
            value: unsafe { core::LLVMGetParam(self.value, param_id) },
        }
    }
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
        Ty { ty: self.i32 }
    }
}

fn with_string<T, F>(s: &str, f: F) -> T
where
    F: for<'a> FnOnce(&'a *mut libc::c_char) -> T,
{
    let s = ffi::CString::new(s).unwrap();
    f(&(s.as_ptr() as *mut libc::c_char))
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
    buf: Vec<libc::c_char>,
}

impl SBuffer {
    fn new(buf: Vec<libc::c_char>) -> Self {
        Self { buf }
    }

    fn push_str(&mut self, s: &str) -> *mut ffi::c_char {
        if s.len() + 1 + self.buf.len() > self.buf.capacity() {
            panic!("buffer overflow");
        }

        let current_offset = self.buf.len();

        self.buf
            .extend(s.bytes().chain(iter::once(0)).map(|b| b as ffi::c_char));

        self.buf[current_offset..].as_mut_ptr()
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
    a: &str => *mut libc::c_char => *mut libc::c_char,
);

impl_string_tuple!(
    a: &str => *mut libc::c_char => *mut libc::c_char,
    b: &str => *mut libc::c_char => *mut libc::c_char,
);

impl_string_tuple!(
    a: &str => *mut libc::c_char => *mut libc::c_char,
    b: &str => *mut libc::c_char => *mut libc::c_char,
    c: &str => *mut libc::c_char => *mut libc::c_char,
);

impl_string_tuple!(
    a: &str => *mut libc::c_char => *mut libc::c_char,
    b: &str => *mut libc::c_char => *mut libc::c_char,
    c: &str => *mut libc::c_char => *mut libc::c_char,
    d: &str => *mut libc::c_char => *mut libc::c_char,
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

impl fmt::Debug for LLVMMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string().to_str().unwrap())
    }
}

impl Drop for LLVMMessage {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeMessage(self.ptr as *mut _);
        }
    }
}

#[must_use]
pub struct LLVMError {
    ptr: error::LLVMErrorRef,
}

impl LLVMError {
    pub fn new(ptr: error::LLVMErrorRef) -> Self {
        Self { ptr }
    }

    pub fn cant_fail(self) {
        unsafe {
            error::LLVMCantFail(self.ptr);
        }
    }
}
