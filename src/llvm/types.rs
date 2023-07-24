use inkwell::{values::{IntValue, PointerValue, FloatValue, FunctionValue, BasicValueEnum}, basic_block::BasicBlock, types::BasicTypeEnum};


/// This is the most important
/// This handles the full values in the compiler
pub enum Value<'a> {
    Int(IntValue<'a>),
    Pointer(PointerValue<'a>),
    // String(PointerValue<'a>),
    Bool(IntValue<'a>),
    Float(FloatValue<'a>),
}
impl<'a> Value <'a>{
    pub(crate) fn as_int(&self) -> IntValue<'a> {
        match self {
            Value::Int(i) => i.clone(),
            Value::Bool(i) => i.clone(),
            _ => unreachable!()
        }
    }
    pub(crate) fn as_float(&self) -> FloatValue<'a> {
        match self {
            Value::Float(f) => f.clone(),
            _ => unreachable!()
        }
    }
    pub(crate) fn as_ptr(&self) -> PointerValue<'a> {
        match self {
            Value::Pointer(s) => s.clone(),
            _ => unreachable!()
        }
    }
    pub(crate) fn as_bool(&self) -> IntValue<'a> {
        match self {
            Value::Bool(b) => b.clone(),
            _ => unreachable!()
        }
    }
    pub(crate) fn as_basic_value(&self) -> BasicValueEnum<'a> {
        match self {
            Value::Int(i) => i.clone().into(),
            Value::Float(f) => f.clone().into(),
            Value::Pointer(s) => s.clone().into(),
            Value::Bool(b) => b.clone().into(),
        }
    }
    pub(crate) fn is_float(&self) -> bool {
        match self {
            Value::Float(_) => true,
            _ => false,
        }
    }
}
/// Basic function value 
pub struct FunctionVal<'a> {
    pub (crate) block: Option<BasicBlock<'a>>,
    pub (crate) value: FunctionValue<'a>,
}
/// Variable
pub struct Variable<'a> {
    pub (crate) value: PointerValue<'a>,
    pub (crate) var_type: BasicTypeEnum<'a>,
}