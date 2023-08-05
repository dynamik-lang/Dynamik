use inkwell::{
    basic_block::BasicBlock,
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue},
};

/// This is the most important
/// This handles the full values in the compiler
#[derive(Debug, Clone, Copy)]
pub enum Value<'a> {
    Int(IntValue<'a>),
    Pointer(PointerValue<'a>),
    // String(PointerValue<'a>),
    Bool(IntValue<'a>),
    Float(FloatValue<'a>),
}

impl<'a> Value<'a> {
    pub(crate) fn as_int(&self) -> IntValue<'a> {
        match self {
            Value::Int(i) => i.clone(),
            Value::Bool(i) => i.clone(),
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_float(&self) -> FloatValue<'a> {
        match self {
            Value::Float(f) => f.clone(),
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_ptr(&self) -> PointerValue<'a> {
        match self {
            Value::Pointer(s) => s.clone(),
            _ => unreachable!(),
        }
    }
    pub fn is_ptr(&self) -> bool {
        match self {
            Value::Pointer(_) => true,
            _ => unreachable!(),
        }
    }
    pub(crate) fn as_bool(&self) -> IntValue<'a> {
        match self {
            Value::Bool(b) => *b,
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_basic_value(&self) -> BasicValueEnum<'a> {
        match self {
            Value::Int(i) => (*i).into(),
            Value::Float(f) => (*f).into(),
            Value::Pointer(s) => (*s).into(),
            Value::Bool(b) => (*b).into(),
        }
    }

    pub(crate) fn is_float(&self) -> bool {
        matches!(self, Value::Bool(_))
    }
}

impl<'ctx> From<BasicValueEnum<'ctx>> for Value<'ctx> {
    fn from(value: BasicValueEnum<'ctx>) -> Self {
        match value {
            BasicValueEnum::IntValue(i) if i.get_type().get_bit_width() == 1 => Self::Bool(i),
            BasicValueEnum::IntValue(i) => Self::Int(i),
            BasicValueEnum::FloatValue(i) => Self::Float(i),
            BasicValueEnum::PointerValue(i) => Self::Pointer(i),

            _ => unimplemented!("Unimplemented type"),
        }
    }
}

/// Function value
#[derive(Debug, Clone, Copy)]
pub struct FunctionVal<'a> {
    pub(crate) block: Option<BasicBlock<'a>>,
    pub(crate) value: FunctionValue<'a>,
}

/// Variable
#[derive(Debug, Clone, Copy)]
pub struct Variable<'a> {
    pub(crate) ptr: PointerValue<'a>,
    pub(crate) var_type: BasicTypeEnum<'a>,
}
