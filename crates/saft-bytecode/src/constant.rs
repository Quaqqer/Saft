use crate::{value::SaftFunction, vm};

#[derive(Clone, Debug)]
pub enum Constant {
    SaftFunction(SaftFunction),
}

#[derive(Clone)]
pub struct NativeFunction {
    _f: fn(&mut vm::Vm) -> Result<(), vm::Error>,
}
