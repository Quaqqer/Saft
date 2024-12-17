use crate::value::Cast;
use crate::value::NativeFunction;
use crate::value::Value;
use crate::vm;
use crate::vm::exotic;
use saft_macro::native_function;
use saft_syntax::span::Span;

pub struct NativeRes(pub Result<Value, vm::Error>);

impl<T: Into<Value>> From<Result<T, vm::Error>> for NativeRes {
    fn from(value: Result<T, vm::Error>) -> Self {
        NativeRes(value.map(Into::into))
    }
}

impl<T: Into<Value>> From<T> for NativeRes {
    fn from(value: T) -> Self {
        NativeRes(Ok(value.into()))
    }
}

#[native_function]
pub fn print(v: Value) {
    match v {
        Value::String(s) => println!("{}", s),
        _ => println!("{}", v.repr()),
    }
}

#[native_function]
pub fn sin(v: f64) -> f64 {
    v.sin()
}

#[native_function]
pub fn cos(v: f64) -> f64 {
    v.cos()
}

#[native_function]
pub fn reverse_bits(v: i64) -> i64 {
    v.reverse_bits()
}

#[native_function]
pub fn hypot(x: f64, y: f64) -> f64 {
    f64::hypot(x, y)
}

#[native_function]
pub fn fib(v: i64, span: &Span) -> Result<i64, vm::Error> {
    if v < 0 {
        exotic!(
            "Only positive numbers can be used in the fibonacci sequence.",
            span
        );
    } else {
        let (mut a, mut b) = (0, 1);

        for _ in 0..v {
            (a, b) = (b, a + b);
        }

        Ok(a)
    }
}
