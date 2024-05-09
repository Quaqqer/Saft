# Saft

Saft is my custom programming language. The syntax has a high resemblence to
rust and some concepts have carried over, such as block expressions and control
flow expressions, and tail expressions being returned automatically.

## State of the language

Currently saft has a lexer, parser, ir (with lowerer) and a bytecode back-end,
which both compiles and interprets bytecode. I have added type annotations, but
type inference and checking has not been implemented yet, so types do nothing
at the moment.

## Examples

Below is an example of the fibonacci function, implemented recursively.

```
fn fib(i: int) -> int {
    if i < 2 {
        i
    } else {
        fib(i-1) + fib(i-2)
    }
}
```

Here is the fibonacci function implemented with a loop.

```
fn fib(n: int) -> int {
    if n == 0 { return 0; };

    let a = 0;
    let b = 1;

    loop {
        if n == 1 {
            break b;
        };

        let c = a + b;
        a = b;
        b = c;

        n = n - 1;
    }
}
```
