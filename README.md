# Ruse Programming Language

Ruse is a dynamically typed language with a tree walk interpreter. Its main purpose? Teaching me how to build such a
project. I've learned a ton while working on it, and I'm looking forward to learning even more as I continue. I've got
some plans in mind, but we'll see which ones actually materialize.

> Ruse currently has no official license, but here are rules I will lay out:
>
> * No express or implied warranty.
> * Modifications or distributions my source code, MUST attribute myself, and this project appropriately.
> * That's it :)

## Quick Start

1. Install Rust (nightly)
2. Clone the repo
3. Run `cargo run --release`

## Examples

> All examples use `rust` as the language tag for syntax highlighting.

Ruse follows an expression-based syntax. Most things are valid expressions, with a few exceptions that typically use
protected keywords.

```rust
let x = 10
let y = x + 2

let z = if y > 10 {
- 5
} else {
5
} // if statements are an expression

print(z=2) // z = 2 now, assignment is an expression
```

Blocks are expressions too! The last statement in a block is considered its value.

```rust
let t = {
// lexical scoping
1 // and implicit return
}

{
// but unlike rust, we ignore the semicolon
2;
} == 2 // true
```

Functions have only a few minor twists:

```rust
fn add(a, b) => {
    a+b
}
add(1, 2)

// shorthand notation
fn sub(a, b) => a-b

// variadics and defaults
fn add(a, *b, c= 2, d) => (a,b,c,d)
add(1, 2, 3, d=5) // (1,(2,3),2,5)
add(1, 2, 3, c=4, d=5) // (1,(2,3),4,5)

// lambdas too
(|a,b| a+b)(1,2) // 3
```

Ruse functions are first class. This means that you can assign them to variables.

```rust
fn say_hi() => print("Hello!")
let hello = say_hi
hello()
```

Closures happen to be quite simple in Ruse.

```rust
fn print_args(f) => |*args| {
    print(*args, sep = ", ")
    f(*args)
}

fn add(a, b) => a+b
add = print_args(add)

print(add(1,2))
/*
1, 2
3
*/ 
```