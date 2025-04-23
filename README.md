<div align="center">
  <a href="https://diablo-lang.org" target="_blank"><img src="https://diablo-lang.org/img/logo.png" width="250" /></a>

  <h1>Diablo</h1>

  <p>
    <strong>The Diablo Programming Language</strong>
  </p>

  <p>
    <a href="https://ocaml.org/"><img alt="OCaml Programming Language" src="https://img.shields.io/badge/OCaml-%23f18c02.svg?style=flat-square&logo=ocaml&logoColor=white"/></a>
  </p>
</div>

## Description

Diablo is a statically typed, functional language designed to augment research in information security and the natural sciences.

The compiler is written in OCaml and uses LLVM as its backend.

It features an intuitive syntax that allows for rapidly developing prototypes:

```
# This is a comment
fn hello_world() {
    print("Hello World!")
}
```

Refer to the [getting started guide](https://diablo-lang.org/guides/getting-started) to get up and running quickly. Also, check out the official [style guide](https://diablo-lang.org/guides/style-guide) for best practices.

### Type Inference

Hindley-Milner Algorithm W is used for type inference. Global functions require explicit type annotations. Although this could be inferred, it is required to be annotated to provide better readability and understanding of the interface.

### Language Constructs

```
# Modules
module Math {
  # Let expression in Math namespace
  pi = 3.14

  # Function expression
  fn add(a: int, b: int) -> int {
    return a + b
  }
}

# Import statement (doesn't return anything)
import Math

# Type declaration
type Person = {
  name: str,
  age: int
}

# Create Person record
p = Person("Foo", 42)

# Let expression
x = 12
y = Math.add(x, Math.pi)
b = y >= x

# List of values (all same type)
l = [1, 2, 3, Math.pi, x, y]
bl = [b, true, false]

# Function expression
fn foo (x: bool, y: str) -> int {
  z = 12 + 32
  # Return last expression (writing "return" is optional)
  z + 12
}

# Function with unit (nil) return type
# Maybe make "impure fn" since it has side effect
fn out (msg: str) -> nil {
  combined_msg = concat("hello", msg)
  print(combined_msg)
}

# Lambda
a = lambda (x: int) -> int { x + 24 }

# Application
b = 12 + a(12)

# Conditional expression
c = 3 + (if 5 == 6 then 10 else 100)

# Pattern matching
fn is_zero(x: int) -> bool {
  match x {
    0 -> true
    _ -> false
  }
}

# Recursion
fn factorial(n: int) -> int {
  if n == 0 then 1 else n * factorial(n - 1)
}

# Polymorphic Types (Generics)
fn map<T>(fn f: T -> T, list: List<T>) -> List<T> {
  match list {
    [] -> []
    x:xs -> f(x) : map(f, xs)
  }
}
```

### Closures

- All functions are closures. Global functions are closures that have a name and don't capture any values.
- External functions are not closures, just function pointers.

```
# Closure (global function)
fn foo(x: int) -> int {
  # Not a closure
  extern __foo(x)
}
```

### Garbage Collector

TBD

### BNF Grammar

```
expr ::= id                                        -- identifier (refer to values)
    | int                                          -- integer
    | bool                                         -- boolean
    | e1 binop e2                                  -- binary operation
    | if e1 then e2 else e3                        -- if-then-else expression
    | let x = e                                    -- let expression (for binding values)
    | fn x -> e                                    -- function abstraction (function definition)
    | e1 e2                                        -- function application
    | [e1, e2, ..., en]                            -- list

binop ::= + | - | * | / | < | > | <= | >= | ==     -- binary operators

id ::= <identifiers>                               -- identifier names (immutable, bound within scopes)

int ::= <integers>                                 -- integer values

bool ::= true | false                              -- boolean values
```

### Building

Build source.

```sh
dune build
```

Compile and dump LLVM IR to `llvm_bin/output.ll`.

```sh
dune exec -- diablo examples/example.dbl
```

Compile dynamically linked C library.

```sh
gcc -shared -fPIC -o libsocket.so socket.c
```

Generate executable from LLVM IR linked with a specified library.

```sh
clang-19 -target x86_64-pc-linux-gnu llvm_bin/output.ll -o llvm_bin/output -L. -lsocket -Wl,-rpath=.
```

Generate executable from LLVM IR linked with libc.

```sh
clang-19 -target x86_64-pc-linux-gnu llvm_bin/output.ll -o llvm_bin/output -lc -Wl,-rpath=.
```

Format all OCaml and dune files.

```sh
opam exec -- dune fmt
```

Lint opam file(s).

```sh
opam lint
```

## Contributing

TBD - Not open for contributions at this time until alpha version is implemented.

## License

Diablo source code is released under the [Apache License 2.0](./LICENSE).

## Resources

- [OCaml Type Systems](https://github.com/tomprimozic/type-systems/tree/master)
- [OCaml LLVM Kaleidoscope](https://releases.llvm.org/12.0.0/docs/tutorial/index.html)
- [OCaml LLVM API Documentation](https://llvm.moe/ocaml/Llvm.html)
- [Swift Closures](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/closures/)
- [OCaml Programming: Correct + Efficient + Beautiful](https://cs3110.github.io/textbook)
- [Creating the Bolt Compiler](https://mukulrathi.com/create-your-own-programming-language/intro-to-compiler/)
