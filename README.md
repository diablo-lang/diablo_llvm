# Diablo

Compiled functional programming language with static typing and a LLVM backend.

<div align="center">
  <a href="https://diablo-lang.org" target="_blank"><img src="./img/logo.png" width="250" /></a>

  <h1>Diablo</h1>

  <p>
    <strong>The Diablo Programming Language</strong>
  </p>

  <p>
    <a href="https://www.crystal-lang.org/"><img alt="The Crystal Programming Language" src="https://img.shields.io/badge/crystal-%23555555.svg?style=flat-square&logo=crystal&logoColor=white"/></a>
  </p>
</div>

## Description

Diablo is a functional language designed to augment research in information security and the natural sciences.

The compiler was written in OCaml and uses LLVM 
Diablo is an interpreted language that runs on top of a bytecode virtual machine (VM). All the core components of the language are written in Crystal.

It features an intuitive syntax that allows for rapidly developing prototypes:

```
fn hello_world() {
    print("Hello World!")
}
```

Refer to the [getting started guide](https://diablo-lang.org/guides/getting-started) to get up and running quickly. Also, check out the official [style guide](https://diablo-lang.org/guides/style-guide) for best practices.

### BNF Grammar

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

## Resources

- [OCaml Programming: Correct + Efficient + Beautiful](https://cs3110.github.io/textbook)
- [Creating the Bolt Compiler](https://mukulrathi.com/create-your-own-programming-language/intro-to-compiler/)

## Contributing

TBD - Not open for contributions at this time until alpha version is implemented.

## License

TBD
