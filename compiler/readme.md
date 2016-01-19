# Inroduction
cpi is a language for programming computers on earth.  Consider yourself intruduced.

## What does CPI stand for?
cpi doesn't stand for anything.  It's not really an idealist.

## How do I declare variables?
Declaring a variable means never having to say you're sorry.  Wait, no. That's love I'm thinking of. Declaring a variable reserves space for that variable on the stack, and relates that allocated space to a name.
  * Mutable variables are declared by giving a name, two colons and a type. Their values can be changed at any time  Here we are declaring a 32-bit signed integer. (See [types](todo) for more info on types).
    ```
    a :: i32;
    ```
  * If you want, you can use a value in place of the type.  The type of the variable will be inferred as the type of the value.  Here is another way to write the same declaration, since 3 is inferred as an i32:
    ```
    a :: 3;
    ```
  * You can declare variables and assign a value to them in the same statement:
    ```
    a :: i32 = 3;
    ```
  * You can leave out the type if you want and the compiler will infer it:
    ```
    a ::= 3;
    ```
  * immutable
    * `a := 2; a = 3; -- error, cannot assign to immutable variable 'a'`
* number formatting
  * `1_000_000`
  * `0b100101`
  * `0x3FA9C3`
* binary operators
  * ```
  a := 8;
  b := 30;
  a += b /= 3;
  ret a;
  ```
* `defer`
* procedures
  * inferred return type
  * shorthand and longhand return
  * anonymous and `rec`
* symbol names (``` `one symbol` ```)
* argument de-structuring
* `|` and `..|`
  * scope
  * explain how pipes work with scope
* pointers, dereference and address-of
* structs
  * type declaration
  * anonymous
  * integer field accessor (e.g. `foo.2`)
  * recursive types
* `do`
* `typeof`
* `rettype`
* module level constants
* module level variables

---

# TODO

* literal conversion
  * `a : i64 = 3` for example


* debugger (godebug style)

* prove that it's actually easy to implement language features as libraries by implementing algebraic data types (sum types) as a library (with full debug support).

* make the compiler more friendly toward people who want to use it as a library.  `#include <cpi.h>` and provide simple c interface for reading, lexing, parsing, codegen, etc.

* make the compiler more unix-like? Perhaps break each pass into its own standalone program so that they can be piped together

* special operators
  * `cmpxchg`


* better std library

* bindings project similar to deimos in D to bind with popular C libraries
