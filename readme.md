# Inroduction
cpi is a language for programming computers on earth.  Consider yourself intruduced.

## What does CPI stand for?
cpi doesn't stand for anything.  It's not really an idealist.

## What's the point?
I wanted a language that was pretty much like C but with more stuff I wanted and less stuff I didn't want.  So I made one. Yeah yeah, pretty much everyone's story I know.
It compiles to llvm ir (literally, it spits out a .ll file). 
I know the world doesn't need another programming language but I actually think there's some cool ideas in here.  And it is such a fun project!
I find it nicer to work with than C for the same kinds of tasks, and it is as performant.


## Just show me an example
Fine you asked for it.  Here's fizzbuzz, the ultimate test of any programming language

```go
require "c.cpi"

module fizzbuzz
{
	main := {} =>
	{
		i ::= 0;
		while i < 100
		{
			import c.io;

			if i mod 15 == 0  { println("fizzbuzz"); }
			elif i mod 3 == 0  { println("fizz"); }
			elif i mod 5 == 0  { println("buzz"); }
			else { i | print_i32; }

      			i += 1;
		}
	}
}
```

## Comments
Comments are `--` instead of `//` like in C, because I think it's prettier.
There are no block comments, only line comments.

## Declaring Variables
Declaring a variable means never having to say you're sorry.  Wait, no. That's love I'm thinking of. Declaring a variable reserves space on the stack, and gives the allocated space a name so you can refer to it later.

#### Symbol names:
a symbol is any alphabetical character ([a-zA-Z]) followed by any alphanumeric character ([a-zA-Z0-9]).  However, you can use any character at all as long as you wrap it in backticks.  So the following is legal:
	
```go
`one $up3r-awesome symbol` := 3;
```
* Mutable variable declaration is done by writing a symbol, two colons and then the type:

```go
a :: i32;
```
* If you want, you can use a value in place of the type.  The type of the variable will be inferred as the type of the value.  Here is another way to write the same declaration, since 3 is inferred as an i32:

```go
a :: 3;
```
* You can declare variables and assign a value to them in the same statement:

```go
a :: i32 = 3;
```
* You can leave out the type if you are lazy and the compiler will fill it in for you:

```go
a ::= 3;
```
* Immutable variables must be allocated and assigned in the same statement. They cannot be reassigned.

```go
a := 2;
```
* If you try to put the following fragment in a program and run it:

```go
a := 3;
a = 5;
```
you will get an error message like this:       

```
foo.cpi 6:5-11 error: cannot assign to an immutable declaration
NOTE: original declaration is here: foo.cpi 5:5-12
```
assuming that the original `a := 3` declaration was made in file foo.cpi on line 5, columns 5-12.

#### Formatting integers
* The lexer will read the following types of integers
  * plain old decimal literals numbers: `123`
  * binary literals: `0b100101`
  * hex literals: `0x3FA9C3`
  * ignores any underscores: `1_000_000`

## Declaring Types:
You can declare a named type and use it in place of the type it represents.  Use the name of the type to construct a value:

```go
type foo = i32;
f := foo(13);
```
Named types are unique.  Even if two named types reference the same underlying type they are not the same.  They can be converted however:

```go
type bar = i32;
g : bar = f; -- *error, type mismatch*
g := bar(f); -- *ok*
```

#### Struct types
Structs are easy to declare and initialize. No keywords or anything.  You can construct a struct by using the name of the struct like a function.  
The order of the arguments never matters if all arguments are specified.

 ```go
 type vec2 = {x : i32, y : i32};
v2_1 := vec2(x : 3, y : 10);
v2_2 := vec2(3, 10);
v2_3 := vec2(y : 10, x : 3);
 ```
Structs can have named and unnamed fields.  All unnamed fields must come before all named fields.

```go
type vec2 := {i32, i32};
v2 := vec2(5, 10);

type vec3 = {i32, i32, z : i32};
v3 := vec3(3, 4, z: 5);
v3_2 := vec3(3, 4, 5);
```

Structs can have default and explicit (non-default) arguments.  All default arguments must come after all explicit arguments

```go
type dfault = {x : i32 y : i32, flag := false}
d1 := dfault(x : 5, y : 8);
d2 := dfault(5, 8);
d3 := dfault(x : i32, y : i32, flag : true);
d4 := dfault(y : 8, flag: true, x : 5);
```

#### Strings
`string` is a builtin type which equates to the following struct type: `{length: i32, ptr: *i8}`.  Just a length and pointer.  Simple, yet a little better than the null-terminated thing that C has.

#### Anonymous struct values:
Anonymous structs (structs which do not come from a `type` definition) are declared with a `#` symbol followed by name value pairs (or just values) wrapped in curly braces and separated by commas.
Contrived example:

```go
a := #{x : 5, y : 10} -- inferred as {x : i32, y : i32}
b := #{5, 10} -- inferred as {i32, i32}
```

#### Accessing fields:
You can access fields by using a dot, as with pretty much every other language.  You can do this to get and set values.  Contrived Example:

```go
fieldx := a.x;
fieldy := a.y;
a.x = 15;
a.y = 89;
```

#### Integer field accessor:
You can reference fields in structs by using the order they were declared in.  Again using the previous example, `field0 := a.0` would set `field0` to 5, and `field1 := a.1` would set the variable `field1` to 10.

Structs can have default arguments. All arguments without a default value must come before all arguments with a default value.  The types of fields with default values can be inferred.

```go
type line = {slope : i32, `y-intercept` := 0.0};
l1 := line(3, 0.5);
l2 := line(slope: 3, `y-intercept`: 1.3);
l2 := line(3, `y-intercept`: 1.3);
l3 := line(5);
```

#### Type conversion:
You can convert a value to any type that makes sense by applying the type to it as if it were a function:

```go
a : i32 = 10;
b := i64(a);
c := f32(a);
d := char(65);
```
etc.

## Procedures
Procedures follow the C calling convention by default.  At some point we should probably support other calling conventions.  Pretty much anything can be done inside a procedure, including declaring another procedure, declaring/importing modules, requiring files, taking over the world, etc.

####Declaring an external procedure (to link with a c library for example):

```go
add_one : {n : i32} => i32;
```

#### Declaring a procedure and implementing it (*loooooong way*)

```go
add_one : {n : i32} => i32 = {n : i32} => {
	ret n + 1;
}
```

#### Declaring procedure and implementing it (inferred return type)

```go
add_one := {n : i32} => {
  ret n + 1;
}
```

#### Expression return (shorthand)

```go
add_one := {n : i32} => n + 1;
```

#### Default arguments:

```go
do_stuff := {m : i32, go_insane := false} => {
	if go_insane { ret 1_000; }
	ret m;
};
r1 := add_two(5); -- *returns 5*
r2 := add_two(m : 5); -- *returns 5*
r3 := add_two(5, go_insane: true); -- *returns 1_000*
r4 := add_two(m : 5, go_insane: true); -- *returns 1_000*
r5 := add_two(m : 5, true); -- *error all named arguments must come after all unnamed arguments*
```

#### Like structs, functions can have default arguments:

```go
dfault := {a := 3, b := 4} => a + b;
d1 := dfault(); -- 7
d2 := dfault(5); -- 5 + 4 = 9
d3 := dfault(5, 8); -- 5 + 8 = 13
d4 := dfault(b: 1); -- 3 + 1 = 4
```

#### Rec 
To let a function call itself recursively, use the `rec` keyword.  Most languages just reuse the name of the function to call itself, but this means that the function must have a name.  Using the `rec` keyword means you can have a recursive anonymous function.  For example here is an anonymous fibonacci funciton:

```go
{n : i32} => {
	answer := 1;
	if n == 0 or n == 1 { ret answer; }
	answer = rec(n - 1) + rec(n - 2);
	ret answer;
}
```

#### Argument de-structuring
If a function takes more than 1 argument, you can pass in a tuple of arguments instead as long as each field in the tuple corresponds to one argument. Use the `..` prefix operator to desctructure the tuple.  Contrived example:

```go
sum := {x : i32, y : i32} => x + y;
get_args := {} => #{3, 4};
s := sum(..get_args());
```

#### `|` and `..|`
The `|` binary operator works kind of like how a pipe works in unix: it takes the output of whatever is to the left of it and passes it to whatever is to the right of it.  If the rhs is a function, it calls the function with the lhs.  If the rhs is the application of a function with several parameters, it inserts the lhs as the first parameter.  Contrived example:
* `3 | foo` becomes `foo(3)`
* `3 | foo(4, 5)` becomes `foo(3, 4, 5)`
This means you can do things like `x|foo|bar|baz` instead of `baz(bar(foo(x)))`, which can make some things much more readable.

In the case where you need to destructure the lhs of a `|` operator, use the `..|` operator instead.  Contrived example:

```go
sum_diff := {x : i32, y : i32} => #{x + y, x - y};
sd2 := #{x : 3, y : 10} ..| sum_diff ..| sum_diff;
```

## Binary operators

`and` and `or` [short-circuit](https://en.wikipedia.org/wiki/Short-circuit_evaluation), while `bitand` and `bitor` do bitwise operations and do not [short-circuit](https://en.wikipedia.org/wiki/Short-circuit_evaluation).  I'll leave it up to you to figure out the rest.
Here's the full list: `+`, `-`, `*`, `/`, `xor`, `mod`, `bitand`, `bitor`, `and`, `or`, `==`, `!=`,  `<`, `<=`, `>`, `>=`

You can also place an `=` at the end of the first 8 on that list.  This applies the operation to the two values and stores the result in the lhs of the binary operator.  Everything else is the same.  So for example in the last statement of the following fragment b first gets set to b/3 = 10 (because division has a higher precedence than addition), and then a gets set to a+10 = 18.

```go
a := 8;
b := 30;
a += b /= 3;
```

## Defer

I'm getting lazy now.  Go read [golang's description of defer](https://golang.org/doc/effective_go.html#defer) if you care. Basically when a scope is exited any defered statements in that scope are run. example:

```go
do {
	defer { println("second!"); }
	println("first!")
}
```

prints "first!" then "second!"

## Require
The `require` keyword is used to indicate a dependency from one file to another.  Contrived example:

```go
require "c.cpi"
```

makes sure that every module defined in the "c.cpi" file is available in whatever file 


## Modules
Modules are introduced with the `module` keyword, and are just a way of putting static things inside a namespace. They are not complicated and will not send you mixed signals. They can contain constants and variables. Contrived example:

```go
module accountant
{
  add_one := {x : i32} => x + constants.ONE;
  sub_one := {x : i32} => x - constants.ONE;

  module constants
  {
    ONE := 1;
    VAR :: i32;
  }
}
```

Every module defined in a file or `require`d from another file is available.  You can use anything from any module by fully qualifying its name:

```go
one := accountant.constants.ONE;
constants.VAR = 10;
```

Modules can be imported, in which case you do not have to fully qualify the names of things defined in that module:

```go
import accountant;
one := constants.ONE;
two := add_one(one);
```

or

```go
import accountant.constants;
one := ONE;
two := accountant.add_one(one);
```

## Attributes

Before any rvalue, module, or type, you can stick any number of attributes.  An attribute is a `@` sign followed by an rvalue.
This can come in very handy when giving the compiler hints about certain things (as we'll see in the next section), and when doing metaprogramming.
Contrived example:

```go
a := @3 @#{1, 2, 3} @"yay!" 17;
```

#### `|` and scope
Imagine you have a type: `type foo = i32`.
It would be nice if that type could have some functions which were just in scope automatically whenever you were doing stuff with something of that type.
This problem is normally solved using objects and vtables but that's been done a billion and twelve times so I wanted to try something different.
enter the `@scope(typename)` attribute.  If you stick this onto any module then whenever you use something of type `typename` as the lhs of a pipe,
everything in that module is imported into scope but only for the context of that pipe.  It might be easier to show a contrived example:

```go
type foo = {}; -- empty struct is kind of like a void type in C
@scope(foo) module foo_impl
{
	say_hi := {} => println("oh hai there!");
}
f : foo;
f | say_hi;
```

See how we piped `f` to `say_hi` even though `say_hi` was not in scope? That's because the module `foo_impl` *was* in scope and since it was attributed with 
`@scope(foo)` and `f` was a `foo`.  The neat thing about these is that they are scoped, so I can do stuff like this:

```go
type foo = {};

@scope(foo) module foo_impl {
	say_hi := {} => println("oh hai there!");
	say_bye := {} => println("oh bye there!");
}

f : foo;

do {
	@scope(foo) module foo_impl {
		say_hi := {} => println("whoa!!");
	}
	
	f | say_hi; -- prints "whoa!!"
	f | say_bye; -- prints "oh bye there!"
}

f | say_hi; -- prints "oh hai there!"
f | say_bye; -- prints "oh bye there!"
```

## Pointers, dereference and address-of
Pointers work the same way they do in C.  If you don't know what pointers are then you should Google them.  Or maybe ask Siri.  Here we declare a to be a pointer to an i32, initialize it to the address of an rvalue.  Notice you can take the address of any rvalue, unlike in C.
You can use `nil` to represent what is called NULL or nullptr in other languages.

```go
a : *i32 = &3;
b := *a;
```

When working with pointers to structures, you do not have to use '->' to access a field like you would in C or C++.  You can just use a '.' like normal.

```go
type vec2 = {i : i32, j : i32};
v2_1 := &vec2(5, 8);
v2_2 := &&&v2_1;
v2_1.i = 108;
v2_2.j = 17;
```

#### Recursive types
You can declare types which have references to themselves.  Obligatory linked-list example:

```go
type linked_list = {val : i32, next : *linked_list = nil}
l := linked_list(5);
l.next = linked_list(10);
l.next = linked_list(15);
assert(l.next.lvalue == 10);
```

## The `do` keyword:
Whenever a statement or rvalue is expected, you can always insert a `do` block instead.  The do block gets its own scope which means you can shadow variables, do `defer`, etc.
Contrived statement example (b == 3 at the end):

```go
a := 3;
do {
	a := 10;
}
b := a
```
If you wrap an identifier in parentheses between the `do` and the brackets, then the entire `do` block resolves to the value of that variable. Contrived rvalue example (b == 3):

```go
b := do(r) {
	get_3 := {} => 3;
	r := get_3();
}
```

## The `typeof` keyword:
gets the type of any declared variable. Contrived example:

```go
a := 4;
type b = typeof(a);
```

the type of `b` is `i32`.

#### `rettype`
you can use `rettype` to get the return type of a function.  Contrived example:

```go
a := {} => 15;
b : rettype(a); -- b is type i32
```

## If

Works about like you'd expect:

```go
i := 0;
if i > 3 {
	println("less than 3!");
} elif i > 2 {
	println("less than 2!");
} else {
	println("tiny number!");
}
```

## While

Works about like you'd expect:

```go
i := 60 
while i < 65 
{
	putchar(char(i));
	i += 1;
}
```

## Labels, Goto

That's right, I know they're considered harmful and all but screw that let's live on the edge.  Labels are created using `#` followed by an identifier, and gotos are a skinny arrow (`->`) followed by an identifier.  Contrived example:

```go
a := 3;
while true 
{
	a += 1;
	if a >= 10 
	{
		->break;
	}
}
#break println("done!");
```

## Cast 

Casts are used exclusively as bitcasts. Unlike in C where casting an int to a float actually does work to convert from one type to another, this is stricty a bitcast. Example:

```go
type foo = {i : i32, j : bool};
type bar = {i : i32, j : bool};

f := &foo(3, false);
b := cast(*bar) f;
```

## Compiler
This compiler is already very modular.  Once lexing has been done, we then do a minimal parse to get the ast, and perform several passes over the ast.
There is a 'require' pass (which resolves dependencies between files), a symbol resolution pass, a type checking/type propagation pass (which should be split in two at some point), and a code generation pass (creating an LLVM module).
At any point it should be trivial to stop and dump the whole ast.

The compiler has two modes.  The first mode is compiling to llvm ir. If you just do `cpi input.cpi`, then it will compile your cpi file and spit out a file called input.ll with 
llvm ir.  If you do `cpi input.cpi output.ll`, you can rename the ir file to output.ll. you can compile a .ll file with llc or clang.

The other mode is compiling to json.  See the next section, if you do `cpi input.cpi output.json` then it will dump the ast as json.

## EXPERIMENTAL: Json serialization of ast
One of the big goals of this language is to have a fully serializable ast.  This means being able to go both ways -- serialize and deserialize.  
Combined with attributes and a very flexible/modular compiler, this should enable lots of really cool features such as writing analyzers, debuggers, 
tools for IDEs like code completion, go to definition, find all usages, etc.

Right now most simple programs work well.  For example the following works on the portaudio example:

```
cpi sine.cpi sine.json; # spits out json just before running code generation, skips code generation.
cpi sine.json; # reads in the json as an ast, runs code generation, spits out .ll file
clang -o sine sine.ll -lportaudio;
./sine
```

I picked json because it's pretty much the standard these days for serialization -- it's supported by every language under the sun, and is human readable.
However, with the current scheme

## Questions that may or may not be frequently asked:

#### Q: Are you serious??
* A: No

#### Q: What's the point?
* A: I wanted to learn how compilers worked.  I seriously never thought I'd get this far, but hey now I have a little toy language.

---

# TODO

Holy shit there's lots of stuff to do.  This doesn't even begin to scratch the surface but here's some stuff that I can think of off the top of my head.

* literal conversion
  * `a : i64 = 3` for example

* Something around simd (either bind to c library or do some kind of wrapper for llvm's [vector type](http://llvm.org/docs/LangRef.html#vector-type))

* debugger ([godebug](https://github.com/mailgun/godebug) style)

* Tagged unions (algebraic data types, whatever).  Had this mostly working but decided that it might be fun to try and implement as a library, to see what it's like to work with that json stuff.  Should be able to implement with full debug support and everything if we go the library route.

* make the compiler more friendly toward people who want to use it as a library.  `#include <cpi.h>` and provide simple c interface for reading, lexing, parsing, codegen, etc.

* make the compiler more unix-like? Perhaps break each pass into its own standalone program so that they can be piped together via the command line.  If it turns out to be efficient enough, then that would be very powerful as people could basically write their own passes to add features, do analysis, enforce style rules, etc. and shell-script their way to glory.

* special operators
  * [`cmpxchg`](http://llvm.org/docs/LangRef.html#cmpxchg-instruction)


* std library

* bindings project similar to [deimos in D](https://github.com/D-Programming-Deimos) to bind with popular C libraries
