# Drizzle Specification

## Foreword

This is a work-in-progress specification for the Drizzle programming language.

The principles behind Drizzle are as follows:

- It should be possible to understand a project without resorting to an IDE;
  GNU Nano should be more than enough.
- Following from that, familiarity - users of common programming languages today
  should be able to read Drizzle code without needing to hunt through the spec
- Constraining that, we should not be afraid to break tradition; if the familiar
  is worse than the unfamiliar, prefer the unfamiliar.
- Multi-paradigm programming should be possible, with imperative and functional
  programming as the main focuses of Drizzle.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" are to be interpreted as
described in RFC 2119.

## Source Code Specification

### Encoding

The source code provided to a Drizzle compiler MUST be a sequence of UTF-8 codepoints.
A byte order mark (`U+FEFF`) MAY be ignored by the compiler.

### Characters

The following are categories or definitions of character(s) which will be
referenced later.

```ebnf
newline    = ? U+000A ? .
id_start   = ? $, _, or any Unicode symbol with the derived property `ID_Start` ? .
id_cont    = ? $, _, or any Unicode symbol with the derived property `ID_Continue` ? .
whitespace = ? Any character defined by Unicode to be whitespace. ? .
any_char   = ? Any Unicode character ? .
```

## Lexical elements

### Comments

Comments serve as documentation. There are three types:

1. Line comments, which start with the sequence `//` and continue until a `newline`,
2. Multi-line comments, which start with the sequence `/*` and end with `*/`,
3. and doc comments, which are parsed similarly to multi-line comments, but with
   `/**` and `**/`. These MAY be used to inform documentation generators.

Comments cannot start within string literals. Any content within a comment MUST
be ignored, save for doc comments, which MUST only inform generated documentation.

```ebnf
line_comment = "//" { any_char } newline .
block_comment = "/*" { any_char (block_comment) } "*/"
doc_comment = "/*" { any_char (block_comment) } "*/"
```

### Whitespace

Whitespace characters are any characters in the "Separator, Space [Zs]" Unicode
category. These are insignificant, but can separate tokens from one another.

### Identifiers

An identifier is used to assign names to variables, types, and other program
elements. An identifier is any `id_start` character followed by any number of
`id_cont` characters:

```ebnf
identifier = (id_start | "_" | "$") { id_cont } .
```

Identifiers are normalized using Normalization Form C as defined in
[Unicode Standard Index #15][ust15]. Two identifiers are equal if their NFC
forms are equal.

[ust15]: https://www.unicode.org/reports/tr15/tr15-53.html

#### Pre-defined identifiers

The following identifiers are available in all scopes:

##### Pre-defined Types

- `bool`
- `string`
- `number`
- `float`

##### Pre-defined Constants

- `true`
- `false`

### Keywords

The following keywords are reserved cannot be used as identifiers.

- `return`
- `if`, `else`
- `import`, `export`, `from`, `as`
- `for`, `in`, `match`
- `continue`, `break`
- `struct`, `type`
- `and`, `or`, `not`, `xor`, `shl`, `shr`

### Operators and punctuation

The following sequences of characters represent operators and punctuation.

```text
+   :  +=  ?   <<   (   .   '  =>
-   =  -=  !   >>   )   ,   "  ->
*   ^  *=  ==  <<=  [   ;   _
/   @  /=  >   >>=  ]       \
%      %=  <        {
           <=       }
           >=
```

### Generic parameters

Generic parameters are parameters that are generic over the type passed to them.
These are used both in function and type definitions.

```ebnf
generic_param = identifier ( "extends"  type) .

generic_parameter = "<" generic_param {","  generic_param} (",") ">" .

```

```drizzle
type Container<T> = {
  value: T
  transforms: LinkedList<(T) -> T>
}
const compute = <T>(container: Container<T>) => {
  let { value } = container
  for transform in transforms {
    value = transform(value)
  }
  return value
}

// for fun, an example of this: (also i'm pretty sure this is a monadic kind of thing)
const transforms = makeLinkedList<int>()
transforms.push((x) => x * 2)
transforms.push((x) => x + 1)
compute({ value: 3, transforms: transforms }) // (3 * 2) + 1 = 7
```

### Directives

Directives are clues given to the compiler.
They are a `@` character followed by the name of the directive.

```ebnf
directive = "@" identifier ( "(" /* TODO: directive args are just like functions lol */ ")" ) .
```

The currently recognized directives are:

1. `debug`, which will remove the function before compilation in release mode.

### Literals

```ebnf
literal = boolean_literal
        | integer_literal
        | string_literal
        | float_literal 
        | function_literal
        | struct_literal 
        | array_literal .
```

#### Boolean literals

A boolean literal represents a Boolean truth value.

```ebnf
boolean_literal = "true" | "false" .
```

#### Integer literals

An integer literal is a sequence of numbers representing an integer. The base
can be changed through prefixes:
`0b / 0B` for binary,
`0c / 0C` for octal, and
`0x / 0X` for hex.

Underscores and apostrophes can be used to separate digits for readability.

```ebnf

binary_digit  = "0" | "1" .
octal_digit   = "0" ... "7" .
decimal_digit = "0" ... "9" .
hex_digit     = "0" ... "9" | "A" ... "F" | "a" ... "f" .

binary_digits  = binary_digit  { [ "_" | "'" ] binary_digit } .
octal_digits   = octal_digit   { [ "_" | "'" ] octal_digit } .
decimal_digits = decimal_digit { [ "_" | "'" ] decimal_digit } .
hex_digits     = hex_digit     { [ "_" | "'" ] hex_digit } .

exponent_leader = "e" | "E" .
integer_exponent = exponent_leader [ "+" ] decimal_digits .

binary_literal  = ( "0b" | "0B" ) binary_digits .
octal_literal   = ( "0c" | "0C" ) octal_digits .
decimal_literal = decimal_digits [ integer_exponent ] .
hex_literal     = ( "0x" | "0X" ) hex_digits .

integer_literal = binary_literal | octal_literal | decimal_literal | hex_literal .
```

#### Float literals

A float literal represents a floating-point number. They resemble decimal integer
literals, though with the addition of a fractional part and negative exponents.

```ebnf
float_exponent = exponent_leader [ "+" | "-" ] decimal_digits .
float_literal = decimal_digits ["." decimal_digits ] [ float_exponent ] [ "f" | "F" ] .
```

Usually, if it is possible to parse a float literal as a decimal literal, it
should be parsed as a decimal literal. However, a trailing `f` or `F` forces the
literal to be read as a float literal.

#### String literals

A string literal has two forms: a _raw_ string literal, and an _interpreted_ string
literal.

An interpreted string literal is delimited by either `"` or `'`, and can include
escape sequences. These are backslashes followed by either a single character,
or a Unicode escape.

The list of accepted escape sequences, and their values:

```text
 Sequence  | Value* | Character represented
   \n      |   0A   | Newline
   \r      |   0D   | Carriage return
   \t      |   09   | Horizontal tab
   \\      |   5C   | Backslash
   \'      |   27   | Apostrophe
   \"      |   22   | Quotation mark
   \a      |   07   | Alert (beep, bell)
   \e      |   1B   | Escape character
  \xNN     |  (any) | Byte; NN is to be parsed as a hexadecimal byte.
\u{NNNNnn} |  (any) | Unicode escape. NNNNnn is intended as between 4 and 6 hex
           |        | characters, which is then parsed as a Unicode character.
   \C      |  (any) | Any character C after a backslash should otherwise be
           |        | interpreted as its literal value.

* In hex, in ASCII.
```

```ebnf
escape_char    = "n" | "r" | "t" | "a" | "e" | "\" | """ | "'".
unicode_escape = "\u" hex_digit hex_digit hex_digit hex_digit [hex_digit hex_digit] .
hex_escape     = "\x" hex_digit hex-digit .

escape = "\" (escape_char | unicode_escape | hex_escape | any_char) .

raw_string = "`" { any_char - "`"} "`" .

single_quote_string = "'" { any_char - "'" | escape } "'" .
double_quote_string = """ { any_char - """ | escape } """ .

string_literal = single_quote_string | double_quote_string | raw_string .
```

#### Rune literals

should we even do em.
i think its better to just like, have strings implicitly convert to runes
if theyre one character if needed; duck typing or smth

#### Function literal

TODO: is this the place im supposed to put this??

A function literal defines a function, or block of code that can be called later
and passed parameters.

```ebnf
fn_argument = identifier ( ":"  type) .
fn_arglist  = (generic_parameter ) "(" 
                [fn_argument  "," ]
                ("..." fn_argument )
              ")" .
fn_return_type = "->"  type  .
short_function_definition = fn_arglist 
                            (fn_return_type)
                            "=>"  expression .
long_function_definition = fn_arglist 
                           fn_return_type
                           "=>"  block .
function_literal = short_function_definition | long_function_definition .
```

There are two types of function literal: short and long. Short function literals
can only have one logical line, and implicitly return the expression that they
hold. They can optionally elide the function return type.

```drizzle
const timesTwo = (x: int) => x * 2
// equivalent to
const timesTwo = (x: int) -> int => x * 2
// equivalent to
const timesTwo = (x: int) -> int => {
  return x * 2
}
```

Parameters can also have the type elided in circumstances where they're being
passed to something where the type can be determined:

```drizzle
const forEach = <T>(arr: []T, callback: T) => {
  for val in arr {
    callback(val)
  }
}

forEach([1, 2, 3, 4], (x) => stdout.print(x))
```

#### Struct literal

Struct literals construct a struct.

```ebnf
struct_literal_field = identifier  ":"  expression .

struct_literal = "{" struct_literal_field {"," struct_literal_field} (",") "}" .

```

#### Array literal

Array literals construct an array.

```ebnf
array_literal = "[" { expression "," } "]" .
```

## Types

Each value and variable has a type associated. This type may be implicitly determined,
or it may be explicitly declared.

```ebnf
type = identifier   |
       bool_type    |
       number_type  |
       string_type  |
       array_type   |
       struct_type  |
       pointer_type |
       fn_type      .
```

### Boolean types

A boolean type represents Boolean truth values - that is, true or false. The
type for booleans is `bool`, and is represented by a boolean literal.

```ebnf
bool_type = "bool" .
```

### Numeric types

A numeric type represents integer or floating-point number values.

```ebnf
number_type = "number" | "float" .
```

#### Number compatability

A number literal may be widened to a type of the same signedness and representation
implicitly in a few cases. For example, in the following code block, `a` can
be implicitly widened into a number losslessly, and thus can be assigned to `b`.

```drizzle
let a: number = 200
let b: number = a
```

### String type

A string is a sequence of bytes. Strings are immutable; the contents cannot be
changed.
strings are utf8 lol

```ebnf
string_type = "str" .
```

### Array types

An array is a sequence of elements of a single type. The length of an array is
the number of elements, and can never be negative.

```ebnf
array_length_specifier = integer_literal | identifier .

array_type = "[" array_length_specifier "]"  type .
```

### Slice types

A dynamically-sized view into a contiguous sequence of a single type.

```ebnf
slice_type = "[]"  type .
```

### Pointer types

A pointer type denotes any pointer to a given type.

```ebnf
pointer_type = "^" type .
```

### Struct types

A struct is a composite data type that defines a list of variables together.

For example, a position could be stored as:

```drizzle
type Position = {
  x: float,
  y: float,
}
```

```ebnf
struct_type_field = identifier  ":"  type .

struct_type = "{" struct_type_field {","  struct_type_field} (",") "}" .
```

### Function types

Functions in Drizzle are first-class. In order to take a function as an argument
or return one, a function type can be provided. A function type

thjis isnt how you define a function this is for like defining the type of smth

```ebnf
fn_type = generic_parameter
          "("   
          type  
          { ","  type  }
          ")"   
          "->"  
          type .
```

```drizzle
type MyCallback = (string) -> void
const a = (name: string) => {
  stdout.println("aaa")
}
// type of a is compatible with MyCallback
```

## Properties of types and values

### Identity of types

Two types are identical, and thus compatible, if they are structurally equivalent.
Thus, the types `A`, `B`, and `C` here are identical:

```drizzle
type asdf = str

type A = [3]str
type B = [3]str
type C = [3]asdf
```

And, similarly, this applies to structs as well. In this example, `A` and `B` are
identical:

```drizzle
type asdf = {
  n: number
}

type A = {
  foo: asdf,
  m: float
}
type B = {
  foo: {
    n: number
  },
  m: float
}
```

### Assigning

A value `foo` can be assigned to a variable of type `T` if one of the following applies:

1. The type of the value `foo` is identical to that of `T`.
1. The type of the value `foo` is a superset of `T`.

These assignments are valid:

```drizzle
// rule 1
const eight: int = 8
const str: string = "hello world!"

// rule 2
type A = {
  foo: int,
  bar: int
}

type B = {
  foo: int,
  bar: int,
  baz: int
}

let a = A {
  foo: 2,
  bar: 3
}

a = B {
  foo: 3,
  bar: 4,
  baz: 5
}
```

## Statements

```ebnf
stmt = expression ";"

statement = type_declaration [ ";" ]
          | var_declaration [ ";" ]
          | assignment [ ";" ]
          | stmt
          | expression
          | "continue" [ ";" ]
          | "break" [ expression ] [ ";" ]
          | "return" [ expression ] [ ";" ] .

top_level_statement = statement
                    | export_declaration
                    | import_declaration .
```

### Declarations

```ebnf
export_declaration = "export" ( type_declaration | var_declaration )
```

Declarations can also be made public within a file, to allow importing them into
another file. This is done with the `export` keyword as the type of declaration.
Exports must be statically determinable.

#### Type declarations

Type declarations associate a type with an identifier.

```ebnf
type_declaration = "type"
                   identifier
                   (generic_parameter)       
                   "="                
                   type .
```

Example:

```drizzle
type Position = {
  x: float,
  y: float,
}
```

#### Variable declarations

Variable declarations associate a value with an identifier.

```ebnf
var_declaration_kind = "let" | "const" .

destructuring_field = identifier ( "as"  identifier) .

destructuring = "{" 
                destructuring_field { "," destructuring_field }
                [","] "}" .

var_declaration_lhs_simple = identifier  ( ":"  type) .

var_declaration_lhs = var_declaration_lhs_simple | destructuring .

var_declaration = var_declaration_kind 
                  var_declaration_lhs  
                  "=" value .
```

The given value must be assignable to the given type. If the type is elided, it
will be inferred from the value.

```drizzle
const a = "aaa"
// is equivalent to
const a: string = "aaa"
```

There are two types of assignment: `let` and `const`. `let` declarations can
be reassigned later, but `const` declarations cannot. For example:

```drizzle
let foo = "aaa"

foo = "bbb" // This is a valid reassignment!

const bar = "aaa"

bar = "aaa" // This will not compile, because `bar` is declared as `const`.
```

Destructuring assignments require a struct type in the value. The selected values
will be assigned from the struct to the variables. For example, the following
are equivalent:

```drizzle
const a = {
  b: 1,
  c: 2,
  d: 3,
}

// Destructuring assignment
const { b, c } = a

// b = 1, c = 2

// Without destructuring assignment
const b = a.b
const c = a.c

// b = 1, c = 2
```

#### Import declarations

Import declarations make identifiers defined in another file available in the
current file.

```ebnf
import_lhs = destructuring | identifier .

import_declaration = "import"  import_lhs 
                     "from"  string_literal .
```

```drizzle
import everything from 'foo' // Imports all values from package 'foo' under the
                             // identifier `everything`
import { a, b } from 'bar'   // Imports the exported members `a` and `b` from
                             // the package 'bar'
import { c as d } from 'baz' // Imports the exported member `c` from the package
                             // 'baz', but binds the value to the identifier `d`
                             // in this file

everything.a // The exported member `a` from package 'foo'
```

Imports can either be:

- from a package, in which case the string is "bare" (e.g. `'std'`)
- a relative import, in which case the string begins with a "." (e.g. `'./foo'`)
- a root-relative import, in which case the string begins with "@/" (e.g. `'@/utils'`)

Package imports import values from either the file specified as `main` in the
manifest for the package being imported from, or from the file `main.rn` in
the same folder as the manifest.

#### Re-export declarations

Re-export declarations can re-export values from other files and make them
directly available in the file the export declaration is within.

```ebnf
re_export_lhs = import_lhs | "*"
re_export_declaration = "export"  re_export_lhs 
                     "from"  string_literal .
```

TODO describe formally how re-exports work

```drizzle
// bar.rn
export const a = 2

// foo.rn
export aaa from './bar'

// baz.rn
import foo from './foo'
foo.aaa.a // = 2

// qux.rn
export * from './bar'

// fifth.rn
import { a } from './qux'
a // = 2
```

## Expressions

```ebnf
operand = literal | "("  expression  ")" .

member = terminal [ "." identifier ] .

index = member [ "[" expression "]"] .

call = index [ "(" { expression "," } ")" ] .

unary = ( "not" | "!" | "~" | "-" ) call .

factor = unary { ( "/" | "%" | "*" ) unary } .

term = factor { ( "-" | "+" ) factor } .

comparison = term { ( ">" | ">=" | "<" | "<=" ) term } .

equality = comparison { ( "!=" | "==" ) comparison } .

definitely_if_expr = "if" expression block
                     { "else" (definitely_if_expr | block)} .

if_expr = definitely_if_expr | equality .

while_expr = ("while" expression block) | if_expr .

expression = while_expr .
```

### Terminals

```ebnf

block = "{" { statement } [ expression ] "}" .

terminal = block
         | identifier
         | literal
         | "(" expression ")" .
```

The above grammar is ambiguous. Final expression statements
in a block should become the final expression of the block.

### Operands

## Specification of compilation

### Compilation of translation units

Separate files in drizzle are separate translation units, and may be compiled
separately. If the contents of a file has not changed, it may not be re-compiled
if already compiled.

If a file is referenced multiple times, it may not be re-compiled for each reference.

In the case of circular imports, TODO WHAT DO WE DO IDK LOL

## Non-normative / unsorted content

object and class method access are done by `.`, pipe operator is `:`; pipe
operator passes the previous value as the first argument by default, but can
be changed with `_` as the "topic variable"
