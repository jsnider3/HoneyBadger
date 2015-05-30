---
layout: default
permalink: /
comments: True
---

# 1 Introduction

This manual describes the programming language Honey Badger.
It is named Honey Badger as a reference to a
[youtube video](https://www.youtube.com/watch?v=4r7wHMg5Yjg) popular
on the internet in the early 2010's. It is a dynamically-scoped,
dynamically-scoped, language with a reference implementation provided
in OCaml.

Honey Badger is a small language that could be implemented in an interpreted
language quite straight-forwardly. It's features were chosen without regard
to practicality with the main goal of being different from other languages that
the author has implemented and providing an avenue to prove his knowledge of 
OCaml by using it for the reference implementation.

A Honey Badger program is a single expression. This expression can contain
loops, functions, branching, arrays, dictionaries, and is Turing-Complete.

The default behavior of a program is to evaluate it and if evaluation 
terminates print what it evaluates to.
This manual is divided into informal and formal components. For a short,
informal overview, the first part (through Section 9) suffices. The formal
description begins with Section 10.  

# 2 Getting Started

If you want to get started coding in Honey Badger you should download the
[reference implementation](https://github.com/jsnider3/HoneyBadger) which is
documented [here](/docs/) and at it's github page. After resolving dependencies
and making it you can run programs with `HB path/to/file.hnb`. Example programs
can be found in the "tests/input" directory, with tictactoe.hnb being
particularly informative.

# 3 Expressions

Expressions represent the entirety of Honey Badger syntax.

## 3.1 Constants

The simplest expressions are constants. Boolean constants are `true` and
`false`. Integer constants are made of the decimal digits without any other
char. String constants are characters enclosed in double
quotes, i.e. "Hello World!"

## 3.2 Identifiers

The names of variables are expressions. Resolution is done by checking the
current scope and if not present checking increasingly higher scopes.
Evaluating an identifiers which does not have a binding in any higher scope
will raise a `ValueException`.

## 3.3 Assignment

An assignment either has the form `id = expr`, `id[expr] = expr`, or
`id[String] = expr`. The first form assigns to regular variables, the second
to elements of an array, and the third to fields of a dictionary.

## 3.4 Method calls

Method calls take the form of `<expr>(<expr>,...,<expr>)`. Let's number these
e0(e1,...en) To evaluate this expression, we evaluate e0, throw an error if
it's not a method or if it's number of arguments does not equal n.
The args are evaluated left-to-right, from e1 to en. Finally, the values from
e1 to en are bound to the corresponding parameters names in a new scope and the
function itself is evaluated.

## 3.5 Conditionals

A conditional has the form
`if <expr> then <expr> (else <expr>)`

To evaluate conditionals, the predicate is evaluated first.
If the predicate is true, then the then branch is evaluated. If the predicate
is false, then the else branch is evaluated. The value of the conditional is
the value of the evaluated branch. If the else branch is not provided it is
defined as `else Unit`.

## 3.6 Loops

A loop has the form
`while <expr> <block>`

The guard is evaluated before each iteration of the loop.
If the guard is false, the loop terminates and returns Unit. 
If it's true, the body of the loop is evaluated and the process repeats.
If it's anything else, a `TypeException` is raised.

There is also a for loop syntax of the form
`for(e1; e2; e3) e4`

This is semantic sugar for {e1; while e2 {e4; e3}}.

## 3.7 Blocks

A block has the form
`{ <expr>; ...; <expr> }`

The expressions are evaluated from first to last. Empty blocks are prohibited.
Blocks evaluate to the last expression.
Semi-colons in Honey Badger are used as seperators in the list of expressions
not as terminators, unlike in C.

## 3.8 Input-Output

Honey Badger has the following built-in methods for performing simple input and
output operations:
print(x) : Unit
readline() : String
print casts its argument to a string, prints the result, and returns Unit. The
method readline, waits for the user to type in a string followed by '\n', and
then returns the string without the newline.

## 3.9 Methods

A method definition has the form
`fun <id, ... ,<id> : <expr>`.
There may be zero or more parameters. The identifiers used in the parameter
list must be distinct.

## 3.10 Casts

Casts have the form `Type(expr)`. Casts from floats to ints are done by truncation.
`true` cast to a number is equal to `1`, `false` cast to a number is equal to `0`.
Non-zero numbers cast to Bool are `true`, zero is false.  Casting an expr e to Str
produces a string that that would evaluate to e. Non-empty arrays and records
cast to Bool are true, empty ones are false. All other casts generate a `TypeException`.

## 3.11 Arithmetic and Comparison Operations

Honey Badger has eight binary operators: +, -, *, /, >=, >, <, <=. Used as
`expr op expr`
To evaluate such an expression first expr1 is evaluated and then expr2. The
result of the operation is the result of the expression.
The static types of the two sub-expressions must be Int. The static type of the
expression is Int. Cool has only integer division.
Cool has three comparison operations: <, <=, =. For < and <= the rules are
exactly the same as for the binary arithmetic operations, except that the result
is a Bool. The comparison is a special case. If either <expr1> or <expr2> has
static type Int, Bool, or String, then the other must have the same static type.
Any other types may be freely compared. On non-basic objects, equality simply
checks for pointer equality (i.e., whether the memory addresses of the objects
are the same). Equality is defined for void.
In principle, there is nothing wrong with permitting equality tests between,
for example, Bool and Int. However, such a test must always be false and almost
certainly indicates some sort of programming error. The Cool type checking rules
catch such errors at compile-time instead of waiting until runtime.
Finally, there is one arithmetic and one logical unary operator. The expression
~ <expr> is the integer complement of <expr>. The expression <expr> must have
static type Int and the entire expression has static type Int. The expression
not <expr> is the boolean complement of <expr>. The expression <expr> must have
static type Bool and the entire expression has static type Bool.

## 3.12 Array and Dictionary Literals

Array literals are in the form of `[e1,...en]`. Dictionary literals are in the
form of `{"f1" : e1, ... "fn" : en}` and prohibit duplicate fields. In both
cases, evaluation proceeds left-to-right.

## 3.13 Lookups

The notation
`expr '[' expr ']'` as in e[i]
  
whether on the left or right hand side on an assignment refers to the ith member of
the array e. The array is evaluated before the index. Array indexes start at 0. 
If an array index evaluates to a float it is cast to an integer by truncation, if it
evaluates to a non-number a `TypeException` is raised. Arrays may be of freely mixed
types. Out-of-bounds errors on arrays generate `OutOfBoundsException`.

The notation `expr '[' String ']'` as in dict["Hi"] refers to the field "Hi" within the
dictionary given by an expression. Evaluating a field not present in a dictionary will
raise a `KeyException`.

## 3.14 Throwing Exceptions

Exceptions can be thrown by language built-ins as described above or thrown
manually with the forms `Except(expr)` and `Except(string, expr)`. The first
type throws a `RuntimeException` with `expr` cast to a Str as its message. The
second form throws an Exception of type specified by the given string with its
message as the expr evaluated and cast to a Str.  

If an `Exception` is thrown without being caught, then its message shall be
print out by the interpreter.

## 3.15 Catching Exceptions

Exceptions can be caught by try-catch blocks. These have the form
`try block (catch name : e)+`. try's without catch blocks are forbidden.
try's where multiple catch blocks are provided for the same exception are also
forbidden.

# 4 Lexical Structure

The lexical units of Honey Badger are numbers, types, operators,
strings, keywords, the various curlies anc brackets, and white space.

## 4.1 Numbers, Identifiers, and Special Notation

Integers are non-empty strings of digits 0-9. Floats are non-empty strings of
digits 0-9 containing a single '.' character. Identifiers are strings (other
than keywords) starting with a lower case letter and consisting of letters, digits, and the underscore character.
All types are currently keywords, but they all start with upper case letters
for clarity.

## 4.2 Strings

Strings are enclosed in double quotes "..." and are all literal strings.

## 4.3 Comments

Honey Badger has one form of comment. Any string starting with "//" and ending
with either a newline or EOF is a comment and ignored.

## 4.4 Keywords

The keywords of Honey Badger are: true, false, if, then, else, for, try, catch,
fun, print, len, readline, Int, Bool, String, Float, Record, Top, Except, and
Arr. Keywords are case sensitive.

## 4.5 White Space

White space consists of any sequence of the characters: ' ', '\n', and '\t'.

# 5 Semi-formal Syntax

    program = expr
 
    expr = ID | ID "[" expr "]" | ID "[" string "]" | ID "=" expr |
           ID "[" expr "]" "=" expr | ID "[" string "]" "=" expr |
           expr bin_op expr | un_op expr | "fun" (ID,*) "=" expr |
           expr "(" (expr,*) ")" | "if" expr "then" expr "else" expr |
           "if" expr "then" expr | "while" expr block | "[" (expr,)* "]" |
           "{" (string ":" expr)* "}" | type "(" expr ")" | Except "(" expr ")" |
           Except "(" charseq "," expr ")" | int | float | bool | "(" expr ")" |
           "for" "(" expr ";" expr ";" expr ")" expr | "print" "(" expr ")" |
           "readline" "(" ")" | "try" block ("catch" charseq ":" expr)+
  
    block = "{" (expr ";")+ ")"

    bool = "true" | "false"
    int = ['0'-'9']+
    float = ['0'-'9']* '.' ['0'-'9']+
    string = '"' charseq '"'

    type = "Int" | "Bool" | "String" | "Float"
    ID = ['a'-'z']['a'-'z' '0'-'9' 'A'-'Z' '_']*
    charseq = [^'"' ' ' '\n' '\t']* 
  
    bin_op = "+"' | "-" | "/" | "*" | "<" | "<=" | ">" | ">="
    un_op = "~" | "not" | "-"
 
