# Honey Badger
A programming language developed by me and implemented in OCaml.

Honey Badger is intended to be a dynamically-scope, dynamically-typed
language, with dictionaries and arrays as language builtins.

# Requirements
Honey Badger currently depends on ocaml, ocamldoc, opam,
JaneStreet's core library, and menhir. The commands listed under
install in the .travis.yml file can tell you what to do to get them
on Ubuntu.

# Usage
After cloning this repository, type make to create the program "HB".
To run a program file, type "HB /path/to/program".

# Distinguishing features of HB.

* Dynamic-scoping with lambdas: Unlike closures in other languages,
  where the value of the variables is as it was when the lambda was
  defined, Honey Badger looks up the values of variables at function
  call.

* Everything is an expression: As opposed to C, but similar to lisp
  everything in Honey Badger including sequences and if statements
  is an expression. print(if b then 1 else 0), is both valid code
  and good programming style.

* The program is a single expression: The entire program is composed
  of a single expression, usually taking the form of a sequence,
  which is evaluated and whose result is printed. Functions are also
  a single expression and "return" whatever they evaluate to.

* Semicolon usage: Semicolons are used to seperate expr's in sequences,
  not like in C where they end every line. C's {expr1; expr2; expr3;}
  is written as {expr1; expr2; expr3} in Honey Badger. This may be changed
  based on feedback.

