# Honey Badger
[![Build Status](https://travis-ci.org/jsnider3/HoneyBadger.svg?branch=master)](https://travis-ci.org/jsnider3/HoneyBadger)
[![License](https://img.shields.io/github/license/jsnider3/HoneyBadger.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

A programming language developed by me and implemented in OCaml.
The documentation is [here.](http://www.joshuasnider.com/HoneyBadger/)

Honey Badger is intended to be a dynamically-scope, dynamically-typed
language, with dictionaries and arrays as language builtins.

# Requirements
Honey Badger currently depends on ocaml, ocamldoc, opam,
JaneStreet's core library, and menhir. On an up-to-date Ubuntu
system you should be able to get the dependencies by running

```
sudo add-apt-repository -y ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y ocaml ocaml-doc opam  \
     ocaml-native-compilers menhir
opam init -n
eval `opam config env`
opam install core -y
```

# Usage
After cloning this repository, type `make` to create the program "HB".
To run a program file, type `./HB /path/to/program`.
tests/input/tictactoe.hnb is a good example of proper syntax.

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

