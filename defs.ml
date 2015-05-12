(**
  Provides types used throughout the interpreter.
*)

(**
  Types that Honey Badger values may belong to.
*)
type kind = TInt | TReal | TBool | TFunc 
  | TArr  | TRecord of (string * kind) list
  | TUnit | TTop | TBottom | TStr

(**
  exprs are used by the interpreter to represent the
  abstract syntax tree parsed from the source file.
*)
type expr = N of int | F of float| Add of (expr * expr)
  | Mul of (expr * expr) | Div of (expr * expr)
  | Sub of (expr * expr) | Less of (expr * expr) |And of (expr * expr) 
  | Or of (expr * expr) | Not of expr |If of (expr * expr * expr) 
  | Equal of (expr * expr) | B of bool |Lam of (string list * expr) 
  | App of (expr * expr list) | Arr of expr array | Len of expr | Unit
  | Get of (expr * expr) | Record of (string * expr) list
  | GetRec of (string * expr) | SetRec of string * string * expr
  | SetInd of (string * expr * expr) 
  | Cast of (expr * kind)
  | Str of string | Readline | Print of expr
  | Seq of expr list | Set of (string * expr) | Lookup of string 
  | While of (expr * expr) | Top | Bottom

(**
  Types of values that are possible in Honey Badger.
*)
type value = VB of bool | VArr of value array | VUnit | VN of int 
  | VF of float | VLam of (string list * expr) | VStr of string 
  | VRecord of (string * value) list ref | VTop | VBottom
