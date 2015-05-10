type kind = TInt | TReal | TBool | TFunc of (kind * kind) 
  | TList of kind | TRecord of (string * kind) list 
  | TUnit | TTop | TBottom | TStr

type expr = N of int | F of float| Add of (expr * expr) | Mul of (expr * expr) 
  | Sub of (expr * expr) | Less of (expr * expr) |And of (expr * expr) 
  | Or of (expr * expr) | Not of expr |If of (expr * expr * expr) 
  | Equal of (expr * expr) | B of bool |Lam of (kind * string * expr) 
  | App of (expr * expr) | List of expr list | Unit
  | Get of (expr * expr) | Record of (string * expr) list 
  | GetRec of (string * expr) | SetRec of string * string * expr
  | SetInd of (string * expr * expr) 
  | Cast of (expr * kind)
  | Str of string | Readline | Print of expr
  | Seq of expr list | Set of (string * expr) | Lookup of string 
  | While of (expr * expr) | Top | Bottom

type value = VB of bool | VList of value list | VUnit | VN of int 
  | VF of float | VLam of (kind * string * expr) | VStr of string 
  | VRecord of (string * value) list | VTop | VBottom

type env_type = (string, value) Hashtbl.t;;
type type_map = (string, kind) Hashtbl.t;;
