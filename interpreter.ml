(** Reference implementation for the Honey Badger
  programming language. *)

open Core.Std
open Defs
open Printf

(**
  HBException represents errors encountered throwable and
  catchable by the user.
  The two arguments they take are the exception's name and
    message.
*)
exception HBException of string * string

let rec string_of_kind arg = match arg with
  TInt -> "Int"
  |TReal -> "Real"
  |TBool -> "Bool"
  |TStr -> "String"
  |TFunc -> "Func"
  |TArr -> "Arr "
  |TRecord a -> "Record"
  |TUnit -> "()"
  |TTop -> "T"
  |TExcept -> "Except"

(** Return the abstract syntax tree rooted at arg represented
  as a string. *)
let rec string_of_expr arg = match arg with
  N a -> "N " ^ string_of_int a
  |F f -> "F " ^ Float.to_string f
  |B b -> "B " ^ string_of_bool b
  |Str s -> "String " ^ s
  |Readline -> "readline()"
  |Len e -> "len(" ^ string_of_expr e ^ ")"
  |Print e -> "print(" ^ string_of_expr e ^ ")"
  |Add (a, b) -> "Add(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Mul (a, b) -> "Mul(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Div (a, b) -> "Div(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Sub (a, b) -> "Sub(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Less (a, b) -> "Less(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |And (a, b) -> "And(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Or (a, b) -> "Or(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Not a -> "Not(" ^ string_of_expr a ^ ")"
  |If (a, b, c)-> "(If " ^ string_of_expr a ^ " then " ^ string_of_expr b ^
                       " else " ^ string_of_expr c ^ ")"
  |Equal (a, b) -> "Equal(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Lam (b, c) -> "Lam(" ^ String.concat ~sep:", " b ^ ", " ^
                      string_of_expr c ^ ")"
  |App (a, b) -> "App(" ^ string_of_expr a ^ ", " ^ 
    String.concat ~sep:", " (List.map b string_of_expr) ^ ")"
  |Arr a -> "List[" ^ String.concat ~sep:", " (List.map (Array.to_list a) string_of_expr )
                ^ "]"
  |Unit -> "()"
  |Top -> "T"
  |Except (t, m) -> t  ^ ": " ^ string_of_expr m
  |Get (a, b) -> "Get(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |GetRec (a, b) -> "GetRec(" ^ a ^ "," ^ string_of_expr b ^ ")"
  |SetRec (a, b, c) -> a ^ "[" ^ b ^ "] <- " ^ string_of_expr c 
  |SetInd (a, b, c) -> a ^ "[" ^ string_of_expr b ^ "] <- " ^ string_of_expr c 
  |Cast (a, b) -> "Cast(" ^ string_of_expr a ^ string_of_kind b
  |Lookup a -> "Lookup " ^ a
  |While (a, b) -> "While(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Record fields -> "Record[" ^ String.concat ~sep:", " 
                      (List.map fields (fun field -> fst field ^ " = " ^
                        string_of_expr (snd field))) ^ "]"
  |Seq a -> "Sequence[" ^ String.concat ~sep:"; " (List.map a (string_of_expr)) 
              ^ "]"
  |Set (s, x) -> "Set (" ^ s ^ ", " ^ string_of_expr x ^ ")"
  |Try (s, x) -> "Try (" ^ string_of_expr s ^ ", [" ^ String.concat ~sep:", "
    (List.map x (fun (c, b) -> "catch " ^ c ^ ": " ^ string_of_expr b )) ^ "])"

(**
  Represent a value as a human-readable string.
*)
and string_of_val arg = match arg with
  VN a -> string_of_int a
  |VF f -> Float.to_string f
  |VB b -> string_of_bool b
  |VStr s -> s
  |VLam (b, c) -> "fun " ^ String.concat ~sep:", " b ^ ": " ^
                        string_of_expr c
  |VArr a -> "[" ^ String.concat ~sep:", " (List.map (Array.to_list a) string_of_val ) 
                       ^ "]"
  |VUnit -> "()"
  |VTop -> "T"
  |VRecord fields -> "{" ^ String.concat ~sep:", " 
                        (List.map !fields (fun field -> fst field ^ " : " ^ 
                          string_of_val (snd field))) ^ "}"

(**
  Throws an HB TypeException with the given message.
*)

let type_exn msg = raise (HBException("TypeException", msg))

(**
  Return a * b.
  Throws an exception in either a or b is a non-number.
*)
let mul a b = match (a, b) with
  (VN x, VN y) -> VN(x * y)
  |(VN x, VF y) -> VF (Float.of_int x *. y)
  |(VF x, VN y) -> VF(x *. Float.of_int y)
  |(VF x, VF y) -> VF(x *. y)
  |_ -> type_exn "Invalid args for multiplication."

(**
  Return a / b.
  Throws an exception if either a or b is a non-number.
*)
let div a b = match (a, b) with
  (VN x, VN y) -> VF(Float.of_int x /. Float.of_int y)
  |(VN x, VF y) -> VF (Float.of_int x /. y)
  |(VF x, VN y) -> VF(x /. Float.of_int y)
  |(VF x, VF y) -> VF(x /. y)
  |_ -> type_exn "Invalid args for division."

(**
  Return a + b.
  If a and b are numbers, performs addition.
  If a and b are strings, concatenates them.
  If a and b are lists, concatenates them.
  Throws an exception otherwise.
*)
let add a b = match (a, b) with
  (VN x, VN y) -> VN(x + y)
  |(VN x, VF y) -> VF (Float.of_int x +. y)
  |(VF x, VN y) -> VF(x +. Float.of_int y)
  |(VF x, VF y) -> VF(x +. y)
  |(VArr f, VArr s) -> VArr (Array.append f s)
  |(VUnit, VArr s) -> VArr s
  |(VArr f, VUnit) -> VArr f
  |(VStr f, VStr s) -> VStr (f ^ s)
  |_ -> type_exn "Invalid args for addition."

(**
  Return a - b.
  Throws an exception if either a or b is a non-number.
*)
let sub a b = match (a, b) with
  (VN x, VN y) -> VN(x - y)
  |(VN x, VF y) -> VF (Float.of_int x -. y)
  |(VF x, VN y) -> VF(x -. Float.of_int y)
  |(VF x, VF y) -> VF(x -. y)
  |_ -> type_exn "Invalid args for subtraction."

(**
  Return a < b.
  Throws an exception if either a or b is a non-number.
*)
let less a b = match (a, b) with
  (VN x, VN y) -> VB(x < y)
  |(VN x, VF y) -> VB (Float.of_int x < y)
  |(VF x, VN y) -> VB(x < Float.of_int y)
  |(VF x, VF y) -> VB(x < y)
  |_ -> type_exn "Invalid args for comparison."

(**
  casts v to an int.
  For ints, this returns v.
  For floats, this returns v rounded towards zero.
  For bools, true is 1 and false is 0.
  For strings, this tries to parse v as an int.
  Throws exceptions for other inputs or if v is a string that
    doesn't represent an int.
*)
let cast_int v = match v with
  VN num -> VN num
  |VF num -> VN (Float.to_int num)
  |VB b -> VN (if b then 1 else 0)
  |VStr s -> VN (Int.of_string s)
  |_ -> type_exn ("Can't cast " ^ string_of_val v ^ " to int.")

(**
  casts v to a float.
  For ints, this returns v.
  For floats, this returns v.
  For bools, true is 1.0 and false is 0.0.
  For strings, this tries to parse v as a float.
  Throws exceptions for other inputs or if v is a string that
    doesn't represent a float.
*)
let cast_real v = match v with
  VN num -> VF (Float.of_int num)
  |VF num -> VF num
  |VB b -> VF (if b then 1.0 else 0.0)
  |VStr s -> VF (Float.of_string s)
  |_ -> type_exn ("Can't cast " ^ string_of_val v ^ " to real.")

(**
  casts v to a bool.
  For numbers, 0 is false and all others are true.
  For strings, "true" is true and "false" is false.
  For arrays and maps, empty is false, otherwise true.
  Throws exceptions for other inputs or if v is a string that
    is not "true" or "false".
*)
let cast_bool v = match v with
  VB b -> VB b
  |VN num -> VB (num <> 0)
  |VF num -> VB (num <> 0.0)
  |VStr s -> VB (Bool.of_string s)
  |VArr a -> VB (Array.length a > 0)
  |VRecord r -> VB (List.length !r > 0)
  |_ -> type_exn ("Can't cast " ^ string_of_val v ^ " to bool.")

(**
  casts v to type t.
  For casting to int, see cast_int.
  For casting to float, see cast_float
  For casting to string, see string_of_val.
  Throws an exception for all others.
*)
let cast v t = match (t, v) with
  (TInt, _) -> cast_int v
  |(TReal, _) -> cast_real v
  |(TBool, _) -> cast_bool v
  |(TStr, _) -> VStr (string_of_val v)
  |(TFunc, VLam _) -> v
  |(TRecord _, VRecord _) -> v
  |(TUnit, VUnit) -> v
  |(TArr, VArr _) -> v
  |(TTop, _) -> v
  |_ -> type_exn ("Can't cast to " ^ string_of_kind t)

(**
  Evaluates expr with the given state and returns
    a value.
*)
let rec eval expr state = match expr with
  N a -> VN a
  |F a -> VF a
  |B b -> VB b
  |Str s -> VStr s
  |Lam a -> VLam a
  |Arr a -> VArr (Array.map a ~f:(fun e -> eval e state))
  |Unit -> VUnit
  |Equal (a, b) -> VB(eval a state = eval b state)
  |Record fields -> VRecord (ref (List.Assoc.map fields 
    (fun a -> eval a state)))

(* Numerical Functions  *)
  |Mul (a, b) -> mul (eval a state) (eval b state)
  |Div (a, b) -> div (eval a state) (eval b state)
  |Add (a, b) -> add (eval a state) (eval b state)
  |Sub (a, b) -> sub (eval a state) (eval b state)
  |Less (a, b) -> less (eval a state) (eval b state)
  |App (lam, vars) -> begin
    match eval lam state with
      VLam(strs, body) ->
        begin
        if (List.length strs = List.length vars) then
          let args = List.zip_exn strs
            (List.map vars (fun arg -> eval arg state)) in
          let newscope = Hashtbl.copy state in
            List.iter args (fun (s,v) -> Hashtbl.replace newscope s v);
            eval body newscope
        else
          type_exn ("Function call with wrong number of args.")
        end
      |_ -> type_exn "Can't apply on non-lambda."
    end

  (* Boolean Functions *)
  |If (condition, thenCase, elseCase) -> begin
    match eval condition state with
      VB true -> eval thenCase state
      |VB false -> eval elseCase state
      |_ -> type_exn "Invalid condition for if."
    end
  |And (a, b) -> begin
    match (eval a state, eval b state) with
      (VB x, VB y) -> VB(x && y)
      |(a, b) -> type_exn ("Invalid args for and " ^ string_of_val a ^ " "
        ^ string_of_val b ^ ".")
    end
  |Or(a, b) -> begin
    match (eval a state, eval b state) with
      (VB x, VB y) -> VB(x || y)
      |_ -> type_exn "Invalid args for or."
    end
  |Not a ->begin
    match eval a state with
      VB x -> VB(not x)
      |_ -> type_exn "Invalid arg for not."
    end

  (* Array functions. *)
  |Get (index, arr) -> begin
    let zero_index = 0 in
    match eval index state with (* Get the indexth member of arr. *)
      (VN num) -> if num < zero_index
        then raise ( HBException("OutOfBoundsException",
          "Negative index " ^ Int.to_string num ^ "."))
        else 
          begin
          match eval arr state with
            VArr ls -> if num < (Array.length ls)
              then ls.(num)
              else raise (HBException("OutOfBoundsException",
                "Out of bounds " ^ Int.to_string num ^ "."))
            |_ -> type_exn "Attempt to index into non-array"
          end
      |(VF num) -> eval (Get (N (Float.to_int num), arr)) state
      |a -> type_exn ("Not a number index " ^ string_of_val a ^ ".")
    end
  |GetRec (str, a) -> 
    begin
    let VRecord fields = eval a state in
    match List.Assoc.find !fields str with
      Some x -> x
      |None -> raise (HBException ("KeyException",
        "Dict does not contain a " ^ str))
    end
  |SetRec (var, field, expr) -> 
    begin
    match eval (Lookup var) state with
      VRecord fields -> 
        fields := List.Assoc.add !fields field (eval expr state);
        VRecord fields
      |v -> type_exn ("Can't set field in non-dict " ^ string_of_val v)
    end
  |SetInd (var, ind, expr) -> 
    begin
    match (eval (Lookup var) state, eval ind state) with
      (VArr ls, VN a) -> Array.set ls a (eval expr state); VArr ls
      |(VArr ls, VF a) -> Array.set ls (Float.to_int a) (eval expr state); VArr ls 
      |(VArr ls, k) -> 
        type_exn ("Invalid array index " ^ string_of_val k)
      |(k, v) -> type_exn ("Index assignment to non array " ^ string_of_val k)
    end
  |Cast (expr, t) -> cast (eval expr state) t
  |Seq a -> List.fold ~init:(VB true) ~f:(fun _ b -> eval b state) a
  |Set (name, a) -> let v = eval a state in
    Hashtbl.replace state name v; v
  |Lookup name -> 
    begin
    match Hashtbl.find state name with
      Some x -> x
      |None -> raise (HBException("ValueException", "Undefined var " ^ name))
    end
  |While (guard, body) -> 
    let rec eval_loop () =
      match eval guard state with
        VB true -> eval body state;
          eval_loop ()
        |VB false -> VUnit
        |v -> type_exn "Loop guard non bool"
    in
      eval_loop ()
  |Top -> VTop
  |Except (name, a) -> let msg = string_of_val (eval a state) in
    raise (HBException (name, msg))
  |Print e -> print_endline (string_of_val (eval e state)); VUnit
  |Readline -> VStr (input_line stdin)
  |Try(e, catches) ->
    begin
    try eval e state
    with
    | HBException (t, m) -> match List.Assoc.find catches t with
      Some x -> eval x state
      |None -> raise(HBException (t, m))
    end
  |Len e -> begin
    match eval e state with
      VArr l -> VN (Array.length l)
      |VStr s -> VN (String.length s)
      |a -> type_exn (string_of_val a ^ " doesn't have a length.")
    end

(**
  Convenience function to wrap eval.
*)
let exec a =
  eval a (Hashtbl.create ~hashable:String.hashable ())

(**
  Read source file from src, parse it as an expr,
    and print what it evaluates to.
*)
let main src =
  let inpt = open_in src in
  let linebuf = Lexing.from_channel inpt in
  try
    let ast = (Parser.main Lexer.token linebuf) in
      if false then
        printf "%s\n" (string_of_expr ast);
      printf "%s\n" (string_of_val (exec ast));
      In_channel.close inpt;
  with
  | HBException (tag, msg) -> print_endline (tag ^ ": " ^ msg)
  | Lexer.Error msg ->
	  fprintf stderr "%s%!" msg
  | Parser.Error -> let pos = Lexing.lexeme_start_p linebuf in
	  fprintf stderr "Syntax error line %d column %d.\n%!"
        pos.pos_lnum pos.pos_bol;;

main Sys.argv.(1)
