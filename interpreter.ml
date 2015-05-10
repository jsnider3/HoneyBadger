
open Core.Std
open Defs
open Printf

let rec string_of_kind arg = match arg with
  TInt -> "Int"
  |TReal -> "Real"
  |TBool -> "Bool"
  |TStr -> "String"
  |TFunc (a, b) -> "Fun " ^ string_of_kind a ^ " -> " ^ string_of_kind b
  |TList a -> "List " ^ string_of_kind a
  |TRecord a -> "Record"
  |TUnit -> "()"
  |TTop -> "T"
  |TBottom -> "Bottom"

let rec string_of_expr arg = match arg with
  N a -> "N " ^ string_of_int a
  |F f -> "F " ^ Float.to_string f
  |B b -> "B " ^ string_of_bool b
  |Str s -> "String " ^ s
  |Readline -> "readline()"
  |Print e -> "print(" ^ string_of_expr e ^ ")"
  |Add (a, b) -> "Add(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Mul (a, b) -> "Mul(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Sub (a, b) -> "Sub(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Less (a, b) -> "Less(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |And (a, b) -> "And(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Or (a, b) -> "Or(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Not a -> "Not(" ^ string_of_expr a ^ ")"
  |If (a, b, c)-> "(If " ^ string_of_expr a ^ " then " ^ string_of_expr b ^
                       " else " ^ string_of_expr c ^ ")"
  |Equal (a, b) -> "Equal(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Lam (a, b, c) -> "Lam(" ^ string_of_kind a ^ ", " ^ b ^ ", " ^
                      string_of_expr c ^ ")"
  |App (a, b) -> "App(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |List a -> "List[" ^ String.concat ~sep:", " (List.map a string_of_expr )
                ^ "]"
  |Unit -> "()"
  |Top -> "T"
  |Bottom -> "Bottom"
  |Get (a, b) -> "Get(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |GetRec (a, b) -> "GetRec(" ^ a ^ "," ^ string_of_expr b ^ ")"
  |SetRec (a, b, c) -> a ^ "[" ^ b ^ "] <- " ^ string_of_expr c 
  |SetInd (a, b, c) -> a ^ "[" ^ string_of_expr b ^ "] <- " ^ string_of_expr c 
  |Cast (a, b) -> "Cast(" ^ string_of_expr a ^ ", kind)"
  |Lookup a -> "Lookup " ^ a
  |While (a, b) -> "While(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Record fields -> "Record[" ^ String.concat ~sep:", " 
                      (List.map fields (fun field -> fst field ^ " = " ^
                        string_of_expr (snd field))) ^ "]"
  |Seq a -> "Sequence[" ^ String.concat ~sep:"; " (List.map a (string_of_expr)) 
              ^ "]"
  |Set (s, x) -> "Set (" ^ s ^ ", " ^ string_of_expr x ^ ")"

and string_of_val arg = match arg with
  VN a -> string_of_int a
  |VF f -> Float.to_string f
  |VB b -> string_of_bool b
  |VStr s -> s
  |VLam (a, b, c) -> "VLam(" ^ string_of_kind a ^ ", " ^ b ^ ", " ^
                        string_of_expr c ^ ")"
  |VList a -> "[" ^ String.concat ~sep:", " (List.map a string_of_val ) 
                       ^ "]"
  |VUnit -> "()"
  |VTop -> "T"
  |VBottom -> "VBottom"
  |VRecord fields -> "{" ^ String.concat ~sep:", " 
                        (List.map fields (fun field -> fst field ^ " = " ^ 
                          string_of_val (snd field))) ^ "}"

(*
  mul ::value -> value -> value
       a -> b -> a * b
*)
let mul a b = match (a, b) with
  (VN x, VN y) -> VN(x * y)
  |(VN x, VF y) -> VF (Float.of_int x *. y)
  |(VF x, VN y) -> VF(x *. Float.of_int y)
  |(VF x, VF y) -> VF(x *. y)
  |_ -> invalid_arg "Invalid args for multiplication."

(*
  add ::value -> value -> value
       a -> b -> a + b
*)
let add a b = match (a, b) with
  (VN x, VN y) -> VN(x + y)
  |(VN x, VF y) -> VF (Float.of_int x +. y)
  |(VF x, VN y) -> VF(x +. Float.of_int y)
  |(VF x, VF y) -> VF(x +. y)
  |(VList f, VList s) -> VList(f @ s)
  |(VUnit, VList s) -> VList s
  |(VList f, VUnit) -> VList f
  |(VStr f, VStr s) -> VStr (f ^ s)
  |_ -> invalid_arg "Invalid args for addition."

(*
  sub ::value -> value -> value
       a -> b -> a - b
*)
let sub a b = match (a, b) with
  (VN x, VN y) -> VN(x - y)
  |(VN x, VF y) -> VF (Float.of_int x -. y)
  |(VF x, VN y) -> VF(x -. Float.of_int y)
  |(VF x, VF y) -> VF(x -. y)
  |_ -> invalid_arg "Invalid args for subtraction."

(*
  less ::value -> value -> value
       a -> b -> a < b
*)
let less a b = match (a, b) with
  (VN x, VN y) -> VB(x < y)
  |(VN x, VF y) -> VB (Float.of_int x < y)
  |(VF x, VN y) -> VB(x < Float.of_int y)
  |(VF x, VF y) -> VB(x < y)
  |_ -> invalid_arg "Invalid args for comparison."

(*
  cast_int ::value -> value
*)
let cast_int v = match v with
  VN num -> VN num
  |VF num -> VN (Float.to_int num)
  |VStr s -> VN (Int.of_string s)
  |_ -> invalid_arg ("Can't cast " ^ string_of_val v ^ " to int.")

let cast_real v = match v with
  VN num -> VF (Float.of_int num)
  |VF num -> VF num
  |VStr s -> VF (Float.of_string s)
  |_ -> invalid_arg ("Can't cast " ^ string_of_val v ^ " to real.")

let cast_string v = match v with
  VN num -> VStr (Int.to_string num)
  |VB num -> VStr (Bool.to_string num)
  |VF num -> VStr (Float.to_string num)
  |VStr s -> VStr s
  |_ -> invalid_arg ("Can't cast " ^ string_of_val v ^ " to string.")

(*
  cast ::value -> type -> value
*)
let cast v t = match t with
  TInt -> cast_int v
  |TReal -> cast_real v
  |TStr -> cast_string v
  |_ -> raise (Failure ("Can't cast to " ^ string_of_kind t))

(*
  eval ::expr -> env_type -> value
       input -> current_state -> result
*)
let rec eval expr state = match expr with
  N a -> VN a
  |F a -> VF a
  |B b -> VB b
  |Str s -> VStr s
  |Lam a -> VLam a
  |List a -> VList (List.map a (fun e -> eval e state))
  |Unit -> VUnit
  |Equal (a, b) -> VB(eval a state = eval b state)
  |Record fields -> VRecord (List.Assoc.map fields (fun a -> eval a state))

(* Numerical Functions  *)
  |Mul (a, b) -> mul (eval a state) (eval b state)
  |Add (a, b) -> add (eval a state) (eval b state)
  |Sub (a, b) -> sub (eval a state) (eval b state)
  |Less (a, b) -> less (eval a state) (eval b state)
  |App (lam, var) -> begin
    match eval lam state with
      VLam(t, str, body) ->
        begin
        match Hashtbl.find state str with
          Some x -> Hashtbl.replace state str (eval var state);
                    let v = eval body state in
                      Hashtbl.replace state str x;
                      v
          |None -> Hashtbl.replace state str (eval var state);
                   let v = eval body state in
                     Hashtbl.remove state str;
                     v
        end
      |_ -> invalid_arg "Can't apply on non-lambda."
    end

  (* Boolean Functions *)
  |If (condition, thenCase, elseCase) -> begin
    match eval condition state with
      VB true -> eval thenCase state
      |VB false -> eval elseCase state
      |_ -> invalid_arg "Invalid condition for if."
    end
  |And (a, b) -> begin
    match (eval a state, eval b state) with
      (VB x, VB y) -> VB(x && y)
      |something -> invalid_arg "Invalid args for and."
    end
  |Or(a, b) -> begin
    match (eval a state, eval b state) with
      (VB x, VB y) -> VB(x || y)
      |_ -> invalid_arg "Invalid args for or."
    end
  |Not a ->begin
    match eval a state with
      VB x -> VB(not x)
      |_ -> invalid_arg "Invalid arg for not."
    end

  (* List functions. *)
  |Get (index, mylist) -> begin
    let zero_index = 0 in
    match eval index state with (* Get the indexth member of list. *)
      (VN num) -> if num < zero_index
        then invalid_arg "Index out of bounds."
        else 
          begin
          match eval mylist state with
            VList ls -> List.nth_exn ls num
            |_ -> raise (Failure "Impossible")
          end
      |(VF num) -> eval (Get (N (Float.to_int num), mylist)) state
      |_ -> invalid_arg "Not a number index"
    end
  |GetRec (str, a) -> 
    begin
    let VRecord fields = eval a state in
    match List.Assoc.find fields str with
      Some x -> x
      |None -> raise (Failure "Non-existent field")
    end
  |SetRec (var, field, expr) -> 
    begin
    match eval (Lookup var) state with
      VRecord fields -> 
        let v = VRecord (List.Assoc.add fields field (eval expr state)) in
          Hashtbl.replace state var v; v
      |_ -> invalid_arg "Impossible"
    end
  |SetInd (var, ind, expr) -> 
    begin
    match (eval (Lookup var) state, eval ind state) with
      (VList ls, VN a) -> 
        let res = List.take ls a @ [eval expr state] @ List.drop ls (a + 1) in
          Hashtbl.replace state var (VList res); VList res
      |(VList ls, VF a) -> 
        let res = List.take ls (Float.to_int a) @ [eval expr state] 
            @ List.drop ls ((Float.to_int a) + 1) in
          Hashtbl.replace state var (VList res); VList res
      |k -> invalid_arg "Invalid list index." 
    end
  |Cast (expr, t) -> cast (eval expr state) t
  |Seq a -> List.fold ~init:(VB true) ~f:(fun _ b -> eval b state) a
  |Set (name, a) -> let v = eval a state in
    Hashtbl.replace state name v; v
  |Lookup name -> 
    begin
    match Hashtbl.find state name with
      Some x -> x
      |None -> raise (Failure ("Undefined var " ^ name))
    end
  |While (guard, body) -> 
    let rec eval_loop () =
      match eval guard state with
        VB true -> eval body state;
          eval_loop ()
        |VB false -> VUnit
        |_-> invalid_arg "Loop guard non-bool." 
    in
      eval_loop ()
  |Top -> VTop
  |Bottom -> invalid_arg "Attempt to eval Bottom"
  |Print e -> print_endline (string_of_val (eval e state)); VUnit
  |Readline -> VStr (input_line stdin)

(*
  exec :: expr         -> Val
      thingToCheck -> What it returns
  Make sure the expr is typesafe and evaluate it if it is.
*)
let exec a =
  eval a (Hashtbl.create ~hashable:String.hashable ())

(*
  Main
  Read from input until eof, parse that as an expr,
  and print what it evaluates to.
*)
let main src =
  let inpt = open_in src in
  let linebuf = Lexing.from_channel inpt in
  try
    let ast = (Parser.main Lexer.token linebuf) in
      printf "%s\n" (string_of_expr ast);
      printf "%s\n" (string_of_val (exec ast));
      In_channel.close inpt;
  with
  | Lexer.Error msg ->
	  fprintf stderr "%s%!" msg
  | Parser.Error ->
	  fprintf stderr "Syntax error at offset %d.\n%!"
        (Lexing.lexeme_start linebuf);;

main Sys.argv.(1)
