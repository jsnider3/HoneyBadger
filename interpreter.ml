
open Core.Std
open Defs
open Printf

let rec string_of_kind arg = match arg with
  TInt -> "Int"
  |TReal -> "Real"
  |TBool -> "Bool"
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
  |Concat (a, b) -> "Concat(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |Get (a, b) -> "Get(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  |GetRec (a, b) -> "GetRec(" ^ a ^ "," ^ string_of_expr b ^ ")"
  |SetRec (a, b, c) -> a ^ "[" ^ b ^ "] <- " ^ string_of_expr c 
  |SetInd (a, b, c) -> a ^ "[" ^ string_of_expr b ^ "] <- " ^ string_of_expr c 
  |As (a, b) -> "As(" ^ string_of_expr a ^ ", kind)"
  |Lookup a -> "Lookup " ^ a
  |While (a, b) -> "While(" ^ string_of_expr a ^ "," ^ string_of_expr b ^ ")"
  |Record fields -> "Record[" ^ String.concat ~sep:", " 
                      (List.map fields (fun field -> fst field ^ " = " ^
                        string_of_expr (snd field))) ^ "]"
  |Seq a -> "Sequence[" ^ String.concat ~sep:"; " (List.map a (string_of_expr)) 
              ^ "]"
  |Set (s, x) -> "Set (" ^ s ^ ", " ^ string_of_expr x ^ ")"

let rec string_of_val arg = match arg with
  VN a -> "VN " ^ string_of_int a
  |VF f -> "VF " ^ Float.to_string f
  |VB b -> "VB " ^ string_of_bool b
  |VLam (a, b, c) -> "VLam(" ^ string_of_kind a ^ ", " ^ b ^ ", " ^
                        string_of_expr c ^ ")"
  |VList a -> "VList[" ^ String.concat ~sep:", " (List.map a string_of_val ) 
                       ^ "]"
  |VUnit -> "()"
  |VTop -> "T"
  |VBottom -> "VBottom"
  |VRecord fields -> "VRecord[" ^ String.concat ~sep:", " 
                        (List.map fields (fun field -> fst field ^ " = " ^ 
                          string_of_val (snd field))) ^ "]"

let poly_set = Set.of_list ~comparator:Comparator.Poly.comparator

(*
  sub_type ::kind -> kind -> bool
*)
let rec subtype t1 t2 = match  (t1, t2) with
  (TInt, TReal) ->true
  |(TBottom, _) -> true
  |(_, TTop) -> true
  |((TRecord rec1), (TRecord rec2)) -> 
    Set.length(Set.diff (poly_set rec2) (poly_set rec1)) = 0
  |(TFunc(a, b), TFunc(c, d)) -> subtype c a && subtype b d
  |(a, b) -> a = b;;

(*
  common_type ::kind -> kind -> kind
*)
let common_type t1 t2 = match (t1, t2) with
  (TRecord rec1, TRecord rec2) -> 
    TRecord(Set.to_list(Set.union (poly_set rec2) (poly_set rec1)))
  |(a,b) -> if subtype a b then b else if subtype b a then a else TTop

(*
  typecheck ::expr -> type_map -> kind
        suspicious expression -> lookup table -> type it returns
  Makes sure an expression uses types correctly and either returns
  a value of a single type or returns an error.
*)
let rec typecheck expr env = match expr with
  |N _ -> TInt
  |F _ -> TReal
  |B _ -> TBool
  |Unit -> TUnit
  |Top -> TTop
  |Bottom -> TBottom
  |Equal (_, _) -> TBool
  |Add (a, b) -> typecheck (Sub (a, b)) env
  |Mul (a, b) -> typecheck (Sub (a, b)) env
  |Sub (a, b) -> 
    if subtype(typecheck a env) TReal && subtype(typecheck b env) TReal
      then common_type (typecheck a env)(typecheck b env)
      else raise (Failure "Can't do arithmetic on non-numbers.")
  |Less (a, b) -> 
    if subtype(typecheck a env) TReal && subtype(typecheck b env) TReal 
      then TBool
      else raise (Failure "Can't compare non-numbers.")
  |Or  (a,b) -> typecheck (And (a, b)) env
  |And (a, b) -> 
    if subtype(typecheck a env) TBool && subtype(typecheck b env) TBool
      then common_type (typecheck a env)(typecheck b env)
      else raise (Failure "Can't do bool ops on non-bools.")
  |If (a, b, c) -> if subtype(typecheck a env) TBool
    then common_type (typecheck b (Hashtbl.copy env))
                     (typecheck c (Hashtbl.copy env))
    else raise (Failure ("If guard " ^ string_of_expr a ^ " is non-bool."))
  |Not a -> if subtype(typecheck a env) TBool 
    then TBool 
    else raise (Failure "Not of non-bool")
  |Concat (a, b) ->  begin
    match (typecheck a env, typecheck b env ) with
      (TList type1, TList type2) -> TList (common_type type1 type2)
      |(TList ty, TUnit) -> TList ty
      |(TUnit, TList ty) -> TList ty
      |_ -> raise (Failure "Can't concat non-lists.")
    end
  |List [] -> TList TBottom
  |List (head::rest) -> begin
    match typecheck head env with
      |goodType -> match typecheck (List rest) env with
        |TList type2 -> TList (common_type goodType type2)
        |_ -> raise (Failure "This should be impossible")
    end
  |Get (ind, mylist) -> begin
    match(typecheck ind env, typecheck mylist env ) with
      (TInt, TList type1) -> type1
      |(TInt, TUnit) -> TBottom
      |_ -> raise (Failure "Invalid args to [].")
    end
  |Seq a -> List.fold ~init:(TBottom) ~f:(fun _ b -> typecheck b env) a
  |App (lam, var) -> begin
    match typecheck lam env  with
      TFunc(from1, to1) -> if subtype(typecheck var env) from1
        then to1
        else raise (Failure "Input to a lambda is of inappropriate type.")
      |TBottom -> TBottom (* Recursive call before we're named. *)
      |_ -> raise (Failure 
              ("Application done to non-lambda " ^ string_of_expr lam ^ "."))
    end
  |Lam (t, s, b) -> 
    Hashtbl.replace env s t;
    TFunc(t, typecheck b env)
  |As (expr, ty) -> typecheck_as (expr, ty) env
  |While (guard, body) -> if typecheck guard env = TBool
    then (typecheck body env; TUnit)
    else raise (Failure "Guard must be boolean")
  |Lookup name -> 
    begin
    match Hashtbl.find env name with
      Some x -> x
      |_ -> TBottom
    end
  |Set (name, expr) ->
    (* Recursive functions *)
    let init = Hashtbl.find env name in
    let ty = typecheck expr env in
    begin
    match init with
      Some ty1 -> if ty1 = ty
        then ty
        else raise (Failure "Attempt to change type of variable.")
      |_ -> Hashtbl.add env name ty; ty
    end
  |Record fields ->
    TRecord (List.Assoc.map fields (fun a -> typecheck a env))
  |GetRec (str, a) -> 
    begin
    let TRecord fields = typecheck a env in
    match List.Assoc.find fields str with
      Some x -> x
      |None -> TBottom
    end
  |SetRec (var, field, expr) -> 
    begin
    match typecheck (Lookup var) env with
      (* Fancy fucking *)
      TRecord fields -> 
        let ty = TRecord (List.Assoc.add fields field (typecheck expr env)) in
          Hashtbl.add env var ty; ty
      |k -> invalid_arg ("Can't do field assignment to " ^ string_of_kind k) 
    end
  |SetInd (var, ind, expr) -> 
    begin
    match (typecheck (Lookup var) env, typecheck ind env) with
      (* Fancy fucking *)
      (TList ty, TInt) -> typecheck expr env; TList ty
      |(TList ty, TReal) -> typecheck expr env; TList ty
      |k -> invalid_arg "Invalid list index." 
    end
  |a -> invalid_arg ("Not a valid expression " ^ string_of_expr a ^ ".")

and typecheck_as (expr, ty) env = match (expr,ty) with
  (*(TL a, TSum (left, right)) -> if subtype(typecheck a env)left
    then if left = right
      then raise (Failure "Sums must be two different types. TL")
      else TSum (left, right)
    else raise (Failure "Typecheck of as failed. TL")
  |(TR b, TSum (left, right)) -> if subtype(typecheck b env)right
    then if left = right
      then raise (Failure "Sums must be two different types. TR")
      else TSum (left, right)
    else raise (Failure "Typecheck of as failed. TR")
  |*)_ -> invalid_arg "TODO typecheck_as"
;;

(*
  make_expr :: Val   ->expr
        result->input
  Inverts eval.
*)
let rec make_expr v = match v with
  VB a -> B a
  |VN a -> N a
  |VLam lambda -> Lam lambda
  |VList a -> List (List.map a make_expr)
  |VRecord fields -> Record (List.Assoc.map fields make_expr)
  |VUnit -> Unit
  |VF f -> F f
  |VTop -> Top
  |VBottom -> Bottom

(*
  eval ::expr -> env_type -> value
       input -> current_state -> result
*)
let rec eval expr state = match expr with
  N a -> VN a
  |F a -> VF a
  |B b -> VB b
  |Lam a -> VLam a
  |List a -> VList (List.map a (fun e -> eval e state))
  |Unit -> VUnit
  |Equal (a, b) -> VB(eval a state = eval b state)
  |Record fields -> VRecord (List.Assoc.map fields (fun a -> eval a state))

(* Numerical Functions  *)
  |Mul (a, b) -> begin
    match (eval a state, eval b state) with
      (VN x, VN y) -> VN(x * y)
      |(VN x, VF y) -> VF (Float.of_int x *. y)
      |(VF x, VN y) -> VF(x *. Float.of_int y)
      |(VF x, VF y) -> VF(x *. y)
      |_ -> invalid_arg "Invalid args for multiplication."
    end
  |Add (a, b) -> begin
    match (eval a state, eval b state) with
      (VN x, VN y) -> VN(x+y)
      |(VN x, VF y) -> VF ((Float.of_int x)+.y)
      |(VF x, VN y) -> VF(x+.(Float.of_int y))
      |(VF x, VF y) -> VF(x+.y)
      |_ -> invalid_arg "Invalid args for addition."
    end
  |Sub (a, b) -> begin
    match (eval a state, eval b state) with
      (VN x, VN y) -> VN(x - y)
      |(VN x, VF y) -> VF (Float.of_int x -. y)
      |(VF x, VN y) -> VF(x -. Float.of_int y)
      |(VF x, VF y) -> VF(x -. y)
      |_ -> invalid_arg "Invalid args for subtracton."
    end
  |Less (a, b) -> begin
    match (eval a state, eval b state) with
      (VN x, VN y) -> VB(x < y)
      |(VN x, VF y) -> VB ( Float.of_int x < y)
      |(VF x, VN y) -> VB(x < Float.of_int y)
      |(VF x, VF y) -> VB(x < y)
      |_ -> invalid_arg "Invalid args for subtracton."
    end
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
  |Concat (a, b) -> begin
    match eval a state with
      VUnit -> eval b state (* Recursive base case *)
      |VList fs -> 
        begin
        match eval b state with
          VUnit -> VList fs
          |VList sn -> VList (fs @ sn)
          |_ -> raise (Failure "Impossible")
        end
      |_ -> raise (Failure "Impossible")
    end
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
  |As (expr, _) -> eval expr state
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
  |Bottom -> raise (Failure "Attempt to eval Bottom")
  |e -> invalid_arg (string_of_expr e ^ " is not a valid expression.")

(*
  exec :: expr         -> Val
      thingToCheck -> What it returns
  Make sure the expr is typesafe and evaluate it if it is.
*)
let exec a =
  typecheck a (Hashtbl.create ~hashable:String.hashable ());
  eval a (Hashtbl.create ~hashable:String.hashable ())

let rec read_all ic =
  try
    let ln = input_line ic in
      ln ^ read_all ic
  with End_of_file -> "";;

(*
  Main
  Read stdin until eof, parse that as an expr,
  and print what it evaluates to.
*)
let () =
  let linebuf = Lexing.from_string (read_all stdin) in
  try
    let ast = (Parser.main Lexer.token linebuf) in
      printf "%s\n" (string_of_expr ast);
      printf "%s\n" (string_of_val (exec ast));
  with
  | Lexer.Error msg ->
	  fprintf stderr "%s%!" msg
  | Parser.Error ->
	  fprintf stderr "Syntax error at offset %d.\n%!" (Lexing.lexeme_start linebuf)

