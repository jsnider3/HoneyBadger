%{

  open Defs
  open Printf

%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VAR STRING
%token PLUS MINUS TIMES DIVIDE 
%token FUNC
%token PRINT READLINE LEN
%token SEMI COMMA COLON ASSIGN
%token EQUAL NOT AND OR NEQ LESS GRE LEQ GEQ
%token IF THEN ELSE WHILE
%token INT_T FLOAT_T BOOL_T STRING_T RECORD_T
%token TOP_T EXCEPT_T UNIT_T ARR_T
%token LPAREN RPAREN LBRACK RBRACK LCURL RCURL
%token EOF

%left PLUS MINUS OR        /* lowest precedence */
%left TIMES DIVIDE AND      /* medium precedence */
%left EQUAL NEQ LESS GRE LEQ GEQ
%nonassoc UMINUS NOT        /* highest precedence */

%start <Defs.expr> main

%%

main:
| e = exp EOF
    { e }
| EOF
    { Unit } 

exp:
| i = INT
    { N i }
| b = BOOL
    { B b }
| f = FLOAT
    { F f }
| s = STRING
    { Str s }
| PRINT LPAREN e = exp RPAREN
    { Print e }
| LEN LPAREN e = exp RPAREN
    { Len e }
| READLINE
    { Readline }
| EXCEPT_T LPAREN e = exp RPAREN
    { Except e }
| v = VAR ASSIGN e = exp
    { Set(v, e) }
| v = VAR LBRACK f = STRING RBRACK ASSIGN e = exp
    { SetRec(v, f, e) }
| v = VAR LBRACK ind = exp RBRACK ASSIGN e = exp
    { SetInd(v, ind, e) }
| e1 = exp EQUAL e2 = exp
    { Equal(e1, e2) }
| e = exp LBRACK v = STRING RBRACK
    { GetRec(v, e) }
| e = VAR LBRACK v = STRING RBRACK
    { GetRec(v, Lookup(e)) }
| e1 = exp LBRACK e2 = exp RBRACK
    { Get(e2, e1) }
| e = VAR LBRACK e2 = exp RBRACK
    { Get(e2, Lookup(e)) }
| v = VAR
    { Lookup v }
| l = exp LPAREN a = expr_list RPAREN
    { App(l, a) }
| t = type_t LPAREN a = exp RPAREN
    { Cast(a, t) }
| FUNC vs = var_list COLON a = exp 
    { Lam(vs, a) }
| LPAREN e = exp RPAREN
    { e }
| LPAREN RPAREN
    { Unit }
| e1 = exp PLUS e2 = exp
    { Add(e1, e2) }
| e1 = exp MINUS e2 = exp
    { Sub(e1, e2) }
| e1 = exp TIMES e2 = exp
    { Mul(e1, e2) }
| e1 = exp DIVIDE e2 = exp
    { Div(e1, e2) }
| e1 = exp AND e2 = exp
    { And(e1, e2) }
| e1 = exp NEQ e2 = exp
    { Not(Equal(e1, e2)) }
| e1 = exp LESS e2 = exp
    { Less(e1, e2) }
| e1 = exp GRE e2 = exp
    { And(Not(Less(e1, e2)), Not(Equal(e1, e2))) }
| e1 = exp GEQ e2 = exp
    { Not(Less(e1, e2)) }
| e1 = exp LEQ e2 = exp
    { Or(Less(e1, e2), Equal(e1, e2)) }
| e1 = exp OR e2 = exp
    { Or(e1, e2) }
| MINUS e = exp %prec UMINUS
    { Sub(N 0, e) }
| NOT e = exp
    { Not e }
| IF e1 = exp THEN e2 = exp ELSE e3 = exp
    { If(e1, e2, e3) }
| IF e1 = exp THEN e2 = exp
    { If(e1, e2, Unit) }
| WHILE e1 = exp LCURL e = expr_seq RCURL
    { While(e1, Seq e) }
| LBRACK e = expr_list RBRACK
    { Arr (Array.of_list e) }
| LCURL e = expr_seq RCURL
    { Seq e }
| LCURL f = fields RCURL
    { Record f }

type_t:
| INT_T
    { TInt }
| BOOL_T
    { TBool }
| TOP_T
    { TTop }
| STRING_T
    { TStr }
| LPAREN RPAREN
    { TUnit }
| FLOAT_T
    { TReal }
| ARR_T
    { TArr }
|RECORD_T LBRACK t = var_typed RBRACK
    { TRecord t }

fields:
|
    { [] }
| v = STRING COLON e = exp
    { [(v, e)] }
| v = STRING COLON e = exp COMMA vt = fields
    { (v, e) :: vt }

var_typed:
|
    { [] }
| v = STRING COLON e = type_t
    { [(v, e)] }
| v = STRING COLON e = type_t COMMA vt = var_typed
    { (v, e) :: vt }

var_list:
|
    { [] }
| v = VAR
    { [v] }
| v = VAR COMMA vs = var_list
    { v :: vs } 

expr_list:
|
    { [] }
| e = exp
    { [e] }
|e1 = exp COMMA e2 = expr_list
    {e1 :: e2} 

expr_seq:
| e = exp
    {[e]}
|e1 = exp SEMI e2 = expr_seq
    {e1 :: e2} 
