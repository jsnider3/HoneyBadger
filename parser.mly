%{

  open Defs
  open Printf

%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VAR STRING
%token PLUS MINUS TIMES 
%token FUNC
%token PRINT READLINE
%token SEMI COMMA COLON ASSIGN
%token EQUAL NOT AND OR NEQ LESS GRE LEQ GEQ
%token IF THEN ELSE WHILE
%token INT_T FLOAT_T STRING_T RECORD_T TOP_T BOTTOM_T UNIT_T LIST_T
%token LPAREN RPAREN LBRACK RBRACK LCURL RCURL
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES             /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <Defs.expr> main

%%

main:
| e = exp EOF
    { e }

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
| READLINE
    { Readline }
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
    { GetRec(v,Lookup(e)) }
| e1 = exp LBRACK e2 = exp RBRACK
    { Get(e2, e1) }
| v = VAR
    { Lookup v }
| l = exp LPAREN a = exp RPAREN
    { App(l, a) }
| t = type_t LPAREN a = exp RPAREN
    { Cast(a, t) }
| FUNC v = VAR COLON t = type_t a = exp 
    { Lam(t, v, a) }
| LPAREN e = exp RPAREN
    { e }
| LPAREN RPAREN
    { Unit }
| LBRACK RBRACK
    { List [] }
| e1 = exp PLUS e2 = exp
    { Add(e1, e2) }
| e1 = exp MINUS e2 = exp
    { Sub(e1, e2) }
| e1 = exp TIMES e2 = exp
    { Mul(e1, e2) }
| e1 = exp AND e2 = exp
    { And(e1, e2) }
| e1 = exp NEQ e2 = exp
    { Not(And(e1, e2)) }
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
| WHILE LPAREN e1 = exp RPAREN LCURL e = expr_seq RCURL
    { While(e1, Seq e) }
| LBRACK e = expr_list RBRACK
    { List e }
| LCURL e = expr_seq RCURL
    { Seq e }
| LCURL f = fields RCURL
    { Record f }

type_t:
| INT_T
    { TInt }
| TOP_T
    { TTop }
| STRING_T
    { TStr }
| BOTTOM_T
    { TBottom }
| UNIT_T
    { TUnit }
| FLOAT_T
    { TReal }
| LIST_T t = type_t
    { TList t }
|RECORD_T LBRACK t = var_typed RBRACK
    { TRecord t }

fields:
| v = STRING COLON e = exp
    { [(v, e)] }
| v = STRING COLON e = exp COMMA vt = fields
    { (v, e) :: vt }

var_typed:
| v = STRING COLON e = type_t
    { [(v, e)] }
| v = STRING COLON e = type_t COMMA vt = var_typed
    { (v, e) :: vt }

expr_list:
| e = exp
    { [e] }
|e1 = exp COMMA e2 = expr_list
    {e1 :: e2} 

expr_seq:
| e = exp
    {[e]}
|e1 = exp SEMI e2 = expr_seq
    {e1 :: e2} 
