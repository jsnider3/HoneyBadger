(*
  
*)

{
  open Parser
  open Printf

  exception Error of string

}

rule token = parse
| '\n'
    { Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t']+
    { token lexbuf }
| "//"[^'\n']*
    { token lexbuf }
| "true"
    { BOOL true}
| "false"
    { BOOL false}
| "while"
    {WHILE}
| "if"
    {IF}
| "then"
    {THEN}
| "else"
    {ELSE}
| "fun"
    {FUNC}
| "print"
    { PRINT }
| "len"
    { LEN }
| "readline" ' '*'('' '*')'
    { READLINE }
| "=="
    {EQUAL}
| "!="
    {NEQ}
| "<"
    {LESS}
| "<="
    {LEQ}
| ">"
    {GRE}
| ">="
    {GEQ}
| "="
    {ASSIGN}
| "&"
    {AND}
| "|"
    {OR}
| "!"
    {NOT}
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| ['0'-'9']*'.'['0'-'9']+ as f
    { FLOAT (float_of_string f) }
| ['a'-'z']['a' - 'z' '0'-'9' '_' 'A' - 'Z']* as v
    { VAR v }
| '"' [^'"']* '"' as s
    { STRING (String.sub s 1 (String.length s - 2)) }
| "Int"
    {INT_T}
| "Bool"
    {BOOL_T}
| "String"
    {STRING_T}
| "Float"
    {FLOAT_T}
| "Record"
    {RECORD_T}
| "Top"
    {TOP_T}
| "Except"
    {EXCEPT_T}
| "Arr"
    {ARR_T}
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIVIDE }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LCURL }
| '}'
    { RCURL }
| '['
    { LBRACK }
| ']'
    { RBRACK }
| ';'
    { SEMI }
| ':'
    { COLON }
| ','
    { COMMA }
| eof
    { EOF }
| _ as c
    { raise (Error (sprintf "At offset %d: unexpected character %c.\n" (Lexing.lexeme_start lexbuf) c)) }
