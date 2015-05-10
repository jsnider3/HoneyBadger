(*
  
*)

{
  open Parser
  open Printf

  exception Error of string

}

rule token = parse
| [' ' '\t' '\n']
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
| "readline" ' '*'('' '*')'
    { printf "Readline 33\n"; READLINE }
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
| ['a'-'z']+ as v
    { VAR v }
| '"' [^'"']* '"' as s
    { STRING s }
| "Int"
    {INT_T}
| "String"
    {STRING_T}
| "Float"
    {FLOAT_T}
| "Record"
    {RECORD_T}
| "Top"
    {TOP_T}
| "Bottom"
    {BOTTOM_T}
| '(' ')'
    {UNIT_T}
| "List"
    {LIST_T}
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '('
    { printf "(\n"; LPAREN }
| ')'
    { printf ")\n"; RPAREN }
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
