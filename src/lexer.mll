{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("let", Parser.LET); 
  ("and", Parser.LETAND); 
  ("in", Parser.IN); 
  ("fun", Parser.FUN); 
  ("rec", Parser.REC); 
  ("dfun", Parser.DFUN); 
  ("quit", Parser.QUIT); 
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "&&" { Parser.AND} 
| "||" { Parser.OR} 
| "=" { Parser.EQ} 
| "->" { Parser.RARROW} 

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }
| "(*" { comment lexbuf; main lexbuf } 
and comment = parse
  "(*" { comment lexbuf; comment lexbuf }
 | "*)" { () }
 | _ { comment lexbuf }


