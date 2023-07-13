{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
]
}

{
  let comment_depth = ref 0
}


rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
  | "(*"  { incr comment_depth; comment lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| ">" { Parser.GT }
| "&&" { Parser.AND }
| "||" {Parser.OR}

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }

and comment = parse
  | "(*"  { incr comment_depth; comment lexbuf }
  | "*)"  { decr comment_depth; if !comment_depth > 0 then comment lexbuf else token lexbuf }
  | _     { comment lexbuf }
  | eof   { failwith "End of file inside a comment" }
