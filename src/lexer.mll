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
(* If we hit the end of file, exit the program *)
| eof { exit 0 }

(* Recognize start of a comment and switch to comment mode *)
| "(*" { comment lexbuf; main lexbuf } 

(* Rule for recognizing comments *)
and comment = parse
  (* If we see another comment start, recurse into another layer of comments *)
  "(*" { comment lexbuf; comment lexbuf }

  (* If we see a comment end, return to the calling context *)
 | "*)" { () }

  (* If we see any other character, ignore it and continue with the comment *)
 | _ { comment lexbuf }
