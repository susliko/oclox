type token_type = 
  (* Single-character *)
  LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE |
  COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR |
  (* One or two character *)
  BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL |
  GREATER | GREATER_EQUAL | LESS | LESS_EQUAL |
  (* Literals *)
  IDENTIFIER | STRING | NUMBER |
  (* Keywords *)
  AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR |
  PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE |
  EOF
  [@@deriving show]

module Keywords = Map.Make(String)
let keywords = 
  Keywords.add "and" AND @@ 
  Keywords.add "class" CLASS @@ 
  Keywords.add "else" ELSE @@ 
  Keywords.add "false" FALSE @@ 
  Keywords.add "for" FOR @@ 
  Keywords.add "fun" FUN @@ 
  Keywords.add "if" IF @@ 
  Keywords.add "nil" NIL @@ 
  Keywords.add "or" OR @@ 
  Keywords.add "print" PRINT @@ 
  Keywords.add "return" RETURN @@ 
  Keywords.add "super" SUPER @@ 
  Keywords.add "this" THIS @@ 
  Keywords.add "true" TRUE @@ 
  Keywords.add "var" VAR @@ 
  Keywords.add "while" WHILE @@ 
  Keywords.empty


type literal = 
  NO_LIT |
  STRING_LIT of string |
  FLOAT_LIT of float
  [@@deriving show]

module Token = struct
  type t = {
    tpe: token_type;
    lexeme: string;
    literal: literal;
    line: int;
  }

  let show_token t = 
    Printf.sprintf "%s %s %s at line %d" 
      (show_token_type t.tpe) 
      t.lexeme 
      (show_literal t.literal)
      t.line end


