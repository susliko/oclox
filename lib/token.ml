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


module Token = struct
  type t = {
    tpe: token_type;
    lexeme: string;
    literal: string option;
    line: int;
  }

  let show_token t = 
    String.concat "" [
      show_token_type t.tpe; " "; t.lexeme; " ";
      (Option.value t.literal ~default:""); " at line ";
      string_of_int t.line
    ]
end


