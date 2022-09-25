open Token

class scanner (source: string) = 
  object (self)
    val mutable tokens = ([]: Token.t list)
    val mutable start = 0
    val mutable current = 0
    val mutable line = 1

    method scan_tokens: Token.t list = 
      while not self#is_at_end do
        start <- current;
        self#scan_token
      done;
      let t: Token.t = { tpe=EOF; lexeme=""; literal=None; line=line } in
      tokens <- List.cons t tokens;
      tokens

    method is_at_end: bool = 
      current >= String.length source

    method scan_token = 
      let c = self#advance in 
      match c with
        '(' -> self#add_token LEFT_PAREN 
        | ')' -> self#add_token RIGHT_PAREN
        | '{' -> self#add_token LEFT_BRACE
        | '}' -> self#add_token RIGHT_BRACE
        | ',' -> self#add_token COMMA
        | '.' -> self#add_token DOT
        | '-' -> self#add_token MINUS
        | '+' -> self#add_token PLUS
        | ';' -> self#add_token SEMICOLON
        | '*' -> self#add_token STAR
        |_ -> self#add_token EOF

    method advance =
      let res = source.[current] in
      current <- current+1;
      res

    method add_token tpe = self#add_generic_token tpe None

    method add_generic_token tpe opt_literal = 
      let text = (String.sub source start (current-start)) in
      let t: Token.t = { tpe=tpe; lexeme=text; literal=opt_literal; line=line} in
      tokens <- List.cons t tokens

  end
