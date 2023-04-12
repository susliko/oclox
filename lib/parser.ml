open Token
open Expr

exception ParseError

class parser (tokens: Token.t list) = 
  object (p)
    method parse: expr option = 
      try Option.Some p#expression
      with ParseError -> None

    val mutable current = 0
    method private expression: expr =
      p#binary_op [COMMA] (fun _ -> p#equality)

    method private equality: expr = 
      p#binary_op [BANG_EQUAL; EQUAL_EQUAL] (fun _ -> p#comparison)

    method private comparison = 
      p#binary_op [LESS; LESS_EQUAL; GREATER; GREATER_EQUAL] (fun _ -> p#term)

    method private term =
      p#binary_op [MINUS; PLUS] (fun _ -> p#factor)

    method private factor = 
      p#binary_op [SLASH; STAR] (fun _ -> p#unary)

    method private unary = 
      if p#matches [BANG; MINUS] then
        let op = p#previous in
        let right = p#unary in
        Unary (op, right)
      else p#primary

    method private primary: expr = 
      if p#matches [FALSE] then Literal (BOOL_LIT false)
      else if p#matches [TRUE] then Literal (BOOL_LIT true)
      else if p#matches [NIL] then Literal NO_LIT
      else if p#matches [NUMBER; STRING] then Literal p#previous.literal
      else if p#matches [LEFT_PAREN] then
        let expr = p#expression in
        let msg = "Expect ')' after expression." in
        p#consume RIGHT_PAREN msg;
        Grouping expr
      else p#raise_err p#peek "Expect expression."

    method private binary_op op_tokens try_next: expr = 
      let rec go expr = 
        if p#matches op_tokens then
          let op = p#previous in 
          let right = (try_next ()) in
          go (Binary (expr, op, right))
        else expr in
      go (try_next ()) 

    method private matches token_types =
      let f el = if p#check el then (ignore(p#advance); true) else false in
      List.exists f token_types

    method private check token_type = 
      if p#is_at_end then false
      else p#peek.tpe = token_type

    method private advance: Token.t = 
      if not p#is_at_end then current <- current + 1;
      p#previous

    method private is_at_end = 
      p#peek.tpe = EOF

    method private peek: Token.t = 
      List.nth tokens current

    method private previous: Token.t = 
      List.nth tokens (current - 1)

    method private consume token_type message = 
      if p#check token_type then ignore(p#advance)
      else p#report_err p#peek message

    method private report_err token message = 
      Lox.error_at_token token message;
    
    method private raise_err token message = 
      p#report_err token message;
      raise ParseError

    method private synchronize = 
      let rec loop x = 
        if p#previous.tpe != SEMICOLON then
          match p#peek.tpe with
            CLASS | FOR | FUN | IF 
            | PRINT | RETURN | VAR | WHILE -> ()
            | _ -> ignore(p#advance); loop x
        in
        ignore(p#advance); loop ()
  end

