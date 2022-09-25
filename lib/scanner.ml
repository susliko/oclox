open Token

class scanner (source: string) = 
  object (s)
    val mutable tokens = ([]: Token.t list)
    val mutable start = 0
    val mutable current = 0
    val mutable line = 1
    val null_char = char_of_int 0


    method scan_tokens: Token.t list = 
      while not s#is_at_end do
        start <- current;
        s#scan_token
      done;
      let t: Token.t = { tpe=EOF; lexeme=""; literal=NO_LIT; line=line } in
      tokens <- List.cons t tokens;
      tokens

    method is_at_end: bool = 
      current >= String.length source

    method private scan_token = 
      let c = s#advance in 
      match c with
        '(' -> s#add_token LEFT_PAREN 
        | ')' -> s#add_token RIGHT_PAREN
        | '{' -> s#add_token LEFT_BRACE
        | '}' -> s#add_token RIGHT_BRACE
        | ',' -> s#add_token COMMA
        | '.' -> s#add_token DOT
        | '-' -> s#add_token MINUS
        | '+' -> s#add_token PLUS
        | ';' -> s#add_token SEMICOLON
        | '*' -> s#add_token STAR
        | '!' -> s#add_token (if s#match1 '=' then BANG_EQUAL else BANG)
        | '=' -> s#add_token (if s#match1 '=' then EQUAL_EQUAL else EQUAL)
        | '<' -> s#add_token (if s#match1 '=' then LESS_EQUAL else LESS)
        | '>' -> s#add_token (if s#match1 '=' then GREATER_EQUAL else GREATER)
        | '/' -> if s#match1 '/' then
                   while s#peek != '\n' && not s#is_at_end do ignore(s#advance) done
                 else
                   s#add_token SLASH
        | ' ' | '\r' | '\t' -> ()
        | '\n' -> line <- line + 1
        | '"' -> s#string
        | c -> 
            if s#is_digit c then s#number
            else if s#is_alpha c then s#identifier
            else Reporting.error line (Printf.sprintf "Unexpected character '%c'" c)

    method private advance =
      let res = source.[current] in
      current <- current+1;
      res

    method private match1 exprected = 
      if s#is_at_end || source.[current] != exprected then false
      else (current <- current + 1; true)

    method private peek = 
      if s#is_at_end then null_char
      else source.[current]

    method private peek_next = 
      if current + 1 >= String.length source then null_char
      else source.[current + 1]

    method private string = 
      while s#peek != '"' && not s#is_at_end do
        if s#peek == '\n' then line <- line + 1;
        ignore(s#advance)
      done;
      if s#is_at_end then
        Reporting.error line "Unterminated string."
      else
        let _ = s#advance in
        let value = String.sub source (start + 1) (current - start - 2) in
        s#add_generic_token STRING (STRING_LIT value)

    method private identifier = 
      while s#is_alpha_numeric s#peek do
        ignore(s#advance)
      done;
      let text = String.sub source start (current - start) in 
      let tpe = Keywords.find_opt text keywords |> Option.value ~default:IDENTIFIER in
      s#add_token tpe

    method private is_digit c = 
      c >= '0' && c <= '9'

    method private is_alpha c = 
      c >= 'a' && c <= 'z' ||
      c >= 'A' && c <= 'Z' ||
      c == '_';

    method private is_alpha_numeric c =
      s#is_digit c || s#is_alpha c

    method private number =
      while s#is_digit s#peek do
        ignore(s#advance)
      done;
      (if s#peek = '.' && s#is_digit s#peek_next then
        ignore(s#advance);
        while s#is_digit s#peek do
          ignore(s#advance)
        done);
      s#add_generic_token NUMBER @@ 
                          FLOAT_LIT (Float.of_string @@ String.sub source start @@ current-start)


    method private add_token tpe = s#add_generic_token tpe NO_LIT

    method private add_generic_token tpe literal = 
      let text = (String.sub source start (current-start)) in
      let t: Token.t = { tpe=tpe; lexeme=text; literal=literal; line=line} in
      tokens <- List.cons t tokens

  end
