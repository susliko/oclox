open Expr
open Token
open RuntimeError

type lox_value = 
  | LoxNil
  | LoxBool of bool
  | LoxNumber of float
  | LoxString of string


class interpreter = 
  object (i)
    method interpret (e : expr) : lox_value = 
      try (
      let res = match e with
        | Literal l -> i#i_literal l
        | Grouping g -> i#interpret g
        | Unary (op, e) -> i#i_unary op e
        | Binary (l, op, r) -> i#i_binary op l r
        in
      print_endline (i#stringify res);
      res)
      with RuntimeError (token, msg) -> 
        print_endline (Printf.sprintf "[line %d] Error at '%s': %s" token.line token.lexeme msg);
        LoxNil
  
    method private i_literal (l : literal) : lox_value = 
      match l with
      | NO_LIT -> LoxNil
      | STRING_LIT s -> LoxString s
      | FLOAT_LIT f -> LoxNumber f
      | BOOL_LIT b -> LoxBool b

    method private i_unary (op : Token.t) (e : expr) : lox_value =
      let res = i#interpret e in
      match op.tpe with
        | MINUS -> (match res with
                    | LoxNumber n -> LoxNumber (-.n)
                    | _ -> i#num_err op)
        | BANG -> LoxBool (i#is_truthy res)
        | _ -> res
    
    method private is_truthy (v: lox_value): bool = 
      match v with
      | LoxNil -> false
      | LoxBool b -> b
      | _ -> true

    method private i_binary (op: Token.t) (l: expr) (r: expr): lox_value = 
      let left = i#interpret l in
      let right = i#interpret r in

      match op.tpe with
      | MINUS -> (match left, right with
                  | LoxNumber l, LoxNumber r -> LoxNumber (l -. r)
                  | _ -> i#num_err op)
      | SLASH -> (match left, right with
                  | LoxNumber l, LoxNumber r -> LoxNumber (l /. r)
                  | _ -> i#num_err op)
      | STAR -> (match left, right with
                  | LoxNumber l, LoxNumber r -> LoxNumber (l *. r)
                  | _ -> i#num_err op)
      | PLUS -> (match left, right with
                  | LoxNumber l, LoxNumber r -> LoxNumber (l +. r)
                  | LoxString l, LoxString r -> LoxString (l ^ r)
                  | _ -> raise (RuntimeError (op, "Operands must be two numbers or two strings.")))
      | GREATER -> (match left, right with
                    | LoxNumber l, LoxNumber r -> LoxBool (l > r)
                    | _ -> i#num_err op)
      | GREATER_EQUAL -> (match left, right with
                          | LoxNumber l, LoxNumber r -> LoxBool (l >= r)
                          | _ -> LoxNil)
      | LESS -> (match left, right with
                 | LoxNumber l, LoxNumber r -> LoxBool (l < r)
                 | _ -> i#num_err op)
      | LESS_EQUAL -> (match left, right with
                       | LoxNumber l, LoxNumber r -> LoxBool (l <= r)
                       | _ -> i#num_err op)
      | BANG_EQUAL -> LoxBool (not (i#is_equal left right))
      | EQUAL_EQUAL -> LoxBool (i#is_equal left right)
      | _ -> LoxNil

    method private is_equal (l: lox_value) (r: lox_value): bool = 
      match l, r with
      | LoxNil, LoxNil -> true
      | LoxBool l, LoxBool r -> l = r
      | LoxNumber l, LoxNumber r -> l = r
      | LoxString l, LoxString r -> l = r
      | _ -> false


    method private num_err (token: Token.t)  : lox_value = raise (RuntimeError (token, "Operands must be numbers."))

    method private stringify (v: lox_value): string = 
      match v with
      | LoxNil -> "nil"
      | LoxBool b -> string_of_bool b
      | LoxNumber n -> string_of_float n
      | LoxString s -> s
  end


