open Expr
open Token

let rec print expr: string = 
  let parenthesize name exprs = 
    exprs |> 
    List.map print |> 
    String.concat " " |> 
    Printf.sprintf "(%s %s)" name in

  match expr with 
    | Binary (left, operator, right) -> 
        parenthesize operator.lexeme [left; right]
    | Grouping expr -> 
        parenthesize "group" [expr]
    | Literal value -> (match value with
      | NO_LIT -> "nil"
      | STRING_LIT s -> s
      | FLOAT_LIT f -> string_of_float f
      | BOOL_LIT b -> string_of_bool b)
    | Unary (operator, right) -> 
        parenthesize operator.lexeme [right]
