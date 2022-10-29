open Token

type expr =
  (* | Assign of {name: Token.t; value: expr} *)
  | Binary of expr * Token.t * expr
  (* | Call of {callee: expr; paren: Token.t; arguments: expr list} *)
  (* | Get of {obj: expr; name: Token.t} *)
  | Grouping of expr
  | Literal of literal
  (* | Logical of {left: expr; operator: Token.t; right: expr} *)
  (* | Set of {obj: expr; name: Token.t; value: expr} *)
  (* | Super of {keyword: Token.t; meth: Token.t} *)
  (* | This of {keyword: Token.t} *)
  | Unary of Token.t * expr
  (* | Variable of {name: Token.t} *)

