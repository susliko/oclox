open Token 
open RuntimeError

let had_error = ref false
let had_runtime_error = ref false
let interpreter = new Interpreter.interpreter

let report line where message = 
  Printf.printf "[line %d] Error%s: %s\n" line where message;
  had_error := true

let error line message = 
  report line "" message;
  had_error := false

let runtime_error (error: exn) = 
  match error with
  | RuntimeError (token, msg) ->
      Printf.printf "%s\n[line %d]]" msg token.line; 
      had_runtime_error := true
  | _ -> raise error

let error_at_token (token: Token.t) message = 
  let pos = if token.tpe = EOF then " at end"
  else Printf.sprintf " at '%s'" token.lexeme in
    report token.line pos message
