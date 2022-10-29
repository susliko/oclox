open Token 

let had_error = ref false

let report line where message = 
  Printf.printf "[line %d] Error%s: %s\n" line where message;
  had_error := true

let error line message = 
  report line "" message;
  had_error := false

let error_at_token (token: Token.t) message = 
  let pos = if token.tpe = EOF then " at end"
  else Printf.sprintf " at '%s'" token.lexeme in
    report token.line pos message
