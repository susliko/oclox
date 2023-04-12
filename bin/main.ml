open Oclox
open Token


let run code: unit = 
  let scanner = new Scanner.scanner code in
  let tokens = scanner#scan_tokens in
  let f (elem: Token.t) = print_endline (Token.show_token elem) in 
  List.iter f tokens;
  let parser = new Parser.parser tokens in
  let expr = parser#parse in
  match expr with
    | None -> print_string "Parsing error"
    | Some expr -> ignore(Lox.interpreter#interpret expr);
  print_endline ""
  

let run_file file = 
  let lines = Util.read_file file in 
  run lines;
  if !Lox.had_error then
    exit(65)
  else if !Lox.had_runtime_error then
    exit(70)

let run_prompt () = 
  let continue = ref true in 
  while !continue do 
    print_string "> ";
    let line = read_line () in
    if not (line = "") then
      let _ = run line in
      Lox.had_error := false
    else 
      continue := false
  done

let ast_printer_test () =
  let open Expr in 
  let aa: expr = Unary ({tpe=MINUS; lexeme=""; literal=NO_LIT; line=1}, 
     Literal (FLOAT_LIT 123.0)) in
  let bb: Token.t = {tpe=STAR; lexeme="*"; literal=NO_LIT; line=1} in
  let cc: expr = Grouping (Literal (FLOAT_LIT 45.67)) in
  let expr: expr = Binary (aa, bb, cc) in
  AstPrinter.print expr |> print_endline

let main () = 
  if Array.length Sys.argv > 2 then begin
    print_endline "Usage: jlox [script]";
    exit 64
  end 
  else if Array.length Sys.argv = 2 then 
    run_file Sys.argv.(1)
  else run_prompt ()

let _ = main ()

(* let _ = ast_printer_test () *)
