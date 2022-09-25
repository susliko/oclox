open Oclox
open Token

let had_error = ref true
let error line message = 
  Reporting.report_error line message;
  had_error := false


(* Runner *)
let run code = 
  let scanner = new Scanner.scanner code in
  let tokens = scanner#scan_tokens in
  let f (elem: Token.t) = print_endline (Token.show_token elem) in 
    List.iter f tokens
  

let run_file file = 
  let lines = Util.read_file file in 
  run lines;
  if !had_error then
    exit(65)

let run_prompt () = 
  let continue = ref true in 
  while !continue do 
    print_string "> ";
    let line = read_line () in
    if not (line = "") then
      let _ = run line in
      had_error := false
    else 
      continue := false
  done

let main () = 
  if Array.length Sys.argv > 2 then begin
    print_endline "Usage: jlox [script]";
    exit 64
  end 
  else if Array.length Sys.argv = 2 then 
    run_file Sys.argv.(1)
  else run_prompt ()

  let _ = main ()
