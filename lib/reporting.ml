let report line where message = 
  print_endline (String.concat "" ["[line "; string_of_int line;  "] Error";  where;  ": ";  message])

let report_error (line: int) (message: string)  = 
  report line "" message
