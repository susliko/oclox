open Printf

let print_list l = 
  print_string "[";
  List.iter (printf "%s, ") l;
  print_string "]"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all 
