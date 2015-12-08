let lines_from_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
List.rev !lines;;

let file_from_lines lines filename =
  let oc = open_out filename in
  List.iter 
    (fun x ->
      Printf.fprintf oc "%s\n" x ) lines;
  close_out oc;
