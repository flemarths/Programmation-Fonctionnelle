type csv_content = string list list
let rec input_lines ic =
try
let line = input_line ic in
line :: input_lines ic
with
| End_of_file -> []
let read_csv name =
let ic = open_in name in
let content : csv_content = List.map (String.split_on_char ’,’) (input_lines ic) in
close_in ic;
