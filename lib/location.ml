type t = {
  file_path : string;
  line : int;
  col : int;
}

let to_string (l : t) : string =
  Printf.sprintf "%s:%d:%d" l.file_path l.line l.col
