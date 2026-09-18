type t = {
  loc : Location.t;
  msg : string;
}

let to_string (r : t) : string =
  let loc_str = Location.to_string r.loc in
  Printf.sprintf "%s: %s" loc_str r.msg

let make (loc : Location.t) (msg : string) : t = { loc; msg }
