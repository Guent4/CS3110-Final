open Coconuts
open Hashtbl

let gen_hash () = string_of_int (hash (Unix.gettimeofday()))

let update_tree (cmd:cmd_expr) (pt:palm_tree) :palm_tree =
  match cmd with
  | (_,INIT,_,_) -> Init (gen_hash (), "initial commit")
  |
  | _ -> failwith "not implemented"