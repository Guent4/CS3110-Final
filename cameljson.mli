open Coconuts


(* Converts the JSON into an OCaml Palmtree type and string form of name of current branch *)
val deserialize : string -> palm_tree * string


(* Parameter:
 *   palmtree - a palmtree
 *   string - branch name
 *   string - file name to write to
 *   string - repository directory
 * Converts an OCaml Palmtree type and string form of name of current branch and writes into the file*)
val serialize : palm_tree -> string -> string -> string -> unit