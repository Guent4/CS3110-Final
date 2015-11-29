open Coconuts


(* Converts the JSON into an OCaml Palmtree type and string form of name of current branch *)
val deserialize : string -> state

(* Parameter:
 *   palmtree - a palmtree
 *   config - repo dir, branch name
 *   string - file name to write to
 * Converts an OCaml Palmtree type and string form of name of current branch and writes into the file*)
val serialize : state -> string -> unit
