open Coconuts

(* Responsible for reading user input from the terminal *)
val read : unit -> string * (string list)

(* Responsible for parsing a string into a cmd_expr option.  Some basic checks
 * will be performed on the input to make sure the cmd and opt combination is
 * supported and if there is the correct number or arguments.  HELP and suggestions
 * for mistyped words are also supported in interpret through calling methods in
 * xiansheng.ml.  Will return (a,b) where a is the repo directory and b is Some x
 * if x is a supported cmd_expr and None otherwise. *)
val interpret: string * string list -> string * (cmd_expr option)

(* Performs the read function and then pipe output into interpret function *)
val read_interpret : unit -> string * cmd_expr

(* Responsible for taking a feedback variant and printing it to the the terminal *)
val output: feedback -> unit