open Coconuts

(* Responsible for reading a line of text from the REPL; this wil be a line of
 *	user input
 *)
val read : unit -> string * (string list)

(* Responsible for lexing and parsing a string into a cmd_expr option.  Will
 *   return None if not a valid cmd_expr. *)
val interpret: string * string list -> string * (cmd_expr option)

(* Performs the read function and then pipe output into interpret function *)
val read_interpret : unit -> string * cmd_expr

(* Responsible for taking a string and printing it to the REPL *)
val output: feedback -> unit