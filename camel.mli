open Coconuts
(* Responsible for reading a line of text from the REPL; this wil be a line of
 *	user input
 *)
val read : unit -> string list

(* Responsible for lexing and parsing a string into a cmd_expr option.  Will
 *   return None if not a valid cmd_expr. *)
val interpret: string list -> cmd_expr option

(* Performs the read function and then pipe output into interpret function *)
val read_interpret : unit -> cmd_expr

(* Responsible for taking a string and printing it to the REPL *)
val output: feedback -> unit