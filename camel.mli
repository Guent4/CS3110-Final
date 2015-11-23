open Coconuts
(* Responsible for reading a line of text from the REPL; this wil be a line of
 *	user input
 *)
val read : unit -> cmd_expr

(* Responsible for taking a string and printing it to the REPL *)
val output: feedback -> unit