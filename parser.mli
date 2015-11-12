open Types

(* Takes in the lexed user input and convert it into a cmd_expr to be
 *	interpreted and executed by OASys *)
val parse : string list -> cmd_expr