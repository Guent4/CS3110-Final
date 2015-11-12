open Lexing

(* This is responsible for taking the string input from the reader and lexing
 * it into a list of strings that will later be parsing into commands
 *)
val lex : string -> string list