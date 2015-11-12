open Types

(* Responsible for taking a string and printing it to the REPL *)
val ouptut: string -> unit

(* This module will have a lot of helper functions that will help format the
 *  string before it get pased into the output function.  The methods will help 
 *  create unique formating like indenting a block of code, asynchronously 
 *	printing *)