open Coconuts
open Hashtbl

(* This is responsible for taking in the cmd_expr, which is the parsed command
 *	that was passed in by the user, the current palm_tree, and the name of the
 *  current branch and then applying modifications on the palm_tree, files in
 *  directory, and name of the branch before returning the new tree and the name
 *  of the branch that we are in, and the path of the repository directory.
 *  Also outputs a string that gives user feedback *)
val update_tree : cmd_expr -> palm_tree -> config -> palm_tree * config * feedback