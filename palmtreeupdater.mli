open Types

(* This is responsible for taking in the cmd_expr, which is the parsed command
 *	that was passed in by the user, and the current palm_tree and then applying
 *	modifications on the palm_tree before returning it. *)
val update_tree : cmd_expr -> palm_tree -> palm_tree