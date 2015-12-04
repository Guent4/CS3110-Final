(* This module is responsible for currying the parsed data from Camel to OASys
 * where the inputted user action is actually executed.  The output of OASys in
 * the format of Coconuts.feedback is then given to Camel to print to terminal. *)

let main () =
  let (repo_dir,parse_cmd_expr) = Camel.read_interpret () in
  let feedback = Oasys.eval repo_dir parse_cmd_expr in
  let () = Camel.output(feedback) in
  ()

let () =
  main()