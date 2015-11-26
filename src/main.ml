open Oasys
open Camel

let tree_file = "head.json"

let main () =
  let _ = Camel.read_interpret () in ()
  (* let feedback = Oasys.eval parse_cmd_expr in
  Camel.output(feedback); *)

let () =
  main()