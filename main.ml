open Oasys
open Camel

let tree_file = "head.json"

let rec main () =
  let _ = Camel.read_interpret () in
  (* let feedback = Oasys.eval parse_cmd_expr in
  Camel.output(feedback); *) main ()

let () =
  main()