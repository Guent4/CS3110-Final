open Oasys
open Camel

let tree_file = "head.json"

let rec main () =
  let input = Camel.read() in
  let _ = Camel.interpret input in
  (* let feedback = Oasys.eval parse_cmd_expr in
  Camel.output(feedback); *) main ()

let () =
  main()