open Palmtreeupdater

let tree_file = "head.json"

let rec main () =
  let parse_cmd_expr = Camel.read() in
  let feedback = Oasys.eval parse_cmd_expr in
  Camel.output(feedback); main ()

let () =
  main()