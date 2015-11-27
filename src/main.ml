let main () =
  let parse_cmd_expr = Camel.read_interpret () in
  let feedback = Oasys.eval parse_cmd_expr in
  let () = Camel.output(feedback) in
  ()

let () =
  main()