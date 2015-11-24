let rec main () =
  let parse_cmd_expr = Camel.read() in
  let feedback = Oasys.eval parse_cmd_expr in
  let () = Camel.output(feedback) in
  main ()

let () =
  main()