let remove l x =
  List.filter (fun x' -> x' <> x) l

let subtract minuend subtrahend =
  List.filter (fun x -> not (List.mem x subtrahend)) minuend

let union l1 l2 =
  List.fold_left (fun a x -> if (not (List.mem x l1)) then x :: a else a) l1 l2

let subset l1 l2 =
  subtract l1 l2 = []

let equal l1 l2 =
  subset l1 l2 && subset l2 l1

let to_string l start sep terminal =
  (List.fold_left (fun a x -> a ^ sep ^ x) start l) ^ terminal