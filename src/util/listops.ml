let remove l x =
  List.filter (fun x' -> x' <> x) l

let subtract minuend subtrahend =
  List.filter (fun x -> not (List.mem x subtrahend)) minuend

let union l1 l2 =
  List.fold_left (fun a x -> if (not (List.mem x l1)) then x :: a else a) l1 l2