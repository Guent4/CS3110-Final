open Types

(* JSON used for sending and receiving data. *)
type json

(* Converts an OCaml Palmtree type into a JSON *)
val deserialize : palmtree -> json

(* Converts a JSON into an OCaml Palmtree type *)
val serialize : json -> palmtree