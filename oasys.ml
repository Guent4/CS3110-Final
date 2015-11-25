open Coconuts
open Fileio
open Cameljson
open Palmtreeupdater

let tree_file_name = "head.json"

let eval cmd =
  let (tree,branch_name) = Cameljson.deserialize tree_file_name in
  let (tree',branch_name',feedback) = Palmtreeupdater.update_tree cmd tree branch_name in
  (* let () = Cameljson.serialize tree' branch_name' tree_file_name in *)
  feedback