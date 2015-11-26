open Coconuts
open Fileio
open Cameljson
open Palmtreeupdater

(* file contains configuration for the current state, which includes the
 * state of the tree, the current working branch name, and the repository
 * directory *)
let state_path = "state.json"

let eval cmd =
  let (tree, config) = Cameljson.deserialize state_path in
  let (tree', config', feedback) = Palmtreeupdater.update_tree cmd tree config in
  let () = Cameljson.serialize tree' config' state_path in
  feedback