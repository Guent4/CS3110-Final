open Coconuts
open Cameljson
open Palmtreeupdater

let master_state_path = "./master_state.json"

(* file contains configuration for the current state, which includes the
 * state of the tree, the current working branch name, and the repository
 * directory *)
let state_path = "./init_state.json"

let eval repo_path cmd =
  let state = Cameljson.deserialize state_path in
  let tree = state.tree in
  let config = state.config in
  let repo_dir = config.repo_dir in
  match repo_dir with
  | "<not set>" ->
    let config = {config with repo_dir = repo_path} in
    let (tree', config', feedback) = Palmtreeupdater.update_tree cmd tree config in
    let state' = {config= config'; tree= tree'} in
    let () = Cameljson.serialize state' state_path in
    feedback
  | _ ->
    let (tree', config', feedback) = Palmtreeupdater.update_tree cmd tree config in
    let state' = {config= config'; tree= tree'} in
    let () = Cameljson.serialize state' state_path in
    feedback