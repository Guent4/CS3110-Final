open Yojson.Basic.Util
open Yojson.Basic

let deserialize_helper (json_node_list) =
  (*Filter nodes of type commit*)
  let commit_json_nodes = List.filter (fun x ->
    let tpe = x |> member "type" |> to_string in
    if tpe = "commit" then true else false) json_node_list in
  (*Filter out nodes of type change*)
  let changes_json_nodes = List.filter (fun x ->
    let tpe = x |> member "type" |> to_string in
    if tpe = "changes" then true else false) json_node_list in
  (*Commit Nodes*)
  let commit_nodes = List.map (fun x ->
    let id = x |> member "id" |> to_string in
    let msg = x |> member "message" |> to_string in
    Commit(id, msg)) commit_json_nodes in
  (*Change Nodes*)
  let change_nodes = List.map (fun x ->
    let added = x |> member "added" |> to_list |> filter_string in
    let deleted = x |> member "deleted" |> to_list |> filter_string in
    let committed = x |> member "committed" |> to_list |> filter_string in
    let c = Changes(added, deleted, committed) in
    c) changes_json_nodes in
  (commit_nodes, change_nodes)

  (* let nodes = convert_each (fun x ->
    let tpe = x |> member "type" |> to_string in
    if tpe = "commit" then
    begin
      let id = x |> member "id" |> to_string in
      let msg = x |> member "message" |> to_string in
      match (id, msg) with
      | Commit (i, m) ->
    end) json_node_list *)


let deserialize (filename:string) =
  let json = from_file filename in
  (*String of current branch*)
  let current_branch = json |> member "config" |> member "current_branch" |> to_string in
  (*String of repo directory*)
  let repo_dir = json |> member "config" |> member "repo_dir" |> to_string in
  (*Config record with values*)
  let config = {repo_dir = repo_dir; current_branch = current_branch} in
  (*Retreiving "nodes" *)
  let json_nodes = json |> member "tree" |> member "master" |> to_list in
  (*Creating palm tree*)

  (* let palmtree = match (deserialize_helper json_nodes) with
    | ([], []) -> PalmTree
    | (lst, [])
    | ([], lst)
    | (lst1, lst2) *)


(* let serialize (palm_tree) (config) (filename:string) =

  let bindings = PalmTree.bindings palm_tree in *)
