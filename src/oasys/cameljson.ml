open Yojson.Basic.Util
open Yojson.Basic
open Stream

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
    Changes(added, deleted, committed)) changes_json_nodes in
  (*Tuple of commit_nodes, and change_nodes*)
  commit_nodes@change_nodes

let deserialize (filename:string) =
  (*Get json from certain file*)
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
  let palmtree = PalmTree.(empty |> add current_branch (deserialize_helper json_nodes)) in
  (*Return tuple, palmtree * config *)
  (palmtree, config)

let serialize (palm_tree) (config) (filename:string) =
  (*String of current_branch*)
  let current_branch = config.current_branch in
  (*String of repo_dir*)
  let repo_dir = config.repo_dir in
  (*Config part of json*)
  let config_json = `Assoc [("config",
    `Assoc [("repo_dir", `String repo_dir); ("current_branch", `String current_branch)])];
  (*Branch name -> node list*)
  let bindings = PalmTree.bindings palm_tree in
  (*Get node list*)
  let nodes = match (List.split bindings) with
              | (lst1, lst2) -> lst2 in
  (*Get list of nodes in json*)
  let node_json = List.map (fun x ->
    match x with
    | Commit (id, msg) -> `Assoc [("type", `String "commit"); ("id", `String id);
      ("message", `String msg)];
    | Changes (add, del, comm) -> `Assoc [("type", `String "changes");
      ("added", `List add);("deleted", `List del); ("committed", `List comm)];
  ) nodes in



  (*Stream to file*)
  stream_to_file filename ([< 'config_json; ])
