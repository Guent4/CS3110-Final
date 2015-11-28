open Yojson.Basic.Util
open Coconuts

let serialize_string s =
  `String s

let serialize_string_list l =
  `List (List.rev (List.fold_left (fun a x -> serialize_string x :: a) [] l))

let serialize_node node =
  match node with
  | Commit (id, msg) ->
    `Assoc
    [("type", `String "Commit");
    ("id", `String id);
    ("msg", `String msg)]
  | Changes (added, removed, committed) ->
    `Assoc
    [("type", `String "Changes");
    ("added", serialize_string_list added);
    ("removed", serialize_string_list removed);
    ("committed", serialize_string_list committed)]

let serialize_branch branch =
  `List (List.rev (List.fold_left (fun a x -> serialize_node x :: a) [] branch))

let serialize_tree tree =
  `Assoc (PalmTree.fold (fun k v a -> (k, serialize_branch v) :: a) tree [])

let serialize_config config =
  `Assoc
  [("repo_dir", serialize_string config.repo_dir);
  ("current_branch", serialize_string config.current_branch)]

let serialize_state state =
  `Assoc
  [("config", serialize_config state.config);
  ("tree", serialize_tree state.tree)]

let deserialize_string s =
  (s |> to_string)

let deserialize_string_list (l:Yojson.Basic.json) =
  let l = (l |> to_list) in
  List.rev (List.fold_left (fun a x -> deserialize_string x :: a) [] l)

let deserialize_node (node:Yojson.Basic.json) =
  match (node |> member "type" |> to_string) with
  | "Commit" ->
    let id = (node |> member "id") in
    let msg = (node |> member "msg") in
    Commit
    (
      deserialize_string id,
      deserialize_string msg
    )
  | "Changes" ->
    let added = (node |> member "added") in
    let removed = (node |> member "removed") in
    let committed = (node |> member "committed") in
    Changes
    (
      deserialize_string_list added,
      deserialize_string_list removed,
      deserialize_string_list committed
    )
  | _ -> assert false

let deserialize_branch branch =
  let branch = (branch |> to_list) in
  List.rev (List.fold_left (fun a x -> deserialize_node x :: a) [] branch)

let deserialize_tree tree =
  match tree with
  | `Assoc x ->
    List.fold_left (fun a (k,v) -> PalmTree.add k (deserialize_branch v) a) PalmTree.empty x
  | _ -> assert false

let deserialize_config config =
  {repo_dir=(config |> member "repo_dir" |> to_string);
  current_branch=(config |> member "current_branch" |> to_string)}

let deserialize_state state =
  {config= deserialize_config (state |> member "config");
  tree= deserialize_tree (state |> member "tree")}

let deserialize filename =
  let json = Yojson.Basic.from_file filename in
  let state = deserialize_state json in
  state

let serialize state filename =
  let state_json = serialize_state state in
  let state_str = Yojson.Basic.pretty_to_string state_json in
  Fileio.write_str filename state_str
