open Yojson.Basic.Util
open Coconuts

let serialize_string s =
  `String s

let serialize_string_list l =
  `List (List.rev (List.fold_left (fun a x -> serialize_string x :: a) [] l))

let serialize_commit commit =
  let (id,msg,committed) = commit in
  `Assoc
  [
  ("id", `String id);
  ("msg", `String msg);
  ("committed", serialize_string_list committed)
  ]

let serialize_branch branch =
  `List (List.rev (List.fold_left (fun a x -> serialize_commit x :: a) [] branch))

let serialize_index index =
  let (added,removed) = index in
  `Assoc
  [
  ("added", serialize_string_list added);
  ("removed", serialize_string_list removed)
  ]

let serialize_commit_tree commit_tree =
  `Assoc (CommitTree.fold (fun k v a -> (k, serialize_branch v) :: a) commit_tree [])

let serialize_palm_tree palm_tree =
  `Assoc
  [
  ("head", serialize_commit palm_tree.head);
  ("index", serialize_index palm_tree.index);
  ("work_dir", serialize_string_list palm_tree.work_dir);
  ("commit_tree", serialize_commit_tree palm_tree.commit_tree)
  ]

let serialize_config config =
  `Assoc
  [("repo_dir", serialize_string config.repo_dir);
  ("current_branch", serialize_string config.current_branch)]

let serialize_state state =
  `Assoc
  [("config", serialize_config state.config);
  ("tree", serialize_palm_tree state.tree)]

let deserialize_string s =
  (s |> to_string)

let deserialize_string_list (l:Yojson.Basic.json) =
  let l = (l |> to_list) in
  List.rev (List.fold_left (fun a x -> deserialize_string x :: a) [] l)

let deserialize_commit (node:Yojson.Basic.json) =
  let id = (node |> member "id") in
  let msg = (node |> member "msg") in
  let committed = (node |> member "committed") in
  (
    deserialize_string id,
    deserialize_string msg,
    deserialize_string_list committed
  )

let deserialize_branch branch =
  let branch = (branch |> to_list) in
  List.rev (List.fold_left (fun a x -> deserialize_commit x :: a) [] branch)

let deserialize_index index =
  match index with
  | `Assoc x ->
    (
      deserialize_string_list (List.assoc "added" x),
      deserialize_string_list (List.assoc "removed" x)
    )
  | _ -> assert false

let deserialize_commit_tree commit_tree =
  match commit_tree with
  | `Assoc x ->
    List.fold_left (fun a (k,v) -> CommitTree.add k (deserialize_branch v) a) CommitTree.empty x
  | _ -> assert false

let deserialize_palm_tree palm_tree =
  match palm_tree with
  | `Assoc x ->
    {
      head= deserialize_commit (List.assoc "head" x);
      index= deserialize_index (List.assoc "index" x);
      work_dir= deserialize_string_list (List.assoc "work_dir" x);
      commit_tree= deserialize_commit_tree (List.assoc "commit_tree" x)
    }
  | _ -> assert false

let deserialize_config config =
  {repo_dir=(config |> member "repo_dir" |> to_string);
  current_branch=(config |> member "current_branch" |> to_string)}

let deserialize_state state =
  {config= deserialize_config (state |> member "config");
  tree= deserialize_palm_tree (state |> member "tree")}

let serialize state filename =
  let state_json = serialize_state state in
  let state_str = Yojson.Basic.pretty_to_string state_json in
  let () = Fileio.write_str filename state_str in
  ()

let deserialize filename =
  let json = Yojson.Basic.from_file filename in
  let state = deserialize_state json in
  state
