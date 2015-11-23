open Hashtbl
open Coconuts

let vcs_dir = ".oasys/"

let gen_hash() = string_of_int (hash (Unix.gettimeofday()))

let file_exists path =
  FileUtil.find FileUtil.Exists path (fun a x -> true || a) false

let get_config config =
  let repo_dir = config.repo_dir in
  let branch_alias = config.current_branch in
  (repo_dir,branch_alias)

let update_tree (cmd:cmd_expr) (tree:palm_tree) (config:config) :palm_tree * config * feedback =
  (* get repo_dir and branch_alias from config *)
  let (repo_dir, branch_alias) = get_config config in
  (* determine which command *)
  match cmd with
  | (_,INIT,_,_) ->
    (* check if .oasys already exists *)
    let exists = file_exists (repo_dir ^ vcs_dir) in
    (
      match exists with
      | true ->
        (* fail over *)
        (tree, config, Failure "repository already exists")
      | false ->
        (* create empty dir .oasys *)
        FileUtil.mkdir (repo_dir ^ vcs_dir);
        (* create initial commit in tree *)
        let branch' = [Commit (gen_hash(), "initial commit")] in
        (* update tree with new branch *)
        let tree' = PalmTree.add branch_alias branch' tree in
        (* return *)
        (tree', config, Success "repository initialized")
    )
  | _ -> failwith "unimplemented"