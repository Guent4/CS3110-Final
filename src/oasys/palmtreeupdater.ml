open Hashtbl
open Coconuts

let oasys_dir = ".oasys/"

let gen_hash() = string_of_int (hash (Unix.gettimeofday()))

let get_config config =
  let repo_dir = config.repo_dir in
  let current_branch = config.current_branch in
  (repo_dir,current_branch)

let commit_changes added removed committed =
  let committed' = Listops.subtract committed removed in
  let committed'' = Listops.union committed' added in
  let added' = [] in
  let removed' = [] in
  (added',removed',committed'')

let to_string commits =
  match commits with
  | Changes(_,_,_) -> ""
  | Commit(id,msg) -> "====\nCommit " ^ id ^ "\n" ^ msg ^ "\n===="

let handle_request (cmd,data) = failwith "unimplemented"

let init tree config repo_dir current_branch =
  let exists = Fileio.file_exists (repo_dir ^ oasys_dir) in
  (
  match exists with
  | true ->
    (tree, config, Failure "repository already exists")
  | false ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | [] ->
      let () = Fileio.create_dir (repo_dir ^ oasys_dir) in
      let id = gen_hash() in
      let () = Fileio.create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in
      let initial_commit = Commit (id, "initial commit") in
      let added = [] in
      let removed = [] in
      let committed = [] in
      let changes = Changes(added, removed, committed) in
      let branch' = changes :: initial_commit :: branch in
      let tree' = PalmTree.add current_branch branch' tree in
      (tree', config, Success "repository initialized")
    | _ -> assert false
    )
  )

let add tree config repo_dir current_branch file_name =
  let exists = Fileio.file_exists (repo_dir ^ file_name) in
  (
  match exists with
  | false -> (tree, config, Failure (file_name ^ " did not match any files"))
  | true ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | Changes(added, removed, committed) :: prev_commits ->
      let added' = Listops.union added [file_name] in
      let removed' = Listops.subtract removed [file_name] in
      let changes' = Changes(added', removed', committed) in
      let branch' = changes' :: prev_commits in
      let tree' = PalmTree.add current_branch branch' tree in
      (tree', config, Success (file_name ^ " added") )
    | _ -> assert false
    )
  )

let rm tree config repo_dir current_branch file_name =
  let exists = Fileio.file_exists (repo_dir ^ file_name) in
  (
  match exists with
  | false -> (tree, config, Failure (file_name ^ " did not match any files"))
  | true ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | Changes(added, removed, committed) :: prev_commits ->
      (
      match List.mem file_name committed with
      | false -> (tree, config, Failure (file_name ^ " has never been committed"))
      | true ->
        let added' = Listops.subtract added [file_name] in
        let removed' = Listops.union removed [file_name] in
        let changes' = Changes(added', removed', committed) in
        let branch' = changes' :: prev_commits in
        let tree' = PalmTree.add current_branch branch' tree in
        (tree', config, Success (file_name ^ " marked for removal"))
      )
    | _ -> assert false
    )
  )

let reset tree config repo_dir current_branch file_name =
  let exists = Fileio.file_exists (repo_dir ^ file_name) in
  (
  match exists with
  | false ->
    (tree, config, Failure (file_name ^ " did not match any files"))
  | true ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | Changes(added, removed, committed) :: prev_commits ->
      (
      match (List.mem file_name added) with
      | false -> (tree, config, Failure "")
      | true ->
        let added' = Listops.subtract added [file_name] in
        let removed' = Listops.subtract removed [file_name] in
        let changes' = Changes(added', removed', committed) in
        let branch' = changes' :: prev_commits in
        let tree' = PalmTree.add current_branch branch' tree in
        (tree', config, Success (file_name ^ " has been reset"))
      )
    | _ -> assert false
    )
  )

let commit tree config repo_dir current_branch message =
  let branch = PalmTree.find current_branch tree in
  (
  match branch with
  | Changes(added, removed, committed) :: prev_commits ->
    let can_commit = max (List.length added) (List.length removed) > 0 in
    (
    match can_commit with
    | false -> (tree, config, Failure "no files added or marked for removal")
    | true ->
      let id = gen_hash() in
      let (added, removed, committed) = commit_changes added removed committed in
      let commit_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
      let () = Fileio.create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in
      let () = List.iter (fun x -> Fileio.copy_file (repo_dir ^ x) commit_dir) committed in
      let commit = Commit (id, message) in
      let changes = Changes(added, removed, committed) in
      let branch' = changes :: commit :: prev_commits in
      let tree' = PalmTree.add current_branch branch' tree in
      (tree', config, Success ("committed: " ^ current_branch ^ " " ^ id ^ "\nmessage: " ^ message))
    )
  | _ -> assert false
  )

let log tree config repo_dir current_branch =
  let branch = PalmTree.find current_branch tree in
  let log_result = List.fold_left (fun a x -> a ^ (to_string x ^ "\n")) "" branch in
  (tree, config, Success log_result)

let to_string_branches tree config repo_dir current_branch =
  let result =
    PalmTree.fold
    (fun k _ a -> a ^ "\n" ^ if (k = current_branch) then "*"^k else k)
    tree ""
  in
  (tree, config, Success result)

let branch tree config repo_dir current_branch branch_name =
  let branch = PalmTree.find current_branch tree in
  (
  match branch with
  | Changes(added, removed, committed) :: prev_commits ->
    let branch' = Changes([], [], committed) :: prev_commits in
    let tree' = PalmTree.add branch_name branch' tree in
    (tree', config, Success ("created branch " ^ branch_name))
  | _ -> assert false
  )

let checkout tree config repo_dir current_branch branch_name =
  let branch = PalmTree.find current_branch tree in
  (
  match branch with
  | Changes(added, removed, committed) :: prev_commits ->
    (
      match added, removed with
      | [],[] ->
        let branch' = Changes([], [], committed) :: prev_commits in
        let tree' = PalmTree.add branch_name branch' tree in
        let config' = {config with current_branch = branch_name} in
        (tree', config', Success ("checked out branch " ^ branch_name))
      | _ -> (tree, config, Failure "cannot checkout a new branch until all changes have been committed")
    )
  | _ -> assert false
  )

let update_tree (cmd:cmd_expr) (tree:palm_tree) (config:config) :palm_tree * config * feedback =
  let (repo_dir, current_branch) = get_config config in
  match cmd with
  | (INIT,_,_) -> init tree config repo_dir current_branch
  | (ADD,_,[file_name]) -> add tree config repo_dir current_branch file_name
  | (RM,_,[file_name]) -> rm tree config repo_dir current_branch file_name
  | (RESET,_,[file_name]) -> reset tree config repo_dir current_branch file_name
  | (COMMIT,_,[message]) -> commit tree config repo_dir current_branch message
  | (LOG,_,_) -> log tree config repo_dir current_branch
  | (BRANCH,_,[]) -> to_string_branches tree config repo_dir current_branch
  | (BRANCH,_,[branch_name]) -> branch tree config repo_dir current_branch branch_name
  | (CHECKOUT,_,[branch_name]) -> checkout tree config repo_dir current_branch branch_name
  | _ -> failwith "unimplemented"
