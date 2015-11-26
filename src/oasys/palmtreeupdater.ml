open Hashtbl
open Coconuts

let oasys_dir = ".oasys/"

let gen_hash() = string_of_int (hash (Unix.gettimeofday()))

let (|+|) l1 l2 = Listops.union l1 l2

let (|-|) l1 l2 = Listops.subtract l1 l2

let to_string_commit commit =
  match commit with
  | Changes(_,_,_) -> Feedback.empty
  | Commit(id,msg) -> Feedback.to_string_commit id msg

let to_string_branch branch =
  List.fold_left (fun a x -> a ^ (to_string_commit x ^ "\n")) "" branch

let to_string_branches tree current_branch =
  PalmTree.fold
  (fun k _ a -> a ^ "\n" ^ if (k = current_branch) then "*"^k else k)
  tree ""

let get_config config =
  let repo_dir = config.repo_dir in
  let current_branch = config.current_branch in
  (repo_dir,current_branch)

let init tree config repo_dir current_branch =
  let exists = Fileio.file_exists (repo_dir ^ oasys_dir) in
  (
  match exists with
  | true ->
    (tree, config, Failure Feedback.repo_exists)
  | false ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | [] ->
      let () = Fileio.create_dir (repo_dir ^ oasys_dir) in
      let id = gen_hash() in
      let () = Fileio.create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in
      let initial_commit = Commit (id, Feedback.init_commit) in
      let added = [] in
      let removed = [] in
      let committed = [] in
      let changes = Changes(added, removed, committed) in
      let branch' = changes :: initial_commit :: branch in
      let tree' = PalmTree.add current_branch branch' tree in
      (tree', config, Success Feedback.repo_initialized)
    | _ -> assert false
    )
  )

let add tree config repo_dir current_branch file_name =
  let exists = Fileio.file_exists (repo_dir ^ file_name) in
  (
  match exists with
  | false -> (tree, config, Failure (Feedback.cannot_find file_name))
  | true ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | Changes(added, removed, committed) :: prev_commits ->
      let added' = added |+| [file_name] in
      let removed' = removed |-| [file_name] in
      let changes' = Changes(added', removed', committed) in
      let branch' = changes' :: prev_commits in
      let tree' = PalmTree.add current_branch branch' tree in
      (tree', config, Success (Feedback.file_added file_name) )
    | _ -> assert false
    )
  )

let rm tree config repo_dir current_branch file_name =
  let exists = Fileio.file_exists (repo_dir ^ file_name) in
  (
  match exists with
  | false -> (tree, config, Failure (Feedback.cannot_find file_name))
  | true ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | Changes(added, removed, committed) :: prev_commits ->
      (
      match List.mem file_name committed with
      | false -> (tree, config, Failure (Feedback.file_never_committed file_name))
      | true ->
        let added' = added |-| [file_name] in
        let removed' = removed |+| [file_name] in
        let changes' = Changes(added', removed', committed) in
        let branch' = changes' :: prev_commits in
        let tree' = PalmTree.add current_branch branch' tree in
        (tree', config, Success (Feedback.file_marked_for_removal file_name))
      )
    | _ -> assert false
    )
  )

let reset tree config repo_dir current_branch file_name =
  let exists = Fileio.file_exists (repo_dir ^ file_name) in
  (
  match exists with
  | false ->
    (tree, config, Failure (Feedback.cannot_find file_name))
  | true ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | Changes(added, removed, committed) :: prev_commits ->
      (
      match (List.mem file_name added) || (List.mem file_name removed) with
      | false -> (tree, config, Failure (Feedback.file_unchanged file_name))
      | true ->
        let added' = added |-| [file_name] in
        let removed' = removed |-| [file_name] in
        let changes' = Changes(added', removed', committed) in
        let branch' = changes' :: prev_commits in
        let tree' = PalmTree.add current_branch branch' tree in
        (tree', config, Success (Feedback.file_reset file_name))
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
    | false -> (tree, config, Failure Feedback.no_changes)
    | true ->
      let id = gen_hash() in
      let committed' = committed |-| removed in
      let committed'' = committed' |+| added in
      let commit_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
      let () = Fileio.create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in
      let () = List.iter (fun x -> Fileio.copy_file (repo_dir ^ x) commit_dir) committed in
      let commit = Commit (id, message) in
      let added' = [] in
      let removed' = [] in
      let changes = Changes(added', removed', committed'') in
      let branch' = changes :: commit :: prev_commits in
      let tree' = PalmTree.add current_branch branch' tree in
      (tree', config, Success (Feedback.changes_committed current_branch id message))
    )
  | _ -> assert false
  )

let log tree config repo_dir current_branch =
  let branch = PalmTree.find current_branch tree in
  let log_result = to_string_branch branch in
  (tree, config, Success log_result)

let get_branches tree config repo_dir current_branch =
  let result = to_string_branches tree current_branch in
  (tree, config, Success result)

let branch tree config repo_dir current_branch branch_name =
  match PalmTree.mem branch_name tree with
  | true -> (tree,config, Failure (Feedback.branch_exists branch_name))
  | false ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | Changes(added, removed, committed) :: prev_commits ->
      let branch' = Changes([], [], committed) :: prev_commits in
      let tree' = PalmTree.add branch_name branch' tree in
      (tree', config, Success (Feedback.branch_created branch_name))
    | _ -> assert false
    )

let checkout tree config repo_dir current_branch branch_name =
  match not (PalmTree.mem branch_name tree) with
  | true -> (tree,config, Failure (Feedback.branch_does_not_exist branch_name))
  | false ->
    let branch = PalmTree.find current_branch tree in
    (
    match branch with
    | Changes(added, removed, committed) :: prev_commits ->
      (
      match added, removed with
      | [],[] ->
        let config' = {config with current_branch = branch_name} in
        (tree, config', Success (Feedback.branch_checkedout branch_name))
      | _ -> (tree, config, Failure Feedback.branch_checkout_uncommitted_changes)
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
  | (BRANCH,_,[]) -> get_branches tree config repo_dir current_branch
  | (BRANCH,_,[branch_name]) -> branch tree config repo_dir current_branch branch_name
  | (CHECKOUT,_,[branch_name]) -> checkout tree config repo_dir current_branch branch_name
  | _ -> failwith "unimplemented"

let handle_request (cmd,data) = failwith "unimplemented"