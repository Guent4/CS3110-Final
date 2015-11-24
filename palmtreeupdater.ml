open Hashtbl
open Coconuts

let oasys_dir = ".oasys/"

let gen_hash() = string_of_int (hash (Unix.gettimeofday()))

let file_exists path =
  FileUtil.find FileUtil.Exists path (fun a x -> true || a) false

let file_copy file_name target_dir =
  FileUtil.cp [file_name] target_dir

let get_config config =
  let repo_dir = config.repo_dir in
  let current_branch = config.current_branch in
  (repo_dir,current_branch)

let create_dir dir =
  FileUtil.mkdir dir

let empty_dir dir =
  FileUtil.rm ~force:FileUtil.Force ~recurse:true [(dir ^ "*")]

let to_string commits =
  match commits with
  | Changes(_,_,_) -> ""
  | Commit(id,msg) -> "====\nCommit " ^ id ^ "\n" ^ msg ^ "\n===="

let copy files source_dir target_dir =
  failwith "unimplemented"

let remove_from_list l x =
  List.filter (fun x' -> x' <> x) l

let commit_changes added removed committed =
  let committed' = List.filter (fun x -> List.mem x removed) committed in
  let committed'' = List.fold_left (fun a x -> if (not (List.mem x committed)) then x :: a else a) committed' added in
  committed''

let update_tree (cmd:cmd_expr) (tree:palm_tree) (config:config) :palm_tree * config * feedback =
  (* get repo_dir and branch_current from config *)
  let (repo_dir, current_branch) = get_config config in
  (* determine which command *)
  match cmd with
  | (_,INIT,_,_) ->
    (* check if .oasys already exists *)
    let exists = file_exists (repo_dir ^ oasys_dir) in
    (
      match exists with
      | true ->
        (* fail over *)
        (tree, config, Failure "repository already exists")
      | false ->
        (* get current branch *)
        let branch = PalmTree.find current_branch tree in
        (
          (* assert invariant *)
          match branch with
          | [] ->

            (* ==== FILE IO ==== *)

            (* create empty dir .oasys *)
            let () = create_dir (repo_dir ^ oasys_dir) in
            (* create hash for id *)
            let id = gen_hash() in
            (* create directory for initial commit *)
            let () = create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in

            (* ==== TREE MANAGEMENT ==== *)

            (* initial commit node *)
            let initial_commit = Commit (id, "initial commit") in
            (* build node *)
            let added = [] in
            let removed = [] in
            let committed = [] in
            let changes = Changes(added, removed, committed) in
            (* build branch *)
            let branch' = changes :: initial_commit :: branch in
            (* update tree *)
            let tree' = PalmTree.add current_branch branch' tree in
            (* return *)
            (tree', config, Success "repository initialized")

          | _ -> assert false
        )
    )
  | (_,ADD,_,[file_name]) ->
    (* check if file exists *)
    let exists = file_exists (repo_dir ^ file_name) in
    (
      match exists with
      | false ->
        (* fail over *)
        (tree, config, Failure (file_name ^ " did not match any files"))
      | true ->
        (* get current branch *)
        let branch = PalmTree.find current_branch tree in
        (
          (* assert invariant *)
          match branch with
          | Changes(added, removed, committed) :: prev_commits ->

            (* ==== FILE IO ==== *)

            (* no file io needed *)

            (* ==== TREE MANAGEMENT ==== *)

            (
              match (not (List.mem file_name added)) with
              | false -> (tree, config, Failure "")
              | true ->
                (* build node *)
                let added' = file_name :: added in
                let changes' = Changes(added', removed, committed) in
                (* build branch *)
                let branch' = changes' :: prev_commits in
                (* update tree *)
                let tree' = PalmTree.add current_branch branch' tree in
                (* return *)
                (tree', config, Success "")
            )

          | _ -> assert false
        )
    )
  | (_,REMOVE,_,[file_name]) ->
    (* check if file exists *)
    let exists = file_exists (repo_dir ^ file_name) in
    (
      match exists with
      | false ->
        (* fail over *)
        (tree, config, Failure (file_name ^ " did not match any files"))
      | true ->
        (* get current branch *)
        let branch = PalmTree.find current_branch tree in
        (
          (* assert invariant *)
          match branch with
          | Changes(added, removed, committed) :: prev_commits ->

            (* ==== FILE IO ==== *)

            (* no file io needed *)

            (* ==== TREE MANAGEMENT ==== *)

            (
              match (List.mem file_name committed && List.mem file_name added && not (List.mem file_name removed)) with
              | false -> (tree, config, Failure "")
              | true ->
                (* build node *)
                let removed' = file_name :: removed in
                let changes' = Changes(added, removed', committed) in
                (* build branch *)
                let branch' = changes' :: prev_commits in
                (* update tree *)
                let tree' = PalmTree.add current_branch branch' tree in
                (* return *)
                (tree', config, Success "")
            )
          | _ -> assert false
        )
    )
  | (_,RESET,_,[file_name]) ->
    (* check if file exists *)
    let exists = file_exists (repo_dir ^ file_name) in
    (
      match exists with
      | false ->
        (* fail over *)
        (tree, config, Failure (file_name ^ " did not match any files"))
      | true ->
        (* get current branch *)
        let branch = PalmTree.find current_branch tree in
        (
          (* assert invariant *)
          match branch with
          | Changes(added, removed, committed) :: prev_commits ->

            (* ==== FILE IO ==== *)

            (* no file io needed *)

            (* ==== TREE MANAGEMENT ==== *)

            (
              match (List.mem file_name added) with
              | false -> (tree, config, Failure "")
              | true ->
                (* build node *)
                let added' = remove_from_list added file_name in
                let changes' = Changes(added', removed, committed) in
                (* build branch *)
                let branch' = changes' :: prev_commits in
                (* update tree *)
                let tree' = PalmTree.add current_branch branch' tree in
                (* return *)
                (tree', config, Success "")
            )
          | _ -> assert false
        )
    )
  | (_,COMMIT,_,[message]) ->
    (* get branch *)
    let branch = PalmTree.find current_branch tree in
    (* assert branch invariant *)
    (
      match branch with
      | Changes(added, removed, committed) :: prev_commits ->
        (* ensure that ether files have been added or removed *)
        let can_commit = max (List.length added) (List.length removed) > 0 in
        (
          match can_commit with
          | false -> (tree, config, Failure "")
          | true ->

            (* ==== PREPARATION ==== *)

            (* gen hash *)
            let id = gen_hash() in
            (* committed = committed - removed + added *)
            let committed = commit_changes added removed committed in
            let added = [] in
            let removed = [] in

            (* ==== FILE IO ==== *)

            (* create directory with id *)
            let commit_dir = (id ^ "/") in
            let () = create_dir commit_dir in
            (* copy files from added to oasys/id/ *)
            let () = List.iter (fun x -> file_copy x (oasys_dir ^ commit_dir)) committed in
            (* copy files from commited to oasys/id/ *)

            (* ==== TREE MANAGEMENT ==== *)

            (* commit node *)
            let commit = Commit (id, message) in
            (* build node *)
            let changes = Changes(added, removed, committed) in
            (* build branch *)
            let branch' = changes :: commit :: branch in
            (* update tree *)
            let tree' = PalmTree.add current_branch branch' tree in
            (* return *)
            (tree', config, Success "repository initialized")

        )
      | _ -> assert false
    )
  | (_,LOG,_,_) ->
    (* get current branch *)
    let branch = PalmTree.find current_branch tree in
    (* iter through branch and add to_string of each node *)
    let log_result = List.fold_left (fun a x -> a ^ (to_string x ^ "\n")) "" branch in
    (tree, config, Success log_result)
  | (_,STATUS,_,_) -> failwith "unimplemented"
  | _ -> failwith "unimplemented"