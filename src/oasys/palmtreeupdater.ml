open Coconuts

let oasys_dir = ".oasys/"

let gen_hash state =
  let hash = Hashtbl.hash state in
  let () = Random.init hash in
  let gen() =
    match Random.int(16) with
    | n when n > 9  -> int_of_char 'a' + (n - 10)
    | n             -> int_of_char '0' + n
  in
  let gen _ = String.make 1 (char_of_int(gen())) in
  String.concat "" (Array.to_list (Array.init 40 gen))

let (|+|) l1 l2 = Listops.union l1 l2

let (|-|) l1 l2 = Listops.subtract l1 l2

let to_string_commit commit =
  let (id,msg,_) = commit in
  Feedback.to_string_commit id msg

let to_string_branch branch =
  List.fold_left (fun a x -> a ^ (to_string_commit x ^ "\n")) "" branch

let to_string_branches tree current_branch =
  CommitTree.fold
  (fun k _ a -> a ^ "\n" ^ if (k = current_branch) then "*"^k else k)
  tree ""

let get_config config =
  let repo_dir = config.repo_dir in
  let current_branch = config.current_branch in
  (repo_dir,current_branch)

let ignore_file repo_dir file_name =
  (
    let regexp = Str.regexp (repo_dir ^ oasys_dir) in
    (Str.string_match regexp file_name 0) || (Str.string_match regexp (file_name^"/") 0)
  )
  ||
  (
    repo_dir = (file_name ^ "/")
  )

let get_work_dir repo_dir =
  let files = FileUtil.find FileUtil.True repo_dir (fun x y -> y :: x) [] in
  let files = List.filter (fun x -> not (ignore_file repo_dir x)) files in
  files

let get_files file_pattern files =
  let regexp = Str.regexp file_pattern in
  List.filter (fun x -> Str.string_match regexp x 0) files

let init tree config repo_dir current_branch =
  let work_dir = get_work_dir repo_dir in
  match Fileio.file_exists (repo_dir ^ oasys_dir) with
  | true ->
    let tree = {tree with work_dir=work_dir} in
    (tree, config, Failure Feedback.repo_exists)
  | false ->
    let commit_tree = tree.commit_tree in
    let branch = CommitTree.find current_branch commit_tree in
    (
    match branch with
    | [] ->
      let () = Fileio.create_dir (repo_dir ^ oasys_dir) in
      let id = gen_hash (tree,config) in
      let () = Fileio.create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in
      let head = (id,[]) in
      let index = ([],[]) in
      let branch = [(id, "initial commit", [])] in
      let commit_tree = CommitTree.add current_branch branch commit_tree in
      let tree = {
        head=head;
        index=index;
        work_dir=work_dir;
        commit_tree=commit_tree
      } in
      (tree,config,Success (Feedback.repo_initialized (repo_dir ^ oasys_dir)))
    | _ -> (tree,config,Failure "fatal: Not an oasys repository: .oasys")
    )

let add tree config repo_dir current_branch file_name =
  let work_dir = get_work_dir repo_dir in
  match List.mem file_name work_dir with
  | false ->
    let tree = {tree with work_dir=work_dir} in
    (tree, config, Failure (Feedback.cannot_find file_name))
  | true ->
    let (added,removed) = tree.index in
    let added = added |+| [file_name] in
    let removed = removed |-| [file_name] in
    let tree = {
      head=tree.head;
      index=(added,removed);
      work_dir=work_dir;
      commit_tree=tree.commit_tree
    } in
    (tree, config, Success (Feedback.file_added file_name) )

let rm_file tree config repo_dir current_branch file_name =
  let work_dir = get_work_dir repo_dir in
  match List.mem file_name work_dir with
  | false ->
    let tree = {tree with work_dir=work_dir} in
    (tree, config, Failure (Feedback.cannot_find file_name))
  | true ->
    let (added,removed) = tree.index in
    let added = added |-| [file_name] in
    let removed = removed |+| [file_name] in
    let tree = {
      head=tree.head;
      index=(added,removed);
      work_dir=work_dir;
      commit_tree=tree.commit_tree
    } in
    (tree, config, Success (Feedback.file_added file_name) )

let rm_branch tree config repo_dir current_branch branch_name =
  let work_dir = get_work_dir repo_dir in
  match current_branch = branch_name with
  | true ->
    let tree = {tree with work_dir = work_dir} in
    (tree, config, Failure "Cannot remove the branch you are on.")
  | false ->
    let commit_tree = tree.commit_tree in
    (
    match CommitTree.mem branch_name commit_tree with
    | false ->
      let tree = {tree with work_dir=work_dir} in
      (tree, config, Failure (Feedback.branch_does_not_exist branch_name))
    | true ->
      let commit_tree = tree.commit_tree in
      let commit_tree = CommitTree.remove branch_name commit_tree in
      let tree = {
        head=tree.head;
        index=tree.index;
        work_dir=work_dir;
        commit_tree=commit_tree
      } in
      (tree, config, Success (branch_name ^ " has been removed"))
    )

let reset_file tree config repo_dir current_branch file_name =
  let work_dir = get_work_dir repo_dir in
  match List.mem file_name work_dir with
  | false ->
    let tree = {tree with work_dir=work_dir} in
    (tree, config, Failure (Feedback.cannot_find file_name))
  | true ->
    let (added,removed) = tree.index in
    let added = added |-| [file_name] in
    let removed = removed |-| [file_name] in
    let tree = {
      head=tree.head;
      index=(added,removed);
      work_dir=work_dir;
      commit_tree=tree.commit_tree
    } in
    (tree, config, Success (Feedback.file_added file_name) )

let commit tree config repo_dir current_branch msg =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  let branch = CommitTree.find current_branch commit_tree in
  match branch with
  | (_,_,committed) :: prev_commits ->
    (
    let (added, removed) = tree.index in
    match max (List.length added) (List.length removed) > 0 with
    | false ->
      let tree = {tree with work_dir=work_dir} in
      (tree, config, Failure Feedback.no_changes)
    | true ->
      let id = gen_hash (tree,config) in
      let committed = (committed |-| removed) |+| added in
      let commit_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
      let () = Fileio.create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in
      let () =
        List.iter
        (fun x -> Fileio.copy_file x commit_dir)
        committed
      in
      let head = (id,committed) in
      let index = (added,removed) in
      let branch = (id,msg,committed) :: branch in
      let commit_tree = CommitTree.add current_branch branch commit_tree in
      let tree = {
        head=head;
        index=index;
        work_dir=work_dir;
        commit_tree=commit_tree
      } in
      (tree, config, Success (Feedback.changes_committed current_branch id msg))
    )
  | _ -> (tree,config,Failure Feedback.no_repo)

let log tree config repo_dir current_branch =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  let branch = CommitTree.find current_branch commit_tree in
  let log_result = to_string_branch branch in
  let tree = {tree with work_dir=work_dir} in
  (tree, config, Success log_result)

let get_branches tree config repo_dir current_branch =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  let result = to_string_branches commit_tree current_branch in
  let tree = {tree with work_dir=work_dir} in
  (tree, config, Success result)

let branch tree config repo_dir current_branch branch_name =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  match CommitTree.mem branch_name commit_tree with
  | true ->
    let tree = {tree with work_dir=work_dir} in
    (tree,config, Failure (Feedback.branch_exists branch_name))
  | false ->
    let branch = CommitTree.find current_branch commit_tree in
    let commit_tree = CommitTree.add branch_name branch commit_tree in
    let tree = {
      head= tree.head;
      index= tree.index;
      work_dir= work_dir;
      commit_tree= commit_tree
    } in
    (tree,config, Success (Feedback.branch_created branch_name))

let checkout tree config repo_dir current_branch branch_name =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  match (CommitTree.mem branch_name commit_tree) with
  | false ->
    let tree = {tree with work_dir=work_dir} in
    (tree,config, Failure (Feedback.branch_does_not_exist branch_name))
  | true ->
    let branch = CommitTree.find current_branch commit_tree in
    (
    match branch with
    | (id,_,commits) :: prev_commits ->
      let config = {config with current_branch=branch_name} in
      let tree = {
        head= (id,commits);
        index= tree.index;
        work_dir= work_dir;
        commit_tree= commit_tree
      } in
      (tree,config, Success ("Switched to branch \'" ^ branch_name ^ "\'"))
    | _ -> (tree,config,Failure "fatal: Not an oasys repository: .oasys")
    )


let file_batch_op op tree config repo_dir current_branch files =
  let files' =
    List.fold_left
    (fun a x -> a |+| (get_files x tree.work_dir))
    []
    files
  in
  match files' with
  | [] ->
  let path_spec = Listops.to_string files "" " " "" in
  (tree,config,Failure (Feedback.cannot_find path_spec))
  | files ->
  let f t c = op t c repo_dir current_branch in
  let result =
    List.fold_left
    (fun a x -> let (t,c,fbs) = a in let (t,c,fb) = f t c x in (t,c,fb::fbs))
    (tree, config, []) (* accum *)
    files (* target *)
  in
  let (tree', config', feedbacks) = result in
  let feedbacks = List.rev feedbacks in
  let feedback =
    List.fold_left
    (fun a x ->
      match x with
      | Success y -> a ^ y ^ "\n"
      | Failure z -> a ^ z ^ "\n")
    ""
    feedbacks
  in
  (tree',config',Success feedback)

(* let rec find_branch branch hash =
  match branch with
  | Changes(_,_) :: commits -> find_branch commits hash
  | Commit(id,msg,_) :: prev_commits ->
    (
      match id = hash with
      | true -> Some branch
      | false -> find_branch prev_commits hash
    )
  | _ -> None *)


let reset_branch tree config repo_dir current_branch hash = failwith "not implemented"
  (* let branch = PalmTree.find current_branch tree in
  match hash with
  | "HEAD" ->
    (
    match branch with
    | Changes(_,_) :: Commit (id,msg,committed) :: prev_commits ->
      let commit_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
      let () =
        List.iter
        (fun x -> Fileio.copy_file (commit_dir ^ x) repo_dir)
        committed
      in
      let added = [] in
      let removed = [] in
      let branch' =
        Changes(added,removed) ::
        Commit(id,msg,committed) ::
        prev_commits
      in
      let tree' = PalmTree.add current_branch branch' tree in
      (tree', config, Success ("Branch has been successfully reset to " ^ hash))
    | _ -> assert false
    )
  | _ ->
    (
    match find_branch branch hash with
    | None -> (tree,config,Failure "No such commit exists in branch")
    | Some (Commit(id,msg,committed) :: prev_commits) ->
      let commit_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
      let () =
        List.iter
        (fun x -> Fileio.copy_file (commit_dir ^ x) repo_dir)
        committed
      in
      let added = [] in
      let removed = [] in
      let branch' =
        Changes(added,removed) ::
        Commit(id,msg,committed) ::
        prev_commits
      in
      let tree' = PalmTree.add current_branch branch' tree in
      (tree', config, Success ("Branch has been successfully reset to " ^ hash))
    | _ -> assert false
    ) *)

let status tree config repo_dir current_branch =
  let work_dir = get_work_dir repo_dir in
  let branch = CommitTree.find current_branch tree.commit_tree in
  match branch with
  | (_,msg,_) :: prev_commits ->
    let (added,removed) = tree.index in
    let feedback = "On branch " ^ current_branch ^ "\n\n" ^ msg ^ "\n\n" ^
    (
      if (List.length added > 0) then
      (
        "Changes to be committed:\n" ^
        (Listops.to_string (added) "\t" "\n\t" "\n\n" )
      )
      else
      ("Nothing to commit\n" )
    )
    ^
    (
      if (List.length (work_dir |-| added) > 0) then
      (
        "Untracked files:\n" ^ (Listops.to_string (work_dir |-| added) "\t" "\n\t" "\n")
      )
      else
      ("Working directory clean" )
    )
    in
    let tree = {tree with work_dir = work_dir} in
    (tree,config,Success feedback)
  | _ -> (tree,config,Failure "fatal: Not an oasys repository: .oasys")


let update_tree (cmd:cmd_expr) (tree:palm_tree) (config:config) :palm_tree * config * feedback =
  let (repo_dir, current_branch) = get_config config in
  match cmd with
  | (INIT,[EMPTY],[]) -> init tree config repo_dir current_branch
  | (ADD,[EMPTY],files) -> file_batch_op add tree config repo_dir current_branch files
  | (ADD,[ALL],[]) -> file_batch_op add tree config repo_dir current_branch tree.work_dir
  | (RM,[FILE],files) -> file_batch_op rm_file tree config repo_dir current_branch files
  | (RM,[BNCH],[branch_name]) -> rm_branch tree config repo_dir current_branch branch_name
  | (RESET,[FILE],files) -> file_batch_op reset_file tree config repo_dir current_branch files
  | (RESET,[BNCH],[hash]) -> reset_branch tree config repo_dir current_branch hash
  | (COMMIT,[MSG],[message]) -> commit tree config repo_dir current_branch message
  | (LOG,[EMPTY],[]) -> log tree config repo_dir current_branch
  | (BRANCH,[EMPTY],[]) -> get_branches tree config repo_dir current_branch
  | (BRANCH,[EMPTY],[branch_name]) -> branch tree config repo_dir current_branch branch_name
  | (CHECKOUT,[EMPTY],[branch_name]) -> checkout tree config repo_dir current_branch branch_name
  | (STATUS,[EMPTY],[]) -> status tree config repo_dir current_branch
  | (HELP,_,_) -> (tree, config, Success Feedback.empty)
  | _ -> (tree,config,Failure Feedback.no_support)

let handle_request (cmd,data) = failwith "unimplemented"