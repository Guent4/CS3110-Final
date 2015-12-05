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

let (|=|) l1 l2 = Listops.equal l1 l2

let to_string_commit commit =
  let (id,msg,_) = commit in
  Feedback.to_string_commit id msg

let to_string_branch branch =
  List.fold_left (fun a x -> a ^ (to_string_commit x ^ "\n")) "" branch

let to_string_branches tree current_branch =
  CommitTree.fold
  (fun k _ a -> a ^
      if (k = current_branch) then "<green>* "^k^"</green>\n"
      else "  "^k^"\n")
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

let abbrev_files repo_dir files =
  List.fold_left
  (fun a x ->
    (String.sub x
      (String.length repo_dir) (String.length x - String.length repo_dir)) :: a)
  []
  files

let copy_over_files repo_dir committed source_dir target_dir =
  List.iter
  (fun x -> Fileio.copy_file source_dir x target_dir)
  (abbrev_files repo_dir committed)

let rec find_commit hash = function
 | [] -> None
 | (id,msg,committed)::prev_commits ->
  (
    match id = hash with
    | false -> find_commit hash prev_commits
    | true -> Some ( (id,msg,committed), prev_commits )
  )

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
      let head = (id, Feedback.init_commit, []) in
      let index = ([],[]) in
      let branch = [head] in
      let commit_tree = CommitTree.add current_branch branch commit_tree in
      let tree = {
        head=head;
        index=index;
        work_dir=work_dir;
        commit_tree=commit_tree
      } in
      (tree,config,Success (Feedback.repo_initialized (repo_dir ^ oasys_dir)))
    | _ -> (tree,config,Failure Feedback.no_repo)
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
    let () = Fileio.remove_file file_name in
    let (added,removed) = tree.index in
    let added = added |-| [file_name] in
    let removed = removed |+| [file_name] in
    let tree = {
      head=tree.head;
      index=(added,removed);
      work_dir=work_dir;
      commit_tree=tree.commit_tree
    } in
    (tree, config, Success (Feedback.file_removed file_name) )

let rm_file_cached tree config repo_dir current_branch file_name =
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
    (tree, config, Success (Feedback.file_removed file_name) )

let rm_branch tree config repo_dir current_branch branch_name =
  let work_dir = get_work_dir repo_dir in
  match current_branch = branch_name with
  | true ->
    let tree = {tree with work_dir = work_dir} in
    (tree, config, Failure "Cannot remove the branch you are on.")
  | false ->
    (
    match branch_name = "master" with
    | true ->
      let tree = {tree with work_dir = work_dir} in
      (tree, config, Failure "Cannot remove master branch.")
    | false ->
      let commit_tree = tree.commit_tree in
      (match CommitTree.mem branch_name commit_tree with
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
        (tree, config, Success (Feedback.branch_removed branch_name))
      )
    )

let reset_file tree config repo_dir current_branch file_name =
  let work_dir = get_work_dir repo_dir in
  let (id,_,committed) = tree.head in
  match List.mem file_name work_dir with
  | false ->
    let tree = {tree with work_dir=work_dir} in
    (tree, config, Failure (Feedback.cannot_find file_name))
  | true ->
    (match List.mem file_name committed with
    | false ->
      let tree = {tree with work_dir=work_dir} in
      (tree, config, Failure ("ambiguous argument \'"^file_name^"\': unknown revision or path not in the working tree."))
    | true ->
      let (added,removed) = tree.index in
      let added = added |-| [file_name] in
      let removed = removed |-| [file_name] in
      let source_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
      let target_dir = repo_dir in
      let () = copy_over_files repo_dir [file_name] source_dir target_dir in
      let tree = {
        head=tree.head;
        index=(added,removed);
        work_dir=work_dir;
        commit_tree=tree.commit_tree
      } in
      (tree, config, Success (Feedback.file_reset file_name) ))

let commit tree config repo_dir current_branch msg =
  let work_dir = get_work_dir repo_dir in
  let (id,_,_) = tree.head in
  let commit_tree = tree.commit_tree in
  let branch = CommitTree.find current_branch commit_tree in
  match find_commit id branch with
  | None -> failwith ""
  | Some ((id',msg',committed'),prev_commits) ->
    (
    let (added, removed) = tree.index in
    match max (List.length added) (List.length removed) > 0 with
    | false ->
      let tree = {tree with work_dir=work_dir} in
      (tree, config, Failure Feedback.no_changes)
    | true ->
      let changed = added |-| (added |-| committed') in
      let created = added |-| committed' in
      let deleted = removed in
      let commit_result =
        "\n" ^
        string_of_int (List.length changed) ^ " file(s) changed\n" ^
        string_of_int (List.length created) ^ " file(s) created\n" ^
        string_of_int (List.length deleted) ^ " file(s) deleted\n"
      in
      let id = gen_hash (tree,config) in
      let committed = (committed' |-| removed) |+| added in
      let source_dir = repo_dir in
      let target_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
      let () = Fileio.create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in
      let () = copy_over_files repo_dir committed source_dir target_dir in
      let head = (id,msg,committed) in
      let index = ([],[]) in
      let branch = head :: (id',msg',committed') :: prev_commits in
      let commit_tree = CommitTree.add current_branch branch commit_tree in
      let tree = {
        head=head;
        index=index;
        work_dir=work_dir;
        commit_tree=commit_tree
      } in
      (tree, config, Success ((Feedback.changes_committed current_branch id msg) ^ commit_result))
    )

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
    let branch = CommitTree.find branch_name commit_tree in
    (
    match branch with
    | (id,msg,committed) :: prev_commits ->
      let config = {config with current_branch=branch_name} in
      let source_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
      let target_dir = repo_dir in
      let () = copy_over_files repo_dir committed source_dir target_dir in
      let tree = {
        head= (id,msg,committed);
        index= tree.index;
        work_dir= work_dir;
        commit_tree= commit_tree
      } in
      (tree,config, Success ("Switched to branch \'" ^ branch_name ^ "\'"))
    | _ -> (tree,config,Failure (Feedback.no_repo))
    )

let file_batch_op op tree config repo_dir current_branch files =
  let files' =
    List.fold_left
    (fun a x -> a |+| (get_files (repo_dir ^ "\\(" ^ x ^ "\\)") tree.work_dir) )
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

let reset_branch_soft tree config repo_dir current_branch hash =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  let branch = CommitTree.find current_branch commit_tree in
  match find_commit hash branch with
  | None ->
    let tree = {tree with work_dir=work_dir} in
    (tree,config,Failure ("No such commit with hash " ^ hash) )
  | Some ((id,msg,committed),_) ->
    let tree = {
      head= (id,msg,committed);
      index= tree.index;
      work_dir= work_dir;
      commit_tree= commit_tree
    } in
    (tree,config,Success ("HEAD was reset to " ^ hash))

let reset_branch_mixed tree config repo_dir current_branch hash =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  let branch = CommitTree.find current_branch commit_tree in
  match find_commit hash branch with
  | None ->
    let tree = {tree with work_dir=work_dir} in
    (tree,config,Failure ("No such commit with hash " ^ hash) )
  | Some ((id,msg,committed),_) ->
    let (added,removed) = tree.index in
    let added = added |-| committed in
    let removed = removed |-| committed in
    let tree = {
      head= (id,msg,committed);
      index= (added,removed);
      work_dir= work_dir;
      commit_tree= commit_tree
    } in
    (tree,config,Success ("HEAD was reset to " ^ hash))

let reset_branch_hard tree config repo_dir current_branch hash =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  let branch = CommitTree.find current_branch commit_tree in
  match find_commit hash branch with
  | None ->
    let tree = {tree with work_dir=work_dir} in
    (tree,config,Failure ("No such commit with hash " ^ hash) )
  | Some ((id,msg,committed),_) ->
    let (added,removed) = tree.index in
    let added = added |-| committed in
    let removed = removed |-| committed in
    let source_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
    let target_dir = repo_dir in
    let () = copy_over_files repo_dir committed source_dir target_dir in
    let tree = {
      head= (id,msg,committed);
      index= (added,removed);
      work_dir= work_dir;
      commit_tree= commit_tree
    } in
    (tree,config,Success ("HEAD was reset to " ^ hash))

let status tree config repo_dir current_branch =
  let work_dir = get_work_dir repo_dir in
  let (id,msg,committed) = tree.head in
  let (added,removed) = tree.index in
  let feedback = "On branch " ^ current_branch ^ "\n\n" ^ msg ^ "\n\n" ^
  (
    if ( max (List.length added) (List.length removed) > 0 ) then
    (
      "Changes to be committed:\n" ^
      (Listops.to_string (abbrev_files repo_dir (added |-| (added |-| committed) ) ) "<green>\t" "\nadded:\t" "</green>\n" ) ^
      (Listops.to_string (abbrev_files repo_dir (added |-| committed) ) "<green>\t" "\nnew file:\t" "</green>\n" ) ^
      (Listops.to_string (abbrev_files repo_dir removed) "<red>\t" "\ndeleted:\t" "</red>\n\n")
    )
    else
    ("Nothing to commit\n" )
  )
  ^
  (
    let untracked_files = ((work_dir |-| committed) |-| added) |-| removed in
    if (List.length (untracked_files) > 0) then
    (
      let untracked_files = abbrev_files repo_dir untracked_files in
      "Untracked files:\n" ^ (Listops.to_string (untracked_files) "<red>\t" "\n\t" "</red>\n")
    )
    else
    ("Working directory clean" )
  )
  in
  let tree = {tree with work_dir = work_dir} in
  (tree,config,Success feedback)

let config_set tree config repo_dir current_branch key value =
  let work_dir = get_work_dir repo_dir in
  match key with
  | "username" -> (tree,{config with username=value},Success ("set " ^ key))
  | "password" ->
    let hashed = string_of_int (Hashtbl.hash value) in
    (
      match config.password with
        | "" -> (tree,{config with password=hashed},Success ("set " ^ key))
        | _ ->
        let () = Printf.printf "\n%s" "Enter old password: " in
        let input = Pervasives.read_line() in
        let hashed' = string_of_int (Hashtbl.hash input) in
      (
        match config.password = hashed' with
        | false -> (tree,config,Failure "\nPassword incorrect.\n")
        | true -> (tree,{config with password=hashed},Success ("set " ^ key))
      )
    )
  | "upstream" ->
  let delim = Str.regexp "@" in
  (
    match Str.split delim value with
    | username::domain::_ ->
    (
      let public_key = Fileio.read_str "./oasys.rsa.pub" in
      let req = {
        host=domain;
        port="6700";
        data=public_key;
        meth="POST";
        cmd="PUBLICKEY"
      } in
      (
        match Camelrider.send_request_to_server req with
        | false,_ -> (tree,config,Failure "failed to set upstream")
        | true,_ ->
          let repo_state_path = (string_of_int (Hashtbl.hash repo_dir)) in
          let config = {
            repo_dir=repo_dir;
            current_branch=current_branch;
            username="";
            password="";
            upstream=""
          } in
          let commit_tree = CommitTree.empty in
          let (id,_,_) = tree.head in
          let head = (id, Feedback.init_commit, []) in
          let index = ([],[]) in
          let branch = [head] in
          let commit_tree = CommitTree.add current_branch branch commit_tree in
          let tree = {
            head=head;
            index=index;
            work_dir=work_dir;
            commit_tree=commit_tree
          } in
          let state = {config=config; tree=tree} in
          let () = Cameljson.serialize state ("./repos/origin-"^repo_state_path^".json") in
          let config = {config with upstream = value} in
          let _ = Sys.command("ssh -i ./oasys.rsa "^config.upstream^" \'mkdir -p ~/.oasys_origin/"^repo_state_path^"\'") in
          let _ = Sys.command("scp -i ./oasys.rsa ./repos/origin-"^repo_state_path^".json "^config.upstream^":~/.oasys_origin/"^repo_state_path^"/") in
          (tree,config,Success ("set " ^ key))
      )
    )
    | _ -> (tree,config,Failure "Bad upstream provided. Try: [username]@[domain]")
  )
  | _ -> (tree,config,Failure "Unrecognized key")

let merge_heads source target repo_dir branch_name state =
  let (id',_,committed') = source in
  let (id'',_,committed'') = target in
  let resolved = (committed' |-| committed'') |+| (committed'' |-| committed') in
  let conflicted = (committed' |+| committed'') |-| resolved in
  let source_resolved = resolved |-| committed'' in
  let target_resolved = resolved |-| committed' in
  let source_dir = repo_dir ^ oasys_dir ^ id' ^ "/" in
  let target_dir = repo_dir ^ oasys_dir ^ id'' ^ "/" in
  let () = copy_over_files repo_dir source_resolved source_dir repo_dir in
  let () = copy_over_files repo_dir target_resolved target_dir repo_dir in
  let conflicted' = abbrev_files repo_dir conflicted in
  let () =
    List.iter
    (fun c ->
      let f1 = source_dir ^ c in
      let f2 = target_dir ^ c in
      let f3 = repo_dir ^ c in
      let () = Fileio.merge_files f1 f2 f3 "HEAD" branch_name in
      ()
    )
    conflicted'
  in
  let id = gen_hash state in
  (id,"merge from "^branch_name,resolved |+| conflicted)

let merge tree config repo_dir current_branch branch_name =
  let work_dir = get_work_dir repo_dir in
  let commit_tree = tree.commit_tree in
  match CommitTree.mem branch_name commit_tree with
  | false ->
    let tree = {tree with work_dir=work_dir} in
    (tree,config,Failure ("No such branch with name "^branch_name))
  | true ->
    let source_branch = CommitTree.find current_branch commit_tree in
    (match source_branch with
      | source_head::source_tail ->
        let target_branch = CommitTree.find branch_name commit_tree in
        let union = source_branch |+| target_branch in
        if union |=| source_branch then
          (let tree = {tree with work_dir=work_dir} in
          (tree,config,Success "The branch you are on is already up-to-date"))
        else if union |=| target_branch then
          (let commit_tree = CommitTree.add current_branch target_branch commit_tree in
          (match target_branch with
          | target_head::target_tail ->
            let tree = {
              head=target_head;
              index=([],[]);
              work_dir=work_dir;
              commit_tree=commit_tree
            } in
            (tree,config,Success "fast forwarded HEAD")
          | [] -> assert false)
          )
        else
          (match target_branch with
          | target_head::_ ->
            let (id,msg,committed) = merge_heads source_head target_head repo_dir branch_name (tree,config) in
            let source_dir = repo_dir in
            let target_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
            let () = Fileio.create_dir (repo_dir ^ oasys_dir ^ id ^ "/") in
            let () = copy_over_files repo_dir committed source_dir target_dir in
            let head = (id,msg,committed) in
            let branch = head :: source_head :: source_tail in
            let commit_tree = CommitTree.add current_branch branch commit_tree in
            let tree = {
              head=head;
              index=([],[]);
              work_dir=work_dir;
              commit_tree=commit_tree
            } in
            (tree,config,Success (Feedback.merge_committed current_branch id msg))
          | [] -> assert false)
      | [] -> assert false
    )

let push tree config repo_dir current_branch =
  let work_dir = get_work_dir repo_dir in
  let value = config.upstream in
  let delim = Str.regexp "@" in
  (
    match Str.split delim value with
    | username::domain::_ ->
    (
      let repo_state_path = (string_of_int (Hashtbl.hash repo_dir)) in
      let _ = Sys.command("scp -i ./oasys.rsa "^config.upstream^":~/.oasys_origin/"^repo_state_path^"/origin-"^repo_state_path^".json ./repos/") in
      let state' = Cameljson.deserialize ("./repos/origin-"^repo_state_path^".json") in
      let tree' = state'.tree in
      let commit_tree' = tree'.commit_tree in
      (
        match CommitTree.mem current_branch commit_tree' with
        | false -> (tree,config,Failure ("No upstream branch exists with name "^current_branch))
        | true ->
          let commit_tree = tree.commit_tree in
          let head = tree.head in
          let branch = CommitTree.find current_branch commit_tree in
          let branch' = CommitTree.find current_branch commit_tree' in
          let union = branch |+| branch' in
          if branch' |=| branch then
            (tree,config,Success "The origin branch is already up-to-date. Nothing to push.")
          else if union |=| branch then
            (let commit_tree' = CommitTree.add current_branch branch commit_tree in
              let tree' = {
                head=head;
                index=([],[]);
                work_dir=work_dir;
                commit_tree=commit_tree'
              } in
              let state' = {state' with tree=tree'} in
              let () = Cameljson.serialize state' ("./repos/origin-"^repo_state_path^".json") in
              let _ = Sys.command("scp -i ./oasys.rsa ./repos/origin-"^repo_state_path^".json "^config.upstream^":~/.oasys_origin/"^repo_state_path^"/") in
              let _ = Sys.command("zip -r ./remote/"^repo_state_path^".zip "^repo_dir^oasys_dir^"*") in
              let _ = Sys.command("scp -i ./oasys.rsa ./remote/"^repo_state_path^".zip "^config.upstream^":~/.oasys_origin/"^repo_state_path^"/") in
              (tree,config,Success "Push succeeded.")
            )
          else if union |=| branch' then
            (
              (tree,config,Failure ("Cannot push. You are "^(string_of_int (List.length branch' - List.length branch))^" commit(s) behind origin."))
            )
          else
            (tree,config,Failure "Unable to push")
      )
    )
    | _ -> (tree,config,Failure "Bad upstream provided. Try: [username]@[domain]")
  )

let pull tree config repo_dir current_branch =
  let work_dir = get_work_dir repo_dir in
  let value = config.upstream in
  let delim = Str.regexp "@" in
  (
    match Str.split delim value with
    | username::domain::_ ->
    (
      let repo_state_path = (string_of_int (Hashtbl.hash repo_dir)) in
      let _ = Sys.command("scp -i ./oasys.rsa "^config.upstream^":~/.oasys_origin/"^repo_state_path^"/origin-"^repo_state_path^".json ./repos/origin-"^repo_state_path^".json") in
      let state' = Cameljson.deserialize ("./repos/origin-"^repo_state_path^".json") in
      let tree' = state'.tree in
      let commit_tree' = tree'.commit_tree in
      (
        match CommitTree.mem current_branch commit_tree' with
        | false -> (tree,config,Failure ("No upstream branch exists with name "^current_branch))
        | true ->
          let commit_tree = tree.commit_tree in
          let head' = tree'.head in
          let branch = CommitTree.find current_branch commit_tree in
          let branch' = CommitTree.find current_branch commit_tree' in
          let union = branch |+| branch' in
          if branch' |=| branch then
            (tree,config,Success "Your branch is already up-to-date. Nothing to pull.")
          else if union |=| branch' then
            (let commit_tree = CommitTree.add current_branch branch' commit_tree in
              let tree = {
                head=head';
                index=([],[]);
                work_dir=work_dir;
                commit_tree=commit_tree
              } in
              let state = {config=config; tree=tree} in
              let () = Cameljson.serialize state ("./repos/"^repo_state_path^".json") in
              let head = tree.head in
              let (id,_,committed) = head in
              let _ = Sys.command("unzip ./remote/"^repo_state_path^".zip -d "^repo_dir^oasys_dir) in
              let source_dir = repo_dir ^ oasys_dir ^ id ^ "/" in
              let target_dir = repo_dir in
              let () = copy_over_files repo_dir committed source_dir target_dir in
              (tree,config,Success "Pull succeeded.")
            )
          else if union |=| branch then
            (
              (tree,config,Failure ("Cannot pull. You are "^(string_of_int (List.length branch - List.length branch'))^" commit(s) ahead of origin."))
            )
          else
            (tree,config,Failure "Unable to pull.")
      )
    )
    | _ -> (tree,config,Failure "Bad upstream provided. Try: [username]@[domain]")
  )

let update_tree (cmd:cmd_expr) (tree:palm_tree) (config:config):palm_tree * config * feedback =
  let (repo_dir, current_branch) = get_config config in
  match cmd with
  | (INIT,[EMPTY],[]) -> init tree config repo_dir current_branch
  | (ADD,[EMPTY],files) -> file_batch_op add tree config repo_dir current_branch files
  | (ADD,[ALL],[]) -> file_batch_op add tree config repo_dir current_branch tree.work_dir
  | (RM,[EMPTY],files) -> file_batch_op rm_file tree config repo_dir current_branch files
  | (RM,[FILE],files) -> file_batch_op rm_file tree config repo_dir current_branch files
  | (RM,[BNCH],[branch_name]) -> rm_branch tree config repo_dir current_branch branch_name
  | (RESET,[EMPTY],files) -> file_batch_op reset_file tree config repo_dir current_branch files
  | (RESET,[FILE],files) -> file_batch_op reset_file tree config repo_dir current_branch files
  | (RESET,[BNCH],[hash]) -> reset_branch_mixed tree config repo_dir current_branch hash
  | (RESET,[HARD],[hash]) -> reset_branch_hard tree config repo_dir current_branch hash
  | (RESET,[MIXED],[hash]) -> reset_branch_mixed tree config repo_dir current_branch hash
  | (RESET,[SOFT],[hash]) -> reset_branch_soft tree config repo_dir current_branch hash
  | (BRANCH,[EMPTY],[]) -> get_branches tree config repo_dir current_branch
  | (BRANCH,[EMPTY],[branch_name]) -> branch tree config repo_dir current_branch branch_name
  | (CHECKOUT,[EMPTY],[branch_name]) -> checkout tree config repo_dir current_branch branch_name
  | (COMMIT,[MSG],[message]) -> commit tree config repo_dir current_branch message
  | (STATUS,[EMPTY],[]) -> status tree config repo_dir current_branch
  | (LOG,[EMPTY],[]) -> log tree config repo_dir current_branch
  | (MERGE,[EMPTY],[branch_name]) -> merge tree config repo_dir current_branch branch_name
  | (PUSH,[EMPTY],[]) -> push tree config repo_dir current_branch
  | (PULL,[EMPTY],[]) -> pull tree config repo_dir current_branch
  | (HELP,_,_) -> (tree, config, Success Feedback.empty)
  | (CONFIG,[CONFIG_SET],[key;value]) -> config_set tree config repo_dir current_branch key value
  | _ -> (tree,config,Failure Feedback.no_support)