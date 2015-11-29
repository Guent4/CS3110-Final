open Coconuts
open Cameljson
open Palmtreeupdater

let repos_path = "./repos.json"

let gen_hash k =
  string_of_int (Hashtbl.hash k)

let get_repo_state_path hash =
  "./repos/" ^ hash ^ ".json"

let context_exists repo_state_path repo_path =
  (Fileio.file_exists repo_state_path) && (Fileio.file_exists (repo_path ^ "/.oasys/"))

let update_state cmd repo_state_path =
  let state = Cameljson.deserialize repo_state_path in
  let tree = state.tree in
  let config = state.config in
  let (tree', config', feedback) = Palmtreeupdater.update_tree cmd tree config in
  let state' = {config= config'; tree= tree'} in
  let () = Cameljson.serialize state' repo_state_path in
  feedback

let initialize_context repo_path repo_state_path =
  let repo_dir = repo_path in
  let current_branch = "master" in
  let config = {repo_dir=repo_dir; current_branch=current_branch} in
  let head = ("","",[]) in
  let index = ([],[]) in
  let work_dir = [] in
  let commit_tree = CommitTree.empty in
  let commit_tree = CommitTree.add current_branch [] commit_tree in
  let tree = {
    head=head;
    index=index;
    work_dir=work_dir;
    commit_tree=commit_tree
  } in
  let state = {config=config; tree=tree} in
  let () = Cameljson.serialize state repo_state_path in
  ()

let eval repo_path cmd =
  let hash = gen_hash repo_path in
  let repo_state_path = get_repo_state_path hash in
  if not (context_exists repo_state_path repo_path) then
  (
    initialize_context repo_path repo_state_path
  );
  let feedback = update_state cmd repo_state_path in
  feedback