open Coconuts
open Hashtbl

let vcs_dir = ".oasys/"

let gen_hash() = string_of_int (hash (Unix.gettimeofday()))

let file_exists path =
  FileUtil.find FileUtil.Exists path (fun a x -> true || a) false

let update_tree cmd tree config = failwith "unimplemented"

(*   let cur_branch = StringMap.find branch tree in
  match cmd with
  | (_,INIT,_,_) ->
    (* check if .oasys already exists *)
    let exists = file_exists (target_dir ^ vcs_dir) in
    match exists with
    | true ->
      (* fail over *)
      (tree,branch,FAILURE("repository already exists"))
    | false ->
      (* tree manipulation *)
      let tree' = Commit (gen_hash(), "initial commit") in
      let branch' = branch in
      let feedback = "branch created" in
      (tree',branch',feedback) *)