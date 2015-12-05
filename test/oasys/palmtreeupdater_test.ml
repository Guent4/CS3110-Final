open Coconuts
open Palmtreeupdater

let setup_tree () =
  let () = Fileio.remove_dir "./test_proj/.oasys/" in
  let dir = (Sys.getcwd()) ^ "/test_proj/" in
  let config = {repo_dir=dir; current_branch="master"; username=""; password=""; upstream=""} in
  let tree = {head=("", "", []); index=([],[]); work_dir=[]; commit_tree= CommitTree.add "master" [] (CommitTree.empty)} in
  (tree,config)

let init tree config =
  let cmd = (INIT,[EMPTY],[]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)

(* let add tree config filename =
  let cmd = (ADD,[],[filename]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback) *)

(* let commit tree config msg =
  let cmd = (COMMIT,[],[msg]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback) *)

TEST_MODULE "init tests" = struct
  let (tree,config) = setup_tree ()
  let (tree',config',feedback) = init tree config
  TEST_UNIT "test .oasys" = assert
  (
    Fileio.file_exists "./test_proj/.oasys/"
  )
  TEST_UNIT "test tree"  = assert
  (
    let master = CommitTree.find "master" tree'.commit_tree in
    let x = [tree'.head] in
    match master with
    | y when y = x -> true
    | _ -> (print_endline "hello"); false
  )
  TEST_UNIT "test config"  = assert
  (
    config = config'
  )
  TEST "test feedback" =(
    let x = "Initialized empty OASYS repository in " ^ config'.repo_dir ^ ".oasys/" in
    match feedback with
    | Success y when y = x -> true
    | _ -> false)

  let (tree'',config'',feedback') = init tree' config'
  TEST_UNIT "test feedback (x2)" = assert
  (
    match feedback' with
    | Failure "an oasys repository already exists in this directory" -> true
    | _ -> false
  )

  TEST_UNIT "test .oasys" = assert
  (
    Fileio.file_exists "./test_proj/.oasys/"
  )

  TEST_UNIT "test tree (x2)"  =
    assert(tree''.head = tree'.head);
    assert(tree''.index = tree'.index);
    assert(tree''.commit_tree = tree'.commit_tree)
    assert(tree''.work_dir = tree'.work_dir)

  TEST_UNIT "test config (x2)"  = assert
  (
    config'' = config'
  )
end

(* TEST_MODULE "add tests" = struct
  let (tree,config) = setup_tree ()
  let (tree,config,feedback) = init tree config
  let (tree',config',feedback) = add tree config "wuggle.txt"
  TEST_UNIT "test tree" = assert
  (
    let master = CommitTree.find "master" tree' in
    match master with
    | Changes(["wuggle.txt"],[],[]) :: Commit (_,"initial commit") :: [] -> true
    | _ -> false
  )
  TEST_UNIT "test config" = assert
  (
    config' = config
  )
  TEST_UNIT "test feedback" = assert
  (
      match feedback with
      | Success "wuggle.txt has been added" -> true
      | _ -> false
  )
  let (tree'',config'',feedback) = add tree' config' "wuggle.txt"
  TEST_UNIT "test tree (x2)" = assert
  (
    let master = CommitTree.find "master" tree'' in
    match master with
    | Changes(["wuggle.txt"],[],[]) :: Commit (_,"initial commit") :: [] -> true
    | _ -> false
  )
  TEST_UNIT "test config (x2)" = assert
  (
    config'' = config'
  )
  TEST_UNIT "test feedback (x2)" = assert
  (
      match feedback with
      | Success "wuggle.txt has been added" -> true
      | Success x | Failure x ->
      (let () = Printf.printf "\n%s\n" x in
      false)
  )
end

TEST_MODULE "commit tests" = struct
  let (tree,config) = setup_tree ()
  let (tree,config,feedback) = init tree config
  let (tree',config',feedback) = add tree config "wuggle.txt"
  TEST_UNIT "test tree" = assert
  (
  let master = CommitTree.find "master" tree' in
  match master with
  | Changes(["wuggle.txt"],[],[]) :: Commit (_,"initial commit") :: [] -> true
  | _ -> false
  )
end *)

(* TEST_MODULE "rm tests" = struct
  let (tree,config) = setup_tree ()
  let (tree',config',feedback) = init tree config
  TEST_UNIT "rm EMPTY" =
    let cmd = (RM,[EMPTY],[]) in


end

 *)
let () = Pa_ounit_lib.Runtime.summarize ()