open Coconuts
open Palmtreeupdater

let setup_tree () =
  let () = Fileio.remove_dir "./test_proj/.oasys/" in
  let tree = CommitTree.empty in
(*   let tree = CommitTree.add "master" [] tree in *)
  let config = {repo_dir= "./test_proj/"; current_branch="master"; username=""; password=""; upstream=""} in
  (tree,config)

let init tree config =
  let cmd = (INIT,[],[]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)

(* let add tree config filename =
  let cmd = (ADD,[],[filename]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)

let commit tree config msg =
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
    let master = CommitTree.find "master" tree' in
    match master with
    | Changes([],[],[]) :: Commit (_,"initial commit") :: [] -> true
    | _ -> false
  )
  TEST_UNIT "test config"  = assert
  (
    config = config'
  )
  TEST_UNIT "test feedback" = assert
  (
    match feedback with
    | Success "a new oasys repository has been initialized" -> true
    | _ -> false
  )
  let (tree'',config'',feedback) = init tree' config'
  TEST_UNIT "test .oasys" = assert
  (
    Fileio.file_exists "./test_proj/.oasys/"
  )
  TEST_UNIT "test tree (x2)"  = assert
  (
    tree'' = tree'
  )
  TEST_UNIT "test config (x2)"  = assert
  (
    config'' = config'
  )
  TEST_UNIT "test feedback (x2)" = assert
  (
    match feedback with
    | Failure "an oasys repository already exists in this directory" -> true
    | _ -> false
  )
end

TEST_MODULE "add tests" = struct
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
end



let () = Pa_ounit_lib.Runtime.summarize ()