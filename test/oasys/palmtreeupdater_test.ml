open Coconuts
open Palmtreeupdater

let setup_tree () =
  let tree = PalmTree.empty in
  let tree = PalmTree.add "master" [] tree in
  let config = {repo_dir= "./test_proj/"; current_branch="master"} in
  (tree,config)

let init tree config =
  let cmd = (INIT,[],[]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)

let add tree config =
  let cmd = (ADD,[],["wuggle.txt"]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)

TEST_MODULE "init tests" = struct

  let (tree,config) = setup_tree ()

  let (tree',config',feedback) = init tree config

  TEST_UNIT "test .oasys" = assert
  (
    Fileio.file_exists "./test_proj/.oasys/"
  )

  TEST_UNIT "test tree"  = assert
  (
    let master = PalmTree.find "master" tree' in
    match master with
    | Changes([],[],[]) :: Commit (_,"initial commit") :: [] -> true
    | _ -> false
  )

  TEST_UNIT "test config"  = assert
  (
    config = config'
  )

  TEST_UNIT "test feedbacK" = assert
  (
    match feedback with
    | Success "repository initialized" -> true
    | _ -> false
  )

  let (tree',config',feedback) = init tree config

  TEST_UNIT "test .oasys" = assert
  (
    Fileio.file_exists "./test_proj/.oasys/"
  )

  TEST_UNIT "test tree"  = assert
  (
    tree' = tree
  )

  TEST_UNIT "test config"  = assert
  (
    config' = config
  )

  TEST_UNIT "test feedbacK" = assert
  (
    match feedback with
    | Failure "repository already exists" -> true
    | _ -> false
  )
end
(*
TEST_MODULE "add tests" = struct
  let (tree,config) = setup_tree ()
  let (tree,config,feedback) = init tree config
  let (tree',config',feedback) = add tree config
  TEST_UNIT "test tree" = assert
  (
    let master = PalmTree.find "master" tree' in
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
      | Success "wuggle.txt added" -> true
      | _ -> false
  )
end
*)
let () = Pa_ounit_lib.Runtime.summarize ()