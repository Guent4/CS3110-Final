open Coconuts
open Palmtreeupdater
open Core.Std

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

let add tree config filename =
  let cmd = (ADD,[EMPTY],[filename]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)

let add_all tree config = 
  let cmd = (ADD,[ALL],[]) in 
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)

let remove tree config filename = 
  let cmd = (RM,[FILE],[filename]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)

let commit tree config msg =
  let cmd = (COMMIT,[MSG],[msg]) in
  let (tree',config',feedback) = update_tree cmd tree config in
  (tree',config',feedback)
(*****************************************************************************)
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
(*****************************************************************************)
TEST_MODULE "add, add_all and remove tests" = struct
  (*TEST FILES*)
  let () = Out_channel.write_all "./test_proj/wuggle.txt" ~data:"david"
  let () = Out_channel.write_all "./test_proj/test.txt" ~data:"david"

  (*ADD TESTS*)
  (*Create file to add*)
  let (tree,config) = setup_tree () 
  let (tree,config,feedback) = init tree config 
  let (tree',config',feedback) = add tree config "wuggle.txt" 
  TEST_UNIT "test added file" = assert
  (
    let (ad, rem) = tree'.index in
    List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt")
  )
  TEST_UNIT "test config" = assert
  (
    config' = config
  )
  
  (*Create another file to add*)
  (* let () = Out_channel.write_all "./test_proj/test.txt" ~data:"david" *)
  let (tree'',config'',feedback) = add tree' config' "test.txt"
  TEST_UNIT "test added file" = assert
  (
    let (ad, rem) = tree''.index in
    List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "test.txt")
  )

  TEST_UNIT "test config (x2)" = assert
  (
    config'' = config'
  )

  (*REMOVE TESTS*)
  (*Remove wuggle.txt*)
  let (rmtree, rmconfig, rmfeedback) = remove tree'' config'' "wuggle.txt"
  TEST_UNIT "remove wuggle.txt" = assert 
  (
    let (ad, rem) = rmtree.index in
    let wug = List.mem rem ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt") in
    let wug2 = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "test.txt") in
    wug && wug2
  )
  TEST_UNIT "test rm config" = assert
  (
    rmconfig = config'
  )
  (*Remove test.txt*)
  let (rmtree', rmconfig', rmfeedback) = remove rmtree rmconfig "test.txt"
  TEST_UNIT "Remove test.txt" = assert
  (
    let (ad, rem) = rmtree'.index in
    let wug = List.mem rem ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt") in
    let wug2 = List.mem rem ((Sys.getcwd()) ^ "/test_proj/"  ^ "test.txt") in
    wug && wug2
  )
  TEST_UNIT "test rm config (x2)" = assert
  (
    rmconfig' = rmconfig
  )
 
  (* (*ADD ALL TESTS*)
  let (alltree, allconfig, allfeedback) = add_all rmtree' rmconfig'
  TEST_UNIT = assert
  (
    let (ad, rem) = alltree.index in 
    let wug = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt") in
    let wug2 = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle2.txt") in
    wug && wug2
  ) *)
end
(*****************************************************************************)
TEST_MODULE "commit tests" = struct
  (*TEST FILES*)
  let () = Out_channel.write_all "./test_proj/wuggle.txt" ~data:"david"
  let () = Out_channel.write_all "./test_proj/test.txt" ~data:"david"

  let (tree,config) = setup_tree ()
  let (tree,config,feedback) = init tree config
  let (tree',config',feedback) = add tree config "wuggle.txt"
  let (tree'', config'', feedback) = add tree' config' "test.txt"
  let (tree3, config3, feedback) = commit tree'' config'' "test"
  let (comtree, comconfig, comfeedback) = commit tree3 config3 "test1"
  TEST_UNIT "test tree when files added" = assert
  (
    let (ad, rem) = tree''.index in
    let wug = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt") in
    let wug2 = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "test.txt") in
    wug && wug2
  )
  TEST_UNIT "test tree when files committed" = assert
  (
    let (ad, rem) = comtree.index in
    let wug = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt") in
    let wug2 = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "test.txt") in
    ((wug && wug2) = false)
  )
  TEST_UNIT "commit head test" = assert
  (
    let (i, m, c) = comtree.head in
    let wug = List.mem c ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt") in
    let wug2 = List.mem c ((Sys.getcwd()) ^ "/test_proj/"  ^ "test.txt") in
    wug && wug2 
  )
  TEST_UNIT "commit-tree test" = assert
  (
    let master = CommitTree.find "master" comtree.commit_tree in
    match master with
    | (i, m, c)::t -> print_endline m;(m = "test") && (List.mem c ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt")) && (List.mem c ((Sys.getcwd()) ^ "/test_proj/"  ^ "test.txt"))
    | _ -> false
  )
end



let () = Pa_ounit_lib.Runtime.summarize ()