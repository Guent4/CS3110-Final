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

  (*ADD ALL TESTS*)
(*   let (alltree, allconfig, allfeedback) = add_all rmtree' rmconfig'
  TEST_UNIT = assert
  (
    let (ad, rem) = alltree.index in
    let wug = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "wuggle.txt") in
    let wug2 = List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "test.txt") in
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
  assert (match feedback with
          | Success _ -> true
          | Failure _ -> false)
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

TEST_MODULE "reset FILES tests" = struct
  let (tree,config) = setup_tree ()
  let (tree',config',feedback) = init tree config
  let () = Out_channel.write_all "./test_proj/add1.txt" ~data:"Your text"
  let (tree'',config'',feedback') = update_tree (ADD,[EMPTY],["add1.txt"]) tree' config'
  let (ad, rem) = tree''.index
  TEST_UNIT =
    assert(List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "add1.txt"))
  let (tree''',config''',feedback'') = update_tree (RESET,[FILE],["add1.txt"]) tree'' config''
  let (ad, rem) = tree'''.index
    TEST_UNIT =
  assert(not (List.mem ad ((Sys.getcwd()) ^ "/test_proj/"  ^ "add1.txt")))
end

TEST_MODULE "reset tests" = struct
  let (tree,config) = setup_tree ()
  let (tree',config',feedback) = init tree config
  let (id,msg,committed) = tree'.head
  (* create some files and add them and commit*)
  let () = Out_channel.write_all "./test_proj/add1.txt" ~data:"Your text"
  let (tree'',config'',feedback') = update_tree (ADD,[EMPTY],["add1.txt"]) tree' config'
  let (tree''',config''',feedback'') = update_tree (COMMIT,[MSG],["add1.txt"]) tree'' config''
  let (tree4, config4, feedback3) = update_tree (RESET,[MIXED],[id]) tree''' config'''
  TEST_UNIT "MIXED tests" =
    (assert (tree4.head = tree'.head));
    (assert (tree4.index = tree'.index));
    (assert (tree4.work_dir = tree'''.work_dir))
  TEST_UNIT "MIXED feedback" = assert(
    match feedback3 with
    | Success _ -> true
    | Failure _ -> false
    )
  let (tree5,config5, feedback4) = update_tree (RESET,[SOFT],[id]) tree''' config'''
  TEST_UNIT "SOFT tests" =
    (assert (tree5.head = tree'.head));
    (assert (tree5.index = tree'''.index));
    (assert (tree5.work_dir = tree'''.work_dir))
  TEST_UNIT "SOFT feedback" = assert(
    match feedback4 with
    | Success _ -> true
    | Failure _ -> false
    )
  let (tree6,config6, feedback5) = update_tree (RESET,[HARD],[id]) tree''' config'''
  TEST_UNIT "HARD tests" =
    (assert (tree6.head = tree'.head));
    (assert (tree6.index = tree'.index));
    (assert (tree6.work_dir = tree'.work_dir))
  TEST_UNIT "HARD feedback" = assert(
    match feedback5 with
    | Success _ -> true
    | Failure _ -> false
    )
end


TEST_MODULE "checkout file" = struct
  (* let _ = FileUtil.rm ["add1.txt";"add2.txt"] *)
  let _ = FileUtil.touch ~create:true "./test_proj/add1.txt"
  let (tree,config) = setup_tree ()
  let (tree',config',feedback) = init tree config
  let () = Out_channel.write_all "./test_proj/add1.txt" ~data:"Your text"
  let (tree'',config'',feedback') = update_tree (ADD,[EMPTY],["add1.txt"]) tree' config'
  let (tree''',config''',feedback'') = update_tree (COMMIT,[MSG],["add1.txt"]) tree'' config''
  let () = Out_channel.write_all "./test_proj/add1.txt" ~data:"Your text123"
  TEST_UNIT "checkout file" =
    let (_,_,f) = update_tree (CHECKOUT,[FILE],["add1.txt"]) tree''' config''' in
    assert(match f with |Success _ -> true
                        |Failure _ -> false);
    assert(In_channel.read_all "add1.txt" = "Your text")
end


TEST_MODULE "branch + checkout tests" = struct
  (* FileUtil.rm ["add1.txt";"add2.txt"] *)
  let (tree,config) = setup_tree ()
  let (tree',config',feedback) = init tree config
  let (id,msg,committed) = tree'.head
  (* create some files and add them and commit*)
  let () = Out_channel.write_all "./test_proj/add1.txt" ~data:"Your text"
  let (tree'',config'',feedback') = update_tree (ADD,[EMPTY],["add1.txt"]) tree' config'
  let (tree''',config''',feedback'') = update_tree (COMMIT,[MSG],["add1.txt"]) tree'' config''
  let (tree4, config4, feedback3) = update_tree (BRANCH,[EMPTY],["hello"]) tree''' config'''
  TEST_UNIT "new branch" =
    let master = CommitTree.find "master" tree4.commit_tree in
    let branch = CommitTree.find "hello" tree4.commit_tree in
    assert(branch=master);
    assert(config4.current_branch="master")
  TEST_UNIT "new branch feedback" = assert(
    match feedback3 with
    | Success _ -> true
    | Failure _ -> false
    )
  let () = Out_channel.write_all "./test_proj/add2.txt" ~data:"Your text2"
  let (tree5,config5,feedback4) = update_tree (ADD,[EMPTY],["add2.txt"]) tree4 config4
  let (tree6,config6,feedback5) = update_tree (COMMIT,[MSG],["add2.txt"]) tree5 config5
  let (tree7,config7,feedback6) = update_tree (CHECKOUT,[EMPTY],["hello"]) tree6 config6
  TEST_UNIT "checkout" =
    assert(tree7.head=tree4.head);
    assert(tree7.index=tree4.index);
    assert(tree7.work_dir=tree4.work_dir);
    assert(config7.current_branch="hello")
  TEST_UNIT "checkout feedback" = assert(
    match feedback6 with
    | Success _ -> true
    | Failure _ -> false
    )
(*   let () = Out_channel.write_all "add1.txt" ~data:"Your text2"
  let (tree8,config8,feedback7) = update_tree (CHECKOUT,[FILE],["add1.txt"]) tree7 config7
  TEST_UNIT "checkout file" =
    assert(In_channel.read_all "add1.txt" = "Your text") *)
end

TEST_MODULE "CONFIG" = struct
  let (tree,config) = setup_tree ()
  let (tree',config',feedback) = init tree config
  let (tree2,config2,feedback1) = update_tree (CONFIG,[CONFIG_SET],["username";"rho"]) tree' config'
  let (tree3,config3,feedback2) = update_tree (CONFIG,[CONFIG_SET],["password";"rho"]) tree2 config2
  let (tree4,config4,feedback3) = update_tree (CONFIG,[CONFIG_SET],["upstream";"vagrant@127.0.0.1"]) tree3 config3
  TEST_UNIT "username" =
    assert(config2.username = "rho")
  let hashed = string_of_int (Hashtbl.hash "rho")
  TEST_UNIT "password" =
    assert(config3.password = hashed)
  TEST_UNIT "upstream" =
    assert(config4.upstream = "vagrant@127.0.0.1")
end

TEST_MODULE "PUSH AND PULL" = struct
  (* let _ = FileUtil.rm ["add1.txt";"add2.txt"] *)
  let (tree,config) = setup_tree ()
  let (tree',config',feedback) = init tree config
  let (id,msg,committed) = tree'.head
  (* create some files and add them and commit*)
  let () = Out_channel.write_all "./test_proj/add1.txt" ~data:"Your text"
  let (tree'',config'',feedback') = update_tree (ADD,[EMPTY],["add1.txt"]) tree' config'
  let (tree''',config''',feedback'') = update_tree (COMMIT,[MSG],["add1.txt"]) tree'' config''
  let (tree4,config4,feedback3) = update_tree (CONFIG,[CONFIG_SET],["upstream";"vagrant@127.0.0.1"]) tree''' config'''
  let (tree4',config4',feedback3) = update_tree (PUSH,[EMPTY],[]) tree4 config4
  TEST_UNIT "push" =
    assert(tree4'=tree4);
    assert(match feedback with |Success _ -> true
                               |Failure s -> (print_endline s); false)
  let (tree5,config5, feedback4) = update_tree (RESET,[HARD],[id]) tree4 config4
  let (tree6,config6, feedback5) = update_tree (PULL,[EMPTY],[]) tree5 config5
  TEST_UNIT "pull" =
    assert(tree6=tree4);
    assert(config6=config4);
    assert(match feedback with |Success _ -> true
                               |Failure s -> (print_endline s); false)
end

let () = Pa_ounit_lib.Runtime.summarize ()