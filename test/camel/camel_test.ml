open Camel
open Coconuts

TEST_MODULE "interpret" = struct
  let lex (s:string) : string list =
    let whitespace_char_string = String.concat "+"
      (List.map (String.make 1) [Char.chr 9; Char.chr 10; Char.chr 11;
          Char.chr 12; Char.chr 13; Char.chr 32]) in
    let whitespace = "[" ^ whitespace_char_string ^ "]+" in
    Str.split (Str.regexp whitespace) s

  let il (s:string) : cmd_expr option =
    interpret (lex s)

  TEST = il "init" = Some (INIT, [EMPTY], [])
  TEST = il "init %-b" = None

  TEST = il "log" = Some (LOG, [EMPTY], [])
  TEST = il "    log      " = Some (LOG, [EMPTY], [])

  TEST = il "status" = Some (STATUS, [EMPTY], [])
  TEST = il "statuS" = None

  TEST = il "add" = None
  TEST = il "add ." = Some (ADD, [ALL], [])
  TEST = il "add file1.ml file2.txt file3" = Some (ADD, [EMPTY], ["file1.ml"; "file2.txt"; "file3"])
  TEST = il "add file1.ml file2.txt \"file3\"" = Some (ADD, [EMPTY], ["file1.ml"; "file2.txt"; "\"file3\""])

  TEST = il "commit %-m" = None
  TEST = il "commit %--message" = None
  TEST = il "commit      %--message asdf" = Some (COMMIT, [MSG], ["asdf"])
  TEST = il "commit %--message \"hello world\"" = Some (COMMIT, [MSG], ["\"hello world\""])
  TEST = il "commit %-a -m" = None
  TEST = il "commi %-m" = None

  TEST = il "branch" = Some (BRANCH, [EMPTY], [])
  TEST = il "branch branch1" = Some (BRANCH, [EMPTY], ["branch1"])
  TEST = il "branch b1 b2 b3" = Some (BRANCH, [EMPTY], ["b1"; "b2"; "b3"])
  TEST = il "branch %--bnch b1 b2 b3" = None

  TEST = il "rm %-b branch1 branch2" = Some (RM, [BNCH], ["branch1"; "branch2"])
  TEST = il "rm %--branch branch1 branch2" = Some (RM, [BNCH], ["branch1"; "branch2"])
  TEST = il "rm %-b" = None
  TEST = il "rm %--file file1 file2" = Some (RM, [FILE], ["file1"; "file2"])
  TEST = il "rm %--file" = None
  TEST = il "rm file1 file2" = Some (RM, [FILE], ["file1"; "file2"])

  TEST = il "checkout b1" = Some (CHECKOUT, [EMPTY], ["b1"])
  TEST = il "checkout" = None
  TEST = il "checkout b1 b2" = None

  TEST = il "push" = Some (PUSH, [EMPTY], [])

  TEST = il "pull" = Some (PULL, [EMPTY], [])

  TEST = il "help" = None
  TEST = il "help word1 word2" = None
  TEST = il "help %--cmd cmd" = None
  TEST = il "help %--cmd" = None
  TEST = il "help %--cmd 1 2" = None

  TEST = il "merge branch1" = Some (MERGE, [EMPTY], ["branch1"])
  TEST = il "merge branch1 branch2" = None

  TEST = il "config %--set-upstream 1 2" = Some (CONFIG, [CONFIG_SET], ["1"; "2"])
  TEST = il "config 1 2" = None
end