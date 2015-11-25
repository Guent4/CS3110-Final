open Coconuts
open Camel



TEST = Camel.interpret "push" = Some (LOCAL, PUSH, [EMPTY], [])

TEST = Camel.interpret "add" = None
TEST = Camel.interpret "add ." = Some (LOCAL, ADD, [ALL], [])
TEST = Camel.interpret "add file1.ml file2.txt file3" = Some (LOCAL, ADD, [EMPTY], ["file1.ml"; "file2.txt"; "file3"])
TEST = Camel.interpret "add file1.ml file2.txt \"file3\"" = None

TEST = Camel.interpret "commit -m" = None
TEST = Camel.interpret "commit --message" = None
TEST = Camel.interpret "commit --message asdf" = None
TEST = Camel.interpret "commit --message \"hello world\"" = Some (LOCAL, COMMIT, [MSG], ["\"hello world\""])
TEST = Camel.interpret "commit -a -m" = None
TEST = Camel.interpret "commi -m" = None

TEST = Camel.interpret "diff" = Some (LOCAL, DIFF, [EMPTY], [])
TEST = Camel.interpret "diff --file file1.ml file2.ml" = Some (LOCAL, DIFF, [FILE], ["file1.ml"; "file2.ml"])
TEST = Camel.interpret "diff -b branch1 branch2" = Some (LOCAL, DIFF, [BNCH], ["branch1"; "branch2"])
TEST = Camel.interpret "diff file1.ml file2.ml" = None
TEST = Camel.interpret "diff \"file1.ml\" \"file2.ml\"" = None

TEST = Camel.interpret "rm branch1 branch2 branch3" = None
TEST = Camel.interpret "rm -b branch1 branch2 branch3" = Some (LOCAL, RM, [BNCH], ["branch1"; "branch2"; "branch3"])
TEST = Camel.interpret "rm --file f1 f2 f3" = Some (LOCAL, RM, [FILE], ["f1"; "f2"; "f3"])


let () = Pa_ounit_lib.Runtime.summarize()