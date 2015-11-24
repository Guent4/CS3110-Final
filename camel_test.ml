open Coconuts
open Camel


TEST = Camel.interpret "push" = Some (LOCAL, PUSH, [], [])

TEST = Camel.interpret "add ." = Some (LOCAL, ADD, [ALL], [])

TEST = Camel.interpret "commit -m" = Some (LOCAL, COMMIT, [MSG], [])
TEST = Camel.interpret "commit --message" = Some (LOCAL, COMMIT, [MSG], [])
TEST = Camel.interpret "commit --message \"hello world\"" = Some (LOCAL, COMMIT, [MSG], [SENTENCE "\"hello world\""])
TEST = Camel.interpret "commit -a -m" = None
TEST = Camel.interpret "commi -m" = None

TEST = Camel.interpret "diff" = Some (LOCAL, DIFF, [], [])
TEST = Camel.interpret "diff file1.ml" = Some (LOCAL, DIFF, [], [WORD "file1.ml"])
TEST = Camel.interpret "diff file1.ml file2.ml" = Some (LOCAL, DIFF, [], [WORD "file1.ml"; WORD "file2.ml"])
TEST = Camel.interpret "diff \"file1.ml\" \"file2.ml\"" = Some (LOCAL, DIFF, [], [SENTENCE "\"file1.ml\""; SENTENCE "\"file2.ml\""])


let () = Pa_ounit_lib.Runtime.summarize()