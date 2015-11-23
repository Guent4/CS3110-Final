open Coconuts
open Camel


TEST = Camel.interpret "push" = Some (LOCAL, PUSH, [], [])

TEST = Camel.interpret "commit -m" = Some (LOCAL, COMMIT, [MSG], [])
TEST = Camel.interpret "commit -a -m" = Some (LOCAL, COMMIT, [ALL;MSG], [])
TEST = Camel.interpret "commit --message" = Some (LOCAL, COMMIT, [MSG], [])


let () = Pa_ounit_lib.Runtime.summarize()