open Coconuts
open Camel


let input = "push"
TEST = Camel.interpret "push" = Some (LOCAL, PUSH, [], [])


let () = Pa_ounit_lib.Runtime.summarize()