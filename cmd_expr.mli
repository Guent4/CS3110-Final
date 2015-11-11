type host = LOCAL | REMOTE
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | STATUS | CONFIG | HELP | CLONE | INIT | REMOVE | LOG | RESET
type arg = string 
type opt = HEAD | MSG | ORIGIN | ALL

val parse : string list -> 'a * 'b