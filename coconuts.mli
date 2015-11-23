(* Describe which repository, local or remote, that the user is referring to *)
type host = LOCAL | REMOTE

(* Commands that OASys can support *)
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | DIFF
  | STATUS | CONFIG | HELP | CLONE | INIT | LOG | RESET | QUIT
  | INVALID_CMD of string

(* A single argument for the command (i.e. "commit message" in
 *  "git commit -m "commit message"", repository name, )
 *)
type arg

(* All of the possible options that will be supported (i.e "-a" in
 *  "git add -a")
 *)
type opt = MSG | ORIGIN | ALL | HARD | SOFT | INCLUDE | SETUPSTREAM
  | QUIET | VERBOSE | FORCE | DELETE | RENAME | NEWBRANCH | BRANCHES
  | INVALID_OPT of string

(* This is the type that the user input will be parsed into.  The actual commands
 *  described in the cmd_expr will be executed later in OASys
 *)
type cmd_expr = host * cmd * opt list * arg list

(* The commit id *)
type id

(* The commit message *)
type msg

type node

type branch

type palm_tree

(* JSON used for sending and receiving data. *)
type json

(* Represents a client request. Contains a command and client information *)
type request