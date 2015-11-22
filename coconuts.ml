(* Describe which repository, local or remote, that the user is referring to *)
type host = LOCAL | REMOTE

(* Commands that OASys can support *)
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | DIFF
  | STATUS | CONFIG | HELP | CLONE | INIT | LOG | RESET | QUIT

(* A single argument for the command (i.e. "commit message" in
 *  "git commit -m "commit message"", repository name, )
 *)
type arg = string

(* All of the possible options that will be supported (i.e "-a" in
 *  "git add -a")
 *)
type opt = HEAD | MSG | ORIGIN | ALL | HARD | SOFT | INCLUDE | SETUPSTREAM
  | QUIET | VERBOSE | FORCE | DELETE | RENAME | NEWBRANCH | BRANCHES

(* This is the type that the user input will be parsed into.  The actual commands
 *  described in the cmd_expr will be executed later in OASys
 *)
type cmd_expr = host * cmd * opt list * arg list

(* The commit id *)
type id = string

(* The commit message *)
type msg = string

type node = Commit of id * msg | Add

type branch = node list

module StringMap = Map.Make(struct type t = string let compare a b = Pervasives.compare a b end)

type palm_tree = branch StringMap.t

(* JSON used for sending and receiving data. *)
type json

(* Represents a client request. Contains a command and client information *)
type request