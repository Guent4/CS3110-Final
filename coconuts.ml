open Yojson.Basic.Util

(* Describe which repository, local or remote, that the user is referring to *)
type host = LOCAL | REMOTE

(* Commands that OASys can support *)
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | DIFF
  | STATUS | CONFIG | HELP | CLONE | INIT | LOG | RESET | QUIT
  | INVALID_CMD of string

(* A single argument for the command (i.e. "commit message" in
 *  "git commit -m "commit message"", repository name, )
 *)
type arg = string

(* All of the possible options that will be supported (i.e "-a" in
 *  "git add -a")
 *)
type opt =  MSG | ORIGIN | ALL | HARD | SOFT | SETUPSTREAM | QUIET | VERBOSE
  | FORCE | DELETE | REMOVE | RENAME | NEWBRANCH | INVALID_OPT of string

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

module StringMap = Map.Make (struct type t = string let compare a b = Pervasives.compare a b end)

type palm_tree = branch StringMap.t

(* JSON used for sending and receiving data. *)
type json = Yojson.Basic.json

(* Represents a client request. Contains a command and client information *)
type request = {host:string; port:int; data:string; cmd:string}