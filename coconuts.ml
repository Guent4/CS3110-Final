open Yojson.Basic.Util

(* Describe which repository, local or remote, that the user is referring to *)
type host = LOCAL | REMOTE

(* Commands that OASys can support *)
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | DIFF
  | STATUS | CONFIG | HELP | CLONE | INIT | LOG | RESET | QUIT | RM
  | INVALID_CMD of string

(* A single argument for the command (i.e. "commit message" in
 *  "git commit -m "commit message"", repository name, )
 *)
type arg = string

(* All of the possible options that will be supported (i.e "-a" in
 *  "git add -a")
 *)
type opt =  MSG | ORIGIN | ALL | SETUPSTREAM | QUIET |  DELETE | REMOVE | RENAME
  | NEWBRANCH | INVALID_OPT of string

(* This is the type that the user input will be parsed into.  The actual commands
 *  described in the cmd_expr will be executed later in OASys
 *)
type cmd_expr = host * cmd * opt list * arg list

type file_path = string

type config = {repo_dir: file_path; current_branch: string}

type feedback = Success of string | Failure of string

(* The commit id *)
type id = string

(* The commit message *)
type msg = string

type added = string list

type deleted = string list

type committed = string list

type node = Commit of id * msg | Changes of added * deleted * committed

type branch = node list

module PalmTree = Map.Make (struct type t = string let compare a b = Pervasives.compare a b end)

type palm_tree = branch PalmTree.t

(* JSON used for sending and receiving data. *)
type json = Yojson.Basic.json

(* Represents a client request. Contains a command and client information *)
type request = {host:string; port:int; data:string; cmd:string}