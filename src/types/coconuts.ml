open Yojson.Basic.Util

(* Commands that OASys can support *)
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | DIFF
  | STATUS | CONFIG | HELP | CLONE | INIT | LOG | RESET | RM | QUIT
  | INVALID_CMD of string

type arg = string

(* All of the possible options that will be supported (i.e "-a" in
 *  "git add -a")
 *)
type opt =  MSG | ALL | SETUPSTREAM | DELETE | REMOVE | RENAME | BNCH | FILE
  | CMD | EMPTY | INVALID_OPT of string

(* This is the type that the user input will be parsed into.  The actual commands
 *  described in the cmd_expr will be executed later in OASys
 *)
type cmd_expr = cmd * opt list * arg list

type file_path = string

type config = {repo_dir: file_path; current_branch: string}

type feedback = Success of string | Failure of string

(* The commit id *)
type id = string

(* The commit message *)
type msg = string

type added = string list

type removed = string list

type committed = string list

type node = Commit of id * msg | Changes of added * removed * committed

type branch = node list

module PalmTree = Map.Make (struct type t = string let compare a b = Pervasives.compare a b end)

type palm_tree = branch PalmTree.t

type state = {config: config; tree: palm_tree}

(* JSON used for sending and receiving data. *)
type json = Yojson.Basic.json

(* Represents a client request. Contains a command and client information *)
type client_req = {host:string; port:int; data:string; cmd:string}
