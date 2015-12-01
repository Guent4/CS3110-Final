open Yojson.Basic.Util

(* Commands that OASys can support *)
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | DIFF
  | STATUS | CONFIG | HELP | CLONE | INIT | LOG | RESET | RM | QUIT
  | INVALID_CMD of string

type arg = string

(* All of the possible options that will be supported (i.e "-a" in
 *  "git add -a")
 *)
type opt = MSG | ALL | SETUPSTREAM | DELETE | REMOVE | RENAME | BNCH | FILE
  | CONFIG_SET | HARD | MIXED | SOFT | CMD | EMPTY | INVALID_OPT of string

(* This is the type that the user input will be parsed into.  The actual commands
 *  described in the cmd_expr will be executed later in OASys
 *)
type cmd_expr = cmd * opt list * arg list

type file_path = string

type config = {
  repo_dir: file_path;
  current_branch: string;
  username: string;
  password: string;
  upstream: string
}

type feedback = Success of string | Failure of string

type id = string

type msg = string

type added = string list

type removed = string list

type committed = string list

type commit = id * msg * committed

type head = commit

type index = added * removed

type work_dir = string list

type branch = commit list

module CommitTree = Map.Make (struct type t = string let compare a b = Pervasives.compare a b end)

type palm_tree = {
  head: head;
  index: index;
  work_dir: work_dir;
  commit_tree: branch CommitTree.t
}

type state = {config: config; tree: palm_tree}

(* JSON used for sending and receiving data. *)
type json = Yojson.Basic.json

(* Represents a client request. Contains a command and client information *)
type client_req = {
  host: string;
  port: string;
  data: string;
  meth: string;
  cmd: string;
}