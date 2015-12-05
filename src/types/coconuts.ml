open Yojson.Basic.Util

(* All the commands that OASys can support; INVALID_CMD is for an input that is
 * not a supported cmd. *)
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | STATUS
  | CONFIG | HELP | INIT | LOG | RESET | RM | INVALID_CMD of string

type arg = string

(* All of the possible options that will be supported (i.e ALL for
 * "git add -a").  INVALID_OPT is for unsupported opts*)
type opt = MSG | ALL | DELETE | REMOVE | RENAME | BNCH | FILE | CONFIG_SET
  | HARD | MIXED | SOFT | CMD | EMPTY | INVALID_OPT of string

(* This is the type that the user input will be parsed into in Camel.  The actual
 * commands described by the cmd_expr will be executed in OASys *)
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