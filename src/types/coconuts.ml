open Yojson.Basic.Util

(* All the commands that OASys can support; INVALID_CMD is for an input that is
 * not a supported cmd *)
type cmd = PUSH | PULL | ADD | COMMIT | BRANCH | CHECKOUT | MERGE | STATUS
  | CONFIG | HELP | INIT | LOG | RESET | RM | INVALID_CMD of string

(*This type is used for external arguments that do not necessarily need or 
require its own type. For example, an arg list is used to update the tree specifically for the RM command where you have the option to specify a [FILE] with a the filename specified as the arg*)
type arg = string

(* All of the possible options that will be supported (i.e ALL for
 * "git add -a").  INVALID_OPT is for unsupported opts*)
type opt = MSG | ALL | DELETE | REMOVE | RENAME | BNCH | FILE | CONFIG_SET
  | HARD | MIXED | SOFT | CMD | EMPTY | INVALID_OPT of string

(* This is the type that the user input will be parsed into in Camel.  The actual commands described by the cmd_expr will be executed in OASys *)
type cmd_expr = cmd * opt list * arg list

(*This is the type that describes the file path for a directory, or file*)
type file_path = string

(*This type gives th user's directory and current branch, and also stores the username and pssword that can be used to access the remote repository on the server*)
type config = {
  repo_dir: file_path;
  current_branch: string;
  username: string;
  password: string;
  upstream: string
}

(*This type describes the messages that the user receives after using various commands. For example, the feedback type would be a Success if the user initialized a repository, and saw the message "Initialized empty OASYS repository in.." in the command prompt*)
type feedback = Success of string | Failure of string

(*This is the identification string for each commit. These are randomly generated and contain 16 characters*)
type id = string

(*This is the message that is included in a commit*)
type msg = string

(*This is the list of files that were added to the staging area by the user*)
type added = string list

(*This is the list of files that the user removed with the RM command. These files are temporarily stored in the removed string list and then deleted from the repository*)
type removed = string list

(*This is the type that stores the recently committed files from the staging area. This information is reflected in the head type*)
type committed = string list

(*This type defines a commit: every commit has a identification string, a message, and a list of files that were committed*)
type commit = id * msg * committed

(*This type represents the current branch and state the user's files are on*)
type head = commit

(*This type represents the files that were either addded or removed from the staging area*)
type index = added * removed

(*This type representes the work directory for the user's loal repository*)
type work_dir = string list

(*This type represents the user's current branch. A branch contains the list of commits the user has made*)
type branch = commit list

(*This type essentially the tree that our implementation is based on. This data structure maps branch names to commit lists*)
module CommitTree = Map.Make (struct type t = string let compare a b = Pervasives.compare a b end)

(**)
type palm_tree = {
  head: head;
  index: index;
  work_dir: work_dir;
  commit_tree: branch CommitTree.t
}
(*This type represents the current of the user's repository: gives the directory, current branch, and the state of the user's files in the directory with the mapping to their commits*)
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