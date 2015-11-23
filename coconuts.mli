(* Describe which repository, local or remote, that the user is referring to *)
type host

(* Commands that OASys can support *)
type cmd

(* A single argument for the command (i.e. "commit message" in
 *  "git commit -m "commit message"", repository name, )
 *)
type arg

(* All of the possible options that will be supported (i.e "-a" in
 *  "git add -a")
 *)
type opt

(* This is the type that the user input will be parsed into.  The actual commands
 *  described in the cmd_expr will be executed later in OASys
 *)
type cmd_expr

type config

type feedback

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