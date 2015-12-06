(*open Types*)
open Core.Std (*In_channel and Out_channel modules*)

(**
 * [read filename] simulates an In_channel that reads the lines from [filename].
 *)
val read_list : string -> string list

(**
 * [write filename lines] simulates an Out_channel that "overwrites" the
 * existing [filename] with [lines]
 *)
val write_list : string -> string list -> unit

(* read a file and get all of its content in the form of a long string *)
val read_str : string -> string

(* write a long string content (latter string) to a file with filename (former string) *)
val write_str : string -> string -> unit

(*returns true if file exists at path *)
val file_exists : string -> bool

(*returns file names of files at path *)
val files_in_dir : string -> string list

(*copies a file from one directory to another, arguments in that order *)
val copy_file : string -> string -> string -> unit

(*creates a file directory at path *)
val create_dir : string -> unit

(*removes a file directory at path *)
val remove_dir : string -> unit

(*removes a file at path*)
val remove_file : string -> unit

(* Three way merge.
 * Arguments: name of ancestor node, name of other node, your node,
              ancestor file, other file, your file,
              result file path, in that order. *)
val merge_files : string -> string -> string -> string -> string -> string -> string -> unit