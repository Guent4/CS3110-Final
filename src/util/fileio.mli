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
val read : string -> string

(* write a long string content (latter string) to a file with filename (former string) *)
val write : string -> string -> unit

(* Creates a zip file with filename string with files in the current directory *)
val zip : string -> unit

(* Unzip a zip file to current directory *)
val unzip : string -> unit

val file_exists : string -> bool

val copy_file : string -> string -> unit

val create_dir : string -> unit

val remove_dir : string -> unit