open Types
open Core.Std (*In_channel and Out_channel modules*)

(**
 * [read filename] simulates an In_channel that reads the lines from [filename].
 *
 *)
val read : string -> string list

(**
 * [write filename lines] simulates an Out_channel that "overwrites" the
 * existing [filename] with [lines]
 *
 *)
val write : string -> string list -> unit