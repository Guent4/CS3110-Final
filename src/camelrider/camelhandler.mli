open Async.Std

(* Starts the server on port int *)
val start : int -> unit

(* Stops the server, output becomes determined when server is closed*)
val stop : unit -> unit Deferred.t