open Async.Std
open Types

(* Takes in a client's request and returns a JSON based on the input *)
val handle_request : request -> json Deferred.t

(* Starts the server *)
val start : string -> int -> unit

(* Stops the server *)
val stop : unit -> unit