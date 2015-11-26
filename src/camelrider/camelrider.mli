open Async.Std
open Coconuts

(* Starts the server on port int *)
val start : int -> unit

(* Stops the server *)
val stop : unit -> unit

(* Constructs an HTTP request to send to a server and returns a bool
 * indicating request success and a string option with potential data *)
val send_request_to_server : client_req -> (bool * string option) Deferred.t