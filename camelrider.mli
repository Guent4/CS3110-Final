open Async.Std
open Coconuts

(* Takes in an http request, manipulates data based on the request, and returns
 * an http response *)
val handle_request : string -> string

(* Starts the server *)
val start : string -> int -> unit

(* Stops the server *)
val stop : unit -> unit

(* Constructs an HTTP request to send to a server and returns a bool
 * indicating request success and a string option with potential data *)
val send_request_to_server : request -> bool * string option