open Coconuts

(* Constructs an HTTP request to send to a server and returns a bool
 * indicating request success and a string option with potential data *)
val send_request_to_server : client_req -> (bool * string option)

val ping : string -> int