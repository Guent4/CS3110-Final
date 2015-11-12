open Async.Std
open Types

(* Represents a client request. Contains a command and client information *)
type request

(* Sends a request to server which will return a JSON *)
val send_request_to_server : request -> json Deferred.t