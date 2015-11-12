open Async.Std
open Types

(* Sends a request to server which will return a JSON *)
val send_request_to_server : request -> json Deferred.t