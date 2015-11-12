open Async.Std
open Cameljson

type request

val send_request_to_server : request -> json Deferred.t