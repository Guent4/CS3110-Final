open Async.Std
open Cameljson

val handle_request : request -> json Deferred.t
val start : unit -> unit 
val stop : unit -> unit