open Camelrider
open Coconuts
open Async.Std

let _ =
  (start 8765);
  ignore(send_request_to_server {host="localhost";port=8765;data="";cmd="VERIFY"});
  Scheduler.go()