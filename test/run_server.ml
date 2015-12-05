open Camelrider
open Async.Std

let _ =
  (start 8765);
  Scheduler.go()