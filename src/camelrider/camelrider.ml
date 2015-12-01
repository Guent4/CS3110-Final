open Coconuts
open Core.Std

(* Constructs an HTTP request to send to a server and returns a bool
 * indicating request success and a string option with potential data *)
let send_request_to_server req =
  match req.meth with
  | "POST" ->
    let req_string = "curl --data \""^req.data^"\" -H\"Content-Type:text/plain\" "^"-X "^req.meth^" "^req.host^":"^req.port^"/?cmd="^req.cmd in
    let _ = Sys.command(req_string^" > server.res") in
    let res = Fileio.read_str "server.res" in
    (true,Some res)
  | "GET" ->
    let req_string = "curl "^req.host^":"^req.port^"/?cmd="^req.cmd in
    let _ = Sys.command(req_string^" > server.res") in
    let res = Fileio.read_str "server.res" in
    (true,Some res)
  | _ -> (false,None)

let ping domain =
  Sys.command("curl " ^ domain)