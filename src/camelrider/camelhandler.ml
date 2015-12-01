open Core.Std
open Async.Std
open Cohttp
open Cohttp_async

(* Ref containing server handle *)
let server = ref(Ivar.create())

(*         Cohttp_async.Body.t =
           [ `Empty
           | `Pipe of string Async.Std.Pipe.Reader.t
           | `String of string
           | `Strings of string list ]
*)
let handle_request ~(body)
                   (client_addr : Socket.Address.Inet.t)
                   (request : Request.t) : Server.response Deferred.t =
  let open Request in
  let cmd = match Uri.get_query_param (request.uri) "cmd" with
            | Some s -> s
            | None -> "NO COMMAND" in
  match request.meth with
    | `GET -> (* if cmd = "VERIFY" then
                Body.to_string body >>= fun body' ->
                Server.respond_with_string "hello"
                match Palmtreeupdater.handle_request (cmd,None) with
                | (true,_) -> Server.respond (Code.status_of_code 200)
                | _ -> Server.respond (Code.status_of_code 401)
              else  *)
              if cmd = "PULL" then
                Body.to_string body >>= fun body' ->
                Server.respond_with_string "Hello"
(*                 match Palmtreeupdater.handle_request (cmd,None) with
                | (true,Some data) -> Server.respond_with_string data
                | _ -> Server.respond (Code.status_of_code 400) *)
              else
                Server.respond_with_string "Bad " ~code: `Bad_request
    | `POST ->  if cmd = "PUBLICKEY" then
                  let () = Printf.printf "%s" "here" in
                  Body.to_string body >>= fun body' ->
                  let () = Printf.printf "%s" "here updating?" in
(
                  let () = Fileio.write_str "./remote/buffer" body' in
                  let _ = Sys.command("cat remote/buffer >> ~/.ssh/authorized_keys") in
                  let _ = Sys.command("chmod 755 ~/.ssh") in
                  let _ = Sys.command("chmod 644 ~/.ssh/authorized_keys") in
                  let () = Printf.printf "%s" "and here" in
                  Server.respond_with_string "success")
                else if cmd = "PUSH" then
                let () = Print.printf "%s" "Hello world" in
                  Body.to_string body >>= fun body' ->
                  Server.respond_with_string "Hello"
                    (* Body.to_string body >>= fun body' ->
                    match Palmtreeupdater.handle_request (cmd,Some body') with
                    | (true,_) -> Server.respond (Code.status_of_code 200)
                    | _ -> Server.respond (Code.status_of_code 400) *)
                else
                  Server.respond_with_string "" ~code: `Bad_request
    | _ -> Server.respond_with_string "" ~code: `Bad_request

(* Starts the server on port p *)
let start p =
    let host_and_port = Server.create (Tcp.on_port p) handle_request in
    (Ivar.fill_if_empty !server host_and_port)

(* Stops the server *)
let stop () =
  if Ivar.is_full !server then
    Ivar.read !server >>= fun s ->
    s >>= fun s ->
    (server:= (Ivar.create ()));
    Server.close s
  else
    failwith "No server to close"

let pause () = after (Core.Std.sec 3.) >>| fun () -> ()

let rec while_pause x =
  ignore(pause());
  match Deferred.peek x with
  | Some y -> y
  | None -> while_pause x

let _ = start (6700)

let _ = Scheduler.go()