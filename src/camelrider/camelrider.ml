open Coconuts
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
    | `GET -> if cmd = "VERIFY" then
                Server.respond (Code.status_of_code 200)
                (* match Palmtreeupdater.handle_request (cmd,None) with
                | (true,_) -> Server.respond (Code.status_of_code 200)
                | _ -> Server.respond (Code.status_of_code 401) *)
              else if cmd = "PULL" then
                Server.respond (Code.status_of_code 200)
(*                 match Palmtreeupdater.handle_request (cmd,None) with
                | (true,Some data) -> Server.respond_with_string data
                | _ -> Server.respond (Code.status_of_code 400) *)
              else
                Server.respond_with_string "" ~code: `Bad_request
    | `POST ->  if cmd = "POST" then
                Server.respond (Code.status_of_code 200)
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

(* Constructs an HTTP request to send to a server and returns a bool
 * indicating request success and a string option with potential data *)
let send_request_to_server req =
  let url_str = "http://" ^ req.host ^ ":" ^(string_of_int req.port) in
  let base_uri = Uri.of_string url_str in
  let query_uri = Uri.add_query_param base_uri ("cmd", [req.cmd]) in
    (* GET request if cmd is PULL *)
    if req.cmd = "PULL" then
      (Client.get(query_uri)) >>= fun (response, body) ->
      match (Response.status response) with
      (* If successful, return true and data *)
      | `OK -> (Body.to_string body) >>= fun body' ->
               return (true, Some body')
      (* Else fail and return no data *)
      | _ -> return (false, None)
    (* GET request if cmd is VERIFY *)
    else if req.cmd = "VERIFY" then
      Client.get(query_uri) >>| fun (response, body) ->
      match (Response.status response) with
      (* If successful, return success *)
      | `OK -> (true, None)
      (* Else fail and return no data *)
      | _ -> (false, None)

    (* POST request if cmd is PUSH *)
    else if req.cmd = "PUSH" then
      (*let (r,w) = Pipe.create() in
      Pipe.write w (req.data) >>= fun () ->
      Client.post r query_uri *)

      Client.post ~body:(`String req.data) query_uri >>| fun (response,body) ->
      match (Response.status response) with
      (* If successful, return success *)
      | `OK -> (true, None)
      (* Else fail and return no data *)
      | _ -> (false, None)
    else
      return (false, None)