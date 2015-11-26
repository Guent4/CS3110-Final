open Core.Std
open Async.Std
open Cameljson
(* open Coconuts *)

(* will have to send header with command *)
let send_request_to_server request =
  (* Connect to server, send request, get output, deserialize *)
  let host_and_port = Tcp.to_host_and_port request.host request.port in
  (Tcp.connect host_and_port) >>= fun (_addr,r,w) ->
  (* write request to server *)
  Writer.write w request.file;
  (*read response from server *)
  let buf = String.create(1024) in
  (Reader.really_read r buf) >>= fun response ->
  Writer.close writer >>= fun () ->
  Reader.close reader >>| fun () ->
  (*deserialize*)
  Cameljson.deserialize response