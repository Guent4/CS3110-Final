open Core.Std
open Async.Std
(* open Cameljson *)
(* open Coconuts *)

let server = ref(Ivar.create())

let handle_request request =
  failwith "unimplemented"


let start host port =
    let host_and_port =
      Tcp.Server.create
        ~on_handler_error:`Raise
        (Tcp.on_port port)
        (fun _addr r w ->
          Pipe.transfer (Reader.pipe r) (Writer.pipe w)
          (*deserialize, manipulate tree, serialize and send back.*)
          fun s -> (Cameljson.deserialize s) |>
                   (Palmtreeupdater.update ???) |>
                   Cameljson.serialize
             (* handle_request *))
    in
    (Ivar.fill_if_empty !server host_and_port)


let stop () =
  if Ivar.is_full !server then
    ignore(
    (Ivar.read !server >>= fun s ->
    s >>= fun s ->
    ignore(Tcp.Server.close s);
    return (server:= (Ivar.create ()))))
  else
    failwith "No server to close"

(* COMMAND LINE TESTING *)

let run ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _addr r w ->
        Pipe.transfer (Reader.pipe r) (Writer.pipe w)
           ~f:(if uppercase then String.uppercase else Fn.id))
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()
