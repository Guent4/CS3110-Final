open Core.Std
open Async.Std

(* Copy data from the reader to the writer, using the provided buffer
   as scratch space *)
(* let run ~uppercase ~port =
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

let () =
  Command.async_basic
    ~summary:"Start an echo server"
    Command.Spec.(
      empty
      +> flag "-uppercase" no_arg
        ~doc:" Convert to uppercase before echoing back"
      +> flag "-port" (optional_with_default 8765 int)
        ~doc:" Port to listen on (default 8765)"
    )
    (fun uppercase port () -> run ~uppercase ~port)
  |> Command.run *)
let server = Ivar.create()

let start host port =
  let run ~uppercase ~port =
    let host_and_port =
      Tcp.Server.create
        ~on_handler_error:`Raise
        (Tcp.on_port port)
        (fun _addr r w ->
          Pipe.transfer (Reader.pipe r) (Writer.pipe w)
             ~f:(if uppercase then String.uppercase else Fn.id))
    in
    (Ivar.fill server host_and_port);
    Deferred.never() in
    Command.async_basic
      ~summary:"Start an echo server"
      Command.Spec.(
        empty
        +> flag "-uppercase" no_arg
          ~doc:" Convert to uppercase before echoing back"
        +> flag "-port" (optional_with_default 8765 int)
          ~doc:" Port to listen on (default 8765)"
      )
      (fun uppercase port () -> run ~uppercase ~port)
    |> Command.run



let host_and_port =
      Tcp.Server.create
        ~on_handler_error:`Raise
        (Tcp.on_port 8765)
        (fun _addr r w ->
          Pipe.transfer (Reader.pipe r) (Writer.pipe w)
Fn.id);;

let f x = x >>= fun x -> Tcp.Server.close x;;
