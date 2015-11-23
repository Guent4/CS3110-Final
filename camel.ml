open Str
open Coconuts

let lex s =
  let whitespace_char_string = String.concat "+"
    (List.map (String.make 1)
       [
         Char.chr 9;  (* HT *)
         Char.chr 10; (* LF *)
         Char.chr 11; (* VT *)
         Char.chr 12; (* FF *)
         Char.chr 13; (* CR *)
         Char.chr 32; (* space *)
       ]) in
  let whitespace = "[" ^ whitespace_char_string ^ "]+" in
  Str.split (Str.regexp whitespace) s

let translate_cmd cmd_string = match cmd_string with
  | "push" -> PUSH
  | "pull" -> PULL
  | "add" -> ADD
  | "commit" -> COMMIT
  | "branch" -> BRANCH
  | "checkout" -> CHECKOUT
  | "merge" -> MERGE
  | "diff" -> DIFF
  | "status" -> STATUS
  | "config" -> CONFIG
  | "help" -> HELP
  | "clone" -> CLONE
  | "init" -> INIT
  | "log" -> LOG
  | "reset" -> RESET
  | "quit" -> ignore(exit 0); QUIT
  | s -> INVALID_CMD s

let translate_opt opt_string = match opt_string with
  | "-m" | "--message" -> MSG
  | "-a" | "--ALL" -> ALL
  | s -> INVALID_OPT s

let parse_cmd cmd_elmt : cmd =
  translate_cmd cmd_elmt

let check_if_opt s =
  if (String.length s) <= 0 then failwith "invalid string"
  else if (String.get s 0 = Char.chr 45) then true
  else false

let rec parse_opt_rec cmd_string cmd_list acc =
  match cmd_list with
  | [] -> (acc, cmd_list)
  | h::t ->
    if (check_if_opt h) then
      let acc = acc@[translate_opt h] in
      parse_opt_rec cmd_string t acc
    else
      (acc, cmd_list)

let parse_opt cmd_string opt_list : opt list * string list =
  parse_opt_rec cmd_string opt_list []

let parse_arg cmd_string arg_list : arg list = []

let check_cmd_expr cmd_string (h:host) (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  Some (h,c,o,a)

let parse cmd_string cmd_list = match cmd_list with
  | [] -> None
  | cmd_elmt::opt_list ->
    let cmd = parse_cmd cmd_elmt in
    let (opts,arg_list) = parse_opt cmd_string opt_list in
    let args = parse_arg cmd_string arg_list in
    check_cmd_expr cmd_string LOCAL cmd opts args








let rec read () =
  Printf.printf ">>> OASys ";
  (String.trim (read_line()))


let interpret input =
  let lexed = lex input in
  (* List.iter (fun x -> Printf.printf "%s\n" x) lexed; *)
  parse input lexed

let read_interpret () =
  interpret (read ())

let output x =
  Printf.printf "%s\n" x

