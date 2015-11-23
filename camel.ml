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

let parse_cmd cmd_elmt : cmd = match cmd_elmt with
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


let rec parse_opt_rec cmd_string cmd_list acc =
  match cmd_list with
  | [] -> (acc, cmd_list)
  | h::t -> (acc, cmd_list)

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

