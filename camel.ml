open Str
open Coconuts

let lex (s:string) : string list =
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

let translate_cmd (cmd_string:string) : cmd =
  match cmd_string with
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

let detranslate_cmd (cmd:cmd) : string =
  match cmd with
  | PUSH -> "push"
  | PULL -> "pull"
  | ADD -> "add"
  | COMMIT -> "commit"
  | BRANCH -> "branch"
  | CHECKOUT -> "checkout"
  | MERGE -> "merge"
  | DIFF -> "diff"
  | STATUS -> "status"
  | CONFIG -> "config"
  | HELP -> "help"
  | CLONE -> "clone"
  | INIT -> "init"
  | LOG -> "log"
  | RESET -> "reset"
  | QUIT -> "quit"
  | INVALID_CMD s -> "s"

let translate_opt (opt_string:string) : opt =
  match opt_string with
  | "-m" | "--message"                  -> MSG
  | "-a" | "--all" | "."                -> ALL
  | "-u" | "--set-upstream"             -> SETUPSTREAM
  | "-d" | "--delete"                   -> DELETE
  | "-rm" | "--remove"                  -> REMOVE
  | "-rn" | "--rename"                  -> RENAME
  | "-b"                                -> NEWBRANCH
  | s                                   -> INVALID_OPT s

let parse_cmd (cmd_elmt:string) : cmd =
  translate_cmd cmd_elmt

let check_if_start_with (s:string) (n:int) : bool =
  if (String.length s) <= 0 then failwith "invalid string"
  else if (String.get s 0 = Char.chr n) then true
  else false

let check_if_end_with (s:string) (n:int) : bool =
  if (String.length s) <= 0 then failwith "invalid string"
  else if (String.get s (String.length s - 1) = Char.chr n) then true
  else false

let parse_opt (cmd_string:string) (opt_list:string list) : opt list * string list =
  let rec parse_opt_rec cmd_string cmd_list acc =
    match cmd_list with
    | [] -> (acc, cmd_list)
    | h::t -> (
      if ((check_if_start_with h 45) || (check_if_start_with h 46)) then
        let acc = acc@[translate_opt h] in
        parse_opt_rec cmd_string t acc
      else
        (acc, cmd_list)) in
  parse_opt_rec cmd_string opt_list []

let parse_sentence (arg_list:string list) : arg * string list =
  let rec parse_sentence_rec arg_list acc = match arg_list with
    | [] -> (INVALID_ARG "Invalid argument.", [])
    | h::t ->
      if (check_if_end_with h 34) then (SENTENCE (String.trim (acc^" "^h)), t)
      else parse_sentence_rec t (acc^" "^h) in
  parse_sentence_rec arg_list ""

let parse_arg (cmd_string:string) (arg_list: string list) : arg list =
  let rec parse_arg_rec cmd_string arg_list acc =
  match arg_list with
  | [] -> acc
  | h::t ->
    if (check_if_start_with h 34) then
      let (sent,rest_of_arg_list) = parse_sentence arg_list in
      let newacc = acc@[sent] in
      parse_arg_rec cmd_string rest_of_arg_list newacc
    else (
      let newacc = acc@[(WORD h)] in
      parse_arg_rec cmd_string t newacc)
  in
  parse_arg_rec cmd_string arg_list []

let print_args args =
  List.iter (fun x -> match x with
    | SENTENCE y -> Printf.printf "sentence %s %i\n" y (String.length y)
    | WORD y -> Printf.printf "word %s %i\n" y (String.length y)
    | INVALID_ARG _ -> print_endline "Invalid arg"
  ) args

let check_opt_arg (cmd_string:string) (h:host) (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match o with
    | [] -> Some (h, c, o, a)
    | f::[] -> (match f with
      | INVALID_OPT x -> Printf.printf "FAILURE: \"%s\" is an invalid option\n" x; None
      | _ -> Some (h, c, o, a))
    | f::r -> Printf.printf "FAILURE: The %s command does not support more than 1 option.\n" (detranslate_cmd c); None

let check_cmd_expr (cmd_string:string) (h:host) (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match c with
  | INVALID_CMD x -> Printf.printf "FAILURE: Invalid command given: \"%s\".\n" x; None
  | _ -> check_opt_arg cmd_string h c o a

let parse (cmd_string:string) (cmd_list:string list) : cmd_expr option =
  match cmd_list with
  | [] -> None
  | cmd_elmt::opt_list ->
    let cmd = parse_cmd cmd_elmt in
    let (opts,arg_list) = parse_opt cmd_string opt_list in
    let args = parse_arg cmd_string arg_list in
    check_cmd_expr cmd_string LOCAL cmd opts args

let rec read () : string =
  Printf.printf ">>> OASys ";
  (String.trim (read_line()))


let interpret (input:string) : cmd_expr option =
  let lexed = lex input in
  (* List.iter (fun x -> Printf.printf "%s\n" x) lexed; *)
  parse input lexed

let rec read_interpret () : cmd_expr =
  match interpret (read ()) with
  | None -> read_interpret ()
  | Some x -> x

let output x =
  match x with
  | Success s -> Printf.printf "%s\n" s
  | Failure s -> Printf.printf "%s\n" s

