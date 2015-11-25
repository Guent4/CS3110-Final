open Str
open Coconuts

type arg_t = WORD of string | SENTENCE of string | INVALID_ARG

type cmd_expr_t = host * cmd * opt list * arg_t list

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
  | "init"          -> INIT
  | "log"           -> LOG
  | "status"        -> STATUS
  | "add"           -> ADD
  | "commit"        -> COMMIT
  | "branch"        -> BRANCH
  | "checkout"      -> CHECKOUT
  | "reset"         -> RESET
  | "rm"            -> RM
  | "diff"          -> DIFF
  | "merge"         -> MERGE
  | "config"        -> CONFIG
  | "push"          -> PUSH
  | "pull"          -> PULL
  | "clone"         -> CLONE
  | "help"          -> HELP
  | "quit"          -> ignore(exit 0); QUIT
  | s               -> INVALID_CMD s

let detranslate_cmd (cmd:cmd) : string =
  match cmd with
  | INIT            -> "init"
  | LOG             -> "log"
  | STATUS          -> "status"
  | ADD             -> "add"
  | COMMIT          -> "commit"
  | BRANCH          -> "branch"
  | CHECKOUT        -> "checkout"
  | RESET           -> "reset"
  | RM              -> "rm"
  | DIFF            -> "diff"
  | MERGE           -> "merge"
  | CONFIG          -> "config"
  | PUSH            -> "push"
  | PULL            -> "pull"
  | CLONE           -> "clone"
  | HELP            -> "help"
  | QUIT            -> "quit"
  | INVALID_CMD s   -> s

let translate_opt (opt_string:string) : opt =
  match opt_string with
  | "-m" | "--message"          -> MSG
  | "-a" | "--all" | "."        -> ALL
  | "-u" | "--set-upstream"     -> SETUPSTREAM
  | "-d" | "--delete"           -> DELETE
  | "-rm" | "--remove"          -> REMOVE
  | "-rn" | "--rename"          -> RENAME
  | "-b" | "--branch"           -> BNCH
  | "--file"                    -> FILE
  | ""                          -> EMPTY
  | s                           -> INVALID_OPT s

let detranslate_opt (opt:opt) : string =
  match opt with
  | MSG             -> "-m or --message"
  | ALL             -> ". or -a or --all"
  | SETUPSTREAM     -> "-u or --set-upstream"
  | DELETE          -> "-d or --delete"
  | REMOVE          -> "-rm or --remove"
  | RENAME          -> "-rn or --rename"
  | BNCH            -> "-b or --branch"
  | FILE            -> "--file"
  | EMPTY           -> "<no options given>"
  | INVALID_OPT s   -> "s"

module M = Map.Make (struct type t = (cmd * opt) let compare a b = Pervasives.compare a b end)
let expected_arg_num_list =
  [ ((INIT,EMPTY),([0],[0]));
    ((LOG,EMPTY),([0],[0]));
    ((STATUS,EMPTY),([0],[0]));
    ((ADD,EMPTY),([-1],[0]));    ((ADD,ALL),([0],[0]));
    ((COMMIT,MSG),([0],[1]));
    ((BRANCH,EMPTY),([1],[0]));
    ((RESET,EMPTY),([1],[0]));
    ((RM,BNCH),([-1],[0]));      ((RM,FILE),([-1],[0]));
    ((DIFF,EMPTY),([0],[0]));    ((DIFF,FILE),([2],[0]));  ((DIFF,BNCH),([0;2],[0]));
    ((PUSH,EMPTY),([0],[0]))
  ]
let expected_arg_num = List.fold_left (fun acc x -> match x with | (x,y) -> M.add x y acc)
  M.empty expected_arg_num_list

let print_error ?s1:(s1="") ?s2:(s2="") ?i1:(i1=0) ?i2:(i2=0) ?i3:(i3=0) ?i4:(i4=0) = function
  | 1 -> Printf.printf "FAILURE: \"%s\" is an invalid option.\n" s1
  | 2 -> Printf.printf "FAILURE: The \"%s\" command does not support more than 1 option.\n" s1
  | 4 -> Printf.printf "FAILURE: Invalid command given: \"%s\".\n" s1
  | 5 -> Printf.printf "FAILURE: Option \"%s\" is not supported for \"%s\".\n" s1 s2
  | 6 -> Printf.printf "FAILURE: No arguments were given when %s word and %s string argument(s) were expected.\n" s1 s2
  | 7 -> Printf.printf "FAILURE: %i word and %i string argument(s) were given when %s word and %s string argument(s) were expected.\n" i1 i2 s1 s2
  | 8 -> Printf.printf "FAILURE: One or more arguments are not in the correct format. Try checking your '\"' placement.\n"
  | _ -> Printf.printf "\n"

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

let parse_sentence (arg_list:string list) : arg_t * string list =
  let rec parse_sentence_rec arg_list acc = match arg_list with
    | [] -> (INVALID_ARG, [])
    | h::t ->
      if (check_if_end_with h 34) then (SENTENCE (String.trim (acc^" "^h)), t)
      else parse_sentence_rec t (acc^" "^h) in
  parse_sentence_rec arg_list ""

let parse_arg (cmd_string:string) (arg_list: string list) : arg_t list =
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
    | INVALID_ARG -> print_endline "Invalid arg"
  ) args

let get_word_sent_num (a:arg_t list) : (int * int) option =
  let rec get_word_sent_num_rec wso a = (match wso with
    | Some (w,s) -> (match a with
      | [] -> wso
      | (WORD _)::e -> get_word_sent_num_rec (Some(w+1,s)) e
      | (SENTENCE _)::e -> get_word_sent_num_rec (Some(w,s+1)) e
      | (INVALID_ARG)::e -> print_error 8; None)
    | None -> None) in
  get_word_sent_num_rec (Some (0,0)) a
  (* let w = List.fold_left (fun acc x -> match x with
    | WORD _ -> acc + 1
    | _ -> acc) 0 a in
  let s = (List.length a) - w in
  (w,s) *)

let get_string_num_opts (os:int list) : string =
  let with_space_in_front =
    if (List.mem (-2) os) then " any number of"
    else if (List.mem (-1) os) then " one or more"
    else List.fold_left (fun acc x -> acc ^ " " ^ string_of_int x) "" os in
  String.sub with_space_in_front 1 (String.length with_space_in_front - 1)

let check_args (cmd_string:string) (h:host) (c:cmd) (o:opt list) (a:arg_t list) : cmd_expr option =
  match o with
  | [] -> failwith "No option given"
  | f::e -> (
    if (M.mem (c,f) expected_arg_num) then
      let (e_ws,e_ss) = M.find (c,f) expected_arg_num in
      match get_word_sent_num a with
      | None -> None
      | Some (a_w,a_s) -> (
        let expected_num_w = get_string_num_opts e_ws in
        let expected_num_s = get_string_num_opts e_ss in
        (if (((List.mem (-1) e_ws) && (a_w = 0)) || ((List.mem (-1) e_ss) && (a_s = 0)))
          then (print_error 6 ~s1:expected_num_w ~s2:expected_num_s; None)
        else if (not ((List.mem (-1) e_ws || List.mem a_w e_ws) && (List.mem (-1) e_ss || List.mem a_s e_ss)))
          then (print_error 7 ~i1:a_w ~i2:a_s ~s1:expected_num_w ~s2:expected_num_s; None)
        else (
          let args = List.fold_left (fun acc x -> match x with
            | WORD x-> acc@[x]
            | SENTENCE x -> acc@[x]
            | INVALID_ARG -> failwith "INVALID_ARG should've been sorted out already"
          ) [] a in
          Some (h,c,o,args))))
    else (print_error 5 ~s1:(detranslate_opt f) ~s2:(detranslate_cmd c); None))

let check_opt (cmd_string:string) (h:host) (c:cmd) (o:opt list) (a:arg_t list) : cmd_expr option =
  match o with
    | [] -> check_args cmd_string h c [EMPTY] a
    | f::[] -> (match f with
      | INVALID_OPT x -> print_error 1 ~s1:x; None
      | _ -> check_args cmd_string h c o a
      )
    | f::r -> print_error 2 ~s1:(detranslate_cmd c); None

let check_cmd_expr_t (cmd_string:string) (h:host) (c:cmd) (o:opt list) (a:arg_t list) : cmd_expr option =
  match c with
  | INVALID_CMD x -> print_error 4 ~s1:x; None
  | _ -> check_opt cmd_string h c o a

let rec read () : string =
  Printf.printf ">>> OASys ";
  (String.trim (read_line()))

let parse (cmd_string:string) (cmd_list:string list) : cmd_expr option =
  match cmd_list with
  | [] -> None
  | cmd_elmt::opt_list ->
    let cmd = parse_cmd cmd_elmt in
    let (opts,arg_list) = parse_opt cmd_string opt_list in
    let args = parse_arg cmd_string arg_list in
    check_cmd_expr_t cmd_string LOCAL cmd opts args

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

