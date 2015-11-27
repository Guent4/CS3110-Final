(* open Str *)
open Coconuts
open Fileio

let doc_loc = "./OASys_Doc.txt"
let help_loc = "./OASys_Help.txt"

let cmd_str_list = ["init";"log";"status";"add";"commit";"branch";"checkout";
  "reset";"rm";"diff";"merge";"config";"push";"pull";"clone";"help";"quit"]

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

let opt_str_list = ["--message";"--all";"--set-upstream";"--delete";"--remove";
  "--rename";"--branch";"--file"]

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
  | "--cmd"                     -> CMD
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
  | CMD             -> "--cmd"
  | EMPTY           -> "<no options given>"
  | INVALID_OPT s   -> "s"

module M = Map.Make (struct type t = (cmd * opt) let compare a b = Pervasives.compare a b end)
let arg_num_expected =
  let lst =
    [ ((INIT,EMPTY),[0]);
      ((LOG,EMPTY),[0]);
      ((STATUS,EMPTY),[0]);
      ((ADD,EMPTY),[-1]);     ((ADD,ALL),[0]);
      ((COMMIT,MSG),[1]);
      ((BRANCH,EMPTY),[1]);
      ((RESET,FILE),[-1]);    ((RESET,BNCH),[1]);
      ((RM,BNCH),[-1]);       ((RM,FILE),[-1]);
      ((DIFF,EMPTY),[0]);     ((DIFF,FILE),[2]);  ((DIFF,BNCH),[0;2]);
      ((PUSH,EMPTY),[0]);
      ((HELP,EMPTY),[-2]);    ((HELP,CMD),[1])
    ] in
  List.fold_left (fun acc x -> match x with | (x,y) -> M.add x y acc) M.empty lst

let arg_num_default =
  let lst =
    [ ((INIT,EMPTY),EMPTY);
      ((LOG,EMPTY),EMPTY);
      ((STATUS,EMPTY),EMPTY);
      ((ADD,EMPTY),EMPTY);
      ((COMMIT,EMPTY),EMPTY);
      ((BRANCH,EMPTY),EMPTY);
      ((CHECKOUT,EMPTY),EMPTY);
      ((RESET,EMPTY),FILE);
      ((RM,EMPTY),FILE);
      ((DIFF,EMPTY),EMPTY);
      ((MERGE,EMPTY),EMPTY);
      ((CONFIG,EMPTY),EMPTY);
      ((PUSH,EMPTY),EMPTY);
      ((PULL,EMPTY),EMPTY);
      ((CLONE,EMPTY),EMPTY);
      ((HELP,EMPTY),EMPTY)
    ] in
  List.fold_left (fun acc x -> match x with | (x,y) -> M.add x y acc) M.empty lst


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
  let whitespace = "[" ^ whitespace_char_string ^ "]" in
  Str.split (Str.regexp whitespace) (String.trim s)

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let calc_lev str1 str2 =
  let minimum a b c = min (min a b) c in
  let str1c n = String.get str1 n in
  let str2c n = String.get str2 n in
  let x = String.length str1 in
  let y = String.length str2 in
  let m = Array.make_matrix (x+1) (y+1) 0 in
  let rec start_m (i,j) =
    if (i = x && j = y) then (m.(x).(0) <- x; m.(0).(y) <- y)
    else if (i = x) then (m.(0).(j) <- j; start_m (i,j+1))
    else if (j = y) then (m.(i).(0) <- i; start_m (i+1,j))
    else (m.(i).(0) <- i; m.(0).(j) <- j; start_m (i+1,j+1))
  in start_m (0,0);
  for i = 1 to x do
    for j = 1 to y do
      let l = (m.(i-1).(j) + 1) in
      let t = (m.(i).(j-1) + 1) in
      let d = m.(i-1).(j-1) +
        match (str1c (i-1)=str2c (j-1)) with true -> 0 | false -> 1 in
      m.(i).(j) <- minimum l t d
    done
  done;
  (* Array.iter (fun x -> Array.iter (fun y -> Printf.printf "%i\t" y) x; print_endline "") m; *)
  m.(x).(y)

let print_sugg (input:string) (dict:string list) =
  let sugg = List.filter (fun x -> calc_lev input x <= 2) dict in
  if (List.length sugg > 0) then (print_endline "\t Do you happen to mean:";
      List.iter (fun x -> Printf.printf "\t\t%s\n" x) sugg)
  else ()

let print_error ?s1:(s1="") ?s2:(s2="") ?i1:(i1=0) ?i2:(i2=0) ?i3:(i3=0) ?i4:(i4=0) = function
  | 0 -> Printf.printf "\nWelcome to OASys!\nCreated by Gu, Ho, Moheed, and Ramalingam\n\nFor help, refer to the readme.txt file in the folder\nTo search up info about a specific command, type \"help --cmd command_name\".\nTo search up more general topics, type \"help search_word(s)\".\n\nFor more questions, contact ____\n\n"
  | 1 -> Printf.printf "FAILURE: \"%s\" is an invalid option.\n" s1; print_sugg s1 opt_str_list
  | 2 -> Printf.printf "FAILURE: The \"%s\" command does not support more than 1 option.\n" s1
  | 4 -> Printf.printf "FAILURE: Invalid command given: \"%s\".\n" s1; print_sugg s1 cmd_str_list
  | 5 -> Printf.printf "FAILURE: Option \"%s\" is not supported for \"%s\".\n" s1 s2
  | 6 -> Printf.printf "FAILURE: No arguments were given when %s argument(s) was expected.\n" s1
  | 7 -> Printf.printf "FAILURE: %i argument(s) were given when %s argument(s) was expected.\n" i1 s1
  | 8 -> Printf.printf "FAILURE: Did not enter a command to search.\n"
  | 9 -> Printf.printf "FAILURE: Searching for too many commands.  Please reduce to only one command.\n"
  | 10 -> Printf.printf "FAILURE: \"%s\" is not a command; command-search cannot be completed.\nTry general-search without \"--cmd\" option.\n" s1
  | 11 -> Printf.printf "UNSUCCESSFUL: Cannot find the command \"%s\" in the documentation.\n" s1
  | 12 -> Printf.printf "UNSUCCESSFUL: Cannot find search in the documentation\n"
  | 13 -> Printf.printf "FAILURE: Input was not a number.\n"
  | 14 -> Printf.printf "UNSUCCESSFUL: Apologies. Unable to find topic in documentation.\n"
  | _ -> Printf.printf "\n"

let search_for_topic (doc:string) (inputs:arg list) : string list =
  if (List.length inputs = 0) then (print_error 0; [])
  else (
    let inputs = List.map (fun x -> String.uppercase x) inputs in
    let desired = List.fold_left (fun acc x -> acc^x^"\\|") "" inputs in
    let desired' = String.sub desired 0 (String.length desired - 2) in
    let regex = "<.*"^desired'^".*>" in
    let rec find_topics acc start =
      (try (
        let start' = (Str.search_forward (Str.regexp regex) doc start) + 1 in
        (find_topics ((Str.matched_string doc)::acc) start'))
      with
        Not_found -> acc) in
    find_topics [] 0)

let help_empty (a_s:arg list) : unit =
  let doc = Fileio.read_str help_loc in
  let topics = search_for_topic doc a_s in
  if (List.length topics = 0) then print_error 2
  else (
    print_endline "These are the search result. Please enter the number corresponding to interest:\n\t0: Quit";
    ignore (List.fold_left (fun a x -> print_endline ("\t"^(string_of_int a)^": "^x); a+1) 1 topics);
    let sel = read_line() in
    if (is_int sel) then
      let sel_num = int_of_string sel in
      if (sel_num = 0) then ()
      else (
        let sel_name = List.nth topics (sel_num - 1) in
        let regex = sel_name^"\\(.*\\(\n\t\\)*\\)*" in
        (try (
          ignore(Str.search_forward (Str.regexp regex) doc 0);
          print_endline (Str.matched_string doc))
        with
          Not_found -> print_error 14); ()
      )
    else print_error 13
  )

let help_cmd (a_s:arg list) : unit =
  if (List.length a_s = 0)
    then print_error 8
  else if (List.length a_s > 1)
    then print_error 9
  else if (not (List.mem (List.hd a_s) cmd_str_list))
    then print_error 10 ~s1:(List.hd a_s)
  else (
    (* List.iter (fun x -> print_endline x) (Fileio.files_in_dir doc_loc) *)
    let doc = Fileio.read_str doc_loc in
    let desired = String.uppercase (List.hd a_s) in
    let regex = "<"^desired^">\\(.*\\(\n\t\\)*\\)*" in
    (try (
      ignore(Str.search_forward (Str.regexp regex) doc 0);
      print_endline (Str.matched_string doc))
    with
      Not_found -> print_error 11 ~s1:desired); ()
  )

let offer_help (expr:cmd_expr option) : unit =
  match expr with
  | Some (HELP,[EMPTY],a_s) -> help_empty a_s
  | Some (HELP,[CMD],a_s) -> help_cmd a_s
  | _ -> ()

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

let parse_opt (c:cmd) (opt_list:string list) : opt list * string list =
  let rec parse_opt_rec opt_list acc =
    match opt_list with
    | [] -> (
      if (List.length acc = 0) then ([EMPTY], opt_list)
      else (acc, opt_list))
    | h::t -> (
      if (h = ".") then
        parse_opt_rec t (acc@[(translate_opt h)])
      else if (String.length h >= 3 && String.sub h 0 2 = "%-") then
        let acc = acc@[translate_opt (String.sub h 1 (String.length h - 1))] in
        parse_opt_rec t acc
      else (
        if (List.length acc = 0) then ([EMPTY], opt_list)
        else (acc, opt_list))) in
  parse_opt_rec opt_list []

let parse_arg (c: cmd) (o: opt list) (arg_list: string list) : arg list =
  match (c,o) with
  | (COMMIT,MSG::[]) -> (
    let message = String.trim (List.fold_left (fun a x -> a^" "^x) "" arg_list) in
    if (message = "") then [] else [message])
  | _ -> arg_list

let get_string_num_opts (os:int list) : string =
  let with_space_in_front =
    if (List.mem (-2) os) then " any number of"
    else if (List.mem (-1) os) then " one or more"
    else List.fold_left (fun acc x -> acc ^ " " ^ string_of_int x) "" os in
  String.sub with_space_in_front 1 (String.length with_space_in_front - 1)

let check_args (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match o with
  | [] -> failwith "No option given"
  | f::e -> (
    let f = if (M.mem (c,f) arg_num_default) then M.find (c,f) arg_num_default
      else f in
    if (M.mem (c,f) arg_num_expected) then
      let es = M.find (c,f) arg_num_expected in
      let es_str = get_string_num_opts es in
      (if (List.mem(-2) es) then (Some (c,o,a))
      else if ((List.mem (-1) es) && (List.length a = 0))
        then (print_error 6 ~s1:es_str; None)
      else if ((not (List.mem (-1) es)) && (not (List.mem (List.length a) es)))
        then (print_error 7 ~i1:(List.length a) ~s1:es_str; None)
      else (Some (c,o,a)))
    else (print_error 5 ~s1:(detranslate_opt f) ~s2:(detranslate_cmd c); None))

let check_opt (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match o with
    | [] -> check_args c [EMPTY] a
    | f::[] -> (match f with
      | INVALID_OPT x -> print_error 1 ~s1:x; None
      | _ -> check_args c o a)
    | f::r -> print_error 2 ~s1:(detranslate_cmd c); None

let check_cmd_expr (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match c with
  | INVALID_CMD x -> print_error 4 ~s1:x; None
  | _ -> check_opt c o a

let rec read () : string list  =
  (* List.iter (fun x -> print_endline x) (Array.to_list Sys.argv); *)
  match Array.to_list Sys.argv with
  | [] -> exit 0
  | h::t -> t

let interpret (cmd_list:string list) : cmd_expr option =
  match cmd_list with
  | [] -> None
  | cmd_elmt::opt_list ->
    let c = parse_cmd cmd_elmt in
    let (opts,arg_list) = parse_opt c opt_list in
    let args = parse_arg c opts arg_list in
    let expr_option = check_cmd_expr c opts args in
    offer_help expr_option; expr_option

let rec read_interpret () : cmd_expr =
  match interpret (read ()) with
  | None -> exit 0
  | Some (c,(h::[]),a) -> (c,[h],a)
  | Some _ -> failwith "other"

let output x =
  match x with
  | Success s -> Printf.printf "%s\n" s
  | Failure s -> Printf.printf "%s\n" s

