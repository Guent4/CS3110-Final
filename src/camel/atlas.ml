open Coconuts


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
  | "-f" | "--file"             -> FILE
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
  | FILE            -> "-f or --file"
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
      ((BRANCH,EMPTY),[-2]);
      ((RESET,FILE),[-1]);    ((RESET,BNCH),[1]);
      ((RM,BNCH),[-1]);       ((RM,FILE),[-1]);
      ((CHECKOUT,EMPTY),[1]);
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

  let print_error ?s1:(s1="") ?s2:(s2="") ?i1:(i1=0) ?i2:(i2=0) ?i3:(i3=0) ?i4:(i4=0) = function
  | 0 -> Printf.printf "\nWelcome to OASys!\nCreated by Gu, Ho, Moheed, and Ramalingam\n\nFor help, refer to the readme.txt file in the folder\nTo search up info about a specific command, type \"help --cmd command_name\".\nTo search up more general topics, type \"help search_word(s)\".\n\nFor more questions, contact ____\n\n"
  | 1 -> Printf.printf "FAILURE: \"%s\" is an invalid option.\n" s1
  | 2 -> Printf.printf "FAILURE: The \"%s\" command does not support more than 1 option.\n" s1
  | 4 -> Printf.printf "FAILURE: Invalid command given: \"%s\".\n" s1
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