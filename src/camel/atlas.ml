open Coconuts

(* This ml is responsible for providing the constants that will be used in the
 * camel modules.  Along with some of the constants, some general helper
 * functions related to the constants are also included. *)




(* Location of the file that contains documentation on the commands relative to
 * the home directory *)
let doc_loc = "./OASys_Doc.txt"
(* Location of the file that contains the help articles relative to the home
 * directory  *)
let help_loc = "./OASys_Help.txt"

(* Codes for printing in color *)
let colors = [
  ("white", "\027[37m");
  ("red","\027[31m");
  ("green","\027[32m");
  ("orange","\027[33m");
  ("blue","\027[34m");
  ("cyan","\027[36m");
  ("bred","\027[41m");
  ("borange","\027[43m")]

(* List of all of the accepted commands *)
let cmd_str_list = ["init";"log";"status";"add";"commit";"branch";"checkout";
  "reset";"rm";"merge";"config";"push";"pull";"help"]

(* [translate_cmd cmd_string] - Converts string form of a command to the cmd form.
 *      If string does not correspond to a cmd, then a INVALID_CMD is returned.
 * Parameters:
 *    - cmd_string - the string that is to be converted to a cmd
 * Return: returns the cmd of the string; INVALID_CMD if cmd_string has no conversion *)
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
  | "merge"         -> MERGE
  | "config"        -> CONFIG
  | "push"          -> PUSH
  | "pull"          -> PULL
  | "help"          -> HELP
  | s               -> INVALID_CMD s

(* [detranslate_cmd cmd] - Converts a cmd into the string version of it
 * Parameters:
 *    - cmd - the cmd that is to be converted to a string
 * Return: the string version of the cmd *)
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
  | MERGE           -> "merge"
  | CONFIG          -> "config"
  | PUSH            -> "push"
  | PULL            -> "pull"
  | HELP            -> "help"
  | INVALID_CMD s   -> s

(* List of all of the accepted opt inputs *)
let opt_str_list = ["--message";"--all";"--set";"--delete";"--remove";
  "--rename";"--branch";"--file";"--hard";"--mixed";"--soft";"--add"]

(* [translate_opt opt_string] - Converts string form of a opt to the opt form.
 *      If string does not correspond to a opt, then a INVALID_OPT is returned.
 * Parameters:
 *    - opt_string - the string that is to be converted to a opt
 * Return: returns the opt of the string; INVALID_OPT if opt_string has no conversion *)
let translate_opt (opt_string:string) : opt =
  match opt_string with
  | "-m" | "--message"          -> MSG
  | "-a" | "--all" | "."        -> ALL
  | "-d" | "--delete"           -> DELETE
  | "-rm" | "--remove"          -> REMOVE
  | "-rn" | "--rename"          -> RENAME
  | "-b" | "--branch"           -> BNCH
  | "--hard"                    -> HARD
  | "--mixed"                   -> MIXED
  | "--soft"                    -> SOFT
  | "-f" | "--file"             -> FILE
  | "--cmd"                     -> CMD
  | "--set"                     -> CONFIG_SET
  | ""                          -> EMPTY
  | s                           -> INVALID_OPT s

(* [detranslate_opt opt] - Converts a opt into the string version of it
 * Parameters:
 *    - opt - the opt that is to be converted to a string
 * Return: the string version of the opt *)
let detranslate_opt (opt:opt) : string =
  match opt with
  | MSG             -> "-m or --message"
  | ALL             -> ". or -a or --all"
  | DELETE          -> "-d or --delete"
  | REMOVE          -> "-rm or --remove"
  | RENAME          -> "-rn or --rename"
  | BNCH            -> "-b or --branch"
  | FILE            -> "-f or --file"
  | HARD            -> "--hard"
  | MIXED           -> "--mixed"
  | SOFT            -> "--soft"
  | CMD             -> "--cmd"
  | CONFIG_SET      -> "--set"
  | EMPTY           -> "<no options given>"
  | INVALID_OPT s   -> "s"

(* A cmd * opt map that is used to create the mpa of the expected number of args
 * and the default opt *)
module M = Map.Make (struct type t = (cmd * opt) let compare a b = Pervasives.compare a b end)

(* Map used to determine how many args a cmd * opt pair should have.  All other
 * combinations of cmd * opt are not supported by OASys.
 * Key - A cmd * opt pair that is accepted by OASys
 * Value - A list corresponding to the number of args that the cmd * opt pair
 *      can support.  -1 corresponds to > 0 and -2 corresponds to >= 0 *)
let arg_num_expected =
  let lst =
    [ ((INIT,EMPTY),[0]);
      ((LOG,EMPTY),[0]);
      ((STATUS,EMPTY),[0]);
      ((ADD,EMPTY),[-1]);     ((ADD,ALL),[0]);
      ((COMMIT,MSG),[1]);
      ((BRANCH,EMPTY),[-2]);
      ((RESET,FILE),[-1]);    ((RESET,BNCH),[1]);
      ((RESET,HARD),[1]);     ((RESET,SOFT),[1]); ((RESET,MIXED),[1]);
      ((RM,BNCH),[-1]);       ((RM,FILE),[-1]);
      ((CHECKOUT,EMPTY),[1]); ((CHECKOUT,FILE),[-1]);
      ((PUSH,EMPTY),[0]);
      ((PULL,EMPTY),[0]);
      ((HELP,EMPTY),[-2]);    ((HELP,CMD),[1]);
      ((MERGE,EMPTY),[1]);
      ((CONFIG,CONFIG_SET),[2]);
    ] in
  List.fold_left (fun acc x -> match x with | (x,y) -> M.add x y acc) M.empty lst

(* Map used to determine what the default opt is if no opt is given for the cmd
 * Key - cmd * EMPTY (Note QUIT and INVALID_CMD are not supported because they
 *      cannot have a default)
 * Value - The default opt *)
let opt_default =
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
      ((MERGE,EMPTY),EMPTY);
      ((CONFIG,EMPTY),EMPTY);
      ((PUSH,EMPTY),EMPTY);
      ((PULL,EMPTY),EMPTY);
      ((HELP,EMPTY),EMPTY)
    ] in
  List.fold_left (fun acc x -> match x with | (x,y) -> M.add x y acc) M.empty lst

(* [print_error ?s1 ?s2 ?i1 ?i2 n] - print the error corresponding to n
 * Parameters:
 *    - n - The int corresponding to the desired error message
 *    - s1,s2 - Optional strings that are to be included in the error
 *    - i1,i2 - Optional ints that are to be included in the error
 * Return: unit (the error is directly printed) *)
let print_error ?s1:(s1="") ?s2:(s2="") ?i1:(i1=0) ?i2:(i2=0) = function
  | 0 -> Printf.printf "__        _______ _     ____ ___  __  __ _____   _____ ___  \n\\ \\      / / ____| |   / ___/ _ \\|  \\/  | ____| |_   _/ _ \\ \n \\ \\ /\\ / /|  _| | |  | |  | | | | |\\/| |  _|     | || | | |\n  \\ V  V / | |___| |__| |__| |_| | |  | | |___    | || |_| |\n   \\_/\\_/  |_____|_____\\____\\___/|_|  |_|_____|   |_| \\___/ \n  ___    _    ____            \n / _ \\  / \\  / ___| _   _ ___ \n| | | |/ _ \\ \\___ \\| | | / __|\n| |_| / ___ \\ ___) | |_| \\__ \\\n \\___/_/   \\_\\____/ \\__, |___/\n                    |___/     \nCreated by Gu, Ho, Moheed, and Ramalingam\n\nTo search up info about a specific command, type \"help --cmd command_name\".\nTo search up more general topics, type \"help search_word(s)\".\n\nFor questions/comments/concerns, contact ____.\n\n"
  | 1 -> Printf.printf "\027[41mFAILURE:\027[49m \"%s\" is an invalid option.\n" s1
  | 2 -> Printf.printf "\027[41mFAILURE:\027[49m The \"%s\" command does not support more than 1 option.\n" s1
  | 4 -> Printf.printf "\027[41mFAILURE:\027[49m Invalid command given: \"%s\".\n" s1
  | 5 -> Printf.printf "\027[41mFAILURE:\027[49m Option \"%s\" is not supported for \"%s\".\n" s1 s2
  | 6 -> Printf.printf "\027[41mFAILURE:\027[49m No arguments were given when %s argument(s) was expected.\n" s1
  | 7 -> Printf.printf "\027[41mFAILURE:\027[49m %i argument(s) were given when %s argument(s) was expected.\n" i1 s1
  | 8 -> Printf.printf "\027[41mFAILURE:\027[49m Did not enter a command to search.\n"
  | 9 -> Printf.printf "\027[41mFAILURE:\027[49m Searching for too many commands.  Please breduce to only one command.\n"
  | 10 -> Printf.printf "\027[41mFAILURE:\027[49m \"%s\" is not a command; command-search cannot be completed.\nTry general-search without \"--cmd\" option.\n" s1
  | 11 -> Printf.printf "\027[43mUNSUCCESSFUL:\027[49m Cannot find the command \"%s\" in the documentation.\n" s1
  | 12 -> Printf.printf "\027[43mUNSUCCESSFUL:\027[49m Cannot find search in the documentation\n"
  | 13 -> Printf.printf "\027[41mFAILURE:\027[49m Not a valid option.\n"
  | 14 -> Printf.printf "\027[43mUNSUCCESSFUL:\027[49m Apologies. Unable to find topic in documentation.\n"
  | _ -> Printf.printf "\n"