open Coconuts
open Xiansheng
open Atlas

(* This module is responsible for reading in the input from the Terminal and then
 * parse the input into a cmd_expr that can be used by the rest of the program.
 * Camel is also responsible for determining if an input is not valid (cannot be
 * be interpretted into a cmd_expr); if invalid, the error is directly printed
 * and None is passed on, effectively stopping the program.  Camel will call
 * xiansheng in order to provide help to the user in the form of suggestions and
 * search results. *)

(* [parse_cmd cmd_str] - convert the string into a cmd (calls method in atlas)
 * Parameters:
 *    - cmd_str - the string that will be converted into a cmd
 * Returns: a cmd (if not a valid cmd, then INVALID_CMD of type cmd is returned)*)
let parse_cmd (cmd_str:string) : cmd =
  translate_cmd cmd_str

(* [parse_opt in_list] - Method goes through the in_list and finds the opts.
 * The opts start with "%-" or is just ".".  It stops looking for opts at the
 * first occurence of an item in in_list that isn't an opt.  If no opts are found,
 * then EMPTY is used as the opt.  At the end, a check is made to see if a default
 * opt was assigned to c in atlas.ml; if so, replace EMPTY with the default opt.
 * Parameters:
 *    - c - the parsed cmd for the input
 *    - in_list - string list of input words after the cmd element has been removed
 * Returns: (a,b) pair where a is the list of the opts and b is the remainder of
 * in_list after the opt words were removed *)
let parse_opt (c:cmd) (in_list:string list) : opt list * string list =
  let rec parse_opt_rec in_list acc =
    match in_list with
    | [] -> (
      if (List.length acc = 0) then ([EMPTY], in_list)
      else (acc, in_list))
    | h::t -> (
      if (h = ".") then (* . is shortcut for --all for ADD cmd*)
        parse_opt_rec t (acc@[(translate_opt h)])
      else if (String.length h >= 3 && String.sub h 0 2 = "%-") then
        let acc = acc@[translate_opt (String.sub h 1 (String.length h - 1))] in
        parse_opt_rec t acc
      else (
        if (List.length acc = 0) then ([EMPTY], in_list)
        else (acc, in_list))) in
  match parse_opt_rec in_list [] with
  | (o,args) ->(
      let o' = if (o = [EMPTY] && M.mem (c,EMPTY) opt_default)
          then [M.find (c,EMPTY) opt_default]
        else o in
        (o',args))

(* [parse_arg c o in_list] - Using the in_list, figure out what are the arg(s) to
 * the input.  If the cmd is COMMIT and the fst of opt is MSG, then concatenate
 * all of the elements in in_list to create the commit message.
 * Parameters:
 *    - c - the parsed cmd for the input
 *    - o - the list of opt parsed from the input
 *    - in_list - the rest of the input after the cmd and opt were parsed out
 * Returns: the list of args after parsing is finished *)
let parse_arg (c: cmd) (o: opt list) (in_list: string list) : arg list =
  match (c,o) with
  | (COMMIT,MSG::[]) -> (
    let message = String.trim (List.fold_left (fun a x -> a^" "^x) "" in_list) in
    if (message = "") then [] else [message])
  | _ -> in_list

(* [get_string_num_opts] - Get word interpretation of the expected number args in
 * expected for the given int list.
 * Parameters:
 *    - os this is the list that is key for a certain cmd * opt pair.  This is
 *      obtained from arg_num_expected in atlas.ml
 * Returns: the word interpretation of the key from arg_num_expected in atlas.ml *)
let get_string_num_opts (os:int list) : string =
  let with_space_in_front =
    if (List.mem (-2) os) then " any number of"
    else if (List.mem (-1) os) then " one or more"
    else List.fold_left (fun acc x -> acc ^ ", " ^ string_of_int x) "" os in
  String.sub with_space_in_front 2 (String.length with_space_in_front - 2)

(* [check_args c o a] - Checks if there is the correct number of args give the
 * cmd and opt.  If not, print an error and return None.
 * Paramters:
 *    - c - the parsed cmd from the user input
 *    - o - the opt list; should only have one element (if there is no cmd * opt
 *          combination, this function will print an error)
 *    - a - the parsed list of args
 * Returns: if no incorrect number of args is detected, then return Some x where
 * x is the fully parsed input to be used by OASys *)
let check_args (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match o with
  | [] -> failwith "No option given"
  | f::e -> (
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

(* [check_opt c o a] - Checks that there is only one opt for the cmd.  If there
 * is no opt given, then give EMPTY as the opt.  If there is more than one opt
 * given, then print an error.  If these checks pass, then method calls check_args
 * to check the parsed args. (Note: the use of an opt list was in order to make
 * potential expansions to allow multiple opt possible).
 * Parameters:
 *    - c - the parsed cmd from the user input
 *    - o - the parsed list of opt (will be checked by this method)
 *    - a - the parsed list of args
 * Returns: Return Some x where x is a parsed cmd_expr that has passed all of the
 * checks in Camel.ml.  If not all of the checks passed, then an error would be
 * printed and None is returned. *)
let check_opt (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match o with
    | [] -> check_args c [EMPTY] a
    | f::[] -> (match f with
      | INVALID_OPT x -> print_error 1 ~s1:x; print_sugg x opt_str_list; None
      | _ -> check_args c o a)
    | f::r -> print_error 2 ~s1:(detranslate_cmd c); None

(* [check_opt c o a] - Checks that c is not INVALID_CMD.  If it is, print an error
 * and return None; if it isn't, start checks for o.
 * Parameters:
 *    - c - the parsed cmd from the user input (will be checked by this method)
 *    - o - the parsed list of opt (will be next if checks for cmd pass)
 *    - a - the parsed list of args
 * Returns: Return Some x where x is a parsed cmd_expr that has passed all of the
 * checks in Camel.ml.  If not all of the checks passed, then an error would be
 * printed and None is returned. *)
let check_cmd_expr (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match c with
  | INVALID_CMD x -> print_error 4 ~s1:x; print_sugg x cmd_str_list; None
  | _ -> check_opt c o a

(* [read] - Read the terminal command.  First element of the read list is discarded
 * becuase it is of no use.  Second element is the repository directory.  The
 * rest contains the actual action given by the user.
 * Parameters:
 *    - NA
 * Returns: (a,b) where a is the repository directory and b is the list strings
 * that constitutes the action given by the user.*)
let read () : string * (string list)  =
  match Array.to_list Sys.argv with
  | h::r::t -> (r,t)
  | _ -> exit 0

(* [interpret input] - Takes the user input from read and parses it.
 * Parameters:
 *    - input - the result from read; only contains the part with the action
 *      commands
 * Returns: Some x if x is a cmd_expr that passed all of the checks in this file
 * and None if there is a check that failed (error message will be printed
 * accordingly) *)
let interpret (input:string list) : cmd_expr option =
  match input with
  | [] -> None
  | cmd_elmt::opt_list ->
    let c = parse_cmd cmd_elmt in
    let (o,arg_list) = parse_opt c opt_list in
    let args = parse_arg c o arg_list in
    let expr_option = check_cmd_expr c o args in
    offer_help expr_option

(* [read_interpret] - Calls read which gets user input and then performs interpret
 * on the input string list after the repository directory entry has been taken
 * out.  If resulting parsed cmd_expr option is None, then break exit; if it is
 * Some x where x is valid, then return the repo directory with x.
 * Paramters:
 *    - NA
 * Returns: (a,b) where a is the repo dir and b is a cmd_expr that passed all
 * checks performed in camel.ml *)
let rec read_interpret () : string * cmd_expr =
  match read () with
  | (repo_dir,rest) -> (
    match interpret rest with
    | None -> exit 0
    | Some (c,o,a) -> (repo_dir,(c,o,a)))

(* [output x] - Print the result of the execution of the command.
 * Parameters:
 *    - x - the result of the exection of the input action; can be a success or
 *      a failure
 * Returns: unit (method only used for its sideeffect) *)
let output (x:feedback) : unit =
  match x with
  | Success s -> Printf.printf "%s\n" s
  | Failure s -> Printf.printf "%s\n" s