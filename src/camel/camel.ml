open Coconuts
open Xiansheng
open Atlas


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
      | INVALID_OPT x -> print_error 1 ~s1:x; print_sugg x opt_str_list; None
      | _ -> check_args c o a)
    | f::r -> print_error 2 ~s1:(detranslate_cmd c); None

let check_cmd_expr (c:cmd) (o:opt list) (a:arg list) : cmd_expr option =
  match c with
  | INVALID_CMD x -> print_error 4 ~s1:x; print_sugg x cmd_str_list; None
  | _ -> check_opt c o a

let rec read () : string list  =
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

let output (x:feedback) : unit =
  match x with
  | Success s -> Printf.printf "%s\n" s
  | Failure s -> Printf.printf "%s\n" s

