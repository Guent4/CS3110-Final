open Coconuts
open Fileio
open Atlas

(* This module is responsible for providing suggestions when an invalid input
 * is detected.  Additionally, it is also responsible for implementing the HELP
 * cmd; helpful information will be printed based on user input. *)


(* [is_int s] - Determines if the string can be converted into an int.
 * Parameters:
 *    - s - the string being tested
 * Returns: true if s can be converted to an int using int_of_string; false otherwise*)
let is_int (s:string) : bool =
  try ignore (int_of_string s); true
  with _ -> false

(* [calc_lev str1 str2] - Calculates the Levenshtein Distance between str1 and
 * str2.  This is used to determine how close two strings are.
 * Parameters:
 *    - str1 - first of the two strings to compare
 *    - str2 - second of the two strings to compare
 * Returns: the Levenshtein Distance between str1 and str2 *)
let calc_lev (str1:string) (str2:string) : int =
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
  m.(x).(y)

(* [print_sugg input dict] - If there are words from dict that have a Levenshtein
 * Distance that is at most 2 units away, then print suggestions.  If there are
 * no words from dict, then do nothing.
 * Parameters:
 *    - input - the user's input that does not match cmd or opt
 *    - dict - the list of all of the accepted cmd or opt to compare input to
 * Returns: unit; wii print words taht are within 2 Lenvenshtein Distance away;
 *    do nothing otherwise *)
let print_sugg (input:string) (dict:string list) : unit =
  let sugg = List.filter (fun x -> calc_lev input x <= 2) dict in
  if (List.length sugg > 0) then (print_endline "\t Do you happen to mean:";
      List.iter (fun x -> Printf.printf "\t\t%s\n" x) sugg)
  else ()

(* [search_for_topic doc inputs] - Used for general search where a list of args
 * are being searched.  Function finds all of the topics headers in doc that
 * have a header that contains one or more of the words in inputs.
 * Parameters:
 *    - doc - the entire help document in the form of a string
 *    - inputs - list of args (strings) that user is trying to search over
 * Returns: all of the topic headers that contain one or more of the words in
 *    inputs *)
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

(* [help_empty a_s] - Given the arg list input, search for topics in the Help
 * document and then print those.  Request user to select which topic they are
 * interested in and then print the seleted article.
 * Parameters:
 *    - a_s - the list of string inputs that mark what user is trying to search for
 * Returns: unit; function performs print and does not return anything useful*)
let help_empty (a_s:arg list) : unit =
  let doc = Fileio.read_str help_loc in
  let topics = search_for_topic doc a_s in
  if (List.length topics = 0) then ()
  else (
    print_endline "These are the search result. Please enter the number corresponding to interest:\n\t0: Quit";
    ignore (List.fold_left (fun a x -> print_endline ("\t"^(string_of_int a)^": "^x); a+1) 1 topics);
    let sel = String.trim (read_line()) in
    if (is_int sel) then
      let sel_num = int_of_string sel in
      if (sel_num = 0) then ()
      else if (sel_num > List.length topics) then print_error 13
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

(* [help_cmd a_s] - User has selected that he wants to search for the documentation
 * on one of the commands.  Function finds the docuemntation for that cmd in the
 * Doc documentation and prints the results.  If a_s is empty, more than 1, or
 * entry is not a command, then an error is printed.
 * Parameters:
 *    - a_s - the list containing the user input; if not valid string form of a
 *      command, then print_error
 * Returns: unit; all actions are print which returns unit *)
let help_cmd (a_s:arg list) : unit =
  if (List.length a_s = 0)
    then print_error 8
  else if (List.length a_s > 1)
    then print_error 9
  else if (not (List.mem (List.hd a_s) cmd_str_list))
    then print_error 10 ~s1:(List.hd a_s)
  else (
    let doc = Fileio.read_str doc_loc in
    let desired = String.uppercase (List.hd a_s) in
    let regex = "<"^desired^">\\(.*\\(\n\t\\)*\\)*" in
    (try (
      ignore(Str.search_forward (Str.regexp regex) doc 0);
      print_endline (Str.matched_string doc))
    with
      Not_found -> print_error 11 ~s1:desired); ()
  )

(* [offer_help expr] - Read expr and determine whether or not the user is trying
 * to conduct a search for a command or a general search for a topic.  Will call
 * the correct helper function accordingly.
 * Parameters:
 *    - expr - the parsed cmd_expr option that; using this, determine if user is
 *    conducting a search and if so what type
 * Returns: None if the cmd was HELP; original expr input if otherwise *)
let offer_help (expr:cmd_expr option) : cmd_expr option =
  match expr with
  | Some (HELP,[EMPTY],a_s) -> help_empty a_s; None
  | Some (HELP,[CMD],a_s) -> help_cmd a_s; None
  | _ -> expr