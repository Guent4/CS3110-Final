open Coconuts
open Fileio
open Atlas


let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

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
  (* Array.iter (fun x -> Array.iter (fun y -> Printf.printf "%i\t" y) x; print_endline "") m; *)
  m.(x).(y)

let print_sugg (input:string) (dict:string list) : unit =
  let sugg = List.filter (fun x -> calc_lev input x <= 2) dict in
  if (List.length sugg > 0) then (print_endline "\t Do you happen to mean:";
      List.iter (fun x -> Printf.printf "\t\t%s\n" x) sugg)
  else ()

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