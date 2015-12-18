(* [file_exists path] - checks if the file with a filepath of path exists
 * Parameters:
 *    - path - the path to the file in question
 * Returns: true if file exists and false otherwise*)
let file_exists (path:string) : bool =
  FileUtil.find FileUtil.Exists path (fun a x -> true || a) false

(* [read_list filename] - Reads the file line by line; returns [] if file does
 * not exist.
 * Parameters:
 *    - filename - the name of the file that you want to read from
 * Returns: result of reading file *)
let read_list (filename:string) : string list =
  match file_exists filename with
  | false -> []
  | true -> Core.Std.In_channel.read_lines filename

(* [write_list filename sl] - write sl into a file with filename
 * Parameters:
 *    - filename - the file to be written to
 *    - sl - the data to be written into the file
 * Returns: unit *)
let write_list (filename:string) (sl:string list) : unit =
  Core.Std.Out_channel.write_lines filename sl

(* [read_str filename] - Reads the file line by line and concat into a single
 * string separated by \n
 * Parameters:
 *    - filename - the name of the file that you want to read from
 * Returns: result of reading file *)
let read_str (filename:string) : string =
  let lst = read_list filename in
  let str = List.fold_left (fun acc x -> acc^"\n"^x) "" lst in
  if (String.length str > 0) then String.sub str 1 (String.length str - 1)
  else ""

(* [write_str filename s] - write s into a file with filename
 * Parameters:
 *    - filename - the file to be written to
 *    - s - the data to be written into the file
 * Returns: unit *)
let write_str (filename:string) (s:string) : unit =
  write_list filename (Str.split (Str.regexp "\n") s)

(* [file_in_dir path] - retrieve all of the files in the directory specified by
 * path
 * Parameters:
 *    - path - the path of the interested directory
 * Returns: list of files/directories in the specified directory *)
let files_in_dir path =
  FileUtil.ls path

(* [copy_file abs_path rel_file_name target_dir] - goes to the file location and
 * then copies the file into the target_dir
 * Parameters:
 *    - abs_path - the path to the directory containing the file
 *    - rel_file_name - the name of the file to be copied
 *    - target_dir - the directory to copy to
 * Returns: unit *)
let copy_file (abs_path:string) (rel_file_name:string) (target_dir:string) : unit =
  let _ = Sys.command("cd " ^ abs_path ^ " && cp -r --parent "
    ^ rel_file_name ^ " " ^ target_dir) in
  ()

(* [create_dir dir] - create a new directory
 * Parameters:
 *    - dir - name of directory to be created
 * Returns: unit *)
let create_dir (dir:string) : unit =
  FileUtil.mkdir dir

(* [remove_dir dir] - remove a directory
 * Parameters:
 *    - dir - the name of the directory to be deleted
 * Returns: unit *)
let remove_dir (dir:string) : unit =
  FileUtil.rm ~force:FileUtil.Force ~recurse:true [dir]

(* [remove_file filename] - remove file with filename
 * Parameters:
 *    - filename - the name of the file to be deleted
 * Returns: unit *)
let remove_file (filename:string) : unit =
  FileUtil.rm [filename]

(* [merge_3_way label_a label_b label_c a b c] - merge three string lists together
 * based on the three-way-merge alogrithm
 * Parameters:
 *    - label_a - the name of the branch that a belongs to (ancestor)
 *    - label_b - the name of the branch that b belongs to (other)
 *    - label_c - the name of the branch that c belongs to (yours)
 *    - a - ancestor data
 *    - b - other data
 *    - c - your data
 * Returns: returns the three way merged string list *)
let merge_3_way (label_a:string) (label_b:string) (label_c:string)
    (a:string list) (b:string list) (c:string list) : string list =
  let resolve_conflict a b c =
    let rec b_c_conflict_rec in_a in_b in_c top bot = match in_a,in_b,in_c with
      | _,[],[] -> (in_a,in_b,in_c,top,bot)
      | _,in_bh::in_bt,[] -> (in_a,in_b,in_c,top,bot)
      | _,[],in_ch::in_ct -> (in_a,in_b,in_c,top,bot)
      | [],in_bh::in_bt,in_ch::in_ct -> (
        if (in_bh = in_ch) then (in_a,in_b,in_c,top,bot)
        else (
          let (a',b',c',top',bot') = b_c_conflict_rec [] in_bt in_ct (top@[in_bh]) bot in
          (a',b',c',top',(bot'@[in_ch]))
        )
      )
      | in_ah::in_at,in_bh::in_bt,in_ch::in_ct -> (
        if (in_bh = in_ch || in_ah = in_bh || in_ah = in_ch)
          then (in_a,in_b,in_c,top,bot)
        else (
          let (a',b',c',top',bot') = b_c_conflict_rec in_at in_bt in_ct (top@[in_bh]) bot in
          (a',b',c',top',(bot'@[in_ch]))
        )
      ) in
    let (a',b',c',top',bot') = b_c_conflict_rec a b c [] [] in
    let resolved = [">>>>>> "^label_b]@top'@["======"]@(List.rev bot')@["<<<<<< "^label_c] in
    (a',b',c',resolved) in
  let rec merge_3_way_rec a b c out =
    match a,b,c with
    | _,[],[] -> out
    | _,bh::bt,[] -> if (a = b) then out else out@b
    | _,[],ch::ct -> if (a = c) then out else out@c
    | [],bh::bt,ch::ct -> (
      if (bh = ch) then merge_3_way_rec a bt ct (out@[bh])
      else (
        let (a',b',c',conflict') = resolve_conflict a b c in
        merge_3_way_rec a' b' c' (out@conflict')
      )
    )
    | ah::at,bh::bt,ch::ct -> (
      if (bh = ch) then merge_3_way_rec at bt ct (out@[bh])
      else if (ah = bh && ah <> ch) then merge_3_way_rec at bt ct (out@[ch])
      else if (ah <> bh && ah = ch) then merge_3_way_rec at bt ct (out@[bh])
      else (
        let (a',b',c',conflict') = resolve_conflict a b c in
        merge_3_way_rec a' b' c' (out@conflict')
      ))
  in merge_3_way_rec a b c []

(* [merge_files n1 n2 n3 f1 f2 f3 f4] - merge three different versions of a file
 * Parameters:
 *    - n1 - branch name of the first version (ancestor)
 *    - n2 - branch name of the second version (other)
 *    - n3 - branch name of the third version (yours)
 *    - f1 - the name of the first version of the file
 *    - f2 - the name of the second version of the file
 *    - f3 - the name of the third version of the file
 *    - f4 - the name of the destination file
 * Returns: unit *)
let merge_files n1 n2 n3 f1 f2 f3 f4 =
  let l1 = read_list f1 in
  let l2 = read_list f2 in
  let l3 = read_list f3 in
  let result = merge_3_way n2 n1 n3 l2 l1 l3 in
  let () = write_list f4 result in
  ()