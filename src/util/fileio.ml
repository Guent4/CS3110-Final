let file_exists path =
  FileUtil.find FileUtil.Exists path (fun a x -> true || a) false

let read_list (filename:string) : string list =
  match file_exists filename with
  | false -> []
  | true -> Core.Std.In_channel.read_lines filename

let write_list (filename:string) (sl:string list) : unit =
  Core.Std.Out_channel.write_lines filename sl

let read_str (filename:string) : string =
  let lst = read_list filename in
  let str = List.fold_left (fun acc x -> acc^"\n"^x) "" lst in
  if (String.length str > 0) then String.sub str 1 (String.length str - 1)
  else ""

let write_str (filename:string) (s:string) : unit =
  write_list filename (Str.split (Str.regexp "\n") s)

let files_in_dir path =
  FileUtil.ls path

let copy_file abs_path rel_file_name target_dir =
  let _ = Sys.command("cd " ^ abs_path ^ " && cp -r --parent " ^ rel_file_name ^ " " ^ target_dir) in
  ()

let create_dir dir =
  FileUtil.mkdir dir

let remove_dir dir =
  FileUtil.rm ~force:FileUtil.Force ~recurse:true [dir]

let remove_file filename =
  FileUtil.rm [filename]

let merge_3_way (label_a:string) (label_b:string) (label_c:string)
    (a:string list) (b:string list) (c:string list) : string list =
  let resolve_conflict a b c =
    let rec b_c_conflict_rec in_a in_b in_c conflict = match in_a,in_b,in_c with
      | _,[],[] -> (in_a,in_b,in_c,conflict@["======"])
      | _,in_bh::in_bt,[] -> (in_a,in_b,in_c,conflict@["======"])
      | _,[],in_ch::in_ct -> (in_a,in_b,in_c,conflict@["======"])
      | [],in_bh::in_bt,in_ch::in_ct -> (
        if (in_bh = in_ch) then (in_a,in_b,in_c,conflict@["======"])
        else (
          let (a',b',c',conflict') = b_c_conflict_rec [] in_bt in_ct (conflict@[in_bh]) in
          (a',b',c',(conflict'@[in_ch]))
        )
      )
      | in_ah::in_at,in_bh::in_bt,in_ch::in_ct -> (
        if (in_bh = in_ch || in_ah = in_bh || in_ah = in_ch)
          then (in_a,in_b,in_c,conflict@["======"])
        else (
          let (a',b',c',conflict') = b_c_conflict_rec in_at in_bt in_ct (conflict@[in_bh]) in
          (a',b',c',(conflict'@[in_ch]))
        )
      ) in
    let (a',b',c',conflict') = b_c_conflict_rec a b c [">>>>>> "^label_b] in
    let conflict' = conflict'@["<<<<<< "^label_c] in
    (a',b',c',conflict') in
  let rec merge_3_way_rec a b c out =
    match a,b,c with
    | _,[],[] -> out
    | _,bh::bt,[] -> out@b
    | _,[],ch::ct -> out@c
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

let merge_files n1 n2 n3 f1 f2 f3 f4 =
  let l1 = read_list f1 in
  let l2 = read_list f2 in
  let l3 = read_list f3 in
  let result = merge_3_way n2 n1 n3 l2 l1 l3 in
  let () = write_list f4 result in
  ()