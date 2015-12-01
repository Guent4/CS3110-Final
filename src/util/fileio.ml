let rec lcs l1 l2 diff sum n1 n2 n3 =
  match l1,l2 with
  | [],_ | _,[] ->
    (diff,
    sum @
    (List.map (fun x -> (x,n1)) l1) @
    (List.map (fun x -> (x,n2)) l2))
  | x::xs,y::ys ->
    (
    match x = y with
    | true ->
      lcs xs ys (diff @ [x]) (sum @ [(x,n3)]) n1 n2 n3
    | false ->
      let (diff1,sum1) = lcs l1 ys [] (sum @ [(y,n2)]) n1 n2 n3 in
      let (diff2,sum2) = lcs xs l2 [] (sum @ [(x,n1)]) n1 n2 n3 in
      (
      match List.length diff1 > List.length diff2 with
      | true -> ((diff @ diff1), sum1)
      | false -> ((diff @ diff2), sum2)
      )
    )

let read_list (filename:string) : string list =
  Core.Std.In_channel.read_lines filename

let write_list (filename:string) (sl:string list) : unit =
  Core.Std.Out_channel.write_lines filename sl

let read_str (filename:string) : string =
  let lst = read_list filename in
  let str = List.fold_left (fun acc x -> acc^"\n"^x) "" lst in
  if (String.length str > 0) then String.sub str 1 (String.length str - 1)
  else ""

let write_str (filename:string) (s:string) : unit =
  write_list filename (Str.split (Str.regexp "\n") s)

let zip s = failwith "Not implemented"

let unzip s = failwith "Not implemented"

let file_exists path =
  FileUtil.find FileUtil.Exists path (fun a x -> true || a) false

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

let diff_files f1 f2 c1 c2 =
  let l1 = read_list f1 in
  let l2 = read_list f2 in
  let (diff,merge) = lcs l1 l2 [] [] c1 c2 "both" in
  (diff,merge)

let merge_files f1 f2 f3 c1 c2 =
  let (_,merge) = (diff_files f1 f2 c1 c2) in
  let (content_list,_) = List.split merge in
  let () = write_list f3 content_list in
  ()