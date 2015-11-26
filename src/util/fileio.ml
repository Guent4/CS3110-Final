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

let copy_file file_name target_dir =
  FileUtil.cp [file_name] target_dir

let create_dir dir =
  FileUtil.mkdir dir

let remove_dir dir =
  FileUtil.rm ~force:FileUtil.Force ~recurse:true [dir]