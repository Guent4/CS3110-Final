open Core.Std

(* (* READ *)

let read filename =
  In_channel.read_lines filename

WRITE

let write filename lines =
  Out_channel.write_lines filename lines *)

let read_list s = failwith "Not implemented"

let write_list s sl = failwith "Not implemented"

let read s = failwith "Not implemented"

let write s1 s2 = failwith "Not implemented"

let zip s = failwith "Not implemented"

let unzip s = failwith "Not implemented"

let file_exists path =
  FileUtil.find FileUtil.Exists path (fun a x -> true || a) false

let copy_file file_name target_dir =
  FileUtil.cp [file_name] target_dir

let create_dir dir =
  FileUtil.mkdir dir

let remove_dir dir =
  FileUtil.rm ~force:FileUtil.Force ~recurse:true [dir]