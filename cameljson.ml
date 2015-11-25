open Yojson.Basic.Util

let deserialize (filename:string) =
  let json = from_file filename in
  (*String of current branch*)
  let current_branch = json |> member "current_branch" |> to_string in
  let json_nodes = json |> member "master" |> to_list in




let serialize (palm_tree) (branch_name:string) (filename:string)
              (repo_dir:string) =
