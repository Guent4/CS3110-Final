open Fileio

let f = "fileio_test.dat"

let lines = read_list f

TEST = (lines = ["testing1";"testing2"])

let () = write_list f []

let lines = read_list f

TEST = (lines = [])

let () = write_list f ["testing1\ntesting2"]

let lines = read_list f

TEST = (lines = ["testing1";"testing2"])

let () = write_list f ["testing3"]

let lines = read_list f

TEST = (lines = ["testing3"])

let () = write_list f ["testing1";"testing2"]