open Fileio

let f = "fileio_test.dat"

let lines = read f

TEST = (lines = ["testing1";"testing2"])

let () = write f []

let lines = read f

TEST = (lines = [])

let () = write f ["testing3"]

let lines = read f

TEST = (lines = ["testing3"])

let () = write f ["testing1";"testing2"]