open Coconuts
open Camel

let lex (s:string) : string list =
  let whitespace_char_string = String.concat "+"
    (List.map (String.make 1)
       [
         Char.chr 9;  (* HT *)
         Char.chr 10; (* LF *)
         Char.chr 11; (* VT *)
         Char.chr 12; (* FF *)
         Char.chr 13; (* CR *)
         Char.chr 32; (* space *)
       ]) in
  let whitespace = "[" ^ whitespace_char_string ^ "]+" in
  Str.split (Str.regexp whitespace) s

TEST = Camel.interpret (lex "push") = Some (LOCAL, PUSH, [EMPTY], [])

TEST = Camel.interpret (lex "add") = None
TEST = Camel.interpret (lex "add .") = Some (LOCAL, ADD, [ALL], [])
TEST = Camel.interpret (lex "add file1.ml file2.txt file3") = Some (LOCAL, ADD, [EMPTY], ["file1.ml"; "file2.txt"; "file3"])
TEST = Camel.interpret (lex "add file1.ml file2.txt \"file3\"") = None

TEST = Camel.interpret (lex "commit -m") = None
TEST = Camel.interpret (lex "commit --message") = None
TEST = Camel.interpret (lex "commit --message asdf") = None
TEST = Camel.interpret (lex "commit --message \"hello world\"") = Some (LOCAL, COMMIT, [MSG], ["\"hello world\""])
TEST = Camel.interpret (lex "commit -a -m") = None
TEST = Camel.interpret (lex "commi -m") = None

TEST = Camel.interpret (lex "diff" = Some (LOCAL, DIFF, [EMPTY], [])
TEST = Camel.interpret (lex "diff --file file1.ml file2.ml") = Some (LOCAL, DIFF, [FILE], ["file1.ml"; "file2.ml"])
TEST = Camel.interpret (lex "diff -b branch1 branch2") = Some (LOCAL, DIFF, [BNCH], ["branch1"; "branch2"])
TEST = Camel.interpret (lex "diff file1.ml file2.ml") = None
TEST = Camel.interpret (lex "diff \"file1.ml\" \"file2.ml\"") = None

TEST = Camel.interpret (lex "rm branch1 branch2 branch3") = None
TEST = Camel.interpret (lex "rm -b branch1 branch2 branch3") = Some (LOCAL, RM, [BNCH], ["branch1"; "branch2"; "branch3"])
TEST = Camel.interpret (lex "rm --file f1 f2 f3") = Some (LOCAL, RM, [FILE], ["f1"; "f2"; "f3"])


let () = Pa_ounit_lib.Runtime.summarize()