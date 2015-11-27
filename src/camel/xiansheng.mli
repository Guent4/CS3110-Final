open Coconuts

(* [print_sugg input dict] - When user does not input a cmd or an opt that is
    spelled correctly, this will take "input" and provide suggestions to the
    user as to the correct spelling.  Suggestions are all words from "dict" that
    are a Levenshtein Distance of at most 2 away *)
val print_sugg : string -> string list -> unit

(* [offer_help expr] - If the cmd in "expr" is HELP, then help results will be
    printed in the terminal.  If opt is EMPTY, then search will be through
    OASys_Help.txt and that is just a general search based on whether or not the
    article titles in OASys_Help.txt contains the interested words.  IF opt is
    CMD, then the search is through OASys_Doc.txt and that is for the documentation
    for a specific command; therefore user must provide a valid command for this
    type of search*)
val offer_help : cmd_expr option -> unit