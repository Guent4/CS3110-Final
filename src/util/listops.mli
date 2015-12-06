(*Removes an element from a list *)
val remove : 'a list -> 'a -> 'a list

(*Removes all elements in the latter list from the former *)
val subtract : 'a list -> 'a list -> 'a list

(*Union of two lists, no duplicates assuming l1 and l2 do not have duplicates *)
val union : 'a list -> 'a list -> 'a list

(*Returns true if the former is a subset of the latter *)
val subset : 'a list -> 'a list -> bool

(*Returns true if two lists contain the same elements *)
val equal : 'a list -> 'a list -> bool

(* [to_string l start seq terminal] - converts a list of strings to a string;
 * this string starts with start and has sep as separaters between list elements
 * and ends with terminal
 *    - l - the string list to be converted into a single string
 *    - start - the string that the resulting string will start with
 *    - sep - the delimeter to be used between elements of the list when concating
 *    - terminal - the result string will end with this *)
val to_string : string list -> string -> string -> string -> string