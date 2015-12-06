(*Removes an element from a list *)
val remove : 'a list -> 'a -> 'a list

(*Removes all elements in the latter list from the former *)
val subtract : 'a list -> 'a list -> 'a list

(*Union of two lists, no duplicates *)
val union : 'a list -> 'a list -> 'a list

(*Returns true if the former is a subset of the latter *)
val subset : 'a list -> 'a list -> bool

(*Returns true if two lists contain the same elements *)
val equal : 'a list -> 'a list -> bool

(*Returns a string representation of a list *)
val to_string : string list -> string -> string -> string -> string