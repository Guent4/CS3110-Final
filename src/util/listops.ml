(* [remove l x] - Execute List.filter on l and return list of all elements <> x
 * Parameters:
 *    - l - the list in question
 *    - x - the value that all returned elements do not equal
 * Returns: a filtered list of l that does not contain any elements equal to x*)
let remove l x =
  List.filter (fun x' -> x' <> x) l

(* [subtract minuend subtrahend] - remove all occurences of elements in minuend
 * that also exist in subtrahend
 * Parameters:
 *    - minuend - the list to be filtered
 *    - subtrahend - resulting list should not have elements that are in subtrahend
 * Returns: a filtered list of minuend that does not contain any elements in
 * subtrahend*)
let subtract minuend subtrahend =
  List.filter (fun x -> not (List.mem x subtrahend)) minuend

(* [unit l1 l2] - creates union (no repeating elements) of l1 and l2 (assuming
 * that l1 and l2 do not have repeating elements). Elements of l2 are added into
 * l1 assuming that they aren't already in l1
 * Parameters:
 *    - l1 - one of the two lists to be unioned
 *    - l2 - the other of the two list to be unioned
 * Returns: the union of l1 and l2*)
let union l1 l2 =
  List.fold_left (fun a x -> if (not (List.mem x l1)) then x :: a else a) l1 l2

(* [subset l1 l2] - checks that no elements in l2 are in l1
 * Parameters:
 *    - l1 - list to be compared to
 *    - l2 - list being checked
 * Returns: true if no elements in l2 are also in l1; false otherwise*)
let subset l1 l2 =
  subtract l1 l2 = []

(* [equal l1 l2] - checks if l1 is equal to l2
 * Parameters:
 *    - l1 - first list in question
 *    - l2 - second list in question
 * Returns: true if l1 equals l2; false otherwise *)
let equal l1 l2 =
  subset l1 l2 && subset l2 l1

(* [to_string l start seq terminal] - converts a list of strings to a string;
 * this string starts with start and has sep as separaters between list elements
 * and ends with terminal
 *    - l - the string list to be converted into a single string
 *    - start - the string that the resulting string will start with
 *    - sep - the delimeter to be used between elements of the list when concating
 *    - terminal - the result string will end with this *)
let to_string l start sep terminal =
  (List.fold_left (fun a x -> a ^ sep ^ x) start l) ^ terminal