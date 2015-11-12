type id = string
type msg = string
type path = string

(* Tree stucture with nodes containing request id, message, and file path *)
type palmtree = Node of id * msg * path * Node list