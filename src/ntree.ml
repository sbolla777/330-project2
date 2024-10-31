type 'a tree = BiNode of 'a tree * 'a * 'a tree | Leaf
type 'a flat = Lf | Nd of 'a
type 'a n_tree = Node of 'a * 'a n_tree list

(* TODO: Implement the functions below *)

let rec flatten (input : 'a tree) : 'a flat list = match input with
  | Leaf -> [Lf]
  | BiNode (lefttree, value, righttree) -> flatten lefttree @ flatten righttree @ [Nd value]

let unflatten (input : 'a flat list) : 'a tree =
  let rec helper lst = match lst with
    | Lf :: rest -> (Leaf, rest)  
    | Nd value :: rest ->
        let (righttree, middleright) = helper rest in
        let (lefttree, middleleft) = helper middleright in
        (BiNode (lefttree, value, righttree), middleleft)
  in
  let (tree, rest) = helper (List.rev input) in
  tree

let rec encode (input : 'a n_tree) : ('a * int) list = match input with
  | Node (value, children) -> (value, List.length children) :: List.concat_map encode children

let decode (input : ('a * int) list) : 'a n_tree = 
  let rec helper lst = match lst with
    | [] -> failwith "Invalid Arguement"
    | (data, childcount) :: rest ->
      let rec processchildren childnum children rem =
        if childnum = 0 then (List.rev children, rem)
        else
          let (child, rem) = helper rem in
          processchildren (childnum - 1) (child :: children) rem
      in
      let (children, rest) = processchildren childcount [] rest in
      (Node(data, children), rest)
  in
  let (tree, _) = helper input in
  tree 
