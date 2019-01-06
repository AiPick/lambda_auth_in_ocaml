open m_merkle

let size_leaf = 1024

type bit = L | R
type tree_elem = Leaf of string | Node of mtree * mtree

(* fetch: get a leaf along a given path *)
(* idx : path *)
let rec fetch (idx:bit list) (t:mtree) : string =
match idx, t with
| [], Leaf a -> a
| L::rest, Node(l,_) -> fetch rest l
| R::rest, Node(_,r) -> fetch rest r
| _ -> failwith "bad index"

(* test mtree data and leaf *)
let idlist = [L;L]
let a = [1,2,3,4,5]
let t1 = Leaf "a"
let t2 = Node( Node(Leaf "a",Leaf "b"), Node(Leaf "c",Leaf "d") )

(* test fetch *)
let b = fetch [L;L] t2
let b = fetch [L;R] t2
let b = fetch [R;L] t2
let b = fetch [R;R] t2
let b = fetch [R;R;R] t2 (*error path, report bad index*)


