(* Merkle *)
type 'a authtype = 
  | Merkle of string * 'a
  | Shallow of string 
  | Ideal of 'a 

(* authtype data test *)
let e1= Merkle ("a","hash_l")
let e2= Merkle ("b","hash_r")
(* ------------------ *)

(* let unauth : 'a authtype -> 'a = function x -> assert false *)
let unauth : 'a authtype -> 'a = function x -> x
(* let auth : 'a -> 'a authtype = function x -> assert false *)
let auth : 'a -> 'a authtype = function x -> x

let a1= auth

(* Merkle of using auth/unauth  *)

let size_leaf = 1024

type bit = L | R
type tree_elem = Leaf of string | Node of tree_elem authtype * tree_elem authtype
type tree= tree_elem authtype

(* fetch: get a leaf along a given path *)
(* idx : path *)
let rec fetch (idx:bit list) (t:tree_elem authtype) : string =
match idx, unauth t with
| [], Leaf a -> a
| L::rest, Node(l,_) -> fetch rest l
| R::rest, Node(_,r) -> fetch rest r
| _ -> failwith "bad index"

(* test mtree data and leaf *)
let t1 = Leaf "a"
let root = Merkle("h1","root")
let twoLeaf = Node(Merkle("h2",Leaf "a"),Merkle("h3",Leaf "b"))

(* 1st: constructive four leaf data *)
let leaf_a_00 = Leaf "a"
let leaf_b_01 = Leaf "b"
let leaf_c_02 = Leaf "c"
let leaf_d_03 = Leaf "d"

(* 2nd: constructive 2-level intermedia node data *)
let node_h2_10 = Node(Merkle("h4",leaf_a_00),Merkle("h5",leaf_b_01))
let node_h3_11 = Node(Merkle("h6",leaf_c_02),Merkle("h7",leaf_d_03))

(* 3rd: constructive top-level root node data *)
let node_h1_20 = Node(Merkle("h2",node_h2_10),Merkle("h7",node_h3_11))

(* final: save h1 in order to authenticate in the client *)
let root = Merkle("h1",node_h1_20)

(* test fetch *)
(* let b = fetch [L;L] root *)
(* let b = fetch [L;R] t2 authtype
 * let b = fetch [R;L] t2 authtype
 * let b = fetch [R;R] t2 authtype
 * let b = fetch [R;R;R] t2 (\*error path, report bad index*\) *)


