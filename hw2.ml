(* 
* Assignment: gt.ml
* Description: Homework 2 - General Trees 
* Author: Julia Nelson
* Date: 06/14/2021
* Pledge: "I pledge my honor that I have abided by the Stevens Honor System."
*)


type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
  Node (33,
        [Node (12,[]);
         Node (77, 
               [Node (37, 
                      [Node (14, [])]); 
                Node (48, []); 
                Node (103, [])])
        ])




(* max_numl l : 
    (helper for height)
    found structure of  max for 2 args with 1 arg  - found at : https://stackoverflow.com/questions/25980927/how-to-write-a-function-to-find-max-number-in-a-list-of-type-number-in-ocaml
    and I added a fail case to it
*)
let rec max_numl l =
  match l with 
  | [] -> failwith "Empty Max"      (*Added fail case to catch error *)
  | x::[] -> x
  | x::xs -> max x (max_numl xs)
(* good practice for me... try to rewrite with List.fold_left *)



(* height t : 
    that given a general tree returns its height. The height of a tree is the length
    of the longest (in terms of number of nodes) path from the root to a leaf.
*) 
let rec height t =      (*fun a b takes list of a returns list of type b*)
  match t with
  | Node(d,[]) -> 1 
  | Node(d,ch) -> 1 +  max_numl (List.map height ch)


  

(* size t : 
    that given a general tree returns its size. The size of a general tree consists of
    the number of nodes
*)
(* need sum of the size of all the children *)
let rec size t =
  match t with
  | Node(d,[]) -> 1
  | Node(_,ch) -> 1 + List.fold_left (fun acc x -> acc + x ) 0 (List.map size ch)
  (* 
  I think the last part can also be written as...
            List.fold_left (+) 0 (List.map size ch)
  *) 



(* paths_to_leaves t :
    returns a list with all the paths from the root to the leaves of the
    general tree t. Let n be the largest number of children of any node in t. A path is a
    list of numbers in the set {0, 1, . . . , n − 1} such that if we follow it on the tree, 
    it leads to a leaf. The order in which the paths are listed is irrelevant.

    EX:
    # paths_to_leaves t;;
    - : int list list = [[0] ; [1; 0; 0]; [1; 1]; [1; 2]]
*)
let rec paths_to_leaves t =
  match t with 
  | Node(d,[]) -> [ [] ]
  | Node (d,ch) ->  
      List.flatten @@ List.mapi (fun ix ll -> List.map (fun l -> ix::l) ll) (List.map paths_to_leaves ch)



(*perfect helper l :
  ALSO TRIED BY:  length of all the same - so map length over all paths to leaves and write lambda to 
                    check if theyre the same or use built in all to map them all to booleans but ran into issues
*)
let rec perf_help l =
  match l with 
  | [] | [_] -> true (* if has no or 1 elements *)
  | h::x::t -> 
      if (List.length h) != (List.length x) 
      then false 
      else perf_help (x::t)


(* is_leaf_perfect t : 
    that determines whether a general tree is leaf perfect. A general tree
    is said to be 'leaf perfect' if all leaves have the same depth.

    EX:
    # is_leaf_perfect t;;
    - : bool = false
*)
(* find the leaves and their paths  - count path lengths - compare - if equal true - if not false*)
let rec is_leaf_perfect t =
  perf_help (paths_to_leaves t)

 

(* preorder t : 
    that returns the pre-order traversal of a general tree.

    EX:
    # preorder t;;
    - : int list = [33 ; 12; 77; 37; 14; 48; 103]
*)
let rec preorder t =
  match t with
  | Node(d,[]) -> [d]
  | Node(d,ch) -> d::(List.flatten (List.map preorder ch)) 




(* mirror t : 
    that returns the mirror image of a general tree.

    EX: 
    # mirror t;;
    - : int gt =
        Node (33 , [ Node (77 , [ Node (103 , []) ; Node (48 , []) ; Node (37 , [ Node (14 , [])])]) ; Node (12 , [])])
*)
(* List.rev ???? *)
(* or maybe make mirror_helper that takes a list ... h::h2::t -> h... *)                       
let rec mirror t =
  match t with
  | Node(d,[]) -> t
  | Node(d, ch)->  Node(d, (List.map mirror (List.rev ch)) )   



(* mapt f t : 
    that produces a general tree resulting from t by mapping function f to each
    data item in d.

    EX:
    mapt ( fun i -> i >20) t;;
    - : bool gt =
        Node ( true ,[ Node ( false , []); Node ( true ,[ Node ( true , [ Node ( false , [])]) ; Node ( true , []) ; Node ( true ,[])])])
*)      
(* DO I NEED TO DO SOME List.map BECAUSE THIS IS A LIST AND MAPt is from tree ????? *)
let rec mapt f t =
  match t with
  | Node(d,ch) -> Node(f d, List.map (fun x -> mapt f x ) ch )       
(* (List.map (mapt f) ch ) *)


(* foldt f t : 
    that encodes the recursion scheme over general trees. Its type is
        foldt: (’a -> ’b list -> ’b) -> ’a gt -> ’b
*)      
let rec foldt f t = 
  match t with
  | Node(d,ch) -> f d (List.map (foldt f) ch ) 
  (*  attempt that didnt work - Node(f d, (List.fold_left (foldt f d ch) ch) )       *)




(* sumt : 
    given in homework directions

    EX: 
    # sumt t ;;
    - : int = 324
*)
let sumt t =
  foldt ( fun i rs -> i + List.fold_left ( fun i j -> i+j ) 0 rs ) t


(* memt : 
    given in homework directions

EX: 
  # memt t 12;;
- : bool = true
           # memt t 33;;
- : bool = true
           # memt t 35;;
- : bool = false
*)
let memt t e = 
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t


(* mirror' : 
    Implement mirror’ using foldt. It should behave just like Exercise 6.-> mirror?
*)
let mirror' t  = 
  foldt (fun i rs -> Node( i, List.rev rs )) t                  (* following structure of foldt usage in sumt / memt *)


















