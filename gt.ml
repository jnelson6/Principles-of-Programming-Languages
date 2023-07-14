(****************************************************************
      FILENAME    : gt.ml
      AUTHOR      : Julia Nelson
      DESCRIPTION : hw2a.pdf 
      DATE        : 02/21/2022
      PLEDGE      : "I pledge my honor that I have abided by the Stevens Honor System."
****************************************************************)

(**
* A general tree - is a non-empty tree in which each node can have any number of children.     
*)
type 'a gt = Node of 'a*('a gt) list 

(** general tree t*)
let t : int gt =
  Node (33,
        [Node (12,[]);
         Node (77, 
               [Node (37, 
                      [Node (14, [])]); 
                Node (48, []); 
                Node (103, [])])
        ])

(** makes leaf nodes -- given*)
let mk_leaf (n:'a) : 'a gt =
    Node (n,[])

(*****
1. height: that given a general tree returns its height. 
            The height of a tree is the length of the longest (in terms of number of nodes) 
            path from the root to a leaf. 
            Eg.       
                # height t;;
                - : int = 4
****)
let rec maxHeightHelper gt = 
  match gt with 
  |[] -> 0    
  |h::[] -> h
  |h::t -> max h (maxHeightHelper t)

let rec height t =      
  match t with
  | Node(l,[])-> 1 
  | Node(l,r)-> 1+maxHeightHelper (List.map height r)



(*****
2. size: that given a general tree returns its size. 
          The size of a general tree consists of the number of nodes.
              # size t;;
               - : int = 7
****)
let rec size t =
  match t with
  | Node(l,[]) -> 1
  | Node(_,r) -> 1 + List.fold_left (fun g t -> g + size t ) 0 r
 


(*****
3. paths_to_leaves t: returns a list with all the paths from the root to the leaves of the general tree t. 
                      - Let n be the largest number of children of any node in t. 
                      - A path is a list of numbers in the set {0, 1, . . . , n − 1} such that if we follow 
                      it on the tree, it leads to a leaf. 
                      - The order in which the paths are listed is irrelevant. 
                      Eg.
                          # paths_to_leaves t;;
                          - : int list list = [[0];[1;0;0];[1;1];[1;2]]
****)
let rec paths_to_leaves t =
  match t with 
  | Node(l,[]) -> [[]]
  | Node (l,r) ->  
      List.flatten @@ (List.mapi (fun x g -> (List.map (fun t -> x::t) g)) (List.map paths_to_leaves r))



(*****
4. is_leaf_perfect: that determines whether a general tree is leaf perfect. 
                    A general tree is said to be leaf perfect if all leaves have the same depth. 
                    Eg.
                        # is_leaf_perfect t;;
                        - : bool = false
****)
let rec is_leaf_perfect t =
  let rec perf_help gt =
        match gt with
        | [] -> true
        | [_] -> true
        | h::x::t ->
            if (List.length h) != (List.length x)
            then false
            else perf_help (x :: t) 
  in 
  perf_help (paths_to_leaves t)



(*****
5. preorder: that returns the pre-order traversal of a general tree. 
              Eg.
                  # preorder t;;
                   - : int list = [33 ; 12; 77; 37; 14; 48; 103]
****)
let rec preorder t =
  match t with
  | Node(l,[]) -> [l]
  | Node(l,r) -> (List.flatten ([l]::(List.map preorder r))) 


  
(*****
6. mirror: that returns the mirror image of a general tree. 
              Eg.
                  # mirror t;;
                   - : int gt =
                                Node (33,
                                    [Node (77,[Node (103,[]); Node (48,[]); Node (37, [Node (14,[])])]);
                                      Node (12 , [])])
****)
let rec mirror t =
  match t with
  | Node(l,r) -> Node(l, List.map mirror (List.rev r))



(*****
7. mapt f t: that produces a general tree resulting from t by mapping function f to each data item in d. 
              Eg.
                  # mapt ( fun i -> i >20) t;;
                   - : bool gt =
                                Node (true,
                                  [Node (false,[]);
                                    Node (true,
                                    [Node (true, [Node (false,[])]); Node (true,[]); Node (true,
                                [])])])
****)
let rec mapt f t =
  match t with
  | Node(l,r) -> Node(f l, (List.map (mapt f) r ))       



(*****
8. foldt f t: that encodes the recursion scheme over general trees. 
              Its type is
                          foldt: (’a -> ’b list -> ’b) -> ’a gt -> ’b
              For example, here is how one may define sumt and memt using foldt:
                    let sumt t =
                        foldt ( fun i rs -> i + List . fold_left ( fun i j -> i+j ) 0 rs ) t
                    let memt t e =
                        foldt ( fun i rs -> i= e || List . exists ( fun i -> i) rs ) t
              For example,
                    # sumt t ;;
                      - : int = 324
                    # memt t 12;;
                      - : bool = true
                    # memt t 33;;
                      - : bool = true
                    # memt t 35;;
                      - : bool = false

        similiar to mapt
****)
let rec foldt f t = 
  match t with
  | Node(l,r) -> f l (List.map (foldt f) r ) 


(*****
9. Implement mirror’ using foldt. It should behave just like Exercise 6.
****)
let mirror' t  = 
  foldt (fun i rs -> Node( i, List.rev rs )) t    


(** sumt using foldt -- given *)
let sumt t =
    foldt ( fun i rs -> i + List . fold_left ( fun i j -> i+j ) 0 rs ) t

(** memt using foldt -- given *)
let memt t e =
    foldt ( fun i rs -> i= e || List . exists ( fun i -> i) rs ) t
