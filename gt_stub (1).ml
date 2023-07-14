(* 

   HOMEWORK 2 MOST RECENTLY UPDATED 

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


let rec height t =
   match t with
   | Empty -> []
   | Node(d,ch) -> 1 + levels ch
  
    

(*MAYBE*)    
let rec size t =
  match t with
  | Empty -> 0
  | Node(_,ch) -> 1 + sizet ch


let rec paths_to_leaves t =
   match t with 
    | Empty -> []
    | Node (d, Empty) -> [[]]
    | Node (d, ch) ->  List.map(fun l -> 0::1) (paths_to_leaves ch)



let rec is_perfect t =
     match t with 
     | Empty → []
     | _ → Node(d, is_perfect ((height t)-1) d)

(*NEED TO CHANGE FOR GENERAL TREE NOT BT*)
let rec preorder (Node(d,ch)) =
  match d with
  | Empty -> []
  | Node(d,ch) -> [d] @ preorder ch

 
(*NEED TO CHANGE FOR GENERAL TREE NOT BT*)                       
let rec mirror (Node(d,ch)) =
  match d with
  | Empty -> Empty
  | Node(d,ch) -> Node(d,mirror ch)

(*NEED TO CHANGE FOR GENERAL TREE NOT BT*)      
let rec mapt f (Node(d,ch)) =
  match d with
  | Empty -> Empty
  | Node(d,ch) -> Node(f d,mapt f ch)


  (*NEED TO CHANGE FOR GENERAL TREE NOT BT*)      
let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
  match ch with
  | Empty -> a
  | Node(d,ch) -> f d (foldt a f ch) 

  (*  OR USE 
    
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t
*)

let sumt t =
  foldt (fun h r -> h+r) 0

let memt t e = 
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let mirror' t  = 
  match t with
  | Empty -> Empty
  | Node(d,ch) -> Node(d, m ch)
