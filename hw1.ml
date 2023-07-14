(****************************************************************
      FILENAME:    hw1.ml
      AUTHOR:      Julia Nelson
      DESCRIPTION: Mini Logo (lists & tuples)
      PLEDGE:      "I pledge my honor that I have abided by the Stevens Honor System."
****************************************************************)

(* Note: you may NOT change the names of the functions you are asked to implement below,
          nor the number of arguments they take, nor the types of their arguments. *)



type program = int list
let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]          


(***  1. mirror_image  - returns a program that draws the mirror image of the input program. ****)
let m_helper : int -> int = fun a ->
  match a with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | _ -> failwith "invalid program"
let mirror_image : int list -> int list = fun program1 ->
  match program1 with
  | [] -> []
  | h::t -> List.map m_helper program1



(***  2. rotate_90_letter - given a program returns a new one which draws the same pictures except that
                            they are rotated 90 degrees                                             ****)
(*helper rotate letter*)                            
let rl_helper : int -> int = fun a ->
  match a with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | _ -> failwith "invalid program"

  let rotate_90_letter : int list -> int list = fun program2 ->
  match program2 with
  | [] -> []
  | h::t -> List.map rl_helper program2



(***  3. rotate_90_word - given a list of programs that represent letters returns a new list in which each
                           program draws the same pictures except that they are rotated 90 degrees.       ****)
let rotate_90_word : int list list -> int list list = fun program3 ->
  match program3 with
  | [] -> []
  | h::t -> List.map rotate_90_letter program3




(***  4. repeat -  such that repeat n x returns a list with n copies of x.    ****)
let rec repeat : int -> 'a -> 'a list = fun n x ->
  match n with
  | 0 -> []
  | m -> x:: repeat (m-1) x




(***  5.   pantograph - such that pantograph n p returns a program that draws the same 
                          things as p only enlarged n-fold. 
                      - Your solution must use map. Propose also a solution pantograph_nm without using map.
                      - Propose also a solution pantograph_f using fold.                                    ****)

(*pantograph repeat helper*)
let rec pant_repeat = fun n p ->
  match n with
  | 0 -> []
  | n -> (match p with
    | 0 -> [0]
    | 1 -> [1]
    | _ -> 
        if (p=0) || (p=1)
        then [p]
        else if p>5 
        then failwith "invalid program"
        else p :: pant_repeat (n-1) p
  )

(* pantograph with map *)
let pantograph : int -> int list -> int list = fun n p ->
  List.concat(List.map(pant_repeat n) p)

(* pantograph with no map *)
let rec pantograph_nm : int -> int list -> int list = fun n p ->
  match p with
  | [] -> []
  | h::t -> 
      if (h!=0) && (h!=1)
      then (repeat n h) @ (pantograph_nm n t)
      else  h :: pantograph_nm n t  

(* pantograph with fold *)
let pantograph_f = fun n p ->
  List.fold_left ( fun x y -> x @ (pant_repeat n y )) [] p
  
  


(***  6. coverage - given a starting coordinate and a program returns the list of coordinates that the program visits. 
                  - You may introduce helper functions to make your code more readable.
                  - Also, you need not concern yourself with repetitions                          ****)

  (*helper coverage functions *)  
  let rec helper_coverage p l =
    match l with
    | [] -> []
    | h::t -> (match h with 
            | 2 -> (fst p, (snd p)+1)
            | 3 -> ((fst p)+1, snd p)
            | 4 -> (fst p, (snd p)-1)
            | 5 -> ((fst p)-1, snd p)
            | _ -> (fst p, snd p)
              ):: helper_coverage (match h with 
                                    | 2 -> (fst p, (snd p)+1)
                                    | 3 -> ((fst p)+1, snd p)
                                    | 4 -> (fst p, (snd p)-1)
                                    | 5 -> ((fst p)-1, snd p)
                                    | _ -> (fst p, snd p)
                                  ) t
let coverage : int*int -> int list -> (int*int) list = fun c p ->
    c :: helper_coverage c p



(*** 7. compress - replacing adjacent copies of the same instruction with a tuple (m,n) where m is the instruction and 
                  n is the number of consecutive times it should be executed                                      ***)
(*helper compress*)  
let rec help_compress : (int*int) -> int list -> (int*int) list = fun a p ->
    match p with
    | [] -> [(fst a, snd a)]
    | h::t -> if h = fst a 
              then (help_compress ((fst a),(snd a+1)) t )
              else (fst a, snd a) :: (help_compress (h,1) t)

let compress : int list -> (int*int) list = fun program7 ->
  help_compress (0,0) program7






(*** 8. uncompress - that decompresses a compressed program. Propose a solution using map uncompress_m and another one using fold uncompress_f. ***)
let rec uncompress : (int*int) list -> int list = fun program8 ->
  match program8 with
  | [] -> []
  | (x,y)::t -> (repeat y x) @ (uncompress t)




(** helper for uncompress with map*)
let help_uncompress = fun p -> 
  match p with
  | (m,n) -> repeat n m


(** uncompress with map*)
let uncompress_m : (int*int) list -> int list = fun p8 ->
  match p8 with
  | [] -> []
  | (x,y)::t -> List.concat(List.map help_uncompress p8 )



(** uncompress with fold*)
let uncompress_f : (int*int) list -> int list = fun p ->
    List.fold_right(fun p j -> (help_uncompress p) @ j) p []




(*** 9. Implement a function - that optimizes a program by eliminating redundant pen up and pen down instructions.
                             - For this exercise, you must assume that the pen is initially in the up position.     ***)
(*helper for optimization*)  
let rec helper_optimize: int list -> int -> int list = fun prog9 currS ->
  match prog9 with
  | [] -> []
  | h::t -> 
        if currS<2 && currS=h 
        then helper_optimize t currS
        else h:: helper_optimize t h 


(**optimize starts in state 1*)
let optimize: program -> program = fun p9 ->
  helper_optimize p9 1



