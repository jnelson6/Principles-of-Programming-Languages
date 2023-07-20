(* 
* Assignment: SA1 - interp.ml
* Author: Julia Nelson
* Date: 06/25/2021
* Pledge: "I pledge my honor that I have abided by the Stevens Honor System."
* Description: Special Assignment 1 - Extending Rec - RECQ
*)

open Ast
open Ds


let rec apply_proc : exp_val -> exp_val -> exp_val ea_result =
  fun f a ->
  match f with
  |  ProcVal (id,body,env) ->
    return env >>+
    extend_env id a >>+
    eval_expr body
  | _ -> error "apply_proc: Not a procVal"
and
  eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) -> return (NumVal n)
  | Var(id) -> apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(v,def,body) ->
    eval_expr def >>= 
    extend_env v >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return (fst p) 
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return (snd p)
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  -> 
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2
  | Letrec(id,par,e1,e2) ->
    extend_env_rec id par e1 >>+
    eval_expr e2
  (*-----------------------------------------------------------------------------------------------*)
(*------------------------------------- Start TODO SECTIONS -------------------------------------*)


  (* EmptyQueue : 
   *  Creates an empty queue. 
   *)
  | EmptyQueue ->
    return (QueueVal [])


  (* AddQ :
   *  Adds an element to the back of the queue.
   *
   * Please note that the order of the QueueVal from right to left represents the 
   * queue from front to back. In this example, the first value is 1, followed by 2, 
   * followed by 3 and finally ending with 4.
   *
   * like "push"
   *  ________________________________________________________________________________________
   *  utop # interp " addq (4 , addq (3 , addq (2 , addq (1 , emptyqueue ))))";;
   * : exp_ val Recq.Ds.result = Ok ( QueueVal [ NumVal 4; NumVal 3; NumVal 2; NumVal 1])
     ________________________________________________________________________________________
    *)
  | AddQ(e1,e2) ->
    eval_expr e1 >>=  fun ev1 ->          
    eval_expr e2 >>= 
    list_of_queueVal >>= fun ev2 -> 
    return (QueueVal (ev1 :: ev2))  (* Basic Walkthrough:
                                                        (1, empty)      -> [1]
                                                    (2, (1, empty))     -> [2,1]  
                                                (3, (2, (1, empty)))    -> [3,2,1]
                                            (4, (3, (2, (1, empty))))   -> [4,3,2,1]...  
                                     *)

  (* Element : 
   *  Returns the front most element on the queue without modifying the queue. 
   * If the queue is empty, return an error indicating that the operation failed 
   * due to an empty queue. 
   * return "Remove: Queue is empty."
   "peek"
   *)
  | Element(e) ->         
    eval_expr e >>=  
    list_of_queueVal >>= fun l ->                
    (match l with 
    | [] -> error "Remove: Queue is empty." 
    | _ -> return QueueVal (List.hd (List.rev l)) )   (* Basic Walkthrough:
                                                                (4, (3, (2, (1, empty)))) -> l = [4, 3,2,1]
                                                                            list.rev l    -> l = [1,2,3]
                                                                            list.hd l     -> NumVal 1
                                                   *)

  (* Remove : 
    like "pop"
   *  Removes the first element from the queue.
   *
   * Building upon the example introduced in the addq operation, the remove operation removes
   * the first value of 1, leaving 2 as the next front value followed by 3 and 4.
   * If the queue is empty, return an error indicating that the operation failed due to an 
   * empty queue. 
   * please return "Remove: Queue is empty."
   *)
  | Remove(e) ->
    eval_expr e >>=  
    list_of_queueVal >>= fun l ->
    (match l with 
    | [] -> error "Remove: Queue is empty." 
    | _ -> return QueueVal (List.rev (List.tl (List.rev l) ) )  )  (* Basic Walkthrough:
                                                                               (4, (3, (2, (1, empty)))) -> [4,3,2,1]...  
                                                                                                        l = [4,3,2,1]
                                                                                           l = List.rev l = [1,2,3,4]
                                                                                            l = List.tl l = [2,3,4]
                                                                                           l = List.rev l = [4,3,2] 
                                                                    *)
              (* first Remove attempt:  | Remove(e) ->
                                            eval_expr e >>= 
                                            list_of_queueVal >> fun r ->
                                            (match r with
                                            | hd::tl -> r <- tl; hd 
                                            | [] -> failwith "Remove: Queue is empty." )                         
               *)

  (* IsEmpty : 
   * Returns a boolean indicating whether the queue is populated or not.
   *)
  | IsEmpty(e) ->
    eval_expr e >>=
    list_of_queueVal >>= fun l -> 
    return (BoolVal (l = []) )           

  (* Size : 
   *  Returns the amount of elements in the queue
   *)
  | Size(e) ->
    eval_expr e >>=
    list_of_queueVal >>= fun l -> 
    return (NumVal (List.length l))

(*------------------------------------- END TODO SECTIONS -------------------------------------*)
(*---------------------------------------------------------------------------------------------*)
 | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | _ -> error "Not implemented yet!"

(** [parse s] parses string [s] into an ast *)
let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(** [interp s] parses [s] and then evaluates it *)
let interp (s:string) : exp_val result =
  let c = s |> parse |> eval_expr
  in run c


