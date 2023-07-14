(*
 * Author: Julia Nelson
 * Date:  3/17/2021
 * Pledge: "I pledge my honor that I have abided by the Stevens Honor System."
 * Description: cs-496 Quiz 3. - Extended LET with Pair & Unpair
 * 
 *)

 
open Ast
open Ds


(**********

Exercise 2.2.4. Consider another extension to LET with pairs. 
Its concrete syntax is: 
⟨Expression⟩ ::= pair(⟨Expression⟩, ⟨Expression⟩)
    | unpair(⟨Identifier⟩, ⟨Identifier⟩) = ⟨Expression⟩ in ⟨Expression⟩

Pairs are constructed in the same way as in Exercise 2.2.3. 
However, to eliminate pairs instead of fst and snd we now have unpair. 
The expression unpair (x,y)=e1 in e2 evaluates e1, makes sure it is a pair with components v1 and v2 
and then evaluates e2 in the extended environment where x is bound to v1 and y to v2. 

Examples of programs in this extension are the first three examples in Exercise 2.2.3 and:



1. unpair (x,y) = pair(3, pair(5 , 12)) in x is a program that evaluates to Ok (NumVal 3). 
2. The program let x = 34 in unpair (y,z)=pair(2,x) in z evaluates to Ok (NumVal 34).
************) 


(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
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
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
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
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | _ -> error "Not implemented yet!"
(*****************LOOK AT THIS *********************)
(***   Pair - expr*expr    ****)
  | Pair(e1,e2)  ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    (v1,v2) (* Does this need to return??  or doI need to change/add to the expr_val?  in ds.ml for pair like int/bool? *)
(***   Unpair - string*string*expr*expr  (like 2 LETs) *****)
  | Unpair(id1,id2,e1,e2)  ->
    eval_exp e1 >>=
    eval_expr e2 >>= fun (v1,v2) ->
    extend_env id1 v1 >>+
    extend_env id2 v2 >>+
    eval_expr e2

(** [parse s] parses string [s] into an ast *)
let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_expr
  in run c




  