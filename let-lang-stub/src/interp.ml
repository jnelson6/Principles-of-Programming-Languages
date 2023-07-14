open Ast
open Ds

(** [eval_expr e] evaluates expression [e] producing a result *)
let rec eval_expr : expr -> env -> exp_val result =
  fun e en ->
  match e with
  | Int(n) -> return (NumVal n)
  | Var(id) -> apply_env id en
  | Sub(e1,e2) ->
    eval_expr e1 en >>= 
    int_of_numVal >>= fun n ->
    eval_expr e2 en >>= 
    int_of_numVal >>= fun m ->
    return (NumVal (n-m))
  | Add(e1,e2) ->
    eval_expr e1 en >>= 
    int_of_numVal >>= fun n ->
    eval_expr e2 en >>= 
    int_of_numVal >>= fun m ->
    return (NumVal (n+m))
  | Mul(e1,e2) ->
    eval_expr e1 en >>= 
    int_of_numVal >>= fun n ->
    eval_expr e2 en >>= 
    int_of_numVal >>= fun m ->
    return (NumVal (n*m))      
  | Div(e1,e2) ->
    eval_expr e1 en >>= 
    int_of_numVal >>= fun n ->
    eval_expr e2 en >>= 
    int_of_numVal >>= fun m ->
    if m=0
    then error "Division by zero"
    else return (NumVal (n/m))
  | IsZero(e) ->
    eval_expr e en >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n=0))
  | ITE(e1,e2,e3) ->
    eval_expr e1 en >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2 en
    else eval_expr e3 en
  | Let(id,e1,e2) ->
    eval_expr e1 en >>= fun w ->
    eval_expr e2 (extend_env id w en)
  | Abs(e) ->
    eval_expr e en >>=
    int_of_numVal >>= fun n ->
    return (NumVal (abs n))
  | Pair(e1,e2) ->
    failwith "not implemented"
  | Fst(e) ->
    failwith "not implemented"
  | Snd(e) ->
    failwith "not implemented"
  | Debug(_e) ->
    string_of_env en >>= fun str ->
    print_endline str; 
    error "Debug called"
  | _ -> failwith "not implemented"

(** [parse s] parses [s] into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(** [interp e] interpret expression [e] returning a result *)
let interp (e:string)  =
  let c = e |> parse |> eval_expr
  in run c
  
  



(* 
Errors in LET

Division by zero
Undeclared variable
Expected a number
Expected a boolean
Debug called


*)
