(** This file defines the result type *)



type 'a result = Ok of 'a | Error of string
                   
let return : 'a -> 'a result =
  fun ev ->
  Ok ev

let error : string -> 'a result =
  fun s ->
  Error s

let (>>=) : 'a result -> ('a -> 'b result) -> 'b result =
  fun c f ->
  match c with
  | Error s -> Error s
  | Ok ev -> f ev


type exp_val =
  | NumVal of int
  | BoolVal of bool

type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env

let rec apply_env =
  fun id en ->
  match en with
  | EmptyEnv -> error (id^" not declared!")
  | ExtendEnv(id',ev,tail) ->
    if id=id'
    then return ev
    else apply_env id tail

let extend_env : string -> exp_val -> env -> env =
  fun id ev en ->
  ExtendEnv(id,ev,en)
               
let run c =
  c EmptyEnv
  
let int_of_numVal : exp_val -> int result =
  fun ev ->
  match ev with
  | NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool result =
  fun ev ->
  match ev with
  | BoolVal b -> return b
  | _ -> error "Expected a boolean!"


let string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
                      
let rec string_of_env' ac = function
  | EmptyEnv -> ac
  | ExtendEnv(id,v,env) -> string_of_env' ((id^":="^string_of_expval v)::ac) env

let string_of_env : env -> string result =
  fun env ->
  match env with
  | EmptyEnv -> Ok ">>Environment:\nEmpty"
  | _ -> Ok (">>Environment:\n"^ String.concat ",\n" (string_of_env' [] env))
