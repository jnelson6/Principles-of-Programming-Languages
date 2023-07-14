open Ast
open ReM
open Dst


let rec chk_expr : expr -> texpr tea_result = function 
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Pair(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    return @@ PairType(t1,t2)
  | Unpair(id1,id2,e1,e2) ->
    chk_expr e1 >>=
    pair_of_pairType "unpair: " >>= fun (t1,t2) ->
    extend_tenv id1 t1 >>+
    extend_tenv id2 t2 >>+
    chk_expr e2  
  | Letrec(id,param,tParam,tRes,body,target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")
  (* EXPLICIT-REFS *)
  | BeginEnd([]) -> 
    failwith -> "implement me"
  
  | BeginEnd(es) ->
    List.fold_left (fun v e -> type_of_expr en e) UnitType es 
  | NewRef(e) ->
    type_of_expr e >>= fun r -> return @@ RefType(r) 
  | DeRef(e) ->
    match type_of_expr en e with
      | RefType(x) -> x
      | _ -> failwith "failed deref"
  | SetRef(e1,e2) ->  
    let t1 = type_of_expr en e1 
    in type_of_expr e2 >>= fun t2 ->
    if(t1 = RefType(t2)) 
    then return UnitType
    else error "need same type" 
  (* list *)
  | EmptyList(t) -> return ListType(t) 
  | Cons(h, t) ->
    type_of_expr h >>= fun v1 ->
    type_of_expr t >>= 
    arg_of_listType "cons: " >>= fun v2 -> 
    if v1=v2
    then return (ListType(v2))
    else error "cons: type of head and tail do not match " 
  | IsNullL(e) ->
    (match (type_of_expr en e) with     (***NOT MINE*)
    | ListType(_) -> BoolType
    | _ -> failwith "Must use Null on a list")
  | Hd(e) ->
    type_of_expr e >>= 
    arg_of_listType "hd: " >>= fun l1 ->
    return (ListType(l1))
  | Tl(e) ->
    type_of_expr e >>= 
    arg_of_listType "tl: " >>= fun l1 ->
    return (ListType(l1))


    
  (* tree *)
  | EmptyTree(t) ->
    return TreeType(t)
  | Node(de, le, re) ->
    type_of_expr de >>= fun data ->
    type_of_expr le >>=
    arg_of_treeType "node: " >>= fun lt -> 
    type_of_expr re >>=
    arg_of_treeType "node: " >>= fun rt -> 
    if rt=lt=de
    then return TreeType(rt)
    else error "node: Expected a tree type"
  | IsNullT(t) ->
    type_of_expr t >>=  
    arg_of_treeType "nullt: " >>= fun t1 ->
    return BoolType(t1)  
  | GetData(t) ->
    type_of_expr t >>= 
    arg_of_treeType "getdata: " >>= fun t1 -> 
    (match t1 with
     | TreeType(t) -> return (TreeType(t1))
     | _ -> error "getdata: Empty tree")
  | GetLST(t) ->
    type_of_expr t >>= 
    arg_of_treeType "getlst: " >>= fun t1 -> 
    (match t1 with
     | TreeType(t) -> return (TreeType(t1))
     | _ -> error "getlst: Empty tree")
  | GetRST(t) ->
    type_of_expr t >>= 
    arg_of_treeType "getrst: " >>= fun t1 -> 
    (match t1 with
     | TreeType(t) -> TreeType(t1)
     | _ -> error "getrst: Empty tree")
  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> error "chk_expr: implement"    



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_expr
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_expr
  in run_teac (c >>= fun t -> return @@ Ast.string_of_texpr t)



