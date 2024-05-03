(* Name: Marcel Castillo, Mohin Patel *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System *)
(* CS496 HW5 *)

open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser
       
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
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")

  (* Part 1: Type Checking References *)
  | BeginEnd([]) ->
    return UnitType
  | BeginEnd(es) -> 
    List.fold_left (fun r e -> r >>= fun _ -> chk_expr e) (return UnitType) es 
  | NewRef(e) -> 
    chk_expr e >>= fun t ->
      return @@ (RefType t)
  | DeRef(e) -> 
    chk_expr e >>= (fun t -> 
        match t with
        | RefType t -> return t
        | _ -> error "deref: expected a reference type")
  | SetRef(e1, e2) ->
    chk_expr e1 >>= (fun t1 ->
      match t1 with
      | RefType a -> 
          chk_expr e2 >>= (fun b ->
              if a = b then return @@ UnitType
              else error "e1 and e2 are of different types.")
      | _ -> error "setref: expected a reference type")

  (* Part 2: Lists *)
  | EmptyList(None) ->
    error "emptylist: needs a type"
  | EmptyList(Some t) ->
      return @@ (ListType t)
  | Cons(h, t) ->
      chk_expr h >>= fun t1 ->
      chk_expr t >>= (fun t2 ->
          match t2 with
          | ListType elem_type when t1 = elem_type -> return @@ (ListType t1)
          | ListType _ -> error "cons: type of head and tail do not match"
          | _ -> error "cons: tail must be a list type")
  | IsEmpty(e) ->
      chk_expr e >>= (fun t ->
          match t with
          | ListType _ -> return BoolType
          | _ -> error "empty: expects a list type")
  | Hd(e) ->
      chk_expr e >>= (fun hd ->
          match hd with
          | ListType t -> return t
          | _ -> error "hd: expected a list type")
  | Tl(e) ->
      chk_expr e >>= (fun tl ->
          match tl with
          | ListType t -> return @@ (ListType t)
          | _ -> error "tl: expected a list type")

  (* Part 3: Trees *)
  | EmptyTree(None) ->
    error @@ "emptytree: needs a type"
  | EmptyTree(Some t) ->
    return @@ (TreeType t)
  | Node(de, le, re) ->
    chk_expr de >>= fun d ->
    chk_expr le >>= arg "node: " >>= fun l ->
    chk_expr re >>= arg "node: " >>= fun r->
    if l = r 
    then return @@ (TreeType r)
    else error "node: node type must match left and right tree types"
  | CaseT(target,emptycase,_,_,_,nodecase) ->
    chk_expr target >>= arg "caseT: " >>= fun t ->
    chk_expr emptycase >>= fun e ->
    begin
    match nodecase with
    | d -> return e
    | lt -> return @@ (TreeType e)
    | rt -> return @@ (TreeType e)
    | _ ->
      chk_expr nodecase >>= fun n ->
      if e = n
      then return @@ e
      else error "caseT: nodecase and emptycase must have the same type"
      end

  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> failwith "chk_expr: implement"    
and
  chk_prog (AProg(_,e)) =
  chk_expr e
and
  arg msg t = 
  match t with
  | TreeType t -> return t
  | _ -> error (msg ^ "expected a tree type")

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)



