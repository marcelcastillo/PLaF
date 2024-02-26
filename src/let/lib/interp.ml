open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
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
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (l,_) ->
    return l
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (_,r) ->
    return r
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"

  (* Binary Tree Aditions*)
  | IsEmpty(e) -> 
    eval_expr e >>=
    tree_of_treeVal >>= fun t ->
      return (BoolVal (t = Empty))

  | EmptyTree(_t) ->
    return (TreeVal Empty)

  | Node(e1,e2,e3) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= 
    tree_of_treeVal >>= fun ev2 ->
    eval_expr e3 >>=
    tree_of_treeVal >>= fun ev3 ->
    return (TreeVal (Node(ev1, ev2, ev3)))

  | CaseT(e1,e2,id1,id2,id3,e3) -> 
    eval_expr e1 >>=
    tree_of_treeVal >>= fun t ->
    (match t with
    | Empty -> eval_expr e2
    | Node(v1,v2,v3) -> 
      extend_env id1 v1 >>+
      extend_env id2 (TreeVal(v2)) >>+
      extend_env id3 (TreeVal(v3)) >>+
      eval_expr e3)

    (* Record Additions *)
  | Record(fs) ->
    (*Extract expressions from fs *)
    let rec extract_expr lst =
      match lst with
      | [] -> []
      | (_, (_, expr))::t -> expr::(extract_expr t) 
    in
    (* Evaluate the extracted expressions *)
    eval_exprs (extract_expr fs) >>= fun vs ->
      (* Combine evaluated values with the field names*)
    let rec combine_fields_with_values fields values =
      (match fields, values with
      | [], [] -> [] (*Both lists are empty *)
      | (name, (_,_)) :: fs_tail, v::vs_tail -> (name, v)::combine_fields_with_values fs_tail vs_tail
      | _, _ -> return Error "Lists fs and vs have different lengths")(* Error handling for mismatched list lengths *)
      in
      return (RecordVal(combine_fields_with_values fs vs))


  | _ -> failwith "Not implemented yet!"


and
  eval_exprs : expr list -> (exp_val list) ea_result =
  fun es ->
  match es with
  | [] -> return []
  | h::t -> eval_expr h >>= fun i ->
    eval_exprs t >>= fun l ->
      return (i::l)



(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


