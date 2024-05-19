module F = Format
open Ast
open Value

let rec len lst =
  match lst with
  | _ :: r -> 1 + len r
  | [] -> 0

let rec interp_expr (e : Ast.expr) (g: FStore.t) (s: Store.t) : Value.t = 
  match e with
  | Num n -> NumV n
  | Add (e1, e2) ->
    let v1 = interp_expr e1 g s in
    let v2 = interp_expr e2 g s in
    let sum v1 v2 =
      match (v1, v2) with
      | (NumV n1, NumV n2) -> NumV (n1 + n2)
    in sum v1 v2
  | Sub (e1, e2) -> 
    let v1 = interp_expr e1 g s in
    let v2 = interp_expr e2 g s in
    let sub v1 v2 =
      match (v1, v2) with
      | (NumV n1, NumV n2) -> NumV (n1 - n2)
    in sub v1 v2
  | Id x ->
    if Store.mem x s then Store.find x s
      else failwith ("Free identifier: " ^ x)
  | LetIn (x, e1, e2) ->
    let v1 = interp_expr e1 g s in
    interp_expr e2 g (Store.add x v1 s)
  | Call (x, el) ->
    if FStore.mem x g then
      match FStore.find x g with
      | (pl, e) ->
        if len el <> len pl then
          failwith ("The number of arguments of " ^ x ^ " mismatched: Required: " 
          ^ string_of_int (len pl) ^ ", Actual: " ^ string_of_int (len el))
        else
          let rec bound_to_parameter el pl (s_func : Store.t) =
          match (el, pl) with
          | (e1 :: elr , p1 :: plr) ->
            let v1 = interp_expr e1 g s 
            in bound_to_parameter elr plr (Store.add p1 v1 s_func)
          | ([], []) -> interp_expr e g s_func
          | _ -> failwith ""
          in bound_to_parameter el pl Store.empty
    else failwith ("Undefined function: " ^ x)
  

let interp_fundef (d: Ast.fundef) (g: FStore.t) : FStore.t = 
  match d with 
  | FunDef (x, pl, e) -> FStore.add x (pl, e) g

let interp (p: Ast.prog) : Value.t = 
  match p with
  | Prog (dl, e) ->
    let rec store_function dl g =
      match dl with
      | d :: dlr -> store_function dlr (interp_fundef d g)
      | [] -> g
    in
    let g = store_function dl FStore.empty
    in
    interp_expr e g Store.empty