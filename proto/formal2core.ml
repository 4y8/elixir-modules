module F2 = Formal2
open Core

let fresh = ref 0
let fresh_name () =
  incr fresh; Printf.sprintf "param_%d" !fresh

let rec subst_type m = function
  | Expr e -> Expr (subst_expr m e)
  | Sig l -> Sig (subst_decl m l)
  | FTy (x, t, t', p) ->
     FTy (x, subst_type m t, subst_type (Utils.SMap.remove x m) t', p)
  | Type -> Type
  | Equ e -> Equ (subst_expr m e)
  | Inter (t, t') -> Inter (subst_type m t, subst_type m t')
  | TAtom a -> TAtom a
and subst_decl m = function
  | [] -> []
  | (x, t) :: tl ->
     (x, subst_type m t) :: (subst_decl (Utils.SMap.remove x m) tl)
and subst_expr m = function
  | Cst c -> Cst c
  | If (e, e', e'') ->
     If (subst_expr m e, subst_expr m e', subst_expr m e'')
  | Struct l -> Struct (subst_bind m l)
  | Dot (e, x) -> Dot (subst_expr m e, x)
  | Fun (x, t, e) ->
     Fun (x, subst_type m t, subst_expr (Utils.SMap.remove x m) e)
  | App (e, e') -> App (subst_expr m e, subst_expr m e')
  | Rei t -> Rei (subst_type m t)
  | Seal (e, t) ->
     Seal (subst_expr m e, subst_type m t)
  | EAtom a -> EAtom a
  | Tuple l -> Tuple (List.map (subst_expr m) l)
  | Var x ->
     match Utils.SMap.find_opt x m with
     | None -> Var x
     | Some e -> e
and subst_bind m = function
  | [] -> []
  | (x, t) :: tl ->
     (x, subst_expr m t) :: (subst_bind (Utils.SMap.remove x m) tl)

let trans_behaviour = function
  | F2.BType (x, y, t) ->
    x, List.fold_right (fun y t -> FTy (y, Type, t, P)) y (Equ (Rei t))
  | F2.BOpaque (x, y) ->
    x, List.fold_right (fun y t -> FTy (y, Type, t, P)) y Type
  | F2.BCallback (x, t) ->
     x, t

let trans_module = function
  | F2.MDef (false, x, yt, t', e) ->
     (x, List.fold_right (fun (y, t) e -> Fun (y, t, e)) yt e),
     [x, List.fold_right (fun (y, t) t' -> FTy (y, t, t', I)) yt t']
  | F2.MDef (true, x, yt, _, e) ->
     (x, List.fold_right (fun (y, t) e -> Fun (y, t, e)) yt e),
     []
  | F2.MType (x, y, t) ->
     (x, List.fold_right (fun y e -> Fun (y, Type, e)) y (Rei t)),
     [x, List.fold_right (fun y t -> FTy (y, Type, t, P)) y (Equ (Rei t))]
  | F2.MOpaque (x, y, t) ->
     (x, List.fold_right (fun y e -> Fun (y, Type, e)) y (Rei t)),
     [x, List.fold_right (fun y t -> FTy (y, Type, t, P)) y Type]
     

let trans_program = function
  | F2.B b ->
     let d = List.map trans_behaviour b.bbody in
     let sign = Sig (List.map (fun x -> x, Type) b.bparam) in
     let param = fresh_name () in
     let res = List.map (fun x -> x, Equ (Dot (Var param, x))) b.bparam in
     Fun (param, sign, Rei (Sig (res @ d)) )
  | F2.M m ->
     let n, d = List.split (List.map trans_module m.mbody) in
     let d = List.fold_right (@) d [] in
     let param = fresh_name () in
     let ms = Utils.smap_of_list
               (List.map (fun x -> (x, Dot (Var param, x))) m.mparam) in
     let xbtb = Utils.(list_of_smap
                         (SMap.map (subst_decl ms) m.mbehaviour)) in
     let param_type = Sig (List.map (fun x -> x, Type) m.mparam) in
     let res =
       Struct (List.map (fun x -> (x, Dot (Var param, x))) m.mparam @ n)
     in
     let e = if param_type = Sig [] then res else
               Fun (param, param_type, res)
     in
     let sg = Sig (List.map (fun x -> x, Equ (Dot (Var param, x))) m.mparam @ d) in
     let rest =
       List.fold_right
         (fun (b, xbtb) t ->
           let xbtb = List.map (fun (x, t) -> x, Rei t) xbtb in
           Inter (Expr (App (Var b, Struct xbtb)), t))
         xbtb @@
         sg
     in
     let t = if param_type = Sig [] then
               rest
             else FTy (param, param_type, rest, P)
     in
     let f_seal =
       if param_type = Sig [] then
         sg
       else FTy (param, param_type, sg, P)
     in
     Seal (Seal (e, f_seal), t)
