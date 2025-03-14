open Format
open Core

let rec print_type fmt = function
  | Expr (Rei t) -> print_type fmt t
  | TTuple l ->
     let l = List.mapi (fun i e -> sprintf "_%d:" i, e) l in
     let print_t fmt (s, e) =
       fprintf fmt "%s@ %a" s print_type e
     in
     fprintf fmt "{"; pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") print_t fmt l; fprintf fmt "}"

  | Expr (EAtom a) -> fprintf fmt "atom_type_%s" a
  | Expr e -> print_expr fmt e
  | Sig d -> fprintf fmt "@[{%a}@]" print_decl d
  | FTy (x, t, t', p) ->
     let arr = if p = P then "=>" else "->" in
     fprintf fmt "@[(%s:%a)@]@ %s@ %a" x print_type t arr print_type t'
  | Type -> fprintf fmt "type"
  | Equ e -> fprintf fmt "@[(= %a)@]" print_expr e
  (* | Inter (t, t') -> fprintf fmt "@[%a@]@ /\\@,@[%a@]" print_type t print_type t' *)
  | Inter (t, _)  -> print_type fmt t
  | TAtom a -> fprintf fmt "atom_type_%s" a
and print_decl fmt = function
  | [] -> fprintf fmt ""
  | (x, t) :: tl -> fprintf fmt "%s : %a;@ %a" x print_type t print_decl tl
and print_expr fmt = function
  | Cst x
  | Var x -> fprintf fmt "%s" x
  | If (e, e', e'') ->
     fprintf fmt "if %a@,then %a@, else %a" print_expr e print_expr e'
       print_expr e''
  | Struct n -> fprintf fmt "@[{%a}@]" print_bind n
  | Dot (e, x) -> fprintf fmt "%a.%s" print_expr e x
  | Fun (x, t, e) ->
     fprintf fmt "fun @[(%s : %a)@]@ =>@ %a" x print_type t print_expr e
  | App (e, e') ->
     fprintf fmt "@[%a@] @[(%a)@]" print_expr e print_expr e'
  | Rei (Expr e) -> print_expr fmt e
  | Rei t ->
     fprintf fmt "type @[(%a)@]" print_type t
  | Seal (e, t) ->
     fprintf fmt "@[(%a)@]@ :>@ @[(%a)@]" print_expr e print_type t
  | EAtom a ->
     fprintf fmt "atom_expr_%s" a
  | Tuple l ->
     let l = List.mapi (fun i e -> sprintf "_%d=" i, e) l in
     let print_e fmt (s, e) =
       fprintf fmt "%s@ %a" s print_expr e
     in
     fprintf fmt "{"; pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") print_e fmt l; fprintf fmt "}"
and print_bind fmt = function
  | [] -> fprintf fmt ""
  | (x, e) :: tl -> fprintf fmt "%s = %a;@ %a" x print_expr e print_bind tl
