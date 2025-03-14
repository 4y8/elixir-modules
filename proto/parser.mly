%{
  open Core
  open Formal1

  let atoms = ref Utils.SSet.empty
  

  let add_atom x =
    if not Utils.SSet.(mem x !atoms) then begin
      Format.printf "type atom_type_%s = {%s:{}};
atom_expr_%s = {%s={}};\n" x x x x;
      atoms := Utils.SSet.add x !atoms
    end
%}

%token PARAM TYPE OPAQUE BEHAVIOUR CALLBACK DEFMODTYPE DEFMODULE DEF DO END DEFP PASSTHROUGH
%token <string> IDENT
%token <string> ATOM
%token EQ DCOL LPAR RPAR LCUR RCUR LSQU RSQU SCOL COMMA DOT ARR PERC EOF
%right ARR

%start program
%type <(string * Formal1.program) list> program

%%

type_decl: x = IDENT DCOL t = typ { x, t } ;
type_assign: x = IDENT EQ t = typ { x, Rei t } ;

modtype:
  | v = IDENT { Var v }
  | v = IDENT LSQU l = separated_list(COMMA, type_assign) RSQU { App (Var v, Struct l) }
;

typ:
  | x = IDENT LPAR l = separated_list(COMMA, expr) RPAR 
    { Expr (List.fold_left (fun f x -> App (f, x)) (Var x) l) }
  | LPAR t = typ RPAR { t }
  | t1 = typ ARR t2 = typ { FTy ("_", t1, t2, I) }
  | a = ATOM { add_atom a; TAtom a }
  | PERC LCUR l = separated_list(SCOL, type_decl) RCUR { Sig l }
  | LCUR l = separated_list(COMMA, typ) RCUR { TTuple l }
  | m = modtype { Expr m }
  | m = modtype DOT v = IDENT { Expr (Dot (m, v)) }
;

expr_assign: x = IDENT EQ e = expr { x, e } ;

expr:
  | a = ATOM { add_atom a; EAtom a }
  | x = IDENT { Var x }
  | LPAR e = expr RPAR { e }
  | e = expr DOT x = IDENT { Dot (e, x) }
  | f = expr LPAR l = separated_list(COMMA, expr) RPAR
    { List.fold_left (fun f x -> App (f, x)) f l }
  | PERC LCUR l = separated_list(SCOL, expr_assign) RCUR { Struct l }
  | LCUR l = separated_list(COMMA, expr) RCUR { Tuple l }
  | LSQU l = separated_list(COMMA, expr) RSQU
    { List.fold_right (fun x xs -> App (App (Var "List.cons", x), xs)) l (Var "List.nil") }
  | v = IDENT LSQU l = separated_list(COMMA, type_assign) RSQU { App (Var v, Struct l) }
;

moduletype_decl:
  | PARAM x = IDENT { BParam (x, []) }
  | PARAM x = IDENT LPAR l = separated_list(COMMA, IDENT) RPAR { BParam (x, l) }
  | TYPE x = IDENT EQ t = typ { BType (x, [], t) }
  | TYPE x = IDENT LPAR l = separated_list(COMMA, IDENT) RPAR EQ t = typ { BType (x, l, t) }
  | OPAQUE x = IDENT { BOpaque (x, []) }
  | OPAQUE x = IDENT LPAR l = separated_list(COMMA, IDENT) RPAR { BOpaque (x, l) }
  | CALLBACK x = IDENT DCOL t = typ { BCallback (x, t) }
;

moduletype: DEFMODTYPE b = IDENT DO l = moduletype_decl* END { b, B l } ;

arg: x = IDENT DCOL t = typ { x, t };

module_decl:
  | PARAM x = IDENT { MParam (x, []) }
  | PARAM x = IDENT LPAR l = separated_list(COMMA, IDENT) RPAR
    { MParam (x, l) }
  | PARAM x = IDENT EQ t = typ { MParamE (x, [], t) } 
  | PARAM x = IDENT LPAR l = separated_list(COMMA, IDENT) RPAR EQ t = typ
    { MParamE (x, l, t) }
  | TYPE x = IDENT EQ t = typ { MType (x, [], t) }
  | TYPE x = IDENT LPAR l = separated_list(COMMA, IDENT) RPAR EQ t = typ
    { MType (x, l, t) }
  | OPAQUE x = IDENT EQ t = typ { MOpaque (x, [], t) }
  | OPAQUE x = IDENT RPAR l = separated_list(COMMA, IDENT) LPAR EQ t = typ
    { MOpaque (x, l, t) }
  | BEHAVIOUR x = IDENT { MBehaviour x }
  | DEF f = IDENT LPAR x = separated_list(COMMA, arg) RPAR DCOL t = typ EQ
      e = expr { MDef (false, f, x, t, e) }
  | DEFP f = IDENT LPAR x = separated_list(COMMA, arg) RPAR DCOL t = typ EQ
      e = expr { MDef (true, f, x, t, e) }
  | PASSTHROUGH x = IDENT
    { MBlk [MParam (x, []); MType (x, [], Expr (Var x))] }
  | PASSTHROUGH x = IDENT LPAR l = separated_list(COMMA, IDENT) RPAR
    { MBlk
	[MParam (x, l);
	 MType (x, l, Expr (List.fold_left (fun f x -> App (f, Var x)) (Var x) l))] }
;

modul: DEFMODULE m = IDENT DO l = module_decl* END { m, M l }
;

top_level:
  | m = modul { m }
  | m = moduletype { m }
;

program:
  l = top_level* EOF { l }
;

%%
