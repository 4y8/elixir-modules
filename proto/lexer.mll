{
  open Parser

  let dolkw = ["param", PARAM; "opaque", OPAQUE; "type", TYPE; "passthrough", PASSTHROUGH; "callback", CALLBACK; "behaviour", BEHAVIOUR]
    |> Utils.smap_of_list

  let kw =
    ["do", DO; "end", END; "defmodule", DEFMODULE; "def", DEF; "defp", DEFP;
     "callback", CALLBACK; "defmodtype", DEFMODTYPE]
    |> Utils.smap_of_list
}

let ident = ((['a' - 'z'] | ['A' - 'Z'] | '_' | ['0'-'9'])+)('.' (['a' - 'z'] | ['A' - 'Z'] | '_' | ['0'-'9'])+)*

rule lexer = parse
  | [' ' '\t' '\r' '\n'] { lexer lexbuf }
  | ("#" [^'\n']*) '\n' { lexer lexbuf }
  | '$' (ident as s) { Utils.SMap.find s dolkw }
  | '.' { DOT }
  | ident as s
    { match Utils.SMap.find_opt s kw with
      | None -> IDENT s
      | Some t -> t }
  | "@behaviour" { BEHAVIOUR }
  | '=' { EQ }
  | '(' { LPAR }
  | ')' { RPAR }
  | '{' { LCUR }
  | '}' { RCUR }
  | '[' { LSQU }
  | ']' { RSQU }
  | ':' (ident as s) { ATOM s }
  | ':' { DCOL }
  | ';' { SCOL }
  | ',' { COMMA }
  | '%' { PERC }
  | "->" { ARR }
  | eof { EOF }
  | _ as c { Printf.eprintf "Unknown character : %c" c; exit 1 }

{

}
