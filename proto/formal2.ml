type bdecl =
  | BType of string * string list * Core.t
  | BOpaque of string * string list | BCallback of string * Core.t

type mdecl =
  | MType of string * string list * Core.t
  | MOpaque of string * string list * Core.t
  | MDef of bool * string * (string * Core.t) list * Core.t * Core.e

type behaviour = { bparam : (string * string list) list ; bbody : bdecl list }

type modul = { mparam : (string * string list) list
             ; mbehaviour : Core.d list Utils.SMap.t
             ; mbody : mdecl list}

type program =
  | M of modul
  | B of behaviour
