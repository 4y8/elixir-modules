type behaviour =
  | BParam of string * string list | BType of string * string list * Core.t
  | BOpaque of string * string list | BCallback of string * Core.t

type modul =
  | MParam of string * string list | MParamE of string * string list * Core.t
  | MType of string * string list * Core.t
  | MOpaque of string * string list * Core.t
  | MBehaviour of string
  | MDef of bool * string * (string * Core.t) list * Core.t * Core.e
  | MBlk of modul list

type program =
  | M of modul list
  | B of behaviour list
