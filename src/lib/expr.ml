type t =
  | EBool of bool
  | EInt of int
  | EVar of Env.name
  | EPrim of string * t list

