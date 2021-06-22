type name = string

type t = (name * Value.t) list

let lookup env n =
  try
    List.assoc n env
  with
    Not_found -> failwith ("lookup_env: " ^ n)

let update env (n,v) =
  let rec scan l = match l with
    | [] -> [n,v]
    | (n',v')::rest -> if n=n' then (n,v)::rest else (n',v') :: scan rest in
  scan env

let equal env env' =
  let incl e e' = List.for_all (fun (n,v) -> lookup e' n = v) e in
  incl env env' && incl env' env (* Not very efficient but easy definition *)
