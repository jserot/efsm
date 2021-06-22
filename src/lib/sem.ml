let sem_prim p =
  try
    List.assoc p Builtins.primitives
  with
    Not_found -> failwith "Unknown primitive"
               
let rec sem_expr a =
  let open Expr in
  fun (env: Env.t) ->
  match a with
  | EBool b -> Value.Bool b
  | EInt n -> Value.Int n
  | EVar n -> Env.lookup env n 
  | EPrim (op, args) -> sem_prim op @@ List.map (fun arg -> sem_expr arg env) args

let sem_instr s =
  let open Instr in
  fun (env: Env.t) ->
  match s with
  | Assign asns ->
     let update env (n,e) = Env.update env (n, sem_expr e env) in
     List.fold_left update env asns
     
let sem_trans env (_,a,s,q') =
  match a env with
  | Value.Bool true -> Some (s env, q')
  | _ -> None
