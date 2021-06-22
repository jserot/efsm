type name = string
type value = Bool of bool | Int of int
          
type env = (name * value) list

let lookup_env (env: env) (n: name) =
  try
    List.assoc n env
  with
    Not_found -> failwith ("lookup_env: " ^ n)

let update_env (env: env) (n,v) =
  let rec scan l = match l with
    | [] -> [n,v]
    | (n',v')::rest -> if n=n' then (n,v)::rest else (n',v') :: scan rest in
  scan env

let equal_env env env' =
  let incl e e' = List.for_all (fun (n,v) -> lookup_env e' n = v) e in
  incl env env' && incl env' env

type state = string

type trans =
    state
    * (env -> value)
    * (env -> env)
    * state

type efsm = {
    states: state list;
    q: state;
    delta: trans list;
  }

type expr =
  | EBool of bool
  | EInt of int
  | EVar of name
  | EPrim of string * expr list

type instr =
  | Assign of (name * expr) list
  
let decode_int = function Int v -> v | _ -> failwith "decode_int"
let decode_bool = function Bool v -> v | _ -> failwith "decode_Bool"
                                              
let prim2_bool f args = match args with
  | [arg1; arg2] -> Bool (f (decode_int arg1) (decode_int arg2))
  | _ -> failwith "primitive arity mismatch"

let prim2_int f args = match args with
  | [arg1; arg2] -> Int (f (decode_int arg1) (decode_int arg2))
  | _ -> failwith "primitive arity mismatch"

let primitives = [
    "=", prim2_bool ( = );
    "<", prim2_bool ( < );
    ">", prim2_bool ( > );
    "<=", prim2_bool ( <= );
    ">=", prim2_bool ( >= );
    "!=", prim2_bool ( != );
    "+", prim2_int ( + );
    "-", prim2_int ( - );
    "*", prim2_int ( * );
    "/", prim2_int ( / );
  ]
  
let sem_prim p =
  try
    List.assoc p primitives
  with
    Not_found -> failwith "Unknown primitive"
               
let rec sem_expr e = fun (env: env) ->
  match e with
  | EBool b -> Bool b
  | EInt n -> Int n
  | EVar n -> lookup_env env n 
  | EPrim (op, args) -> sem_prim op @@ List.map (fun arg -> sem_expr arg env) args

let sem_act a = fun (env: env) ->
  match a with
  | Assign asns ->
     let update env (n,e) = update_env env (n, sem_expr e env) in
     List.fold_left update env asns
     
let sem_trans (env: env) ((q,a,s,q'): trans) =
  match a env with
  | Bool true -> Some (s env, q')
  | _ -> None

let react m env = 
  match
    List.find_opt
      (fun (q,a,s,q') -> q = m.q && a env = Bool true)
      m.delta
  with
  | None -> m, env  (* Rule WAIT *)
  | Some (q,a,s,q') -> { m with q = q' }, s env (* Rule NEXT *)

let m_fact = {
    states = ["q0"; "q1"; "q2"];
    q = "q0";
    delta = [
        "q0",
          sem_expr (EBool true),
          sem_act (Assign (["x", EInt 1])),
          "q1";
        "q1",
          sem_expr (EPrim (">", [EVar "n"; EInt 0])),
          sem_act (Assign ["x", EPrim ("*", [EVar "x"; EVar "n"]); "n", EPrim ("-", [EVar "n"; EInt 1])]),
          "q1"; 
        "q1",
          sem_expr (EPrim ("<=", [EVar "n"; EInt 0])),
          sem_act (Assign ["r", EVar "x"]),
          "q2"; 
      ]
  }

let run m env =
  let rec step trace m env =
    let m', env' = react m env in
    if m.q=m'.q && equal_env env env'
    then List.rev trace
    else step ((m.q, env)::trace) m' env' in
  step [] m env
  
let _ = run m_fact ["n", Int 5]

