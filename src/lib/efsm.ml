type state = string

type trans =
    state
    * (Env.t -> Value.t)
    * (Env.t -> Env.t)
    * state

type t = {
    states: state list;
    q: state;
    delta: trans list;
  }

let react m env = 
  match
    List.find_opt
      (fun (q,a,_,_) -> q = m.q && a env = Value.Bool true)
      m.delta
  with
  | None -> m, env  (* Rule WAIT *)
  | Some (_,_,s,q') -> { m with q = q' }, s env (* Rule NEXT *)

let run m env =
  let rec step trace m env =
    let m', env' = react m env in
    if m.q=m'.q && Env.equal env env'
    then List.rev trace
    else step ((m.q, env)::trace) m' env' in
  step [] m env
  
