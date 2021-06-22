let m_fact =
  let open Expr in
  let open Instr in
  let open Sem in
  {
    states = ["q0"; "q1"; "q2"];
    q = "q0";
    delta = [
        "q0",
          sem_expr (EBool true),
          sem_instr (Assign (["x", EInt 1])),
          "q1";
        "q1",
          sem_expr (EPrim (">", [EVar "n"; EInt 0])),
          sem_instr (Assign ["x", EPrim ("*", [EVar "x"; EVar "n"]); "n", EPrim ("-", [EVar "n"; EInt 1])]),
          "q1"; 
        "q1",
          sem_expr (EPrim ("<=", [EVar "n"; EInt 0])),
          sem_instr (Assign ["r", EVar "x"]),
          "q2"; 
      ]
  }

let _ = run m_fact ["n", Int 5]

