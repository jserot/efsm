let decode_int = function Value.Int v -> v | _ -> failwith "decode_int"
let decode_bool = function Value.Bool v -> v | _ -> failwith "decode_Bool"
                                              
let prim2_bool f args = match args with
  | [arg1; arg2] -> Value.Bool (f (decode_int arg1) (decode_int arg2))
  | _ -> failwith "primitive arity mismatch"

let prim2_int f args = match args with
  | [arg1; arg2] -> Value.Int (f (decode_int arg1) (decode_int arg2))
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
