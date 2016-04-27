module TSet = Set.Make(Type)

type t = { env: TSet.t; typ: Type.t}
let make set t = {env=set;typ=t}

let ( |- ) set t = make set t
let empty t = make TSet.empty t
let ( ||- ) () t = empty t

let compare s t =
  let r = TSet.compare s.env t.env in
  if r = 0 then Type.compare s.typ t.typ
  else r
    
let env t = t.env
let typ t = t.typ
let union env1 env2 t = make (TSet.union (env env1) (env env2)) t
let set_type t u = {t with typ = u }
let set_env t e = {t with env = e}

let add_to_env t u = { t with env = TSet.add u t.env}

let abstract e v =
  make (TSet.remove v (env e)) (Type.arrow v (typ e))

let output outch e = 
  Printf.fprintf outch "[";
  TSet.iter (fun x -> Printf.fprintf outch "%a;" Type.output x) (env e);
  Printf.fprintf outch "] %a" Type.output (typ e)


  
