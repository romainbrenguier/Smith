type t = 
| Atom of string
| Arrow of (t * t)
| Var of string
| Apply of (t * t)
| Tuple of t list
| Union of (string * t) list

let atom s = Atom s
let arrow t1 t2 = Arrow (t1,t2)
let var s = Var s
let tuple tl = Tuple tl
let union tl = Union tl
let _of s t = Apply (s,t)

let is_arrow = function Arrow _ -> true | _ -> false

let compare a b = match a , b with
  | Atom s, Atom t -> compare s t
  | Atom _, _ -> -1
  | _, Atom _ -> 1
  | Arrow (a,b), Arrow (c,d) -> 
    let r1 = compare a c in 
    if r1 <> 0 then r1 else compare b d
  | Arrow _ , _ -> -1
  | _ , Arrow _ -> 1
  | Var s, Var t -> compare s t
  | Var _, _ -> -1
  | _, Var _ -> 1
  | Apply (a,b), Apply (c,d) -> 
    let r1 = compare a c in 
    if r1 <> 0 then r1 else compare b d
  | Apply _ , _ -> -1
  | _, Apply _ -> 1
  | Tuple [], Tuple [] -> 0
  | Tuple [], _ -> -1
  | _, Tuple [] -> 1
  | Tuple (t1::tl1), Tuple (t2::tl2) ->
    let r1 = compare t1 t2 in
    if r1 <> 0 then r1 
    else  compare (Tuple(tl1)) (Tuple(tl2))
  | Tuple _ , _ -> -1
  | _, Tuple _ -> 1
  | Union (t1::tl1), Union (t2::tl2) ->
    let r1 = compare t1 t2 in
    if r1 <> 0 then r1 
    else  compare (Union(tl1)) (Union(tl2))


let rec output outch t = match t with
  | Atom s -> output_string outch s
  | Arrow (t1,t2) -> 
    Printf.fprintf outch "(%a -> %a)" output t1 output t2
  | Var x -> output_string outch x
  | Apply (t1,t2) -> Printf.fprintf outch "(%a %a)" output t1 output t2
  | Tuple (hd :: tl) -> 
    Printf.fprintf outch "(%a" output hd;
    List.iter (fun t -> Printf.fprintf outch " * %a" output t) tl;
    Printf.fprintf outch ")" 
  | Union ((c,t) :: tl) -> 
    Printf.fprintf outch "(%s of %a" c output t;
    List.iter (fun (c,t) -> Printf.fprintf outch " | %s of %a" c output t) tl;
    Printf.fprintf outch ")" 

exception NoUnification
let rec unify u v = match u with
  | Var a -> [Var a,v]
  | Arrow (x,y) -> 
    (match v with 
    | Var a -> [u,Var a]
    | Arrow (x1,y1) ->
      List.rev_append (unify x x1) (unify y y1)
    | _ -> raise NoUnification
    )
  | Tuple ul -> 
    (match v with 
    | Var a -> [u,Var a]
    | Tuple vl when List.length ul = List.length vl ->
      List.fold_left 
	(fun accu (ut,vt) ->
	  List.rev_append (unify ut vt) accu
	) [] (List.combine ul vl)
    | _ -> raise NoUnification
    )
  | Atom s -> 
    (match v with
    | Var a -> [u,Var a]
    | Atom t when s = t -> [] 
    | _ -> raise NoUnification)
      

let rec map_variables map = function
  | Atom x -> Atom x
  | Var v -> 
    (try List.assoc (Var v) map
     with Not_found -> Var v)
  | Arrow (a,b) -> Arrow (map_variables map a, map_variables map b)
  | Apply (a,b) -> Apply (map_variables map a, map_variables map b)
  | Tuple tl -> Tuple (List.map (map_variables map) tl)
  | Union tl -> Union (List.map (fun (c,t) -> c, map_variables map t) tl)

let apply a b = match a with
  | Arrow (x,y) -> 
    (
      let map = try Some (unify x b) with _ -> None in
      match map with
      | None -> None
      | Some m -> Some (map_variables m y)
    )
  | _ -> None

(** default name for a variable of that type *)
let rec default_name = function
  | Atom s -> s
  | Var x -> "some_type_"^x
  | Arrow (x,y) -> default_name x^"_to_"^default_name y
  | Apply (x,y) -> default_name x^"_"^default_name y 
  | Tuple (hd::tl) -> 
    List.fold_left (fun accu t -> accu^"_and_"^default_name t)
      (default_name hd) tl
  | Union ((_,hd)::tl) -> 
    List.fold_left (fun accu (s,t) -> accu^"_or_"^default_name t)
      (default_name hd) tl

let direct_subtypes = function
  | Arrow (a, b) -> [a;b]
  | Apply (a, b) -> [a]
  | Tuple list -> list
  | Union list -> List.map snd list
  | _ -> []

let rec strict_subtypes = function
  | Arrow (a, b) -> a :: b :: (List.rev_append (strict_subtypes a) (strict_subtypes b))
  | Apply (a, b) -> a :: strict_subtypes a 
  | Tuple list -> List.fold_left (fun accu a -> List.rev_append (strict_subtypes a) accu) list list
  | Union list -> List.fold_left (fun accu (_,a) -> List.rev_append (strict_subtypes a) accu) (List.map snd list) list
  | _ -> []

let subtypes a = a :: strict_subtypes a

let rec union_subtypes = function
  | Union x -> List.fold_left
    (fun accu (_,t) -> List.rev_append (union_subtypes t) accu) [Union x] x
  | Arrow (a,b) | Apply (a,b) -> List.rev_append (union_subtypes a) (union_subtypes b)
  | Tuple list -> List.fold_left (fun accu a -> List.rev_append (union_subtypes a) accu) [] list
  | _ -> []

let rec expend find_type = function
  | Atom x -> (match find_type x with Some y -> expend find_type y | None -> Atom x)
  | Var x -> Var x
  | Arrow (a,b) -> Arrow(expend find_type a, expend find_type b)
  | Tuple list -> Tuple(List.map (expend find_type) list)
  | Union list -> 
    Union (List.map (fun (s,t) -> (s,expend find_type t)) list)


type declaration =
  NewType of (string * t option) | Decl of (string * t) | Nothing
