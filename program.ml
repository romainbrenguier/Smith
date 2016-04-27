type t = | Var of string | Apply of (t * t) 
	 | Alt of t list | Fun of t * t 
	 | Tuple of t list
	 | Constr of string * t
	 | Match of t * ((string * t * t) list)

let rec compare x y = match x,y with
  | Var s, Var t -> Pervasives.compare s t
  | Var _, _ -> -1 | _, Var _ -> 1
  | Apply (a,b), Apply(c,d) -> 
    let r = compare a c in 
    if r = 0 then compare b d else r
  | Apply _, _ -> -1 | _, Apply _ -> 1
  | Alt t, Alt u ->
    if List.length t <> List.length u
    then List.length t - List.length u
    else 
      List.fold_left (fun accu (a,b) ->
	min accu (compare a b)
      ) 0 (List.combine t u )
  | Alt _, _ -> -1 | _, Alt _ -> 1
  | Fun (t,u) , Fun (v,w) -> 
    let r = compare t v in
    if r = 0 then compare u w else r
  | Fun _, _ -> -1 | _, Fun _ -> 1
  | Tuple t, Tuple u ->
    if List.length t <> List.length u
    then List.length t - List.length u
    else 
      List.fold_left (fun accu (a,b) ->
	min accu (compare a b)
      ) 0 (List.combine t u )
  | Tuple _, _ -> -1 | _, Tuple _ -> 1
  | Constr (s,t) , Constr (u,v) ->
    let r = String.compare s u in
    if r = 0 then compare t v else r
  | Constr _, _ -> -1 | _, Constr _ -> 1
  | Match (x,l), Match (y,m) ->
    if compare x y <> 0 then compare x y
    else
      List.fold_left
	(fun accu ((s,a,b),(t,c,d)) ->
	  if String.compare s t = 0
	  then if compare a c = 0
	    then if compare b d = 0
	      then accu
	      else compare b d
	    else compare a c
	  else String.compare s t
	) 0 (List.combine l m)
	
let rec insert_in_list x = function
  | [] -> [x]
  | hd :: tl when compare x hd < 0 -> x :: hd :: tl
  | hd :: tl when compare x hd = 0 -> hd :: tl
  | hd :: tl -> hd :: (insert_in_list x tl)
let merge_list =
  List.fold_left (fun accu elt -> insert_in_list elt accu)
    

let var v = Var v
let apply f x = Apply (f,x)
let alt a b = match a, b with
  | x, Alt [] | Alt [], x -> x
  | Alt x, Alt y -> Alt (merge_list x y)
  | Alt x, y | y, Alt x -> Alt (insert_in_list y x)
  | x , y when compare x y < 0 -> Alt [x;y]
  | x , y when compare x y > 0 -> Alt [x;y]
  | x , _ -> x
let tuple l = Tuple l
let constr s t = Constr(s,t)
let match_sum x l = Match (x,l)
let empty = Alt []
  
let rec output outch = function
  | Var s -> output_string outch s
  | Apply (f,x) -> Printf.fprintf outch "%a (%a)" output f output x
  | Alt list -> 
    Printf.fprintf outch "[ ";
    List.iter (fun x -> Printf.fprintf outch "(%a) | "output x) list;
    Printf.fprintf outch " ]"
  | Fun (a,b) -> Printf.fprintf outch "(fun %a -> %a)" output a output b
  | Tuple (hd::tl) -> 
    Printf.fprintf outch "(%a" output hd;
    List.iter (fun x -> Printf.fprintf outch ", %a" output x) tl;
    Printf.fprintf outch " )"
  | Constr (s,p) -> Printf.fprintf outch "%s(%a)" s output p
  | Match (x,l) -> 
    Printf.fprintf outch "match %a with \n" output x;
    List.iter 
      (fun (c,a,b) -> 
	Printf.fprintf outch "| %s %a -> %a\n" c output a output b
      ) l
    
let function_output outch name x = 
  let rec aux1 = function
    | Fun (a,b) -> 
      Printf.fprintf outch "%a " output a;
      aux1 b 
    | p -> Printf.fprintf outch "= %a" output p
  in
  Printf.fprintf outch "let %s " name;
  aux1 x;
  Printf.fprintf outch "\n"

let rec select_one = function
  | Alt (hd :: _) -> select_one hd
  | Apply (f,x) -> Apply (select_one f, select_one x)
  | Tuple list -> Tuple (List.map select_one list)
  | Fun (a,b) -> Fun (a,select_one b)
  | x -> x

