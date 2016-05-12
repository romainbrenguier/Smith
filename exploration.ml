module TSet = Term.TSet
(*Map.Make(Term)*)
module ESet = Set.Make(Term)
(* module PSet = Set.Make(Program) *)
open Term
open Common

let map_to_set map =
  TermMap.fold (fun k _ accu ->
    ESet.add k accu
  ) map ESet.empty
    
(* We should use antichains to add only if a more general environment is not there. *)
let add_program map env prog =
  let old = try TermMap.find env map with Not_found -> Program.empty in
  TermMap.add env (Program.alt prog old) map 

let output_map map = 
  TermMap.iter
    (fun t p -> Printf.fprintf stdout "%a : %a\n" Program.output p Term.output t) 
    map

let output_keys map = 
  TermMap.iter
    (fun t p -> Printf.fprintf stdout "%a\n" Term.output t) 
    map
 
let subtypes map =
  TermMap.fold (fun env p accu ->
    List.fold_left
      (fun accu t -> 
	TSet.add t accu
      ) accu (Type.subtypes (Term.typ env))
  ) map TSet.empty

let merge_maps = 
  TermMap.merge
    (fun k a b -> match a,b with 
    | None, None -> None
    | Some a, None | None, Some a -> Some a
    | Some a, Some b -> Some (Program.alt a b)
    ) 

let add_to_map = 
  TermMap.merge
    (fun k a b -> match a,b with 
    | None, None -> None
    | Some a, None | None, Some a 
    | Some a, Some _ -> Some a
    ) 

let intro map types = 
  List.fold_left (fun accu t ->
    add_program accu
      (TSet.singleton t |- t)
      (Program.var (Type.default_name t))
  ) TermMap.empty types

 
let apply map1 map2 =
  TermMap.fold (fun enva p accu ->
    TermMap.fold (fun envb pb accu ->
      match Type.apply (Term.typ enva) (Term.typ envb) with
      | None -> accu
      | Some t -> 
	add_program accu 
	  (Term.union enva envb t)
	  (Program.apply p pb)
    ) map2 accu
  ) map1 TermMap.empty

let abstract map =
  TermMap.fold (fun enva p accu ->
    TSet.fold (fun (t:Type.t) accu -> 
      let e = Term.abstract enva t in
      TermMap.add e (Program.Fun (Program.Var (Type.default_name t),p)) accu 
    ) (Term.env enva) accu
  ) map TermMap.empty

let tuple map goal = 
  let subtypes = Type.subtypes goal in
  let is_subtype enva envb = 
    List.mem 
      (Type.tuple [Term.typ enva; Term.typ envb])
      subtypes
  in
  List.fold_left 
    (fun accu -> 
      function Type.Tuple list ->
	(try
	   let maps = List.map (fun x -> TermMap.find_typ x map) list in
	   let first = List.map
	     (fun m -> let k,p = TermMap.EMap.choose m in k,p) maps
	   in
	   let p = Program.Tuple (List.map snd first) in
	   let e = List.map fst first |> List.fold_left TSet.union TSet.empty in
	   TermMap.add (e |- Tuple list) p accu
	 with Not_found -> accu)
      | _ -> accu
    ) TermMap.empty subtypes 
    
(*failwith "in Exploration.tuple: need to be improved.";
  TermMap.fold (fun enva p accu ->
    TermMap.fold (fun envb q accu ->
      if is_subtype enva envb 
      then
	add_program accu
	  (Term.union enva envb 
	     (Type.tuple [Term.typ enva; Term.typ envb]))
	  (Program.tuple [p;q])
      else accu
    ) map accu
  ) map TermMap.empty*)

let select_in_tuple tupl typ p = 
  let open Program in
  Apply (Fun (Tuple(List.map (fun x -> Var (Type.default_name x)) tupl),
	      Var(Type.default_name typ)) , p)
  
let untuple map =
  TermMap.fold (fun enva p accu ->
    match Term.typ enva with
    | Tuple list ->
      List.fold_left (fun accu t ->
	add_program accu
	  (Term.set_type enva t)
	  (select_in_tuple list t p)
      ) accu list
    | _ -> accu
  ) map TermMap.empty

let sum map union_types =
  TermMap.fold (fun enva p accu ->
    List.fold_left (fun accu t ->
      List.fold_left (fun accu (s,u) ->
	let typa = Term.typ enva in
	(*Printf.printf "Sum of %a with %a\n" Type.output u Type.output typa;*)
	if Type.compare typa u = 0 
	then add_program accu
	  (Term.set_type enva t)
	  (Program.constr s p)
	else accu
      ) accu (match t with Union tl -> tl)
    ) accu union_types 
  ) map TermMap.empty

let match_sum map union_types goals = 
  List.fold_left (fun accu goal ->
    List.fold_left (fun accu ut ->
      try
	let (env,prog) = 
	  List.fold_left (fun (env,prog) (c,t) ->
	  (* warning: are taking an empty environment here *)
	    (* Printf.printf "Match %s of %a for type %a \n" c Type.output t Type.output goal;*)
	    match
	      let emap = TermMap.find_typ goal map in
	      TermMap.EMap.fold (fun (e:Term.TSet.t)  p accu ->
		if TSet.mem t e
		then Some (e,p) else accu
	      ) emap None
	    with
	    | None -> raise Not_found
	    | Some (e,p) -> 
	      TSet.union env (TSet.remove t e), 
	      ((c,Program.Var (Type.default_name t),p) :: prog)
	  ) (TSet.empty,[]) (match ut with Type.Union tl -> tl)
	in
	add_program accu
	  ( TSet.add ut env |- goal )
	  (Program.match_sum (Program.Var (Type.default_name ut)) prog)
      with Not_found -> 
	(*print_endline "not found";*)
	accu
    ) accu union_types 
  ) TermMap.empty goals 

    
let exploration_step map goal =
  let nmap = intro map (Type.strict_subtypes goal) in
  let amap = apply map map in 
  let abmap = abstract map in 
  let tmap = tuple map goal in 
  let utmap = untuple map in 
  let sum_map = sum map (Type.union_subtypes goal) in
  let match_map = match_sum map (Type.union_subtypes goal) (Type.subtypes goal) in

  if debug () 
  then 
    (
      print_endline "Intro:";
      output_keys nmap;
      print_endline "Apply:";
      output_keys amap;
      print_endline "Abstract:";
      output_keys abmap;
      print_endline "Tuples:";
      output_keys tmap;
      print_endline "Untuple:";
      output_keys utmap;
      print_endline "Sum:";
      output_keys sum_map;
      print_endline "Match:";
      output_keys match_map;
    );
  merge_maps nmap amap
  |> merge_maps abmap
  |> merge_maps tmap
  |> merge_maps utmap
  |> merge_maps sum_map
  |> merge_maps match_map
  |> add_to_map map

