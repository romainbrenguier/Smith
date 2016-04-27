(* This should allow to search by type then environment *)

module EMap = Map.Make(Term.TSet)
module TMap = Map.Make(Type)

open Term
type 'a t = 'a EMap.t TMap.t

let empty = TMap.empty

let add term v map = 
  let m = try TMap.find (Term.typ term) map with Not_found -> EMap.empty in
  TMap.add (Term.typ term) (EMap.add (Term.env term) v m) map

let find term map =
  TMap.find (Term.typ term) map 
  |> EMap.find (Term.env term)

let find_typ t map =
  TMap.find t map 

let fold f map accu =
  TMap.fold
    (fun t m accu ->
      EMap.fold
	(fun e v accu -> f (e |- t) v accu)
	m accu
    ) map accu

let iter f map = 
  fold (fun t m () -> f t m) map ()

let merge f map1 map2 =
  TMap.merge (fun t ma mb ->
    match ma,mb with
    | None, None -> None
    | Some x, None | None, Some x -> Some x
    | Some ma, Some mb ->
      Some (EMap.merge (fun e a b ->
	f (e |- t) a b
      ) ma mb)
  ) map1 map2
