open Lexer
open Lexing
open Common

module TSet = Exploration.TSet
module ESet = Exploration.ESet

let filename = ref ""
let include_files = ref []
let output_file = ref None

let () = 
  Arg.parse
    [
      "-i", String (fun x -> include_files := x :: !include_files), "-i <file.mli> \tAdd function included in <file.mli> to the list of functions to use.";
      "-o", String (fun x -> output_file := Some x), "-o <file.ml> \tSet output file to <file.ml>.";
      "-info", Set info_ref, "-info \tActivate the printing of information during execution.";
      "-debug", Set debug_ref, "-debug \tActivate the printing of debuging information.";
    ]
    (fun x -> filename := x)
    "Usage: smith (-i file.mli)* file.mli"


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prototype Lexer.read lexbuf with
  | SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    Nothing
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)


let rec parse (tenv, accu) lexbuf =
  match parse_with_error lexbuf with
  | Decl (n,t) ->
    if info () then Printf.printf "val %s : %a\n" n Type.output t;
    parse (tenv, (n,t) :: accu) lexbuf
  | NewType (n,Some t) ->
    if info () then Printf.printf "type %s = %a\n" n Type.output t;
    parse ((n,Some t) :: tenv, accu) lexbuf
  | NewType (n,None) ->
    if info () then Printf.printf "type %s\n" n;
    parse ((n,None) :: tenv, accu) lexbuf
  | Nothing -> (tenv,accu)

let loop filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let tenv,list = parse ([],[]) lexbuf in
  close_in inx;
  List.rev tenv, List.rev list

let test_map map typ = 
  TermMap.fold (fun a p accu ->
    match accu with 
    | Some x -> Some x
    | None -> if Term.compare a (Term.empty typ) = 0 then Some p else None
  ) map None

let test =
  let type_table = Hashtbl.create 10 in
  let find_type x = try Some (Hashtbl.find type_table x) with Not_found -> None in

  let list =
    List.fold_left
      (fun accu filename ->
	if info() then Printf.printf "Opening %s\n" filename;
	let (tenv,list) = loop filename in
	List.iter (function (n,Some t) -> Hashtbl.add type_table n t | _ -> ()) tenv;
	List.rev_append (List.map (fun (n,t) -> (n,Type.expend find_type t)) list) accu
      ) [] !include_files
  in
  

  let outch = match !output_file with None -> stdout | Some x -> open_out x in

  if info() then Printf.printf "Opening %s for synthesis\n" !filename;
  let tenv, to_synthesize = loop !filename in
  
  List.iter (function 
  | (n,Some t) -> 
    Printf.fprintf outch "type %s = %a\n" n Type.output t;
    Hashtbl.add type_table n t
  | _ -> ()
  ) tenv;

  let to_synthesize = List.map (fun (n,t) -> (n,Type.expend find_type t)) to_synthesize in

  let map = 
    List.fold_left (fun a (n,t) -> 
      TermMap.add (Term.empty t) (Program.var n) a
    ) TermMap.empty list 
  in

  
  let rec loop goal i to_visit visited = 
    if debug () then Printf.printf "step %d\n" i;
    flush stdout;
    if i > 10 then to_visit, None
    else
      let new_map = Exploration.exploration_step to_visit goal in

      match test_map new_map goal with 
      | Some x -> new_map, Some x
      | None ->
	let new_set = Exploration.map_to_set new_map in
	(*Exploration.TSet.iter
	  (fun t -> Printf.fprintf stdout "%a\n" Type.output t) 
	  new_set;*)
	let new_to_visit = (*TermMap.filter (fun t _ -> Type.is_arrow (Term.typ t)) new_map*) new_map in
	let new_set_to_visit = Exploration.map_to_set new_to_visit in
	if ESet.subset new_set_to_visit visited
	then (if debug () then print_endline "fixpoint"; new_map, None)
	else loop goal (i+1) new_to_visit (ESet.union visited new_set)
  in

  
  let aux accu (n1,t1) = 
    match test_map accu t1 with
    | None ->
      (match loop t1 1 accu (Exploration.map_to_set accu) with 
      | new_map, Some x ->
	Program.function_output outch n1 (Program.select_one x);
	new_map
      | new_map, None -> Printf.fprintf outch "let %s = failwith \"Synthesis failed for function %s\"\n" n1 n1; new_map
      )
    | Some x ->
      Program.function_output outch n1 (Program.select_one x);
      accu
	
  in 
  List.fold_left aux map to_synthesize;
  close_out outch
  

	
    

