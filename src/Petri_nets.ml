open List
open Utils

type place = int
type alphabet = string
type transition = {pre : place list; post : place list; lambda : alphabet}
type pn = {places : place list; transitions : transition list; m0 : place list; mf : place list}


type transition_index = int

let arcs_to_transitions
    (input_arcs : (place * transition_index) list)
    (output_arcs : (transition_index * place) list)
    (tr_list : (transition_index * alphabet) list) =
  map
    (fun ti ->
      {pre = map fst (filter (fun a -> snd a = fst ti) input_arcs);
       post = map snd (filter (fun a -> fst a = fst ti) output_arcs);
       lambda = snd ti}
    )
    tr_list


let remove_quotes s = hd (Str.split (Str.regexp "\"") s)

let read_pn pn_file =
  let ic = open_in_bin pn_file
  and places = ref []
  and transitions  = ref []
  and place_counter = ref 0
  and initial_marking = ref [] 
  and final_marking = ref [] in
  (try
     while true do
       match (Str.split (Str.regexp "[ \t;\r]+") (input_line ic)) with
       | "place" :: p :: initialization ->(
	        places := (remove_quotes p) :: !places;
		match initialization with 
			| [] ->incr place_counter;
			| "init" :: s ->initial_marking := !place_counter :: !initial_marking;incr place_counter;
			| "final" :: s ->  final_marking := !place_counter :: !final_marking;incr place_counter;
    	       		| _ -> failwith ("place error\n"))
       | "trans" :: tr :: "in" :: in_out ->
       	  let pre = ref []
       	  and rest_in_out = ref in_out in
       	  while hd(!rest_in_out) <> "out" do
       	    pre := (!place_counter - 1 - (pos (remove_quotes (hd !rest_in_out)) !places)) :: !pre;
       	    rest_in_out := (tl !rest_in_out)
       	  done;
	  let post = map (fun p -> (!place_counter - 1 - (pos (remove_quotes p) !places))) (tl !rest_in_out) in 
	  transitions :=
       	    {pre = !pre;
       	     post = post ;
       	     lambda = nth (Str.split (Str.regexp "\"") tr) 2}
       	  :: !transitions
       | [] -> () (* empty line *)
       | _ -> failwith ("Wrong line format when reading " ^ pn_file ^ "\n")
     done
   with End_of_file -> close_in ic);
  let list_places = list_of 0 (!place_counter - 1) (fun i -> i) in 
  {places = list_places ;
   transitions = !transitions;
   m0 = !initial_marking; mf= !final_marking}
let add_ww pn =
  {places = pn.places;
   transitions = {pre = []; post = []; lambda = "ww"} :: pn.transitions;
   m0 = pn.m0; mf=pn.mf}

let add_ww_at_the_end pn =
  {places = pn.places;
   transitions = ( map (fun fp -> {pre = [fp]; post = [fp]; lambda = "w"}) pn.mf ) @(* { pre=pn.mf;post=pn.mf;lambda="w" }  ::*) pn.transitions;
   m0 = pn.m0; mf=pn.mf}
