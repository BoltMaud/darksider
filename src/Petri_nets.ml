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
  and not_final_marking = ref [] in
  (try
     while true do
       match (Str.split (Str.regexp "[ \t;\r]+") (input_line ic)) with
       | "place" :: p :: initialization ->
    	  (places := (remove_quotes p) :: !places;
	   if initialization <> [] then initial_marking := !place_counter :: !initial_marking;
	   incr place_counter)
       | "trans" :: tr :: "in" :: in_out ->
       	  let pre = ref []
       	  and rest_in_out = ref in_out in
       	  while hd(!rest_in_out) <> "out" do
       	    pre := (!place_counter - 1 - (pos (remove_quotes (hd !rest_in_out)) !places)) :: !pre;
       	    rest_in_out := (tl !rest_in_out)
       	  done;
	  let post = map (fun p -> (!place_counter - 1 - (pos (remove_quotes p) !places))) (tl !rest_in_out) in 
	  not_final_marking := (filter (fun p ->  (not (mem p post))) !pre) @ !not_final_marking;
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
   m0 = !initial_marking; mf= (filter (fun p -> (not (mem p !not_final_marking))) list_places) }

let add_ww pn =
  {places = pn.places;
   transitions = {pre = []; post = []; lambda = "ww"} :: pn.transitions;
   m0 = pn.m0; mf=pn.mf}
