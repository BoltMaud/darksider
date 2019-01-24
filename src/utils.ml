open List

let rec find l x n=
	if l.(n) = x then n else find l x (n+1);;

let output_newline (oc : out_channel) = output_string oc "\n"
let output_int (oc : out_channel) n = output_string oc (string_of_int n)

let output_int_list (oc : out_channel) l =
  iter (fun n -> output_int oc n; output_string oc " ") l


let push (x : 'a) (l : 'a list ref) =
  l := x :: !l

let rec pos x l = match l with
  | [] -> failwith "element not in list"
  | xx :: ll when xx = x -> 0
  | _ :: ll -> 1 + pos x ll

let rec remove_nth l n = match l, n with
  | (x :: ll), 0 -> ll
  | (x :: ll), n -> x :: (remove_nth ll (n - 1))
  | [], _ -> failwith "remove_nth: index out of bounds"


let list_of inf sup f =
  let res = ref [] in
  for i = inf to sup do
    res := (f i) :: !res;
  done;
  rev !res

(* redo List.init *)
let rec listinit last l=
if last==0 then l else (listinit (last-1) ((last-1)::l));;


(* actually rev is probably not needed here *)
let map f l = rev (rev_map f l)

(* actually rev is probably not needed here *)
let (@) l1 l2 = rev (rev_append l1 l2)

(* The non tail-recursive concat in the library is not efficient enough for the banktransfer example *)
let concat l =
  let res = ref []
  and ll = ref l in
  while !ll <> [] do
    res := rev_append (hd !ll) !res;
    ll := tl !ll
  done;
  rev !res


(* removes only adjacent duplicates *)
let rec remove_duplicates = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: l ->
     if x = y
     then remove_duplicates (y :: l)
     else x :: (remove_duplicates (y :: l))

(* available in module List from OCaml 4.02 *)
let sort_uniq comparison l = remove_duplicates (sort comparison l)


(* returns a list of all elements of l1 which are not in l2 *)
(* could easily be optimised... one day *)
let set_minus l1 l2 =
  filter (fun x -> not (mem x l2)) l1



let array_of_string s =
  let res = Array.make (String.length s) ' ' in
  for i = 0 to (String.length s) - 1 do
    res.(i) <- s.[i]
  done;
  res

let string_array_of_string s =
  Array.map (String.make 1) (array_of_string s);;
