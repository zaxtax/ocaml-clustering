open Common
open Suffix_tree

module M = Make (struct let get_visible _ = (0,0) end)
open M;;

let score_node st node = 
  let w = label st node in
  let docs = children st node in
  let d = List.length docs in
  (* Should have a merge base clusters here *)
  (* let merge nodes = nodes in *)
  let f str = 
    let sl = String.length str in
    if sl < 2 then 1
    else if sl<12 then sl
    else 10 in
  let score = d*(f w) in (score,docs,d)

let suffix_clus data len c = 
  let nodes = Array.init 40000 (fun _ -> (0,[],0) ) in
  let ind = ref 0 in
  let st = create () in
  Array.iter (fun x -> ignore (add st (print_syslog x))) data;
  fold_tree st (fun _ _ -> true) (fun _ _ -> 0)
    (fun _ _ n -> 
      nodes.(!ind) <- (score_node st n); ind:=!ind+1) 0;
  Array.sort (fun x y -> match x,y with
    | (sx,_,_),(sy,_,_) -> 
	if sx = sy then 0 
	else if sx > sy then (-1) else 1) nodes;
  Array.init c (fun i -> 
    let entry = nodes.(i) in
    let str = match entry with 
      | score,docs,d -> get st (List.hd (ext st (List.hd docs))) in
    {cl_cen=make_log str;cl_elm=[]})

let loader data len param =
  let c = int_of_string param in
  let res, time = time_it (fun _ -> suffix_clus data len c) in
  res,c,time;;
