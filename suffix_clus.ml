open Common
open Suffix_tree

module M = Make (struct let get_visible _ = (0,0) end)
open M;;

let score_node st node = 
  let w = label st n in
  let docs = ext st n in
  let d = List.length docs in
  let f str = 
    let sl = String.length str in
    if sl < 2 then 1
    else if sl<12 then sl
    else 15 in
  let score = d*(f w) in (score,docs,d)

let suffix_clus data len c = 
  let nodes = Hashtbl.create 500 in
  let st = create () in
  Array.iter (fun x -> add st (print_cluster x)) data;
  fold_tree st (fun _ _ -> true) (fun _ _ -> 0)
    (fun _ _ n -> Hashtbl.add nodes $ score_node st n) 0;
  


let loader data len c =
  let c = int_of_string param in
  let res, time = time_it (fun _ -> suffix_clus data len c) in
  res,c,time;;
