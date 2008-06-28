open Common;;

let k_means data k =
  (* Initial Centers *)
  let cen = choice data k in
  let cof = normalize_coeffs data in
  let dist_log = dist_log_gen cof in
  for trials = 0 to 10 do

    (* Assign to Clusters *)
    Array.iter (fun elm ->
      let m = ref 0 in
	for i = 0 to k-1 do
	  if dist_log elm cen.(i).cl_cen <
	     dist_log elm cen.(!m).cl_cen
	  then m:=i
	done;
	let m_cen = Array.get cen !m in
	  m_cen.cl_elm <- elm :: m_cen.cl_elm) data;
    (* Find new centers *)
    Array.iter (fun center -> 
      let candidate_centers = List.map (fun elm -> 
	let m = List.fold_left (fun x y -> x +. dist_log elm y) 0.0 center.cl_elm in elm,m
      ) center.cl_elm in
      let new_cen,_ = foldl1 
	(fun x y -> match x,y with 
	  | (xn,xv),(yn,yv) -> if xv>yv then x else y) candidate_centers in
	center.cl_cen <- new_cen; 
	center.cl_elm <- []) cen;
  done; cen;;
