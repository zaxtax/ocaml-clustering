open Common;;

let fuzzy_cmeans data c =
  let len = Array.length data in
  let cen = choice data c in
  (* Initial Centers *)
  let u = Array.make_matrix len c (1.0 /. (float_of_int c)) in
  let diff k i =
    (1.0-.u.(k).(i))*(dist_log data.(k) cen.(i));;

  for trials = 0 to 10 do
    (* Find new Centers *)
    Array.iteri (fun ci center ->
      let candidate_centers = List.map (fun elm -> 
	let m = List.fold_left (fun x y -> x +. dist_log elm y) 0.0 center.cl_elm in elm,m
      ) center.cl_elm in
      let new_cen,_ = foldl1 
	(fun x y -> match x,y with 
	  | (xn,xv),(yn,yv) -> if xv>yv then x else y) candidate_centers in
	center.cl_cen <- new_cen; 
	center.cl_elm <- []) cen;

    (* Assign to Clusters *)


  done; cen;;
