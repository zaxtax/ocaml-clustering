open Common;;

let folder = Array.fold_left;;

let fuzzy_cmeans data len c =
  let cen = choice data c in
  let ind = Array.init len id in (* indices of data *)
  let cof = normalize_coeffs data in
  let dist_log = dist_log_gen cof in (* this might as well be a matrix now *)
  (* Initial Centers *)
  let u = Array.make_matrix len c (1.0 /. (float_of_int c)) in
  let diff a b i =
    (1.0-.u.(a).(i))*.
    (1.0-.u.(b).(i))*.
    (dist_log data.(a) data.(b)) in
  for trials = 0 to 10 do
    (* Find new Centers *)
    Array.iteri (fun ci center ->
      let candidate_centers = Array.mapi (fun ei elm -> 
	let m = Array.fold_left 
	  (fun acc x -> acc +. diff ei x ci) 0.0 ind in elm,m) data in
      let new_cen,_ = Array.fold_left 
	(fun x y -> match x,y with 
	  | (xn,xv),(yn,yv) -> if xv>yv then x else y) 
	candidate_centers.(0) 
	candidate_centers in
      center.cl_cen <- new_cen) cen;
    (* Assign to Clusters *)
    Array.iter (fun elm ->
      let m = ref 0.0 in
      for i = 0 to c-1 do
	m:=!m+.dist_log data.(elm) cen.(i).cl_cen
      done;
      for i = 0 to c-1 do
	u.(elm).(i) <- (dist_log data.(elm) cen.(i).cl_cen) /. !m
      done) ind;
  done; cen;;

let loader data len param =
  let c = int_of_string param in
  let res, time = time_it (fun _ -> fuzzy_cmeans data len c) in
  res,c,time;;

