open Common;;

let fuzzy_cmeans data len c =
  let cen = choice data c in
  let ind = Array.init len id in (* indices of data *)
  let cof = normalize_coeffs data in
  let dist_log = dist_log_gen cof in 
  (* this might as well be a matrix now *)
  let gram = Array.init len 
    (fun i -> Array.init len 
      (fun j -> dist_log data.(i) data.(j))) in
    
  (* Initial Centers *)
  let u = Array.make_matrix len c (1.0 /. (float_of_int c)) in
  let diff a b i =
    u.(a).(i) *.
    u.(b).(i) *.
    ( gram.(a).(b) ) /.
    ( gram.(a).(i) ) /.
    ( gram.(a).(i) ) in
  for trials = 0 to 100 do
    (* Assign to Clusters *)
    Array.iter (fun elm ->
      let m = ref 0.0 in
      for i = 0 to c-1 do
	m:=!m+. gram.(elm).(i)
      done;
      for i = 0 to c-1 do
	u.(elm).(i) <- ( gram.(elm).(i) ) /. !m
      done) ind;
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
  done; cen;;

let loader data len param =
  let c = int_of_string param in
  let res, time = time_it (fun _ -> fuzzy_cmeans data len c) in
  res,c,time;;

