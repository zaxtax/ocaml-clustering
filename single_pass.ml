open Common;;

let single_pass data _ thresh =
  let cof = normalize_coeffs data in
  let dist_log = dist_log_gen cof in
  let group clus pt =
    let m = ref 0 in
    for i = 0 to (List.length clus)-1 do
      if dist_log pt (List.nth clus i).cl_cen <
	 dist_log pt (List.nth clus !m).cl_cen
      then m:=i
    done;
    if !m=0 then {cl_cen=pt;cl_elm=[]} :: clus
    else if dist_log pt (List.nth clus !m).cl_cen < thresh then
      let m_cen = List.nth clus !m in
      m_cen.cl_elm <- pt :: m_cen.cl_elm; clus
    else {cl_cen=pt;cl_elm=[]} :: clus in 
  Array.fold_left group [] data;;

let loader data len param =
  let thresh = float_of_string param in
  let res, time = time_it (fun _ -> single_pass data len thresh) in
  (Array.of_list res),(List.length res),time;;
