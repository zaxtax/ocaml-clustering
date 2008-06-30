open ExtArray;;
open Common;;

let usage = "usage: " ^ Sys.argv.(0) ^ " alg file param\n";; 
type 'a lo = K 

let _ = 
  Sys.catch_break true;
  if (Array.length Sys.argv) = 4 then
    let buf,len = load_file Sys.argv.(2) in
    let param = Sys.argv.(3) in
    let data = Array.of_enum buf in
    let loader = match Sys.argv.(1) with
      | "fuzzy_cmeans" -> Fuzzy_cmeans.loader
      | "k_means" -> K_means.loader 
      | "single_pass" -> Single_pass.loader
      | "agglo" -> Agglo.loader
      | "buck" -> Buckshot.loader
      | _ -> failwith usage in
    let clus,clus_len,time = loader data len param in
    print_float $ time; 
    print_string $ " clusters: " ^ string_of_int clus_len; 
    print_string $ Array.fold_left
      (fun x y -> x ^ "\n" ^ print_cluster y) "\n" clus
  else prerr_string $ usage
