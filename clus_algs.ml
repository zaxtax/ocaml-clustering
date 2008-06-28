open ExtArray;;
open Common;;

let usage () = prerr_string $ "usage: " ^ Sys.argv.(0) ^ " file param\n";; 

let _ = 
  if (Array.length Sys.argv) = 4 then
    let loader,folder = match Sys.argv.(1) with
      | "fuzzy_cmeans" -> Fuzzy_cmeans.loader,Fuzzy_cmeans.folder
      | "k_means" -> K_means.loader,K_means.folder
      | "single_pass" -> Single_pass.loader,Single_pass.folder  
      | _ -> usage () in
    let buf,len = load_file Sys.argv.(2) in
    let data = Array.of_enum buf in
    Sys.catch_break true;
    let clus,clus_len,time = loader data len Sys.argv(3) in
    print_float $ time; 
    print_string $ " clusters: " ^ string_of_int clus_len; 
    print_string $ folder
      (fun x y -> x ^ "\n" ^ print_cluster y) "\n" res
  else usage ()
