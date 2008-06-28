open ExtArray;;
open Common;;
open Single_pass;;
    
let _ = 
  if (Array.length Sys.argv) = 3 then
    let buf,len = load_file Sys.argv.(1) in
    let data = Array.of_enum buf in
    let thresh = float_of_string Sys.argv.(2) in
    Sys.catch_break true;
    let res,time = time_it (fun _ -> Single_pass.single_pass data thresh) in
    print_float $ time; 
    print_string $ " clusters: " ^ string_of_int (List.length res); 
    print_string $ List.fold_left 
      (fun x y -> x ^ "\n" ^ print_cluster y) "\n" res
  else prerr_string $ "usage: " ^ Sys.argv.(0) ^ " file param\n";; 
