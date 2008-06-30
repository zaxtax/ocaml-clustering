open Common;;
open Agglo;;

let buckshot data len k = 
  let size = int_of_float $ sqrt (float_of_int len) in
  let sample = Array.init size (fun i -> 
    let r = Random.int size in data.(r)) in
  agglomerative sample size k;;

let loader data len param =
  let k = int_of_string param in
  let res, time = time_it (fun _ -> buckshot data len k) in
  res,k,time;;
