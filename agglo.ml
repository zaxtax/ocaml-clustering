open Common;;

type 'a bucket = {mutable blen : int; mutable belm : 'a array} 

let distill buckets k = 
  let tmp = Hashtbl.create k in
  Hashtbl.fold (fun a _ ind ->
    let r = Random.int a.blen in 
    Hashtbl.add tmp ind {cl_cen=(a.belm.(r));cl_elm=[]}; ind+1 ) buckets 0;
  Array.init k (fun i -> Hashtbl.find tmp i);;

let agglomerative data len k =
  let cof = normalize_coeffs data in
  let dist_log = dist_log_gen cof in
  let buckets = Hashtbl.create len in
  let _dummy = {blen=1;belm=[|data.(0)|]} in
  let diff a b = 
    let res = Array.fold_left (fun acc x -> 
      Array.fold_left (fun bcc y ->
	acc +. bcc +. dist_log x y) 0.0 b.belm) 0.0 a.belm in
    res /. (float_of_int (a.blen*b.blen)) in
  let merge a b =
    let c = {blen=a.blen + b.blen; belm=Array.append a.belm b.belm} in
    Hashtbl.remove buckets a; 
    Hashtbl.remove buckets b;
    Hashtbl.add buckets c true in
  (* Add initial data *)
  Array.iter (fun x -> Hashtbl.add buckets {blen=1;belm=[|x|]} true) data;
  (* Merge clusters down to specification *)
  for pts = len downto (k+1) do
    let m = ref 10000.0 in
    let x,y = (ref _dummy, ref _dummy) in
    Hashtbl.iter (fun a _ -> 
    Hashtbl.iter (fun b _ ->
    if a!=b then
      let i = diff a b in
      if i < !m 
      then m:=i;x:=a; y:=b;
    ) buckets ) buckets;
    merge !x !y
  done; distill buckets k;;
    
let loader data len param =
  let k = int_of_string param in
  let res, time = time_it (fun _ -> agglomerative data len k) in
  res,k,time;;
