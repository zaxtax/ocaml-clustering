open ExtList;;
open ExtArray;;

type date = {dt_mon : int; dt_dat : int; dt_hour : int; dt_min : int; dt_sec : int};; 
type syslog = {timestamp : date; host : string; app : string; msg : string} ;; 
type 'a cluster = {mutable cl_cen : 'a ; mutable cl_elm : 'a list};;   
exception NotAMonth;;
exception EmptyList;;

(* utility functions *)
let ($) = fun f x -> f x;; (* from Haskell  *)
let (|-) s i = s ^ " " ^ (string_of_int i);; 
let sum = List.fold_left (+) 0;;
let id = Std.identity;; 
let fromEnum x = if x then 1 else 0;; 
let efoldl1 f tl = match Enum.get tl with
  | Some hd -> Enum.fold f hd tl
  | None -> raise EmptyList;;
let foldl1 f = function
  | hd :: tl -> List.fold_left f hd tl
  | _ -> raise EmptyList;;
let arg_func cmp f a = 
  let m = ref 0 in
  for i = 0 to Array.length a - 1 do
    if (f a.(i)) < (f a.(!m)) then m := i
  done ;
  !m;;

let argmin f = arg_func (<) f;;

let minimum = foldl1 min;;
let stol = ExtString.String.explode;; (* convert strings to list of chars *)

let rec range a b = match a,b with
  | x,y when x<=y -> x :: range (x+1) y
  | _ -> [];;
let rec zip3 l1 l2 l3 = match l1,l2,l3 with
  | x::xs, y::ys, z::zs -> (x,y,z) :: zip3 xs ys zs
  | _ -> [];;
let rec scanl f x = function
  | y::ys -> let res = f x y in
	       x :: scanl f res ys
  | _ -> [x];;

let memoize2 f size =
  let cache = Hashtbl.create size in
  fun x y ->
    try Hashtbl.find cache (x,y) with Not_found ->
      let res = f x y in
      Hashtbl.add cache (x,y) res; 
      Hashtbl.add cache (y,x) res; 
      res;;

let levenshtein str_a str_b =
  let sa,sb = stol str_a, stol str_b in
  let transform xs c =
    let compute z = function
      | d,x,y -> minimum [y+1; z+1; x + if (d = c) then 0 else 1] in
    match xs with
      | y :: ys -> scanl compute (y+1) (zip3 sa xs ys) in
  List.last $ List.fold_left transform (range 0 (List.length sa)) sb;;

let dt2str = function
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | _ -> raise NotAMonth;;
  
let mon2int = function
  | "Jan" -> 0
  | "Feb" -> 1
  | "Mar" -> 2
  | "Apr" -> 3
  | "May" -> 4
  | "Jun" -> 5
  | "Jul" -> 6
  | "Aug" -> 7
  | "Sep" -> 8
  | "Oct" -> 9
  | "Nov" -> 10
  | "Dec" -> 11
  | _ -> raise NotAMonth;;

let mktm mon day time_str =
  let time = 
    List.map int_of_string $ Str.split (Str.regexp ":") time_str in
    match time with
      |	[hour; min; sec] -> {dt_mon=mon2int mon; dt_dat=int_of_string day;
			     dt_hour=hour;dt_min=min;dt_sec=sec};;

let make_log str =  (* loads log into data-struct *)
  let fields = Str.split (Str.regexp " +") str in
  match fields with
    | mon :: day :: time :: h :: a :: rest -> 
	{timestamp=mktm mon day time; host=h;app=a;msg = String.concat " " rest} 
    | _ -> failwith "not a logfile!";;
	  
let choice data k =
  let len = Array.length data in
  Array.init k (fun _ ->
    let n = Random.int len in
    {cl_cen=data.(n);cl_elm=[]});;
  
let load_file file = 
  let buf = Std.input_lines $ open_in file in
  let len = Enum.count buf in
  Enum.map make_log buf, len;;

let dist_dt date1 date2 = 
  let mon2day = [|0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334; 365|] in
  let dt2epoch d = (((mon2day.(d.dt_mon) + d.dt_dat)*24+d.dt_hour)*60+d.dt_min)*60+d.dt_sec in
  abs $ dt2epoch date1 - dt2epoch date2;;

let dist_log = memoize2 (fun log1 log2 ->
   
   sum [ dist_dt log1.timestamp log2.timestamp;
	 fromEnum (log1.host<>log2.host);
	 fromEnum (log1.app<>log2.app);
(* 	 fromEnum (log1.msg<>log2.msg)]) 1500000;;  *)
	 levenshtein log1.msg log2.msg ]) 1500000;;

let k_means bin k =
  (* Initial Centers *)
  let data = Array.of_enum bin in
  let cen = choice data k in
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
	let m = List.fold_left (fun x y -> x + dist_log elm y) 0 center.cl_elm in elm,m
      ) center.cl_elm in
      let new_cen,_ = foldl1 
	(fun x y -> match x,y with 
	  | (xn,xv),(yn,yv) -> if xv>yv then x else y) candidate_centers in
	center.cl_cen <- new_cen; 
	center.cl_elm <- []) cen;
  done; cen;;


let single_pass data thresh =
  let group pt clus =
    let m = ref 0 in
    for i = 0 to (List.length clus)-1 do
      if dist_log pt (List.nth clus i).cl_cen <
	 dist_log pt (List.nth clus !m).cl_cen
      then m:=i
    done;
    if dist_log pt (List.nth clus !m).cl_cen < thresh then
      let m_cen = List.nth clus !m in
      m_cen.cl_elm <- pt :: m_cen.cl_elm; clus
    else {cl_cen=pt;cl_elm=[]} :: clus in  
  match Enum.get data with
    | Some hd -> Enum.fold group [{cl_cen=hd;cl_elm=[]}] data
    | None -> raise EmptyList;;  

let print_time t = String.concat ":" 
  (List.map 
    (fun x-> let s = string_of_int x in if x<10 then "0" ^ s else s) t);; 
let print_date = function
  | {dt_mon=mon;dt_dat=day;dt_hour=hour;dt_min=min;dt_sec=sec} ->
      (dt2str mon |- day) ^ " " ^ print_time [hour;min;sec];;

let print_syslog = function
  | {timestamp=ts;host=h;app=a;msg=m} -> 
      String.concat " " [print_date ts;h;a;m];;

let print_cluster = function
  | {cl_cen=cen} -> print_syslog cen;;

let time_it f = 
  let before = Unix.gettimeofday () in
  let res = f () in
  let after = Unix.gettimeofday () in
  res, (after -. before);;
    
let _ = 
  if (Array.length Sys.argv) = 3 then
    let data,len = load_file Sys.argv.(1) in
    let k = int_of_string Sys.argv.(2) in
    Sys.catch_break true;
    let res,time = time_it (fun _ -> k_means data k) in
    print_float time; 
    print_string $ Array.fold_left 
      (fun x y -> x ^ "\n" ^ print_cluster y) "\n" res
  else prerr_string $ "usage: " ^ Sys.argv.(0) ^ " log k\n";; 
