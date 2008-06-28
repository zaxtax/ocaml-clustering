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

let normalize_coeffs data = (* normalization coefficents for kernel function *)
  let len = Array.length data in
  let log_max = 
    Array.fold_left 
      (fun x y -> 
	let len = String.length y.msg in if x>len then x else len) 
      0 data in
  let dt_max = dist_dt data.(0).timestamp data.(len-1).timestamp in
  List.map float_of_int [dt_max; 1; 1; log_max];;

let dist_log_gen cof  = memoize2 (fun log1 log2 ->
   List.fold_left2 (fun acc d c -> acc +. (float_of_int d /. c) ) 0.0 
     [ dist_dt log1.timestamp log2.timestamp;
       fromEnum (log1.host<>log2.host);
       fromEnum (log1.app<>log2.app);
(*     fromEnum (log1.msg<>log2.msg)]) 1500000;;  *)
       levenshtein log1.msg log2.msg ] cof) 1500000;;

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
