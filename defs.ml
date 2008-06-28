#use "topfind";;
#require "extlib";;

#load "str.cma";;
#load "unix.cma";;
#use  "clus_algs.ml";;

type date = {dt_mon : int; dt_dat : int; dt_hour : int; dt_min : int; dt_sec : int};; 
type syslog = {timestamp : Date, host : string, app : string, msg : string} ;; 

exception NotAMonth;;

let rec filteri f t =
  let idx = ref (-1) in
  let rec next() =
    incr idx;
    let x = t.next() in
    if f !idx x then x else next() in
  from2 next (fun () -> filter f (t.clone()));;

let rec last = function
  | hd :: tl -> List.fold_left (fun x y -> y) hd tl
  | _ -> raise EmptyList;;
