(**
   Suffix trees.

   Suffix trees with incremental addition and removal of strings
   plus incremental maintenance of maximal factors.


   Author: S.AŽébastien FerrŽé <ferre@irisa.fr>
   License: LGPL
*)

(* for test *)
(*
#load "cis.cmo";;
#load "lSet.cmo";;
*)

(* copied from module Common *)

let rec fold_while : ('a -> 'a option) -> 'a -> 'a =
  fun f e ->
    match f e with
    | None -> e
    | Some e' -> fold_while f e'

let rec mapfind : ('a -> 'b option) -> 'a list -> 'b =
  fun f -> function
  | [] -> raise Not_found
  | x::l -> match f x with
      | None -> mapfind f l
      | Some y -> y

(* end of copy *)

module type PARAM =
  sig
    val get_visible : string -> int * int
	(** [get_visible s] returns the sizes of the prefix and suffix of [s]
	   that can be removed from [s] without damage to its meaning. *)
  end

module type T =
  sig
    type strid = int
	  (** Type of string ids. Functions using such ids are unspecified if the id is not valid. *)
    type t
	  (** Type of suffix trees. This is not a pure functional data-structure. *)

(** {1 Suffix trees as string sets. } *)

    val create : unit -> t
	(** [create ()] returns a fresh and empty suffix tree. *)
    val size : t -> int
	(** [size st] returns the number of strings registered in the suffix tree [st]. *)
    val add : t -> string -> strid
	(** [add st s] adds the string [s], and all its suffixes in the suffix tree [st], unless [s] has already been added.
	   It also returns the string id as an handle on this string. *)
    val remove : t -> strid -> unit
	(** [remove st id] removes the string identified by [id], and all its suffixes, from the suffix tree [st]. *)
    val get : t -> strid -> string
	(** [get st id] returns the string associated to [id]. *)
    val find : t -> string -> strid
	(** [find st s] returns the id associated to the string [s], if the strings exists in the suffix tree [st].
	   Otherwise raise Not_found. *)
    val fold : (strid -> string -> 'a -> 'a) -> t -> 'a -> 'a
	(** [fold f st e] is a classic folding on all strings in the suffix tree [st]. *)

(** {1 Low-level interface on suffix trees. } *)

    type node
	  (** Type of the nodes of suffix trees.
	     Nodes are either leaves or internal nodes. *)

    val root : t -> node
	(** [root st] returns the root node of the suffix tree [st]. *)
    val is_leaf : t -> node -> bool
	(** [is_leaf st n] returns whether the node [n] is a leaf. *)
    val label : t -> node -> string
	(** [label st n] returns the string labelling the node [n]. *)
    val length : t -> node -> int
	(** [length st n] returns the length of the string labelling the node [n]. *)
    val path : t -> node -> string
	(** [path st n] returns the full path from the root to the node [n]. *)
    val height : t -> node -> int
	(** [height st n] returns the height of node [n], i.e. the length of the path from root to [n]. *)
    val ext : t -> node -> strid LSet.t
	(** [ext st n] returns an ordered list of string ids that match the path of the node [n]. *)
    val children : t -> node -> node LSet.t
	(** [children st n] returns the list of children nodes of [n]. *)
    val parent : t -> node -> node option
	(** [parent st n] returns the parent node of [n], unless [n] is the root node. *)
    val succ : t -> node -> node option
	(** [succ st n] returns the successor node through the suffix link of [n], unless there is no suffix link. *)
    val preds : t -> node -> node LSet.t
	(** [preds st n] returns the list of all nodes having [n] as successor node. *)
    val suffix : t -> node -> strid * int
	(** [suffix st n] returns the suffix represented by the leaf node [n] as a couple [(string id, position in the string)].
	   Raise Not_found if [n] is not a leaf. *)
    val find_node : t -> string -> node
	(** [find_node st s] returns the node whose path is equal to the string [s], if it exists.
	   Raise Not_found otherwise. *)
    val fold_tree : t -> ('h -> node -> bool) -> ('h -> node -> 'h) -> ('s list -> 'h -> node -> 's) -> 'h -> 's
	(** [fold_tree st filter herit synth h0] returns the result of an attribute evaluation on the suffix tree [st].
	   - [filter] is used to filter which children of a node should be explored given the heritance value of the parent node,
	   - [herit] defines the heritance value of a node, given the heritance value of its parent,
	   - [synth] defines the synthesized value of a node given its heritance value, and the list of synthesized values of its filtered children,
	   - [h0] is the heritance value given to the root.
	 *)

(** {1 Exploring the suffix tree through the substring relation. } *)
		
    val path_restrictions : t -> node -> node list
	(** [path_restrictions st n] returns the list of nodes whose path is a direct restriction of the path of [n]. *)
    val path_extensions : t -> node -> node list
	(** [path_extensions st n] returns the list of nodes whose path is a direct extension of the path of [n]. *)
    val is_maximal : t -> node -> bool
	(** [is_maximal st n] returns whether a node is maximal.
	   A node is maximal is each of its extensions has a strictly smaller extent, or the node represents a full string. *)
    val set_visible : t -> node -> int * int -> unit
	(** [set_visible st node (left_pos, right_pos)] sets which part of a node path should be visible when maximal. *)
    val max_restrictions : t -> node -> node list
	(** [max_restrictions st n] returns the list of maximal nodes whose path is a restriction of the path of [n]. *)
    val max_extensions : t -> node option -> node list * strid list
	(** [max_extensions st n_opt] returns the list of maximal nodes and leaves whose path is an extension of the path of [n], when given.
	   If a start node is not given, then the maximal nodes with shortest path are returned. *)
(*
    val string_extensions : t -> node option -> strid list
	(** [string_extensions st n_opt] completes the result of [max_extensions st n_opt] with full strings through their ids. *)
*)
    val string_restrictions : t -> strid -> node list
	(** [string_restrictions st strid] returns the list of maximal nodes having [strid] as a string extension. *)

(** {1 Searching in a suffix tree} *)

    type factor = node * string * node
	  (** [(parent,s,child)] locates a factor string on the edge from node [parent] to node [child], where [s] is a prefix of the label of [child].
	     If [s] is the empty string, then [parent] and [child] are a same node.
	     The path of a factor is the concatenation of [path st parent] and [s]. *)

    val find_factor : t -> string -> factor
	(** [find_factor st s] returns the factor locating [s] in the suffix tree [st].
	   This means the path of the result factor is equal to [s].
	   Raise [Not_found] if the string [s] does not appear in any string of [st]. *)
    val suffixes : t -> factor -> (strid * int) list
	(** [suffixes st f] returns the list of all suffixes [(strid,pos)] that have the path of [f] as a prefix: this path occurs in string [strid] at position [pos]. *)
    val strings : t -> factor -> strid LSet.t
	(** [strings st f] returns the ids of all string containing the path of [f]. *)

(** {1 Simpler representation of a suffix tree (for debugging purpose at top-level)} *)

    type tree = Node of string * int * int list * tree list | Leaf of string * (strid * int)

    val tree : t -> tree

  end


(* --------------------------------------------------------------------------------
   Operations on substrings of sequences
   -------------------------------------------------------------------------------- *)

    module Subseq =
      struct
	type t = string * int * int  (* (seq, pos, len) *)
	  
	let empty = ("",0,0)  (* non-significant subseq *)
    
	let is_empty (s,pos,len) = len = 0

	let get (s,pos,len) i = s.[pos+i]

	let length (s,pos,len) = len

	let sub (s,pos,len) pos' len' = (s,pos+pos',len')

	let extend (s,pos,len) = (s,pos,len+1)
      end


module Ext =
  struct
    type t = int * Cis.t

    let cardinal (k,_) = k

    let empty = (0,Cis.empty)

    let mem i (k,cis) = Cis.mem i cis

    let singleton i = (1,Cis.singleton i)

    let add i (k,cis as ext) =
      if Cis.mem i cis
      then ext
      else (k+1,Cis.add i cis)

    let remove i (k,cis as ext) =
      if Cis.mem i cis
      then (k-1,Cis.remove i cis)
      else ext

    let union (_,cis1) (_,cis2) =
      let cis = Cis.union cis1 cis2 in
      (Cis.cardinal cis, cis)

    let diff (_,cis1) (_,cis2) =
      let cis = Cis.diff cis1 cis2 in
      (Cis.cardinal cis, cis)

    let elements (_,cis) = Cis.elements cis
  end


module Make (Param : PARAM) : T =
  struct
    type strid = int

(* type of nodes in suffix trees *)
    type node = {
	mutable seqid : strid; (* sequence index in which the positions start and final are defined *)
	mutable start : int;     (* start and final position of the word labelling the node *)
	mutable final : int ref;
	mutable parent : node; (* prefix link, the root for the root itself *)
	v : node_value
      }
    and node_value =
      | I of node_internal (* for non-leaves, internal nodes *)
      | L of int (* for leaves: position of recognized suffix *)
    and node_internal = {
	children : (char,node) Hashtbl.t;
	mutable link : node; (* suffix link *)
	mutable backlinks : node LSet.t;
	mutable ext : Ext.t; (* set of strids under this node *)
	mutable locals : Ext.t; (* subset of ext, strids only in leaves of the node (only on maximal nodes) *)
	mutable maximal_right : bool; (* whether this node is maximal on its right given its ext (a concept intent) *)
	mutable maximal : int; (* by which strid (>0) this node became a maximal node (a concept intent) *)
	mutable visible : int * int; (* which part of this node is visible: left and right offset. *)
      }

    type factor = node * string * node

(* type of suffix trees *)
    type t = {
	mutable cpt : strid;
	ht : (strid,string) Hashtbl.t;
	mutable root : node;
      }

    let get0 st strid =
      try Hashtbl.find st.ht strid
      with Not_found -> failwith ("Invalid string id: " ^ string_of_int strid)

    let ext0 node =
      match node.v with
      | I x -> x.ext
      | L _ -> Ext.singleton node.seqid

    let maximal0 node =
      match node.v with
      | I x -> x.maximal
      | L _ -> assert false

    let locals0 node =
      match node.v with
      | I x -> x.locals
      | L _ -> Ext.empty

    let size st = st.cpt

    let is_root st node = node == st.root

    let root st = st.root

    let is_leaf st node =
      match node.v with
      | L _ -> true
      | _ -> false

    let length st node =
      !(node.final) - node.start + (match node.v with I _ -> 1 | L _ -> 0)

    let label st node =
      if node == st.root
      then ""
      else String.sub (get0 st node.seqid) node.start (length st node)

    let ext st node =
      match node.v with
      | I x -> LSet.of_list (Ext.elements x.ext)
      | L _ -> LSet.singleton node.seqid

    let card st node =
      match node.v with
      | I x -> Ext.cardinal x.ext
      | L _ -> 1

    let is_maximal st node =
      match node.v with
      | I x -> x.maximal > 0
      | L pos -> pos=0

    let children st node =
      match node.v with
      | I x ->
	  Hashtbl.fold (fun c n l -> LSet.add n l) x.children (LSet.empty ())
      | L _ -> []

    let parent st node =
      if is_root st node
      then None
      else Some node.parent

    let succ st node =
      if is_root st node
      then None
      else 
	match node.v with
	| I x -> Some x.link
	| L _ -> None

    let preds st node =
      match node.v with
      | I x -> x.backlinks
      | L _ -> LSet.empty ()

    let suffix st node =
      match node.v with
      | I _ -> raise Not_found
      | L i -> (node.seqid, i)

    let rec path st node =
      match parent st node with
      | None -> ""
      | Some parent -> path st parent ^ label st node

    let rec height st node =
      match parent st node with
      | None -> 0
      | Some parent -> height st parent + length st node

(*
    let set_maximal st node b =
      match node.v with
      | I x -> x.maximal <- b
      | L _ -> ()

    let set_maximal_right st node b =
      match node.v with
      | I x -> x.maximal_right <- b
      | L _ -> ()
*)

    let set_visible st node lr =
      match node.v with
      | I x -> x.visible <- lr
      | L _ -> ()

    let get_visible st node =
      match node.v with
      | I x -> x.visible
      | _ -> raise (Invalid_argument "Suffix_tree.get_visible: applied to a leaf")

    let rec reduce st node (sl, sr) =
      match node.v with
      | I x ->
	  let nl = reduce_left st sl node in
	  let nr, residue = reduce_right st sr nl in
	  nr, residue
      | _ -> raise (Invalid_argument "Suffix_tree.reduce_to_visible: applied to a leaf")
    and reduce_left st sl node =
      if sl <= 0
      then node
      else match node.v with I x -> assert (height st node = (height st x.link) + 1); reduce_left st (sl - 1) x.link | _ -> assert false
    and reduce_right st sr node =
      if node == st.root
      then node, sr
      else begin
	let node_len = !(node.final) - node.start + 1 in
	if sr < node_len
	then node, sr
	else
	  reduce_right st (sr - node_len) node.parent end


(* -------------------------------------------------------------------------------
   Operations on implicit nodes (explicit, implicit, child : node * subseq * node)
   the snd node [child] is significant only when [implicit] is not the empty string,
   and is the child that recognizes [implicit] starting from [explicit]. [implicit] is
   defined by a sequence, a start and a length.
   ------------------------------------------------------------------------------- *)

    let eq_char c1 c2 =
      c1<>'\000' & c1=c2  (* ensures that 2 terminal symbols '\000' are pairwise different (for GST only, not necessary for ST) *)

(* returns the child node that recognizes [implicit] from the node [explicit] *)
    let get_child (explicit,implicit) =
      if Subseq.is_empty implicit
      then explicit
      else
	let c = Subseq.get implicit 0 in
	if c = '\000'
	then raise Not_found
	else
	  match explicit.v with
	  | I x -> Hashtbl.find x.children c
	  | L _ -> raise Not_found

(* ensures that implicit does not span over another node below [explicit] *)
    let rec canonical (explicit,implicit,child) =
      if Subseq.is_empty implicit
      then (explicit,implicit,child)
      else
	let l = !(child.final) - child.start + 1 in
	let a = Subseq.length implicit in
	if a < l
	then (explicit,implicit,child)
	else
	  let implicit' = Subseq.sub implicit l (a-l) in
	  canonical (child, implicit', get_child (child,implicit'))

(* test whether an implicit node is the root node *)
    let is_root st (explicit,implicit,_) =
      explicit == st.root & Subseq.is_empty implicit

(* test whether the extension of an implicit node by [seqar.(k).[i]] is still recognized in the GST,
   and if yes, returns the implicit node extended by 1 position, otherwise returns [None]. *)
    let has_child st (explicit,implicit,child) (k,i) =
      let a = Subseq.length implicit in
      if a <> 0 then
	if eq_char (get0 st child.seqid).[child.start+a] (get0 st k).[i]
	then Some (explicit, Subseq.extend implicit, child)
	else None
      else
	try
	  let implicit' = (get0 st k,i,1) in
	  Some (explicit, implicit', get_child (explicit,implicit'))
	with Not_found -> None

(* test whether the extension of an implicit node by '\000' exists in the GST,
   and if so, returns the corresponding leaves, otherwise returns None. *)
    let has_end st (explicit,implicit,child) =
      let a = String.length implicit in
      if a <> 0 then
	if (get0 st child.seqid).[child.start+a] = '\000'
	then Some [child]
	else None
      else
	match explicit.v with
	| I x -> Some (Hashtbl.find_all x.children '\000')
	| L _ -> None

(* --------------------------------
   creation of new nodes and leaves
   -------------------------------- *)

    let add_leaf st node seqid start final_ref index =
      match node.v with
      | I x ->
	  let child = {seqid=seqid; start=start; final=final_ref; parent=node; v=(L index)} in
	  Hashtbl.add x.children (get0 st seqid).[start] child
      | L _ -> raise (Invalid_argument "Suffix_tree.add_leaf: 2nd argument must not be a leaf")

(* make explicit an implicit node by inserting a new node between [explicit] and [child] *)
    let insert_node st strid (explicit,implicit,child) =
	match explicit.v with
	| I x ->
	    let a = Subseq.length implicit in
	    if a = 0
	    then begin
	      if x.maximal = 0 then begin (* otherwise explicit has been made maximal by another strid *)
		x.maximal <- strid;
		x.maximal_right <- true;
		x.locals <- Ext.add strid x.locals end;
	      explicit end
	    else begin
	      let c_child_old = (get0 st child.seqid).[child.start] in
	      let c_child_new = (get0 st child.seqid).[child.start+a] in
	      let h' = Hashtbl.create (Hashtbl.length x.children) in Hashtbl.add h' c_child_new child;
	      let n' = {
		seqid = child.seqid;
		start = child.start;
		final = ref (child.start+a-1);
		parent = explicit;
		v = I
		  { children = h';
		    link = st.root;
		    backlinks = LSet.empty ();
		    ext = ext0 child;
		    locals = Ext.singleton strid;
		    maximal = strid;
		    maximal_right = true;
		    visible = (0,0);
		  };
	      } in
	      child.start <- child.start+a;
	      child.parent <- n';
	      Hashtbl.replace x.children c_child_old n';
	      set_visible st n' (Param.get_visible (path st n'));
	      n' end
	| L _ -> raise (Invalid_argument "Suffix_tree.insert_node: first part of 2nd argument must not be a leaf")

(* add some strid in the extent of all ancestor of a node (except the root) *)
    let rec add_strid st strid node =
      match node.v with
      | I x ->
	  if node != st.root && not (Ext.mem strid x.ext) then begin
	    x.ext <- Ext.add strid x.ext;
	    add_strid st strid node.parent end
	  else
	    if x.maximal = strid then begin (* this node has been made maximal when adding strid *)
	      x.maximal <- 0;
	      x.maximal_right <- false;
	      x.locals <- Ext.remove strid x.locals end
(*	      LSet.remove node new_maximal *)
      | _ -> assert false

(* add a suffix link from [pred_opt] (if defined) to [explicit] *)
    let add_link strid pred_opt explicit =
      match pred_opt with
      | Some n ->
	  ( match n.v, explicit.v with
	  | I x0, I x ->
	      x0.link <- explicit;
	      x.backlinks <- LSet.add n x.backlinks;
	      if x.maximal = strid then begin
		x.maximal <- 0; (* maximal_right is left unchanged *)
		x.locals <- Ext.remove strid x.locals end
(*	      LSet.remove explicit new_maximal *)
	  | _ -> assert false)
      | None -> ()

(* ------------ 
   suffix links
   ------------ *)

(* extends suffix_link for implicit nodes *)
    let get_link st = function  (* TODO *)
      | (explicit,implicit,_) when Subseq.is_empty implicit ->
	  let explicit' = match explicit.v with I x -> x.link | _ -> assert false in  (*suffix_link root explicit*)
	  (explicit', Subseq.empty, explicit')
      | (explicit,implicit,_) ->
	  if explicit == st.root
	  then
	    let implicit' = Subseq.sub implicit 1 (Subseq.length implicit - 1) in
	    canonical (st.root, implicit', get_child (st.root,implicit'))
	  else
	    let explicit' = match explicit.v with I x -> x.link | _ -> assert false in  (*suffix_link root explicit*)
	    canonical (explicit', implicit, get_child (explicit',implicit))

(* --------------------------------------------------------------
   GST update for the new character c at position i in sequence k
   -------------------------------------------------------------- *)

(* state for 'update' *)
    type res = {
	terminal : int ref;
	mutable startj : int;
	mutable startnode : node * Subseq.t * node;
(*	mutable new_maximal : node LSet.t; *)
      }

    let rec update st (strid,i) res pred_opt =
      (* c = seqar.(strid).[i] *)
      match has_child st res.startnode (strid,i) with
      | Some extended_startnode -> (* startnode can be extended by [c] *)
	  let explicit, implicit, _ = res.startnode in
	  assert (pred_opt = None or Subseq.is_empty implicit);
          (* if a link has been followed after node creation, then we are on an explicit node *)
	  add_link strid pred_opt explicit;
	  res.startnode <- canonical extended_startnode
      | None -> (* startnode cannot be extended by [c] ... *)
	  let n' = insert_node st strid res.startnode in (* ... so we insert a new node ... *)
	  if (get0 st strid).[res.startj] <> '\000' then begin
	    add_leaf st n' strid i res.terminal res.startj;  (* ... and a new leaf for the suffix at position [res.startj] *)
	    add_strid st strid n';  (* updating the extent of ancestor nodes *)
	    add_link strid pred_opt n';  (* ... a suffix link from the last created node (if defined) ... *)
	  end;
	  res.startj <- res.startj + 1; (* prepare for the next suffix *)
	  if not (is_root st res.startnode)
	  then begin (* while [res.startnode] is not the root, and cannot be extended by [c] ... *)
	    res.startnode <- get_link st res.startnode; (* ... follow the suffix link to find the next suffix ... *)
	    update st (strid,i) res (Some n') end  (* ... and loop on [update] *)

(* -------------------------------
   implementing the signature T
   ------------------------------- *)

    let create () =
      let rec root = {
	seqid = -1;
	start = 0;
	final = ref (-1);
	parent = root;
	v=I
	  { children = Hashtbl.create 2;
	    link = root;
	    backlinks = LSet.empty ();
	    ext = Ext.empty;
	    locals = Ext.empty;
	    maximal = 0;
	    maximal_right = false;
	    visible = (0,0);
	  }} in
      let st =
	{ cpt = 0;
	  ht = Hashtbl.create 100;
	  root = root
	} in
      set_visible st root (0,0);
      st


(* general fold *)
    let rec fold_tree : t -> ('h -> node -> bool) -> ('h -> node -> 'h) -> ('s list -> 'h -> node -> 's) -> 'h -> 's =
      fun gst f h s init ->
	fold_node gst f h s init (root gst)
    and fold_node gst f h s h_node node =
      s
	(List.map
	   (fun child -> fold_node gst f h s (h h_node child) child)
	   (List.filter (f h_node) (children gst node)))
	h_node
	node
	
(* synthesized attributes only *)
    let fold_s_node gst s node = fold_node gst (fun _ _ -> true) (fun _ _ -> ()) (fun l _ n -> s l n) () node
    let fold_s_tree gst s = fold_s_node gst s (root gst)
	
(* filtering and synthesizing, no inheritance *)
    let fold_fs_node gst f s node = fold_node gst (fun _ n -> f n) (fun _ _ -> ()) (fun l _ n -> s l n) () node
    let fold_fs_tree gst f s = fold_fs_node gst f s (root gst)

    let path_restrictions st node =
      let lp = match parent st node with None -> LSet.empty () | Some p -> LSet.singleton p in (* the prefix restriction, if it exists *)
      let ls = match succ st node with None -> LSet.empty () | Some s -> LSet.singleton s in (* the suffix restriction, if it exists *)
      LSet.union lp ls

    let path_extensions st node =
      let lr = List.filter (fun n -> not (is_leaf st n)) (children st node) in (* right extensions *)
      let lf = preds st node in (* left extensions *)
      LSet.union lr lf

    let rec max_restrictions st node =
      let res1 = max_restrictions_aux st (LSet.empty ()) (path_restrictions st node) in
      let _, res2 =
	fold_while
	  (fun (res1, res2) ->
	    match res1 with
	    | [] -> None
	    | (_,n)::hns -> Some (LSet.diff hns (max_restrictions_aux st (LSet.empty ()) (path_restrictions st n)), n::res2))
	  (res1, []) in
      res2
    and max_restrictions_aux st acc = function
      | [] -> acc
      | n::ns ->
	  if is_maximal st n
	  then max_restrictions_aux st (LSet.add (height st n, n) acc) ns
	  else max_restrictions_aux st acc (path_restrictions st n @ ns)

    module SetShiftNode = Set.Make (struct type t = (* int * int * *) node let compare = Pervasives.compare end)

    let rec max_extensions st node_opt =
      let on_start, start_is_root, start =
	match node_opt with
	| None -> false, true, st.root
	| Some n -> true, n == st.root, n in
      let incrs0 = max_extensions_right st start_is_root on_start (get_visible st start) start SetShiftNode.empty (* LSet.empty () *) in
      let incrs1 =
	SetShiftNode.fold
	  (fun ((* sl, sr, *) n) incrs1 ->
	    max_extensions_remove_right st false true (* sl, sr *) n incrs1)
	  incrs0 incrs0 in
      let incrs = SetShiftNode.fold (fun ((* _, _, *) n) res -> n::res) incrs1 [] in
      let ext = ext0 start in
      let locals =
	Ext.elements (List.fold_left (fun res n -> Ext.diff res (ext0 n)) ext incrs) in
      incrs, locals
    and max_extensions_right st start_is_root on_start (shift_left, shift_right) node acc = (* node is right extension of start (possibly start itself (on_start = true)) *)
      match node.v with
      | I x ->
	  let sl, sr = x.visible in
	  if not on_start && x.maximal > 0 && ((* sl <= shift_left && sr <= shift_right && *) sl + sr < shift_left + shift_right) then
	    SetShiftNode.add ((* shift_left, shift_right, *) node) acc (* LSet.add node acc *)
	  else
	    let acc1 =
	      if not start_is_root && x.maximal_right
	      then max_extensions_left st on_start (shift_left, shift_right) node acc
	      else acc in
	    Hashtbl.fold
	      (fun _ n acc -> max_extensions_right st start_is_root false (shift_left, shift_right + !(n.final) - n.start + 1) n acc)
	      x.children
	      acc1
      | _ -> acc
    and max_extensions_left st on_start (shift_left, shift_right) node acc = (* node is right-maximal right extension of start (possibly start itself) *)
      match node.v with
      | I x ->
	  let sl, sr = x.visible in
	  if not on_start && x.maximal > 0 && ((*sl <= shift_left && sr <= shift_right && *) sl + sr < shift_left + shift_right) then
	    SetShiftNode.add ((* shift_left, shift_right, *) node) acc (* LSet.add node acc *)
	  else
	    List.fold_left
	      (fun acc n -> max_extensions_left st false (shift_left + 1, shift_right) n acc)
	      acc
	      x.backlinks
      | _ -> assert false
    and max_extensions_remove_right st start_is_root on_start (*shift_left, shift_right*) node acc = (* node is right extension of start (possibly start itself (on_start = true)) *)
      match node.v with
      | I x ->
	  let acc0 =
	    if not on_start && x.maximal > 0
	    then SetShiftNode.remove ((* shift_left, shift_right, *) node) acc
	    else acc in
	  let acc1 =
	    if not start_is_root && x.maximal_right
	    then max_extensions_remove_left st on_start (*shift_left, shift_right*) node acc0
	    else acc0 in
	  Hashtbl.fold
	    (fun _ n acc -> max_extensions_remove_right st start_is_root false (*shift_left, shift_right + !(n.final) - n.start + 1*) n acc)
	    x.children
	    acc1
      | _ -> acc
    and max_extensions_remove_left st on_start (*shift_left, shift_right*) node acc = (* node is right-maximal right extension of start (possibly start itself) *)
      match node.v with
      | I x ->
	  let acc0 =
	    if not on_start && x.maximal > 0
	    then SetShiftNode.remove ((* shift_left, shift_right, *) node) acc
	    else acc in
	  List.fold_left
	    (fun acc n -> max_extensions_remove_left st false (*shift_left + 1, shift_right*) n acc)
	    acc0
	    x.backlinks
      | _ -> assert false


    let string_restrictions st strid =
      fold_fs_tree st  (* looking for the nodes having strid as a local *)
	(fun n -> Ext.mem strid (ext0 n))
	(fun l n ->
	  List.fold_left
	    LSet.union
	    (if is_maximal st n && Ext.mem strid (locals0 n) then LSet.singleton n else LSet.empty ())
	    l)

(*
    let string_extensions st node_opt =
      Ext.elements (locals0 (match node_opt with None -> st.root | Some n -> n))
*)


    let rec find_factor st str =
      let (explicit, (s,i,len), child) = find_factor_aux st st.root (str,0,String.length str) in
      (explicit, String.sub s i len, child)
    and find_factor_aux st node implicit =
      let w = Subseq.length implicit in
      if w = 0
      then (node,implicit,node)
      else
	let child = get_child (node,implicit) in
	let l = !(child.final) - child.start + 1 in
	let a = ref 1 in
	while !a < l & !a < w & eq_char (get0 st child.seqid).[child.start + !a] (Subseq.get implicit !a) do
	  incr a
	done; (* [!a] is the first mismatch position, or the length of [child] label *)
	if ! a = l
	then find_factor_aux st child (Subseq.sub implicit !a (w - !a))
	else 
	  if !a = w
	  then (node,implicit,child)
	  else raise Not_found
(*      
      if !a < w then
	if !a < l
	then raise Not_found
	else find_factor_aux st child (Subseq.sub implicit !a (w - !a))
      else (node,implicit,child) 
*)


    let suffixes st (_,_,child) =
      fold_s_node st
	(fun l n -> if l=[] then [suffix st n] else List.concat l)
	child

    let strings st (_,_,child) = ext st child
(*
      fold_s_node st
	(fun l n -> if l=[] then LSet.singleton (fst (suffix st n)) else LSet.union_r l)
	child
*)

    let get st strid =
      let str0 = get0 st strid in
      String.sub str0 0 (String.length str0 - 1)

    let find st str =
      let factor = find_factor st str in
      match has_end st factor with
      | Some leafs ->
	  mapfind
	    (fun leaf ->
	      let (strid,pos) = suffix st leaf in
	      if pos = 0 then Some strid else None) (* there should be only one *)
	    leafs
      | None -> raise Not_found

    let find_node st str =
      let (explicit,implicit,_) = find_factor st str in
      if implicit = ""
      then explicit
      else raise Not_found

(* add a string and returns its strid. If the string already exists, the GST is not modified,
   and the existing id is returned *)
    let add st str =
      try
	find st str
      with Not_found ->
	let strid = st.cpt <- st.cpt+1; st.cpt in
	let str0 = str ^ String.make 1 '\000' in (* add a terminal symbol *)
	Hashtbl.add st.ht strid str0;
	( match st.root.v with
	| I x ->
	    x.ext <- Ext.add strid x.ext;
	    x.maximal <- strid;
	    x.maximal_right <- true;
	    x.locals <- Ext.add strid x.locals
	| _ -> assert false ); (* update the extent of the root *)
	let res = {terminal=ref (-1); startj=0; startnode=(st.root,Subseq.empty,st.root); (* new_maximal=LSet.singleton st.root *)} in (* initialize for [update] ... *)
	for pos = 0 to String.length str0 - 1 do (* for every position [i] in the sequence ... *)
	  incr res.terminal; (* increment the leaves final position ... *)
	  update st (strid,pos) res None (* call [update] for updating the suffix tree with the character at position [pos] *)
	done;
(*
	List.iter
	  (fun n -> 
	    match n.v with
	    | I x ->
		x.locals <- Ext.add strid x.locals;
		x.maximal <- true
	    | _ -> assert false)
	  res.new_maximal; (* recording new maximal nodes *)
*)
	strid

    let rec remove st strid =
      ignore (remove_node true st strid st.root);
      Hashtbl.remove st.ht strid
    and remove_node on_root st strid node =
      match node.v with
      | L _ ->
	  if node.seqid = strid
	  then None
	  else Some (node, Ext.empty)
      | I x ->
	  if not (Ext.mem strid x.ext)
	  then Some (node, Ext.empty)
	  else
	    let new_children =
	      Hashtbl.fold
		(fun c n res -> match remove_node false st strid n with None -> res | Some (n',moved_locals) -> (c,n',moved_locals)::res)
		x.children [] in
	    match new_children with
	    | [] ->
		if on_root
		then begin
		  Hashtbl.clear x.children;
		  x.ext <- Ext.remove strid x.ext;
		  x.locals <- Ext.remove strid x.locals;
		  x.maximal <- 0;
		  x.maximal_right <- false; (* ? *)
		  Some (node, Ext.empty) end
		else begin
		  remove_link st node;
		  None end
	    | [(c,child,_)] when not on_root ->
		child.start <- child.start - (!(node.final) - node.start + 1);
		child.parent <- node.parent;
		remove_link st node;
		Some (child, x.locals)
	    | (_,n0,_)::_ as l ->
		Hashtbl.clear x.children;
		List.iter
		  (fun (c,n,moved_locals) ->
		    Hashtbl.add x.children c n;
		    n.parent <- node;
		    x.locals <- Ext.union x.locals moved_locals
		  ) l;
		x.ext <- Ext.remove strid x.ext;
		x.locals <- Ext.remove strid x.locals;
		let card = Ext.cardinal x.ext in
		let maxcard_children, maxcard_backlinks = remove_maxcard l x.backlinks in
		if max maxcard_children maxcard_backlinks = card then x.maximal <- 0;
		if maxcard_children = card then x.maximal_right <- false;
		if node.seqid = strid
		then begin (* strid string does not exists any more *)
		  let a = !(node.final) - node.start + 1 in
		  node.seqid <- n0.seqid;
		  node.start <- n0.start - a;
		  node.final <- ref (n0.start - 1) end;
		Some (node, Ext.empty)
    and remove_link st node =
      match succ st node with
      | None -> ()
      | Some n ->
	  match n.v with
	  | I x -> x.backlinks <- LSet.remove node x.backlinks
	  | _ -> assert false
    and remove_maxcard children backlinks =
      let maxcard_children =
	List.fold_left (fun res (_,n,_) -> max res (Ext.cardinal (ext0 n))) 0 children in
      let maxcard_backlinks =
	List.fold_left (fun res n -> max res (Ext.cardinal (ext0 n))) 0 backlinks in
      maxcard_children, maxcard_backlinks


    let fold f st init =
      Hashtbl.fold f st.ht init


(* readable version of a GST *)

    type tree = Node of string * int * int list * tree list | Leaf of string * (strid * int)
	
    let tree st =
      fold_s_tree st
	(fun l n ->
	  if n == st.root
	  then Node ("", maximal0 n, ext st n, l)
	  else
	    let w = label st n in
	    if l=[]
	    then Leaf (w, suffix st n)
	    else Node (w, maximal0 n, ext st n, l))

  end

(* for test *)
(*
module M = Make (struct let get_visible _ = (0,0) end)
open M

let st = create ();;
let _ =
  ignore (add st "formal concept analysis");
  ignore (add st "logical concept analysis");
  ignore (add st "conceptual graphs");
  tree st;;
*)
