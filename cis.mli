(**
   Cis : compact integer sets

   This module implements compact integer sets, represented as a (custom) list
   of integer intervals. Usual set operations are provided.
   The advantage compared to ordered lists is that the actual size may be smaller
   than the cardinal of a set when many elements are contiguous. Most set operations
   are linear w.r.t. the size of the structure, not the cardinal of the set.

   Author: Sébastien Ferré <ferre@irisa.fr>
   License: LGPL
*)

type t (** Type of cis *)

val max_elt : t -> int
    (** [max_elt cis] returns the maximum integer in [cis]. Takes constant time. *)
val min_elt : t -> int
    (** [min_elt cis] returns the minimum integer in [cis]. Takes linear time. *)
val append : t -> t -> t
    (** [append cis1 cis2] returns the union of [cis1] and [cis2] assuming that all elements of [cis1] are greater than any element of [cis2].
       Takes linear time in the size of [cis1]. Not tail-recursive. *)
val empty : t
    (** [empty] is the empty set. *)
val is_empty : t -> bool
    (** [is_empty cis] returns whether [cis] is the empty set. *)
val cardinal : t -> int
    (** [cardinal cis] returns the cardinal of [cis]. Takes linear time in the size of [cis]. *)
val mem : int -> t -> bool
    (** [mem x cis] returns whether [x] belongs to [cis]. Takes linear time in the size of [cis]. *) 
val singleton : int -> t
    (** [singleton x] returns a singleton set with element [x]. *)
val add : int -> t -> t
    (** [add x cis] adds element [x] to [cis]. Takes linear time in the size of [cis], but constant time when [x] is greater than any element in [cis].
       Not tail-recursive. *)
val remove : int -> t -> t
    (** [remove x cis] removes element [x] from [cis]. Not tail-recursive. *)
val of_list : int list -> t
    (** [of_list l] builds a cis from an integer list. *)
val union : t -> t -> t
    (** The set union. *)
val inter : t -> t -> t
    (** The set intersection. *)
val diff : t -> t -> t
    (** The set difference. *)
val subset : t -> t -> bool
    (** [subset cis1 cis2] returns whether [cis1] is a subset of [cis2]. *)
val equal : t -> t -> bool
    (** [equal cis1 cis2] returns whether [cis1] is equal to [cis2]. *)
val iter : (int -> unit) -> t -> unit
    (** Iteration on the elements of a cis. *)
val fold_left : ('a -> int -> 'a) -> 'a -> t -> 'a
    (** Left folding. Elements are visited in decreasing order. *)
val fold_right : (int -> 'a -> 'a) -> t -> 'a -> 'a
    (** Right folding. Integers are visited in increasing order. *)
val elements : t -> int list
    (** [elements cis] returns the elements of [cis] as list of decreasing integers. *)
