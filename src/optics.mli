(** {1 Types} *)

(** {2:kind Optic kind} *)
(** Types representing optics hierarchy. *)

type setter = [`Setter]
(** Tag for {!section:setter}. *)

type affine_fold = [`Affine_fold]
(** Tag for {!section:afold}. *)

type affine_traversal = [setter|affine_fold|`Affine_traversal]
(** Tag for {!section:atraversal}. *)

type getter = [affine_fold|`Getter]
(** Tag for {!section:getter}. *)

type prism = [affine_traversal|`Prism]
(** Tag for {!section:prism}. *)

type lens = [getter|affine_traversal|`Lens]
(** Tag for {!section:lens}. *)

type iso = [prism|lens|`Iso]
(** Tag for {!section:iso}. *)


(** {2 Optic type} *)

type ('k, -'s, +'t, +'a, -'b) _t
(** Type of the whole family of optics.
    ['k] identifies the particular {{!section:kind} optic kind}. *)

type ('k, -'s, +'t, +'a, -'b) t = unit -> ('k, 's, 't, 'a, 'b) _t
(** Eta-expanded type synonym to avoid value restriction. *)

type ('k, 's, 'a) _t' = ('k, 's, 's, 'a, 'a) _t
(** Type synonym for type-preserving optics. *)

type ('k, 's, 'a) t'  = ('k, 's, 's, 'a, 'a) t
(** Type synonym for type-preserving optics + eta-expansion. *)


(** {1:setter Setter} *)

val sets : (('a -> 'b) -> 's -> 't) -> ([< setter], 's, 't, 'a, 'b) _t
(** Build a setter from a function to modify the element(s). *)

val sets' : (('a -> 'b) -> 's -> 't) -> ([< setter], 's, 't, 'a, 'b) t
(** Eta-expanded version of {!val:sets}. *)

val over : ([> setter], 's, 't, 'a, 'b) t -> ('a -> 'b) -> ('s -> 't)
(** Apply a setter as a modifier. *)

val set : ([> setter], 's, 't, 'a, 'b) t -> 'b -> 's -> 't
(** Apply a setter. *)


(** {1:afold Affine Fold} *)

val affine_fold : ('s -> 'a Option.t) -> ([< affine_fold], 's, 'a) _t'
(** Build an affine fold from a partial function. *)

val affine_fold' : ('s -> 'a Option.t) -> ([< affine_fold], 's, 'a) t'
(** Eta-expanded version of {!val:affine_fold}. *)

val preview : ([> affine_fold], 's, 'a) t' -> 's -> 'a Option.t
(** Retrieve the value targeted by an affine fold. *)

val previews : ([> affine_fold], 's, 'a) t' -> ('a -> 'r) -> 's -> 'r Option.t
(** Retrieve a function of the value targeted by an affine fold. *)

val filtered : ('a -> bool) -> ([< affine_fold], 'a, 'a) _t'
(** Filter result(s) of a fold that don't satisfy a predicate. *)

val filtered' : ('a -> bool) -> ([< affine_fold], 'a, 'a) t'
(** Eta-expanded version of {!val:filtered}. *)

val isn't : ([> affine_fold], 's, 'a) t' -> 's -> bool
(** Test whether this affine fold does not match. *)

val afailing : ([> affine_fold], 's, 'a) t' -> ([> affine_fold], 's, 'a) t' -> ([< affine_fold], 's, 'a) t'
(** Try the first affine fold. If it returns no entry, try the second one. *)

(** {1:atraversal Affine Traversal} *)

val affine_traversal :
  ('s -> ('a, 't) Result.t) -> ('s -> 'b -> 't) -> ([< affine_traversal], 's, 't, 'a, 'b) _t
(** Build an affine traversal from a destructor and a setter. *)

val matching : ([> affine_traversal], 's, 't, 'a, 'b) t -> 's -> ('a, 't) Result.t
(** Retrieve the value targeted by an affine traversal
    or return the original value while allowing the type to change if it does not match. *)


(** {1:getter Getter} *)

val to_  : ('s -> 'a) -> ([< getter], 's, 'a) _t'
(** Build a getter from a function. *)

val to_'  : ('s -> 'a) -> ([< getter], 's, 'a) t'
(** Eta-expanded version of {!val:to_}. *)

val get : ([> getter], 's, 't, 'a, 'b) t -> 's -> 'a
(** Get the value pointed to by a getter. *)

val view : ([> getter], 's, 't, 'a, 'b) t -> 's -> 'a
(** An alias to {!val:get}. *)


(** {1:prism Prism} *)

val prism  : ('b -> 't) -> ('s -> ('a, 't) Result.t) -> ([< prism], 's, 't, 'a, 'b) _t
(** Build a prism from a constructor and a destructor. *)

val prism' : ('b -> 's) -> ('s ->       'a Option.t) -> ([< prism], 's, 's, 'a, 'b) _t
(** Build a prism from a constructor and a cast function. *)


(** {1:lens Lens} *)

val lens   : ('s -> 'a) -> ('s -> 'b -> 't) -> ([< lens], 's, 't, 'a, 'b) _t
(** Build a lens from a getter and a setter. *)


(** {1:iso Isomorphism} *)

val iso : ('s -> 'a) -> ('b -> 't) -> ([< iso], 's, 't, 'a, 'b) _t
(** Build an iso from a pair of inverse functions. *)


(** {1 Operators} *)

(** A submodule meant to be locally opened. *)
module O : sig
  val (//) : ('k, 'a, 'b, 'c, 'd) t -> ('k, 'c, 'd, 'e, 'f) t -> ('k, 'a, 'b, 'e, 'f) t
  (** Optics composition. *)

  val (.%[]) : 's -> ([> getter], 's, 't, 'a, 'b) t -> 'a
  (** Index operator version of {!val:get}.

      [x.%[o]]
      === [x |> get o]
      === [get o x].
  *)

  val (.%[]<-) : 's -> ([> setter], 's, 't, 'a, 'b) t -> 'b -> 't
  (** Index operator version of {!val:set}.

      [x.%[o] <- v]
      === [x |> set o v]
      === [set o v x].
  *)

  val (%~) : ([> setter], 's, 't, 'a, 'b) t -> ('a -> 'b) -> 's -> 't
  (** Infix version of {!val:over}.

      [x |> o %~ f] === [over o f x].
  *)
end

include module type of O

(** {1 Instances} *)

val id : ([< iso], 's, 'a, 's, 'a) t
(** The identity optics. *)

val _1 : ([< lens], 'a * 'x, 'b * 'x, 'a, 'b) t
(** Access the 1st element of a 2-tuple. *)

val _2 : ([< lens], 'x * 'a, 'x * 'b, 'a, 'b) t
(** Access the 2nd element of a 2-tuple. *)

val _Ok    : ([< prism], ('a, 'x) Result.t, ('b, 'x) Result.t, 'a, 'b) t
(** A Prism that matches on the [Ok] constructor of [Result.t]. *)

val _Error : ([< prism], ('x, 'a) Result.t, ('x, 'b) Result.t, 'a, 'b) t
(** A Prism that matches on the [Error] constructor of [Result.t]. *)

val _Some : ([< prism], 'a Option.t, 'b Option.t, 'a, 'b) t
(** A Prism that matches on the [Some] constructor of [Option.t]. *)

val _None : ([< prism], 'a Option.t, unit) t'
(** A Prism that matches on the [None] constructor of [Option.t]. *)
