(** {1 Types} *)

(** {2:kind Optic kind} *)
(** Types representing optics hierarchy. *)

type setter = [`Setter]
(** Tag for {!section:setter}. *)

type getter = [`Getter]
(** Tag for {!section:getter}. *)

type lens = [setter|getter|`Lens]
(** Tag for {!section:lens}. *)

type prism = [setter|`Prism]
(** Tag for {!section:prism}. *)

type iso = [lens|prism|`Iso]
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

(** {1:iso Isomorphism} *)

val iso : ('s -> 'a) -> ('b -> 't) -> ([< iso], 's, 't, 'a, 'b) _t
(** Build an iso from a pair of inverse functions. *)


(** {1:getter Getter} *)

val to_  : ('s -> 'a) -> ([< getter], 's, 'a) t'
(** Build a getter from a function. *)

val get : ([> getter], 's, 't, 'a, 'b) t -> 's -> 'a
(** Get the value pointed to by a getter. *)


(** {1:setter Setter} *)

val sets : (('a -> 'b) -> 's -> 't) -> ([< setter], 's, 't, 'a, 'b) t
(** Build a setter from a function to modify the element(s). *)

val over : ([> setter], 's, 't, 'a, 'b) t -> ('a -> 'b) -> ('s -> 't)
(** Apply a setter as a modifier. *)

val set : ([> setter], 's, 't, 'a, 'b) t -> 'b -> 's -> 't
(** Apply a setter. *)


(** {1:lens Lens} *)

val lens   : ('s -> 'a) -> ('s -> 'b -> 't) -> ([< lens], 's, 't, 'a, 'b) _t
(** Build a lens from a getter and a setter. *)


(** {1:prism Prism} *)

val prism  : ('b -> 't) -> ('s -> ('a, 't) Result.t) -> ([< prism], 's, 't, 'a, 'b) _t
(** Build a prism from a constructor and a destructor. *)

val prism' : ('b -> 's) -> ('s ->       'a Option.t) -> ([< prism], 's, 's, 'a, 'b) _t
(** Build a prism from a constructor and a cast function. *)

(** {1 Operators} *)

(** A submodule meant to be locally opened. *)
module O : sig
  val (//) : ('k, 'a, 'b, 'c, 'd) t -> ('k, 'c, 'd, 'e, 'f) t -> ('k, 'a, 'b, 'e, 'f) t
  (** Optics composition. *)

  val (.%[]) : 's -> ([> getter], 's, 't, 'a, 'b) t -> 'a
  (** Index operator version of {!val:get}.

      [x.%[t]] === [get t x].
  *)

  val (.%[]<-) : 's -> ([> setter], 's, 't, 'a, 'b) t -> 'b -> 't
  (** Index operator version of {!val:set}.

      [x.%[t] <- v] === [set t v x].
  *)

  val (%~) : ([> setter], 's, 't, 'a, 'b) t -> ('a -> 'b) -> 's -> 't
  (** Infix version of {!val:over}.

      [x |> t %~ f] === [over t f x].
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