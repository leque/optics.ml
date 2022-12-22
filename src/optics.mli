(** {1 Types} *)
(** {2 Optic type} *)

type ('k, -'s, +'t, +'a, -'b) _t
(** Type of the whole family of optics.
    ['k] identifies the particular {{!section:kind} optic kind}. *)

type ('k, -'s, +'t, +'a, -'b) t = unit -> ('k, 's, 't, 'a, 'b) _t
(** Type synonym for optics wrapped in a thunk to avoid the value restriction. *)

type ('k, 's, 'a) _t' = ('k, 's, 's, 'a, 'a) _t
(** Type synonym for type-preserving optics. *)

type ('k, 's, 'a) t'  = ('k, 's, 's, 'a, 'a) t
(** Type synonym for type-preserving optics wrapped in a thunk. *)


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


(** {1 Optics} *)

(** {2:setter Setter} *)

val sets : (('a -> 'b) -> 's -> 't) -> ([< setter], 's, 't, 'a, 'b) _t
(** Build a setter from a function to modify the element(s). *)

val sets' : (('a -> 'b) -> 's -> 't) -> ([< setter], 's, 't, 'a, 'b) t
(** Thunk-wrapping variant of {!val:sets}. *)

val over : ([> setter], 's, 't, 'a, 'b) t -> ('a -> 'b) -> ('s -> 't)
(** Apply a setter as a modifier. *)

val set : ([> setter], 's, 't, 'a, 'b) t -> 'b -> 's -> 't
(** Apply a setter. *)


(** {2:afold Affine Fold} *)

val affine_fold : ('s -> 'a Option.t) -> ([< affine_fold], 's, 'a) _t'
(** Build an affine fold from a partial function. *)

val affine_fold' : ('s -> 'a Option.t) -> ([< affine_fold], 's, 'a) t'
(** Thunk-wrapping variant of {!val:affine_fold}. *)

val preview : ([> affine_fold], 's, 'a) t' -> 's -> 'a Option.t
(** Retrieve the value targeted by an affine fold. *)

val previews : ([> affine_fold], 's, 'a) t' -> ('a -> 'r) -> 's -> 'r Option.t
(** Retrieve a function of the value targeted by an affine fold. *)

val filtered : ('a -> bool) -> ([< affine_fold], 'a, 'a) _t'
(** Filter result(s) of a fold that don't satisfy a predicate. *)

val filtered' : ('a -> bool) -> ([< affine_fold], 'a, 'a) t'
(** Thunk-wrapping variant of {!val:filtered}. *)

val is : ([> affine_fold], 's, 'a) t' -> 's -> bool
(** Test whether this affine fold matches. *)

val isn't : ([> affine_fold], 's, 'a) t' -> 's -> bool
(** Test whether this affine fold does not match. *)

val afailing : ([> affine_fold], 's, 'a) t' -> ([> affine_fold], 's, 'a) t' -> ([< affine_fold], 's, 'a) _t'
(** Try the first affine fold. If it returns no entry, try the second one. *)

val afailing' : ([> affine_fold], 's, 'a) t' -> ([> affine_fold], 's, 'a) t' -> ([< affine_fold], 's, 'a) t'
(** Thunk-wrapping variant of {!val:afailing}. *)

(** {2:atraversal Affine Traversal} *)

val affine_traversal :
  destruct:('s -> ('a, 't) Result.t) ->
  update:('s -> 'b -> 't) ->
  ([< affine_traversal], 's, 't, 'a, 'b) _t
(** Build an affine traversal from a destructor and a setter. *)

val matching : ([> affine_traversal], 's, 't, 'a, 'b) t -> 's -> ('a, 't) Result.t
(** Retrieve the value targeted by an affine traversal
    or return the original value while allowing the type to change if it does not match. *)


(** {2:getter Getter} *)

val to_  : ('s -> 'a) -> ([< getter], 's, 'a) _t'
(** Build a getter from a function. *)

val to_'  : ('s -> 'a) -> ([< getter], 's, 'a) t'
(** Thunk-wrapping variant of {!val:to_}. *)

val view : ([> getter], 's, 't, 'a, 'b) t -> 's -> 'a
(** View the value pointed to by a getter. *)

val views : ([> getter], 's, 't, 'a, 'b) t -> ('a -> 'r) -> 's -> 'r
(** View the function of the value pointed to by a getter. *)

val get : ([> getter], 's, 't, 'a, 'b) t -> 's -> 'a
(** An alias to {!val:view}. *)


(** {2:prism Prism} *)

val prism :
  construct:('b -> 't) ->
  destruct:('s -> ('a, 't) Result.t) ->
  ([< prism], 's, 't, 'a, 'b) _t
(** Build a prism from a constructor and a destructor. *)

val prism' :
  construct:('b -> 's) ->
  cast:('s -> 'a Option.t) ->
  ([< prism], 's, 's, 'a, 'b) _t
(** Build a prism from a constructor and a cast function. *)

val only : ?equal:('a -> 'a -> bool) -> 'a -> ([< prism], 'a, unit) _t'
(** This prism compares for exact equality with a given value. *)

val only' : ?equal:('a -> 'a -> bool) -> 'a -> ([< prism], 'a, unit) t'
(** Thunk-wrapping variant of {!val:only}. *)

val nearly : f:('a -> bool) -> 'a -> ([< prism], 'a, unit) _t'
(** This prism compares for approximate equality with a given value and a predicate for testing.
    {[
# let nearly_nan () = nearly ~f:Float.is_nan Float.nan;;
val nearly_nan : unit -> ([< prism ], float, unit) Optics._t' = <fun>
# 3.14.%?[nearly_nan];;
- : unit option = Option.None
# (0.0 /. 0.0).%?[nearly_nan];;
- : unit option = Option.Some ()
    ]}

    To comply with the prism laws the arguments you supply to [nearly a p] are somewhat constrained.

    We assume [p x] holds iff [x] ≡ [a]. Under that assumption then this is a valid prism.

    This is useful when working with a type where you can test equality for only a subset of its values, and the prism selects such a value.
*)

val nearly' : f:('a -> bool) -> 'a -> ([< prism], 'a, unit) t'
(** Thunk-wrapping variant of {!val:nearly}. *)

(** {2:lens Lens} *)

val lens :
  get:('s -> 'a) ->
  set:('s -> 'b -> 't) ->
  ([< lens], 's, 't, 'a, 'b) _t
(** Build a lens from a getter and a setter. *)

val choosing :
  left:('k, 'sl, 'tl, 'a, 'b) t ->
  right:('k, 'sr, 'tr, 'a, 'b) t ->
  ('k, ('sl, 'sr) Either.t, ('tl, 'tr) Either.t, 'a, 'b) _t
(** Merge two optics. *)

(** {2:iso Isomorphism} *)

val iso : f:('s -> 'a) -> g:('b -> 't) -> ([< iso], 's, 't, 'a, 'b) _t
(** Build an iso from a pair of inverse functions. *)

val non : ?equal:('a -> 'a -> bool) -> 'a -> ([< iso], 'a option, 'a) _t'
(**
   If [v : a] and [a' = a ∖ {v}], then [non v] is an isomorphism from [a' option] to [a].

   Keep in mind this is only a real isomorphism if you treat the domain as being [(a ∖ {v}) option].

   {!val:non} lets you "relabel" a [option] by equating [None] to an arbitrary value (which you can choose):
   {[
# let non0 () = non 0;;
val non0 : unit -> ([< iso ], int option, int) Optics._t' = <fun>
# (Some 1).%[non0];;
- : int = 1
# None.%[non0];;
- : int = 0
   ]}

   The most useful thing about {!val:non} is that relabeling also works in other direction. If you try to {!val:set} the "forbidden" value, it will be turned to [None]:
   {[
# (Some 1).%[non0] <- 0;;
- : int option = None
   ]}
   Setting anything else works just fine:
   {[
# (Some 1).%[non0] <- 42;;
- : int option = Some 42
   ]}
   Same happens if you try to modify a value:
   {[
# Some 1 |> non0 %~ pred;;
- : int option = None
# Some 1 |> non0 %~ succ;;
- : int option = Some 2
   ]}
*)

val non' : ?equal:('a -> 'a -> bool) -> 'a -> ([< iso], 'a option, 'a) t'
(**
   Thunk-wrapping variant of {!val:non}.
*)

val anon : f:('a -> bool) -> 'a -> ([< iso], 'a option, 'a) _t'
(**
   [anon a p] generalizes {!val:non} a to take any value and a predicate.

   {[
# let non_nan () = anon ~f:Float.is_nan Float.nan;;
val non_nan : unit -> ([< iso ], float option, float) Optics._t' = <fun>
# Some 0.0 |> non_nan %~ (Float.div 1.0);;
- : float option = Some infinity
# Some 0.0 |> non_nan %~ (Float.div 0.0);;
- : float option = None
   ]}
*)

val anon' : f:('a -> bool) -> 'a -> ([< iso], 'a option, 'a) t'
(** Thunk-wrapping variant of {!val:anon}. *)


(** {1 Operators} *)

(** A submodule meant to be locally opened. *)
module O : sig
  val (//) : ('k, 'a, 'b, 'c, 'd) t -> ('k, 'c, 'd, 'e, 'f) t -> ('k, 'a, 'b, 'e, 'f) t
  (** Optics composition. *)

  val (.%?[]) : 's -> ([> affine_fold], 's, 'a) t' -> 'a Option.t
  (** Index operator version of {!val:preview}.

      [x.%?[o]]
      === [x |> preview o]
      === [preview o x].
  *)

  val (.%[]) : 's -> ([> getter], 's, 't, 'a, 'b) t -> 'a
  (** Index operator version of {!val:view}.

      [x.%[o]]
      === [x |> view o]
      === [view o x].
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

      NB: Thanks to the operator precedence and the associativity,
      we can write [over (o1 // o2) f x] as [x |> o1 // o2 %~ f],
      since [(//)] and [(%~)] have the same precedence,
      bind tighter than [(|>)], and are left associative.
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

val _Left  : ([< prism], ('a, 'x) Either.t, ('b, 'x) Either.t, 'a, 'b) t
(** A Prism that matches on the [Left] constructor of [Either.t]. *)

val _Right : ([< prism], ('x, 'a) Either.t, ('x, 'b) Either.t, 'a, 'b) t
(** A Prism that matches on the [Right] constructor of [Either.t]. *)
