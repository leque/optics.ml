(*
   based on OCaml van Laarhoven CPS lenses. https://gist.github.com/tel/08d2a94de21f483cbb20
   plus eta expansion
   https://stackoverflow.com/questions/29187287/sneaking-lenses-and-cps-past-the-value-restriction
   *)
type setter = [`Setter]
type getter = [`Getter]
type lens = [setter|getter|`Lens]
type prism = [setter|`Prism]
type iso = [lens|prism|`Iso]

type (+'k, -'s, +'t, +'a, -'b) _t =
  { op : 'r. ('a -> ('b -> 'r) -> 'r) -> ('s -> ('t -> 'r) -> 'r) }

type (+'k, -'s, +'t, +'a, -'b) t = unit -> ('k, 's, 't, 'a, 'b) _t

type ('k, 's, 'a) _t' = ('k, 's, 's, 'a, 'a) _t
type ('k, 's, 'a) t'  = ('k, 's, 's, 'a, 'a) t

let app t = (t ()).op

let iso sa bt =
  let op acont s tcont =
    acont (sa s) (fun b -> tcont (bt b))
  in { op }

let to_ sa () =
  let op acont s _tcont =
    acont (sa s) (fun _b -> assert false)
  in { op }

let get t s =
  app t Fun.const s (fun _ -> assert false)

let uncps (type b r) : ('a -> (b -> r) -> r) -> ('a -> b) =
  fun f a ->
  let exception Return of b in
  try
    ignore (f a (fun b -> raise (Return b)) : r);
    assert false
  with Return b -> b

let sets f () =
  let op acont s tcont =
    tcont (f (uncps acont) s)
  in
  { op }

let over t f s =
  app t (fun a bcont -> bcont (f a)) s Fun.id

let set t v s =
  over t (Fun.const v) s

let lens get set =
  let op acont s tcont =
    acont (get s) (fun b -> tcont (set s b))
  in { op }

let prism construct destruct =
  let op acont s tcont =
    Result.fold (destruct s)
      ~error:tcont
      ~ok:(fun x -> acont x (fun b -> tcont (construct b)))
  in { op }

let prism' construct destruct =
  prism construct (fun s ->
      match destruct s with
      | Some x -> Result.ok x
      | None -> Result.error s)

module O = struct
  let (//) f g () = { op = fun z -> app f (app g z) }

  let (.%[]) s t =
    get t s

  let (.%[]<-) s t v =
    set t v s

  let (%~) t f s =
    over t f s
end

include O

let id () = { op = Fun.id }

let _1 () = lens fst (fun (_, x) b -> (b, x))
let _2 () = lens snd (fun (x, _) b -> (x, b))

let _Ok () =
  prism Result.ok
    (function
      | Result.Ok x -> Result.ok x
      | Result.Error _ as x -> Result.error x)

let _Error () =
  prism Result.error
    (function
      | Result.Error x -> Result.ok x
      | Result.Ok _ as x -> Result.error x)

let _Some () =
  prism Option.some
    (function
      | Some x -> Result.ok x
      | None as x -> Result.error x)

let _None () =
  prism (fun () -> Option.none)
    (function
      | None -> Result.ok ()
      | Some _ as x -> Result.error x)
