(*
   based on OCaml van Laarhoven CPS lenses. https://gist.github.com/tel/08d2a94de21f483cbb20
   plus eta expansion
   https://stackoverflow.com/questions/29187287/sneaking-lenses-and-cps-past-the-value-restriction
   *)
type setter = [`Setter]
type affine_fold = [`Affine_fold]
type affine_traversal = [setter|affine_fold|`Affine_traversal]
type getter = [affine_fold|`Getter]
type prism = [affine_traversal|`Prism]
type lens = [getter|affine_traversal|`Lens]
type iso = [prism|lens|`Iso]

type ('k, -'s, +'t, +'a, -'b) _t =
  { op : 'r. ('a -> ('b -> 'r) -> 'r) -> ('s -> ('t -> 'r) -> 'r) }

type ('k, -'s, +'t, +'a, -'b) t = unit -> ('k, 's, 't, 'a, 'b) _t

type ('k, 's, 'a) _t' = ('k, 's, 's, 'a, 'a) _t
type ('k, 's, 'a) t'  = ('k, 's, 's, 'a, 'a) t

let app o = (o ()).op

let uncps (type b r) : ('a -> (b -> r) -> r) -> ('a -> b) =
  fun f a ->
  let exception Return of b in
  try
    ignore (f a (fun b -> raise (Return b)) : r);
    assert false
  with Return b -> b

let sets f =
  let op acont s tcont =
    tcont (f (uncps acont) s)
  in
  { op }

let sets' f () = sets f

let over o f s =
  app o (fun a bcont -> bcont (f a)) s Fun.id

let set o v s =
  over o (Fun.const v) s

let affine_fold f =
  let op acont s tcont =
    match f s with
    | Some a -> acont a (fun _b -> assert false)
    | None -> tcont s
  in { op }

let affine_fold' f () = affine_fold f

let previews o f s =
  app o (fun a _bcont -> Some (f a)) s (fun _ -> None)

let preview o s =
  previews o Fun.id s

let filtered p =
  let op acont s tcont =
    if p s then
      acont s (fun _b -> assert false)
    else
      tcont s
  in { op }

let filtered' p () = filtered p

let isn't o s =
  Option.is_none (preview o s)

let afailing o1 o2 =
  affine_fold' (fun s ->
      match preview o1 s with
      | Some _ as x -> x
      | None -> preview o2 s)

let affine_traversal destruct update =
  let op acont s tcont =
    Result.fold (destruct s)
      ~error:tcont
      ~ok:(fun x -> acont x (fun b -> tcont (update s b)))
  in { op }

let matching o s =
  app o (fun a _bcont -> Result.ok a) s (fun t -> Result.error t)

let to_ sa =
  let op acont s _tcont =
    acont (sa s) (fun _b -> assert false)
  in { op }

let to_' sa () = to_ sa

let get o s =
  app o Fun.const s (fun _ -> assert false)

let prism construct destruct =
  let op acont s tcont =
    Result.fold (destruct s)
      ~error:tcont
      ~ok:(fun x -> acont x (fun b -> tcont (construct b)))
  in { op }

let prism' construct cast =
  prism construct (fun s ->
      match cast s with
      | Some x -> Result.ok x
      | None -> Result.error s)

let lens get set =
  let op acont s tcont =
    acont (get s) (fun b -> tcont (set s b))
  in { op }

let iso sa bt =
  let op acont s tcont =
    acont (sa s) (fun b -> tcont (bt b))
  in { op }

module O = struct
  let (//) f g () = { op = fun z -> app f (app g z) }

  let (.%[]) s o =
    get o s

  let (.%[]<-) s o v =
    set o v s

  let (%~) o f s =
    over o f s
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
