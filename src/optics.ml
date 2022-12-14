(*
   based on OCaml van Laarhoven CPS lenses. https://gist.github.com/tel/08d2a94de21f483cbb20
   plus eta expansion
   https://stackoverflow.com/questions/29187287/sneaking-lenses-and-cps-past-the-value-restriction
   *)
type ('k, -'s, +'t, +'a, -'b) __t =
  { op : 'r. ('a -> ('b -> 'r) -> 'r) -> ('s -> ('t -> 'r) -> 'r) }

type ('k, 'outer, 'inner) _t = ('k, 's, 't, 'a, 'b) __t
    constraint 'outer = 's -> 't
    constraint 'inner = 'a -> 'b

type ('k, 'outer, 'inner) t = unit -> ('k, 'outer, 'inner) _t

type ('k, 's, 'a) _t' = ('k, 's -> 's, 'a -> 'a) _t
type ('k, 's, 'a) t'  = ('k, 's -> 's, 'a -> 'a) t

type setter = [`Setter]
type affine_fold = [`Affine_fold]
type affine_traversal = [setter|affine_fold|`Affine_traversal]
type getter = [affine_fold|`Getter]
type prism = [affine_traversal|`Prism]
type lens = [getter|affine_traversal|`Lens]
type iso = [prism|lens|`Iso]

let app o = (o ()).op

let uncps (type b r) : ('a -> (b -> r) -> r) -> ('a -> b) =
  fun f a ->
  let exception Return of b in
  match f a (fun b -> raise_notrace (Return b)) with
  | _ -> assert false
  | exception Return b -> b

let sets f =
  let op acont s tcont =
    tcont (f (uncps acont) s)
  in { op }

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

let is o s =
  Option.is_some (preview o s)

let isn't o s =
  not (is o s)

let afailing o1 o2 =
  affine_fold (fun s ->
      match preview o1 s with
      | Some _ as x -> x
      | None -> preview o2 s)

let afailing' o1 o2 () = afailing o1 o2

let affine_traversal ~destruct ~set =
  let op acont s tcont =
    Result.fold (destruct s)
      ~error:tcont
      ~ok:(fun x -> acont x (fun b -> tcont (set s b)))
  in { op }

let matching o s =
  app o (fun a _bcont -> Result.ok a) s (fun t -> Result.error t)

let to_ sa =
  let op acont s _tcont =
    acont (sa s) (fun _b -> assert false)
  in { op }

let to_' sa () = to_ sa

let views o f s =
  app o (fun a _bcont -> f a) s (fun _ -> assert false)

let view o s =
  views o Fun.id s

let get = view

let prism ~construct ~destruct =
  let op acont s tcont =
    Result.fold (destruct s)
      ~error:tcont
      ~ok:(fun x -> acont x (fun b -> tcont (construct b)))
  in { op }

let prism_ ~construct ~cast =
  prism ~construct ~destruct:(fun s ->
      match cast s with
      | Some x -> Result.ok x
      | None -> Result.error s)

let only ?(equal = (=)) x =
  prism_ ~construct:(fun () -> x)
    ~cast:(fun a ->
        if equal a x then
          Some ()
        else
          None)

let only' ?(equal = (=)) x () =
  only ~equal x

let nearly ~f x =
  prism_ ~construct:(fun () -> x)
    ~cast:(fun a ->
        if f a then
          Some ()
        else
          None)

let nearly' ~f x () =
  nearly ~f x

let lens ~get ~set =
  let op acont s tcont =
    acont (get s) (fun b -> tcont (set s b))
  in { op }

let choosing ~left ~right =
  let op acont s tcont =
    match s with
    | Either.Left l -> app left acont l (fun t -> tcont (Either.Left t))
    | Either.Right r -> app right acont r (fun t -> tcont (Either.Right t))
  in { op }

let iso ~f:sa ~g:bt =
  let op acont s tcont =
    acont (sa s) (fun b -> tcont (bt b))
  in { op }

let non ?(equal = (=)) x =
  iso
    ~f:(fun opt -> Option.value opt ~default:x)
    ~g:(fun a ->
      if equal a x then
        None
      else
        Some a)

let non' ?(equal = (=)) x () =
  non ~equal x

let anon ~f x =
  iso
    ~f:(fun opt -> Option.value opt ~default:x)
    ~g:(fun a ->
      if f a then
        None
      else
        Some a)

let anon' ~f x () =
  anon ~f x

module O = struct
  let (//) f g () = { op = fun z -> app f (app g z) }

  let (.%?[]) s o =
    preview o s

  let (.%[]) s o =
    get o s

  let (.%[]<-) s o v =
    set o v s

  let (%~) o f s =
    over o f s
end

include O

let id () = { op = Fun.id }

let _1 () = lens ~get:fst ~set:(fun (_, x) b -> (b, x))
let _2 () = lens ~get:snd ~set:(fun (x, _) b -> (x, b))

let _Ok () =
  prism ~construct:Result.ok
    ~destruct:(function
        | Result.Ok x -> Result.ok x
        | Result.Error _ as x -> Result.error x)

let _Error () =
  prism ~construct:Result.error
    ~destruct:(function
        | Result.Error x -> Result.ok x
        | Result.Ok _ as x -> Result.error x)

let _Some () =
  prism ~construct:Option.some
    ~destruct:(function
        | Some x -> Result.ok x
        | None as x -> Result.error x)

let _None () =
  prism ~construct:(fun () -> Option.none)
    ~destruct:(function
        | None -> Result.ok ()
        | Some _ as x -> Result.error x)

let _Left () =
  prism ~construct:Either.left
    ~destruct:(function
        | Either.Left x -> Result.ok x
        | Either.Right _ as x -> Result.error x)

let _Right () =
  prism ~construct:Either.right
    ~destruct:(function
        | Either.Right x -> Result.ok x
        | Either.Left _ as x -> Result.error x)
