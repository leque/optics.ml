open Optics

(* Json type a la jsonaf *)
type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]

let _Null : ([< prism], t, unit) Optics.t'
  = fun () ->
    prism_ ~construct:(fun () -> `Null)
      ~cast:(function
          | `Null -> Some ()
          | _ -> None)

let _False : ([< prism], t, unit) Optics.t'
  = fun () ->
    prism_ ~construct:(fun () -> `False)
      ~cast:(function
          | `False -> Some ()
          | _ -> None)

let _True : ([< prism], t, unit) Optics.t'
  = fun () ->
    prism_ ~construct:(fun () -> `True)
      ~cast:(function
          | `True -> Some ()
          | _ -> None)

let _Bool : ([< prism], t, bool) Optics.t'
  = fun () ->
    prism_ ~construct:(fun b -> if b then `True else `False)
      ~cast:(function
          | `True -> Some true
          | `False -> Some false
          | _ -> None)

let _String : ([< prism], t, string) Optics.t'
  = fun () ->
    prism_ ~construct:(fun x -> `String x)
      ~cast:(function
          | `String x -> Some x
          | _ -> None)

let _Number : ([< prism], t, string) Optics.t'
  = fun () ->
    prism_ ~construct:(fun x -> `Number x)
      ~cast:(function
          | `Number x -> Some x
          | _ -> None)

let _Int : ([< prism], t, int) Optics.t'
  = fun () ->
    prism_ ~construct:(fun x -> `Number (Int.to_string x))
      ~cast:(function
          | `Number x -> int_of_string_opt x
          | _ -> None)

let _Float : ([< prism], t, float) Optics.t'
  = fun () ->
    prism_ ~construct:(fun x -> `Number (Float.to_string x))
      ~cast:(function
          | `Number x -> float_of_string_opt x
          | _ -> None)

let _Object : ([< prism], t, (string * t) List.t) Optics.t'
  = fun () ->
    prism_ ~construct:(fun kvs -> `Object kvs)
      ~cast:(function
          | `Object kvs -> Some kvs
          | _ -> None)

let key : string -> ([< affine_traversal], (string * t) List.t, t) Optics.t'
  = fun k () ->
    let update kvs v =
      (k, v) :: List.remove_assoc k kvs
    in
    affine_traversal
      ~update
      ~destruct:(fun x -> Option.to_result ~none:x (List.assoc_opt k x))

let _Array : ([< prism], t, t List.t) Optics.t'
  = fun () ->
    prism_ ~construct:(fun vs -> `Array vs)
      ~cast:(function
          | `Array vs -> Some vs
          | _ -> None)

let nth : int -> ([< affine_fold], t List.t, t) Optics.t'
  = fun n () ->
    affine_fold (fun x -> List.nth_opt x n)
