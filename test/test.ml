open Optics

let%expect_test _ =
  let safe_div a b =
    if b = 0 then None else Some (a / b)
  in
  print_string @@ [%derive.show: int option list]
  begin
    [ ( 0, "hello") |> preview (_1 // affine_fold' (safe_div 42))
    ; (42, "hello") |> preview (_1 // affine_fold' (safe_div 42))
    ]
  end; [%expect {| [None; (Some 1)] |}]

let%expect_test _ =
  let is_positive n = n > 0 in
  print_string @@ [%derive.show: int option list]
  begin
    [ ( 0, "hello") |> preview (_1 // filtered' is_positive)
    ; (42, "hello") |> preview (_1 // filtered' is_positive)
    ]
  end; [%expect {| [None; (Some 42)] |}]

let%expect_test _ =
  let is_positive n = n > 0 in
  let is_negative n = n < 0 in
  let o () = afailing (filtered' is_positive) (filtered' is_negative) in
  print_string @@ [%derive.show: int option list]
  begin
    [ ( 0, "hello") |> preview (_1 // o)
    ; ( 1, "hello") |> preview (_1 // o)
    ; (-1, "hello") |> preview (_1 // o)
    ]
  end; [%expect {| [None; (Some 1); (Some -1)] |}]

let%expect_test _ =
  print_string @@ [%derive.show: int option]
  begin
    ("hello", "world!") |> preview (_1 // to_' String.length)
  end; [%expect {| (Some 5) |}]

let%expect_test _ =
  print_string @@ [%derive.show: string option list]
  begin
    [ ("hello", Some "world!") |> preview (_2 // _Some)
    ; ("hello", None) |> preview (_2 // _Some)
    ]
  end; [%expect {| [(Some "world!"); None] |}]

let%expect_test _ =
  print_string @@ [%derive.show: string option]
  begin
    ("hello", "world!") |> preview _1
  end; [%expect {| (Some "hello") |}]

let%expect_test _ =
  print_string @@ [%derive.show: (string, string * string) result]
  begin
    ("hello", "world!") |> matching _1
  end; [%expect {| (Ok "hello") |}]

let%expect_test _ =
  print_string @@ [%derive.show: (string, string * string option) result list]
  begin
    [ ("hello", Some "world!") |> matching (_2 // _Some)
    ; ("hello", None) |> matching (_2 // _Some)
    ]
  end; [%expect {| [(Ok "world!"); (Error ("hello", None))] |}]


(* Some examples from https://github.com/ekmett/lens/wiki/Examples *)

let%expect_test _ =
  print_string @@ [%derive.show: string]
  begin
    ("hello", "world").%[_2]
  end; [%expect {| "world" |}]

let%expect_test _ =
  print_string @@ [%derive.show: string * int]
  begin
    set _2 42 ("hello", "world")
  end; [%expect {| ("hello", 42) |}]

let%expect_test _ =
  print_string @@ [%derive.show: string * (int * string)]
  begin
    set (_2 // _1) 42 ("hello", ("world", "!!!"))
  end; [%expect {| ("hello", (42, "!!!")) |}]

let%expect_test _ =
  print_string @@ [%derive.show: int]
  begin
    "hello".%[to_' String.length]
  end; [%expect {| 5 |}]

let%expect_test _ =
  print_string @@ [%derive.show: int]
  begin
    ("hello", ("world", "!!!")).%[_2 // _2 // to_' String.length]
  end; [%expect {| 3 |}]

let%expect_test _ =
  print_string @@ [%derive.show: string * string]
  begin
    ((), "world") |> set _1 "hello"
  end; [%expect {| ("hello", "world") |}]

let%expect_test _ =
  print_string @@ [%derive.show: string * string]
  begin
    ((), "world").%[_1] <- "hello"
  end; [%expect {| ("hello", "world") |}]

let%expect_test _ =
  print_string @@ [%derive.show: int]
  begin
    view _2 (10, 20)
  end; [%expect {| 20 |}]

let%expect_test _ =
  print_string @@ [%derive.show: int list]
  begin
    over (sets' List.map) succ [1; 2; 3]
  end; [%expect {| [2; 3; 4] |}]

let%expect_test _ =
  print_string @@ [%derive.show: (int * int) list]
  begin
    over (sets' List.map // _2) succ [(1, 2); (3, 4)]
  end; [%expect {| [(1, 3); (3, 5)] |}]

let%expect_test _ =
  let char_succ c = Char.chr (Char.code c + 1) in
  print_string @@ [%derive.show: (int * string) list * string]
  begin
    ([(42, "hello")], "world") |> _1 // sets' List.map // _2 // sets' String.map %~ char_succ
  end; [%expect {| ([(42, "ifmmp")], "world") |}]
