open OUnit
open Fmt

module F = Ed.Curve_25519.Field_25519
module C = Ed.Curve_25519.Curve_25519

let rec range = function
  | 0 -> []
  | n ->
    let n = n - 1 in
    n :: range n

let assert_equal_Z x y =
  let msg = Fmt.strf "%a != %a" Z.pp_print x Z.pp_print y in
  msg @? Z.(x = y)

let assert_equal_F x y =
  let msg = Fmt.strf "%a != %a" F.pp x F.pp y in
  msg @? F.Infix.(x = y)

let test_base_point _ =
  let base_point_montgomery = Z.of_int 9 in
  let base_point = C.of_montgomery_u base_point_montgomery in
  let u = C.to_montgomery_u base_point in
  assert_equal_Z Z.(~$9) u;

  "base point is on curve" @? C.I.on_curve base_point;
  (* Order taken from https://cr.yp.to/ecdh/curve25519-20060209.pdf,
     "Small-subgroup attacks." on p.8 *)
  let order = Z.(pow ~$2 252 + of_string "27742317777372353535851937790883648493") in
  "base point has advertised order" @? C.I.(Infix.(scale order base_point = zero))

let suite =
  "Curve_25519_suite" >::: [ "base_point_test" >:: test_base_point
                           ]

let _ =
  run_test_tt_main suite
