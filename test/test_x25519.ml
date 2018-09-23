open OUnit

open Ed.X

let assert_equal_gen pp x y =
  let msg = Fmt.strf "%a != %a" pp x pp y in
  msg @? (x = y)

let assert_equal = assert_equal_gen Fmt.string

let assert_equal_Z = assert_equal_gen Z.pp_print

(* This test is implicitly derived from RFC 7748, because it contains the
   scalars for its test both as hex string and as decimal numbers (test 1/2). *)
let test_private_key_masking_1 _ =
  let expected = Z.of_string "31029842492115040904895560451863089656472772604678260265531221036453811406496" in
  let sanitized = Wire.cstruct_of_hex "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
                  |> Wire.z_of_cstruct
                  |> Wire.sanitize_scalar in
  assert_equal_Z expected sanitized

let test_private_key_masking_2 _ =
  let expected = Z.of_string "35156891815674817266734212754503633747128614016119564763269015315466259359304" in
  let sanitized = match X25519.priv_of_string "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d" with
    | X25519.Private_key pub -> pub in
  assert_equal_Z expected sanitized

(* This test is explicitly specified in RFC 7748 (test 1/2). *)
let test_rfc_single_1 _ =
  let priv = X25519.priv_of_string "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4" in
  let pub = X25519.pub_of_string "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c" in
  let out = X25519.x25519 priv pub |> X25519.string_of_pub in
  assert_equal "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552" out

let test_rfc_single_2 _ =
  let priv = X25519.priv_of_string "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d" in
  let pub = X25519.pub_of_string "e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a493" in
  let out = X25519.x25519 priv pub |> X25519.string_of_pub in
  assert_equal "95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957" out

let rec call_multiple k u = function
  | 0 -> k
  | n ->
    let u' = k in
    let k' =
      X25519.x25519 (X25519.priv_of_string k) (X25519.pub_of_string u)
      |> X25519.string_of_pub
    in
    call_multiple k' u' (n - 1)

let test_call_multiple _ =
  let base = "0900000000000000000000000000000000000000000000000000000000000000" in
  let a = call_multiple base base 1 in
  assert_equal "422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079" a;
  let a = call_multiple base base 1000 in
  assert_equal "684cf59ba83309552800ef566f2f4d3c1c3887c49360e3875f2eb94d99532c51" a
  (* let a = call_multiple base base 1000000 in
  assert_equal "7c3911e0ab2586fd864497297e575e6f3bc601c0883c30df5f4dd2d24f665424" a *)

let suite =
  "X25519_suite" >::: [ "private_key_masking_1" >:: test_private_key_masking_1
                      ; "private_key_masking_2" >:: test_private_key_masking_2
                      ; "rfc_single_1" >:: test_rfc_single_1
                      ; "rfc_single_2" >:: test_rfc_single_2
                      ; "call_multiple" >:: test_call_multiple
                      ]

let _ =
  run_test_tt_main suite
