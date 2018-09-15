open OUnit

open Ed.X25519

(* This test is implicitly derived from RFC 7748, because it contains the
   scalars for its test both as hex string and as decimal numbers (test 1/2). *)
let test_private_key_masking _ =
  let expected = Z.of_string "31029842492115040904895560451863089656472772604678260265531221036453811406496" in
  let sanitized = Wire.cstruct_of_hex "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
                  |> Wire.z_of_cstruct
                  |> Wire.sanitize_scalar in
  assert_equal expected sanitized

(* This test is explicitly specified in RFC 7748 (test 1/2). *)
let test_rfc_single_1 _ =
  let priv = X25519.priv_of_string "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4" in
  let pub = X25519.pub_of_string "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c" in
  let out = X25519.x25519 priv pub |> X25519.string_of_pub in
  assert_equal "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552" out

let suite =
  "X25519_suite" >::: [ "private_key_masking" >:: test_private_key_masking
                      ; "rfc_single_1" >:: test_rfc_single_1
                      ]

let _ =
  run_test_tt_main suite
