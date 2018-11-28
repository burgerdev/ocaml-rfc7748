open OUnit

open Rfc7748

let module_suite: (module S) -> test list = fun m ->
  let module M = (val m) in
  let check_key_size _ =
    assert_equal ~printer:string_of_int
      M.((string_of_public_key base |> String.length))
      M.(key_size * 2)
  in
  let check_base_point _ =
    (* TODO: use a random value instead of a made-up one. *) 
    let priv = M.private_key_of_string @@ String.make M.(key_size * 2) '4' in
    assert_equal ~printer:(M.string_of_public_key)
      M.(public_key_of_private_key priv)
      M.(scale priv base)
  in
  [ "key_size" >:: check_key_size
  ; "base_point" >:: check_base_point]

let test_x25519 _ = ()
let test_x448 _ = ()

let _ =
  "Library_Suite" >::: [ "x25519" >::: module_suite (module X25519)
                       ; "x448" >::: module_suite (module X448)]
  |> run_test_tt_main
