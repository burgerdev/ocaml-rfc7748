open OUnit

module F = Ed.Curve_25519.Field_25519

let assert_equal_F x y =
  let msg = Fmt.strf "%a != %a" F.pp x F.pp y in
  msg @? F.Infix.(x = y)

let assert_equal_Z x y =
  let msg = Fmt.strf "%a != %a" Z.pp_print x Z.pp_print y in
  msg @? (x = y)

let test_field_axioms _ =
  let open F in
  let open Infix in
  let two = one + one in
  let three = one + two in
  let four = one + three in
  "zero is neutral element for addition" @? (zero + one = one);
  "one is neutral element for multiplication" @? (one * one = one);
  "addition is associative" @? ((one + two) + three = one + (two + three));
  "addition is commutative" @? (one + two = two + one);
  "multiplication is associative" @? ((four + two) + three = four + (two + three));
  "multiplication is commutative" @? (three + two = two + three);
  "double negation cancels" @? (neg two |> neg = two);
  "negation works" @? (neg two + two = zero);
  "double inversion cancels" @? (inv two |> inv = two);
  "negation works" @? (inv two * two = one)

let test_residues _ =
  let two = Z.of_int 2 |> F.of_Z in
  let four = Z.of_int 4 |> F.of_Z in
  let five = Z.of_int 5 |> F.of_Z in
  "2 is not a quadratic residue" @? not @@ F.is_quadratic_residue two;
  "2 is not a quartic residue" @? not @@ F.is_quartic_residue two;
  "4 is a quadratic residue" @? F.is_quadratic_residue four;
  "4 is not a quartic residue" @? not @@ F.is_quartic_residue four;
  "5 is a quadratic residue" @? F.is_quadratic_residue five;
  "5 is a quartic residue" @? F.is_quartic_residue five;
  let sqrt_five = F.sqrt five in
  let sqrt_sqrt_five = F.sqrt sqrt_five in
  assert_equal_F sqrt_five F.Infix.(sqrt_sqrt_five * sqrt_sqrt_five);
  assert_equal_F five F.Infix.(sqrt_sqrt_five * sqrt_sqrt_five * sqrt_sqrt_five * sqrt_sqrt_five)

let test_twisted_edwards_parameters _ =
  (* Parameters from https://en.wikipedia.org/wiki/EdDSA#Ed25519 *)
  "a=-1 for twisted Edwards curve definition is a quadratic residue" @?
  F.(is_quadratic_residue @@ neg one);
  "d=-121665/121666 for twisted Edwards curve definition is not a quadratic residue" @?
  begin
    let num = Z.of_int 121665 |> F.of_Z in
    let denom = Z.of_int 121666 |> F.of_Z in
    not F.(is_quadratic_residue @@ neg Infix.(num / denom))
  end;
  "Montgomery conversion factor can be computed" @?
  begin
    let c = Z.of_int 486664 |> F.of_Z |> F.neg in
    F.is_quadratic_residue c
  end


let suite =
  "Field_25519 suite" >::: [ "field_axioms" >:: test_field_axioms
                           ; "residues" >:: test_residues
                           ; "twisted_edwards_parameters" >:: test_twisted_edwards_parameters
                           ]

let _ =
  run_test_tt_main suite
