
module Constants = struct
  let p = Z.sub (Z.pow (Z.of_int 2) 255) (Z.of_int 19)

  let euler_exp = Z.((p - ~$1) / ~$2)
  let euler_exp_half = Z.(euler_exp / ~$2)

  let legendre_exp = Z.((p + ~$3) / ~$8)
end

module Field_25519 = struct
  open Constants

  include Field.Zp(struct let p = p end)

  let is_quadratic_residue: t -> bool = fun n ->
    Infix.(Z.powm (to_Z n) euler_exp p |> of_Z = one)

  let is_quartic_residue: t -> bool = fun n ->
    Infix.(Z.powm (to_Z n) euler_exp_half p |> of_Z = one)

  (* Legendre (p == 5 mod 8)
     https://en.wikipedia.org/wiki/Quadratic_residue#Prime_or_prime_power_modulus

     The result is only valid if [n] is a quadratic residue.
  *)
  let sqrt n =
    let sqrt_n =
      if is_quartic_residue n then
        Z.powm (to_Z n) legendre_exp p
      else
        Z.(powm (to_Z n) legendre_exp p * powm ~$2 euler_exp_half p)
    in of_Z sqrt_n
end

(** Parameters for constructing curve 25519 and converting it to a twisted
    Edwards curve.

    Curve25519 is of shape
      b*v^2 = u^3 + a*u^2 + u,
    the birationally equivalent Edwards curve is of shape
      c*x^2 + y^2 = 1 + d*x^2*y^2.

    Refer to below sources for details on the parameters.

    - Parameters of Curve25519: https://en.wikipedia.org/wiki/Curve25519#Mathematical_properties
    - Conversion to Edwards curve: https://en.wikipedia.org/wiki/Montgomery_curve#Equivalence_with_twisted_Edwards_curves
    - Derived conversion factors: https://en.wikipedia.org/wiki/EdDSA#Ed25519

*)
module Params_25519 = struct

  let (~$) x = Z.of_int x |> Field_25519.of_Z

  (* Montgomery parameters *)

  let a = ~$ 486662
  let b = Field_25519.one

  (* Edwards parameters. [c] is chosen as -1 by scaling [d] accordingly. *)

  let c = Field_25519.(neg one)
  let d =
    let num = Field_25519.Infix.(a - ~$2) in
    let denom = Field_25519.Infix.(a + ~$2) in
    Field_25519.(neg Infix.(num / denom))

  let conversion_factor =
    Field_25519.(neg @@ Infix.(a + ~$2)) |> Field_25519.sqrt
end

module Curve_25519 = struct
  module I = Edwards.Twisted_edwards(Field_25519)(struct let a = Params_25519.c let d = Params_25519.d end)

  type point = I.point

  (* let of_montgomery: Z.t * Z.t -> point =
     fun p ->
      let p = match p with
        | (u, v) when Z.(v = zero) ->
          (Field_25519.zero, Field_25519.one)
        | (u, v) ->
          let open Field_25519 in
          let open Infix in
          let u = of_Z u in
          let v = of_Z v in
          (u * Params_25519.conversion_factor / v, (u - one) / (u + one))
      in
      match I.of_pair p with
      | Some x -> x
      | None ->
        let (u, v) = p in
        failwith @@ Format.asprintf "not a point on the twisted Edwards curve: (%a, %a)" Field_25519.pp u Field_25519.pp v *)

  let of_montgomery_u: Z.t -> point = fun u ->
    let open Field_25519 in
    let open Infix in
    let u = of_Z u in
    let u2 = u * u in
    let u3 = u2 * u in
    let p = match u3 + Params_25519.a * u2 + u with
      | z when Field_25519.Infix.(z = zero) -> (Field_25519.zero, Field_25519.one)
      | rhs when is_quadratic_residue rhs ->
        (* TODO check whether failing is ok or if we should continue *)
        (u * Params_25519.conversion_factor / sqrt rhs, (u - one) / (u + one))
      | _ ->
        failwith (Format.asprintf "not a compressed curve-point: %a!" pp u)
    in match I.of_pair p with
    | Some x -> x
    | None ->
      let (u, v) = p in
      failwith @@ Format.asprintf "not a point on the twisted Edwards curve: (%a, %a)" Field_25519.pp u Field_25519.pp v


  let to_montgomery_u: point -> Z.t = fun p ->
    let open Field_25519 in
    let open Infix in
    match I.to_pair p |> snd with
    | y when y = one -> Z.zero
    | y -> (one + y) / (one - y) |> to_Z
end
