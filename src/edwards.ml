
module type Edwards = sig
  module F: Field.Field

  type point

  val zero: point
  val of_pair: F.t * F.t -> point option
  val to_pair: point -> F.t * F.t

  val eq: point -> point -> bool
  val on_curve: point -> bool

  val add: point -> point -> point
  val scale: int -> point -> point

  module Infix: sig
    val ( + ): point -> point -> point
    val ( = ): point -> point -> bool
  end
end

(** Edward's curve over a generic field.

    https://en.wikipedia.org/wiki/Edwards_curve

    The set of points in the curve is given by
      C = { (x, y) | x in F, y in F, x^2 + y^2 = 1 + d*x^2*y^2 },
    with parameter d in F.

    The curve points form an algebraic group, with the neutral element (0, 1)
    and a special addition formula (see Wikipedia article).
*)
module Edwards(F: Field.Field)(S: sig val d: F.t end): Edwards with module F = F = struct
  module F = F
  type point = F.t * F.t

  let on_curve: point -> bool = function (x, y) ->
    let open F.Infix in
    let xx = x * x in
    let yy = y * y in
    let lhs = xx + yy in
    let rhs =
      let xxyy = xx * yy in
      xxyy + xxyy + F.one
    in
    lhs = rhs

  (* TODO: do we need an [of_pair_unsafe]? *)
  let of_pair: F.t * F.t -> point option = fun p ->
    if on_curve p then Some p else None

  let to_pair: point -> F.t * F.t = fun p -> p

  let naive_add (x1, y1) (x2, y2) =
    let open F.Infix in
    let y1y2 = y1 * y2 in
    let x1x2 = x1 * x2 in
    let k = S.d * x1x2 * y1y2 in
    let x3 = x1 * y2 + x2 * y1 in
    let y3 = y1y2 + F.neg x1x2  in
    (x3 * (F.one + k |> F.inv), y3 * (F.one + (F.neg k) |> F.inv))

  let add: point -> point -> point = naive_add

  let zero: point = (F.zero, F.one)
  let eq: point -> point -> bool = fun (x, y) (x', y') ->
    F.Infix.(x = x' && y = y')

  let double_and_add n p =
    let rec aux acc p = function
      | n when n = 0 -> acc
      | n when n = 1 -> add acc p
      | n when n mod 2 = 0 -> aux acc (add p p) (n / 2)
      | n -> aux (add acc p) (add p p) ((n - 1) / 2)
    in aux zero p n

  let scale: int -> point -> point = double_and_add

  module Infix = struct
    let ( + ) = add
    let ( = ) = eq
  end
end
