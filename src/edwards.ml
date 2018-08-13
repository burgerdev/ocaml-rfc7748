
module type Edwards = sig
  module F: Field.Field

  type point

  val zero: point
  val of_pair: F.t * F.t -> point option
  val to_pair: point -> F.t * F.t

  val eq: point -> point -> bool
  val on_curve: point -> bool

  val add: point -> point -> point
  val scale: Z.t -> point -> point

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

    NOTE: The scalar [d] must not be a square in F, otherwise the addition formula
          will not work.
    https://en.wikipedia.org/wiki/Euler%27s_criterion

    The curve points form an algebraic group, with the neutral element (0, 1)
    and a special addition formula (see Wikipedia article).
*)
module Edwards(F: Field.Field)(S: sig val d: F.t end): Edwards with module F = F = struct
  (* TODO: maybe require 'proof' that d is not square. *)
  module F = F
  type point = F.t * F.t

  let on_curve: point -> bool = function (x, y) ->
    let open F.Infix in
    let xx = x * x in
    let yy = y * y in
    let lhs = xx + yy in
    let rhs =
      let xxyy = xx * yy in
      S.d * xxyy + F.one
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

  let double_and_add: Z.t -> point -> point =
    let two = Z.of_int 2 in
    let even n = Z.(rem n two = zero) in
    let half_even n = Z.(n / two) in
    let half_odd n = Z.((n - one) / two) in
    fun n p ->
      let rec aux acc p = function
        | n when n = Z.zero -> acc
        | n when n = Z.one -> add acc p
        | n when even n -> aux acc (add p p) (half_even n)
        | n (* when odd n *) -> aux (add acc p) (add p p) (half_odd n)
      in aux zero p n

  let scale: Z.t -> point -> point = double_and_add

  module Infix = struct
    let ( + ) = add
    let ( = ) = eq
  end
end

(** Twisted Edwards curve over a generic field.

    https://en.wikipedia.org/wiki/Twisted_Edwards_curve

    The set of points in the curve is given by
      C = { (x, y) | x in F, y in F, a*x^2 + y^2 = 1 + d*x^2*y^2 },
    with parameters a and d in F.

    NOTE: [a] must be a square in [F], [d] must not be a square in [F].
    https://en.wikipedia.org/wiki/Euler%27s_criterion

    The curve points form an algebraic group, with the neutral element (0, 1)
    and a special addition formula (see Wikipedia article).
*)
module Twisted_edwards(F: Field.Field)(S: sig val d: F.t  val a: F.t end): Edwards with module F = F = struct
  (* TODO: maybe require 'proof' that a is square and d is not. *)
  module F = F
  type point = F.t * F.t

  let on_curve: point -> bool = function (x, y) ->
    let open F.Infix in
    let xx = x * x in
    let yy = y * y in
    let lhs = S.a * xx + yy in
    let rhs =
      let xxyy = xx * yy in
      S.d * xxyy + F.one
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
    let y3 = y1y2 + F.neg (S.a * x1x2) in
    (x3 * (F.one + k |> F.inv), y3 * (F.one + (F.neg k) |> F.inv))

  let add: point -> point -> point = naive_add

  let zero: point = (F.zero, F.one)
  let eq: point -> point -> bool = fun (x, y) (x', y') ->
    F.Infix.(x = x' && y = y')

  let double_and_add: Z.t -> point -> point =
    let two = Z.of_int 2 in
    let even n = Z.(rem n two = zero) in
    let half_even n = Z.(n / two) in
    let half_odd n = Z.((n - one) / two) in
    fun n p ->
      let rec aux acc p = function
        | n when n = Z.zero -> acc
        | n when n = Z.one -> add acc p
        | n when even n -> aux acc (add p p) (half_even n)
        | n (* when odd n *) -> aux (add acc p) (add p p) (half_odd n)
      in aux zero p n

  let scale: Z.t -> point -> point = double_and_add

  module Infix = struct
    let ( + ) = add
    let ( = ) = eq
  end
end
