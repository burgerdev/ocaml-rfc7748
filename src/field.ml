
module type Raw_field = sig
  type t

  val eq: t -> t -> bool

  val zero: t
  val add: t -> t -> t
  val neg: t -> t

  val one: t
  val mul: t -> t -> t
  val inv: t -> t
end

(** Infix operations on fields. *)
module type Infix = sig
  type t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( / ): t -> t -> t
  val ( = ): t -> t -> bool
  val ( != ): t -> t -> bool
end

(** A mathematical field.

    https://en.wikipedia.org/wiki/Field_(mathematics)#Definition

    Axioms:
    - Associativity of addition: [Field.Infix.(a + (b + c) = (a + b) + c)]
    - Commutativity of addition: [Field.Infix.(a + b = b + a)]
    - Existence of neutral element for addition: [Field.(Infix.(a + zero = a))]
    - Existence of inverse element for addition: [Field.(Infix.(a + (neg a) = zero))]

    - Associativity of multiplication: [Field.Infix.(a * (b * c) = (a * b) * c)]
    - Commutativity of multiplication: [Field.Infix.(a * b = b * a)]
    - Existence of neutral element for multiplication: [Field.(Infix.(a * one = a))]
    - Existence of inverse element for multiplication: [Field.(Infix.(a * (inv a) = one))]

    - Distributivity: [F.eq (F.mul a (F.add x y)) (F.add (F.mul a x) (F.mul a y))]

    Rules (can be derived from axioms):
    - [Field.(Infix.(a * zero = zero))]
    - [Field.(Infix.(a * (neg one) = neg a))]
    - [Field.(Infix.((neg a) * b = neg (a * b)))]

*)
module type Field = sig
  include Raw_field
  module Infix: Infix with type t := t
end

module Infix(F: Raw_field): Infix with type t = F.t = struct
  include F
  let ( + ) = add
  let ( - ) m s = m + neg s
  let ( * ) = mul
  let ( / ) n d = n * inv d
  let ( = ) = eq
  let ( != ) x y = not (eq x y)
end

(** A generic mod-p field backed by [Z] (Zarith). *)
module Zp(S: sig val p: Z.t end): sig
  include Field

  val of_Z: Z.t -> t
  val to_Z: t -> Z.t
  val pp: Format.formatter -> t -> unit
end = struct

  module A = struct
    type t = Z.t

    let eq = Z.equal

    let zero = Z.zero
    let one = Z.one

    let add x y = Z.rem (Z.add x y) S.p

    let ( * ) x y = Z.rem (Z.mul x y) S.p
    let mul x y = x * y

    let rec neg = function
      | x when Z.lt x Z.zero -> Z.add x S.p |> neg
      | x -> Z.rem (Z.sub S.p (Z.rem x S.p)) S.p

    let inv x = Z.invert x S.p

  end

  include A

  let of_Z x = Z.(rem x S.p)

  let to_Z x = x

  let pp = Z.pp_print

  module Infix: Infix with type t := t = Infix(A)
end
