
module Zp(P: sig val p: Z.t end): Curve.Field with type t = Z.t = struct
  type t = Z.t

  include P

  let zero = Z.zero
  let one = Z.one

  let ( + ) x y = Z.rem (Z.add x y) p
  let ( * ) x y = Z.rem (Z.mul x y) p

  let double x = x + x
  let square x = x * x

  let rec negate = function
    | x when Z.lt x Z.zero -> Z.add x p |> negate
    | x -> Z.rem (Z.sub p (Z.rem x p)) p

  let invert x = Z.invert x p

  let ( / ) x y = x * invert y
  let ( - ) x y = x + negate y
end
