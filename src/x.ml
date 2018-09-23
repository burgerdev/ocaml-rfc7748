
module Wire = X25519.Wire

module F = struct
  let p = Curve_25519.Constants.p
  include Curve_25519.Field_25519

  let positive c =
    Z.(gt (to_Z c) zero)

  let cswap c a b =
    (* TODO unsafe (not constant time)! *)
    if positive c then (b, a) else (a, b)

  let two = Z.of_int 2 |> of_Z
  let a24 = Z.of_int 121665 |> of_Z

  let bit z n = Z.((Z.(asr) z n) mod ~$2) |> of_Z

  let (^^) a b = Z.logxor (to_Z a) (to_Z b) |> of_Z

  let sq a = mul a a

  let (+) a b = add a b
  let (-) a b = neg b |> add a
  let ( * ) a b = mul a b
end

module X25519: sig

  type pub = Public_key of Z.t
  type priv = Private_key of Z.t

  val pub_of_string: string -> pub
  val string_of_pub: pub -> string
  val priv_of_string: string -> priv
  val string_of_priv: priv -> string

  val x25519: priv -> pub -> pub

end = struct

  type pub = Public_key of Z.t
  type priv = Private_key of Z.t

  let pub_of_string: string -> pub = fun s ->
    let p = Wire.cstruct_of_hex s
            |> Wire.z_of_cstruct in
    let high = Z.(logand p (~$128 lsl 248)) in
    Public_key Z.(p - high)

  let string_of_pub: pub -> string = function Public_key pk ->
    pk
    |> Wire.cstruct_of_z 32
    |> Wire.hex_of_cstruct

  let priv_of_string: string -> priv = fun s ->
    let z = Wire.cstruct_of_hex s
            |> Wire.z_of_cstruct
            |> Wire.sanitize_scalar
    in Private_key z

  let string_of_priv: priv -> string = function Private_key pk ->
    pk
    |> Wire.cstruct_of_z 32
    |> Wire.hex_of_cstruct



  let x25519: priv -> pub -> pub = fun (Private_key k) (Public_key u) ->
    let open F in
    let rec aux x1 x2 x3 z2 z3 swap = function
      | t when t < 0 ->
        let (x2, _) = cswap swap x2 x3 in
        let (z2, _) = cswap swap z2 z3 in
        x2 * (inv z2)
      | t ->
        let kt = bit k t in
        let swap = swap ^^ kt in
        let (x2, x3) = cswap swap x2 x3 in
        let (z2, z3) = cswap swap z2 z3 in
        let swap = kt in

        let a = x2 + z2 in
        let aa = sq a in
        let b = x2 - z2 in
        let bb = sq b in
        let e = aa - bb in
        let c = x3 + z3 in
        let d = x3 - z3 in
        let da = d * a in
        let cb = c * b in

        let x3 = sq (da + cb) in
        let z3 = x1 * sq (da - cb) in
        let x2 = aa * bb in
        let z2 = e * (aa + a24 * e) in

        aux x1 x2 x3 z2 z3 swap Pervasives.(t - 1)
    in

    let x1 = of_Z u in
    let x2 = one in
    let z2 = zero in
    let x3 = of_Z u in
    let z3 = one in
    let swap = zero in
    let foo = aux x1 x2 x3 z2 z3 swap 254 in
    Public_key (to_Z foo)
end
