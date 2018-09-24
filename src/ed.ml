module type S = sig
  type private_key
  type public_key

  val public_key_of_string: string -> public_key
  val private_key_of_string: string -> private_key
  val public_key_of_private_key: private_key -> public_key

  val string_of_public_key: public_key -> string
  val string_of_private_key: private_key -> string

  val scale: private_key -> public_key -> public_key
end

module X25519: S = struct
  type private_key = Private_key of Z.t
  type public_key = Public_key of Z.t

  module A = struct
    type element = Z.t
    type integral = Z.t

    let p = Z.(one lsl 255 - ~$19)

    let bits = 255

    let a24 = Z.of_int 121665

    let constant_time_conditional_swap cond a b =
      (* TODO: this needs to be constant time *)
      if Z.(gt cond zero) then (b, a) else (a, b)
  end

  module C = Curve.Make(Zfield.Zp(A))(Z)(A)

  (* Quoth the RFC:
     set the three least significant bits of the first byte and the most significant bit
     of the last to zero, set the second most significant bit of the last byte to 1
  *)
  let sanitize_scalar =
    let unset_this = Z.logor Z.(~$7) (Z.shift_left Z.(~$128) (8*31)) in
    let set_that = Z.shift_left Z.(~$64) (8*31) in
    fun z ->
      Z.(z - (logand z unset_this))
      |> Z.logor set_that

  let public_key_of_string: string -> public_key = fun s ->
    let p = Serde.cstruct_of_hex s
            |> Serde.z_of_cstruct in
    let high = Z.(logand p (~$128 lsl 248)) in
    Public_key Z.(p - high)

  let string_of_public_key: public_key -> string = function Public_key pk ->
    pk
    |> Serde.cstruct_of_z 32
    |> Serde.hex_of_cstruct

  let private_key_of_string: string -> private_key = fun s ->
    let z = Serde.cstruct_of_hex s
            |> Serde.z_of_cstruct
            |> sanitize_scalar
    in Private_key z

  let string_of_private_key: private_key -> string = function Private_key pk ->
    pk
    |> Serde.cstruct_of_z 32
    |> Serde.hex_of_cstruct

  let scale (Private_key priv) (Public_key pub) = Public_key (C.scale priv pub)

  let public_key_of_private_key priv =
    scale priv (public_key_of_string "0900000000000000000000000000000000000000000000000000000000000000")
end

module X448: S = struct
  (* TODO: implement *)
  include X25519
end
