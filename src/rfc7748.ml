module type DH = sig
  type private_key
  type public_key

  val key_size: int

  val base: public_key

  val public_key_of_string: string -> public_key
  val private_key_of_string: string -> private_key

  val string_of_public_key: public_key -> string
  val string_of_private_key: private_key -> string

  val public_key_of_bytes: Bytes.t -> public_key
  val private_key_of_bytes: Bytes.t -> private_key

  val bytes_of_public_key: public_key -> Bytes.t
  val bytes_of_private_key: private_key -> Bytes.t

  val scale: private_key -> public_key -> public_key
  val public_key_of_private_key: private_key -> public_key
end

module X25519: DH = struct
  open Hacl_star

  type public_key = Public_key of Hacl.C.t
  type private_key = Private_key of Hacl.C.t

  let key_size = 32

  let public_key_of_string s = Public_key (Serde.bytes_of_hex s)
  let public_key_of_bytes buf =
    assert (Bytes.length buf = key_size);
    Public_key (buf)
  let private_key_of_string s = Private_key (Serde.bytes_of_hex s)
  let private_key_of_bytes buf =
    assert (Bytes.length buf = key_size);
    Private_key (buf)

  let base = public_key_of_string "0900000000000000000000000000000000000000000000000000000000000000"

  let string_of_public_key (Public_key k) =
    Serde.hex_of_bytes k
  let string_of_private_key (Private_key k) =
    Serde.hex_of_bytes k
  let bytes_of_public_key (Public_key k) = k
  let bytes_of_private_key (Private_key k) = k


  let scale (Private_key priv) (Public_key pub) =
    let out = Bytes.make 32 '\x00' in 
    if EverCrypt.Curve25519.ecdh out priv pub then
      Public_key out
    else
      failwith "arrg"
  let public_key_of_private_key (Private_key priv) =
    let out = Bytes.make 32 '\x00' in 
    EverCrypt.Curve25519.secret_to_public out priv;
    Public_key out

end

let x25519 ~priv ~pub =
  X25519.(
    scale (private_key_of_string priv) (public_key_of_string pub)
    |> string_of_public_key
  )

module X448: DH = struct
  type private_key = Private_key of Z.t
  type public_key = Public_key of Z.t

  let key_size = 56

  module A = struct
    type element = Z.t
    type integral = Z.t

    let p = Z.(one lsl 448 - one lsl 224 - ~$1)

    let bits = 448

    let a24 = Z.of_int 39081

    let two = Z.(~$2)

    let constant_time_conditional_swap cond a b =
      let c = Z.(rem cond two) in
      let c' = Z.(one - c) in
      let a' = Z.(c'*a + c*b) in
      let b' = Z.(c'*b + c*a) in
      a', b'
  end

  module C = Curve.Make(Zfield.Zp(A))(Z)(A)

  (* Quoth the RFC:
     set the two least significant bits of the first byte to 0, and the most
     significant bit of the last byte to 1.
  *)
  let sanitize_scalar =
    let unset_this = Z.(~$3) in
    let set_that = Z.shift_left Z.(~$128) (8*55) in
    fun z ->
      Z.(z - (logand z unset_this))
      |> Z.logor set_that

  let public_key_of_string: string -> public_key = fun s ->
    let p = Serde.z_of_hex s in
    Public_key p

  let public_key_of_bytes buf =
    let p = Serde.z_of_bytes buf in
    Public_key p

  let string_of_public_key: public_key -> string = function Public_key pk ->
    Serde.hex_of_z key_size pk

  let bytes_of_public_key = function Public_key pk ->
    Serde.bytes_of_z key_size pk

  let private_key_of_string: string -> private_key = fun s ->
    let z = Serde.z_of_hex s |> sanitize_scalar in
    Private_key z

  let private_key_of_bytes buf =
    let z = Serde.z_of_bytes buf |> sanitize_scalar in
    Private_key z

  let string_of_private_key: private_key -> string = function Private_key pk ->
    Serde.hex_of_z key_size pk

  let bytes_of_private_key = function Private_key pk ->
    Serde.bytes_of_z key_size pk

  let scale (Private_key priv) (Public_key pub) = Public_key (C.scale priv pub)

  let base = Public_key (Z.of_int 5)

  let public_key_of_private_key priv = scale priv base
end

let x448 ~priv ~pub =
  X448.(
    scale (private_key_of_string priv) (public_key_of_string pub)
    |> string_of_public_key
  )
