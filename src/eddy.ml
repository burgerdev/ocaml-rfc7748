module type S = sig
  type private_key
  type public_key

  val public_key_of_string: string -> public_key
  val private_key_of_string: string -> private_key

  val string_of_public_key: public_key -> string
  val string_of_private_key: private_key -> string

  val base: public_key
  val scale: private_key -> public_key -> public_key
end

module X25519: S = struct
  open X
  type private_key = X25519.priv
  type public_key = X25519.pub

  let public_key_of_string s = X25519.pub_of_string s
  let private_key_of_string s = X25519.priv_of_string s

  let string_of_public_key pub = X25519.string_of_pub pub
  let string_of_private_key priv = X25519.string_of_priv priv

  let base = X25519.pub_of_string "0900000000000000000000000000000000000000000000000000000000000000"
  let scale priv pub = X25519.x25519 priv pub
end

module X448: S = struct
  include X25519
end
