
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

module X25519: S

module X448: S
