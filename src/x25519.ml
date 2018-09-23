

module Wire = struct

  let byte_size = Z.of_int 256

  let z_of_cstruct cs =
    let iter = Cstruct.iter (fun _ -> Some 1) (fun cs -> Cstruct.get_uint8 cs 0) cs in

    let rec aux acc shift = match iter () with
      | None -> acc
      | Some b ->
        let acc = Z.(acc + (~$b lsl shift)) in
        let shift = shift + 8 in
        aux acc shift
    in aux Z.zero 0

  let cstruct_of_z size z =
    let cs = Cstruct.create size in
    for i = 0 to size - 1 do
      let v = Z.rem (Z.(asr) z (8*i)) byte_size |> Z.to_int in
      Cstruct.set_uint8 cs i v
    done;
    cs

  let hex_of_cstruct cs =
    let n = Cstruct.len cs in
    let dst = Bytes.create (2 * n) in
    for i = 0 to n - 1 do
      let src = Format.sprintf "%02x" Cstruct.(get_uint8 cs i) in
      BytesLabels.blit_string ~src ~src_pos:0 ~dst ~dst_pos:(i*2) ~len:2
    done;
    (* This is ok because we created the string and will forget it after returning. *)
    Bytes.unsafe_to_string dst

  let cstruct_of_hex = Cstruct.of_hex

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

end

module X25519 = struct
  module C = Curve_25519.Curve_25519

  type pub = Public_key of C.point
  type priv = Private_key of Z.t


  (* TODO: clarify failure mode for points not on curve *)
  let pub_of_string: string -> pub = fun s ->
    let p = Wire.cstruct_of_hex s
            |> Wire.z_of_cstruct
            |> C.of_montgomery_u
    in Public_key p

  let string_of_pub: pub -> string = function Public_key pk ->
    C.to_montgomery_u pk
    |> Wire.cstruct_of_z 32
    |> Wire.hex_of_cstruct

  let priv_of_string: string -> priv = fun s ->
    let z = Wire.cstruct_of_hex s
            |> Wire.z_of_cstruct
            |> Wire.sanitize_scalar
    in Private_key z

  let x25519: priv -> pub -> pub = fun (Private_key sk) (Public_key pk) ->
    let pk = C.I.scale sk pk in
    Public_key pk

end
