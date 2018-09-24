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