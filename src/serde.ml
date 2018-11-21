let z_of_cstruct cs =
  let iter = Cstruct.iter (fun _ -> Some 1) (fun cs -> Cstruct.get_uint8 cs 0) cs in

  let rec aux acc shift = match iter () with
    | None -> acc
    | Some b ->
      let acc = Z.(acc + (~$b lsl shift)) in
      let shift = shift + 8 in
      aux acc shift
  in aux Z.zero 0

let cstruct_of_hex = Cstruct.of_hex

let z_of_hex hex = cstruct_of_hex hex |> z_of_cstruct

let hex_of_z n z =
  let num_hex = 2 * n in
  let upper_bound = n - 1 in
  let src = Z.format ("%0" ^ string_of_int num_hex ^ "x") z in
  let dst = Bytes.create num_hex in
  for i = 0 to upper_bound do
    BytesLabels.blit_string ~src ~src_pos:(2*i) ~dst ~dst_pos:(2*(upper_bound-i)) ~len:2
  done;
  (* This is ok because we created the string and will forget it after returning. *)
  Bytes.unsafe_to_string dst
