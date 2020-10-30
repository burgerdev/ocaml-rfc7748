
let z_of_bytes buf = Bytes.unsafe_to_string buf |> Z.of_bits

let bytes_of_hex hex =
  let n = String.length hex / 2 in
  let buf = Bytes.create n in
  let ic = Scanf.Scanning.from_string hex in
  for i = 0 to (n - 1) do
    Bytes.set buf i @@ Scanf.bscanf ic "%02x" char_of_int
  done;
  buf

let z_of_hex hex = bytes_of_hex hex |> z_of_bytes

let bytes_of_z n z =
  let buf = Bytes.create n in
  let zbuf = Z.to_bits z in
  Bytes.blit_string zbuf 0 buf 0 String.(length zbuf);
  buf

let hex = function
  | n when n >= 0 && n < 10 -> char_of_int (int_of_char '0' + n)
  | n when n >= 10 && n < 16 -> char_of_int (int_of_char 'a' + n - 10)
  | _ -> assert false

let hex_of_bytes buf = 
  let n = Bytes.length buf in
  let dst = Bytes.create (2*n) in
  for i = 0 to (n - 1) do
    let c = Bytes.get buf i |> int_of_char in
    Bytes.set dst (2*i) @@ hex (c / 16);
    Bytes.set dst (2*i + 1) @@ hex (c mod 16)
  done;
  Bytes.unsafe_to_string dst


let hex_of_z n z =
  let num_hex = 2 * n in
  let upper_bound = n - 1 in
  let src = Z.format ("%0" ^ string_of_int num_hex ^ "x") z in
  let dst = Bytes.create num_hex in
  for i = 0 to upper_bound do
    Bytes.blit_string src (2*i) dst (2*(upper_bound-i)) 2
  done;
  (* This is ok because we created the string and will forget it after returning. *)
  Bytes.unsafe_to_string dst
