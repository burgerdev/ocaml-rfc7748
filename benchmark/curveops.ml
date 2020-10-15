
module type DH = sig
  type private_key
  type public_key

  val private_key_of_bytes: Bytes.t -> private_key
  val public_key_of_private_key: private_key -> public_key
  val scale: private_key -> public_key -> public_key
end

let rand () =
  Bytes.init 32 @@ fun _ -> char_of_int @@ Random.int 256 

let run: (module DH) -> unit -> unit = fun m ->
  let module M = (val m) in
  Random.init 4711;
  let pub = rand () |> M.private_key_of_bytes |> M.public_key_of_private_key in
  let priv = rand () |> M.private_key_of_bytes in
  fun () -> M.scale priv pub |> Sys.opaque_identity |> ignore

let curveops = [ "Rfc7748", run (module Rfc7748.X25519), ()
               ]

let _ =
  Benchmark.throughputN 1 curveops |> Benchmark.tabulate

