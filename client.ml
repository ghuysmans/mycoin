open Sexplib.Std
module I = struct
  type t = string [@@deriving sexp]
end
module M = Merkle.Make(I)
module B = Blockchain.Make(I)


let () =
  if Array.length Sys.argv = 3 && Sys.argv.(2) = "check" then
    let block = B.block_of_sexp (Sexplib.Sexp.input_sexp stdin) in
    let difficulty = int_of_string Sys.argv.(1) in
    B.verify difficulty block |> function
    | true -> print_endline "valid"
    | false -> print_endline "INVALID"
  else if Sys.argv.(1) = "--help" then
    prerr_endline @@ Sys.argv.(0) ^ " difficulty [check]"
  else
    let difficulty = int_of_string Sys.argv.(1) in
    let x = M.Node (M.Leaf "hello", M.Leaf "world") in
    let block = B.mine difficulty (Dgst.string "root") x in
    B.sexp_of_block block |> Sexplib.Sexp.output stdout
