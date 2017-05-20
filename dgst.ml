open Sexplib.Std

(* FIXME is it even useful to wrap this up? *)
type t = Digest of string [@@deriving sexp]

let sexp_of_t (Digest d) = sexp_of_t (Digest (Digest.to_hex d))
let t_of_sexp x = t_of_sexp x |> function Digest s -> Digest (Digest.from_hex s)

let to_hex (Digest d) = Digest.to_hex d
let string s = Digest (Digest.string s)

(*
let () =
  Dgst.sexp_of_t (Dgst.string "oh") |> Sexplib.Sexp.to_string |> print_endline;
  let oh_d = "(Digest c7218260ef2b966ab0454e07c55cf4e9)") in
  let x = Dgst.t_of_sexp (Sexplib.Sexp.of_string oh_d) in
  Dgst.to_hex x |> print_endline
*)
