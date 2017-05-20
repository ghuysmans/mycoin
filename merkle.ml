module type Item = sig
  type t [@@deriving sexp]
end

module Make(I: Item) = struct
  type t =
    | Leaf of I.t
    | Node of t * t
    [@@deriving sexp]

  let string_of_item s =
    I.sexp_of_t s |> Sexplib.Conv.string_of_sexp

  let rec hash = function
    | Leaf s ->
      string_of_item s |> Dgst.string
    | Node (l, r) ->
      Dgst.to_hex (hash l) ^ Dgst.to_hex (hash r) |> Dgst.string

  let rec print ?(indent="") t =
    match t with
    | Leaf s ->
      indent ^ string_of_item s ^ ": " ^ Dgst.to_hex (hash t) |> print_endline
    | Node (l, r) ->
      indent ^ Dgst.to_hex (hash t) |> print_endline;
      let indent = indent ^ "  " in
      print ~indent l;
      print ~indent r
end

(*
let () =
  let x = M.Node (M.Node (M.Leaf "hello", M.Leaf "cruel"), M.Leaf "world") in
  M.sexp_of_t x |> Sexplib.Sexp.to_string |> print_endline;
  let x = M.t_of_sexp (Sexplib.Sexp.of_string "(Node (Leaf h) (Leaf c))") in
  M.print x
*)
