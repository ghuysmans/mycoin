open Sexplib.Std

module type Item = sig
  type t [@@deriving sexp]
end

module Make(I: Item) = struct
  module M = Merkle.Make(I)
  type header = {
    prev: Dgst.t;
    nonce: int;
    tree_hash: Dgst.t
  } [@@deriving sexp]

  let print_header {prev; nonce; tree_hash} =
    "prev: " ^ Dgst.to_hex prev |> print_endline;
    "nonce: " ^ string_of_int nonce |> print_endline;
    "tree_hash: " ^ Dgst.to_hex tree_hash |> print_endline

  let test difficulty hash =
    let expected = String.make difficulty '0' in
    String.sub (Dgst.to_hex hash) 0 difficulty = expected

  type block = {
    difficulty: int;
    header: header;
    tree: M.t
  } [@@deriving sexp]

  let mine difficulty prev tree =
    let tree_hash = M.hash tree in
    let fixed = Dgst.to_hex prev ^ Dgst.to_hex tree_hash in
    let rec f nonce =
      if fixed ^ string_of_int nonce |> Dgst.string |> test difficulty then
        let header = {prev; nonce; tree_hash} in
        {difficulty; header; tree}
      else
        f (nonce + 1) in
    f 0

  let hash {prev; nonce; tree_hash} =
    Dgst.to_hex prev ^ Dgst.to_hex tree_hash ^ string_of_int nonce |>
    Dgst.string

  let print_block {header; tree} =
    print_header header;
    print_endline "tree:";
    M.print ~indent:"  " tree;
    "hash: " ^ Dgst.to_hex (hash header) |> print_endline

  let verify difficulty block =
    block.difficulty >= difficulty &&
    M.hash block.tree = block.header.tree_hash &&
    hash block.header |> test difficulty

  (* TODO verify_chain using List.fold_left... *)
end
