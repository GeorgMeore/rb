(*
 * This is a red-black tree implementation based on the articles:
 * "Red-Black Trees in a Functional Setting" by Chris Okasaki
 * "Deletion: The curse of the red-black tree" by Kimball Germane and Matthew Might.
 *)

type color = R | B | BB

type 'a node = E | T of 'a tree * 'a * 'a tree
 and 'a tree = color * 'a node

let fixRR (t : 'a tree) : 'a tree =
  match t with
  | (B, T((R, T(a, x, (R, T(b, y, c)))), z, d))
  | (B, T((R, T((R, T(a, x, b)), y, c)), z, d))
  | (B, T(a, x, (R, T((R, T(b, y, c)), z, d))))
  | (B, T(a, x, (R, T(b, y, (R, T(c, z, d))))))
    -> (R, T((B, T(a, x, b)), y, (B, T(c, z, d))))
  | (BB, T((R, T(a, x, (R, T(b, y, c)))), z, d))
  | (BB, T(a, x, (R, T((R, T(b, y, c)), z, d))))
    -> (B, T((B, T(a, x, b)), y, (B, T(c, z, d))))
  | _ -> t

let insert (x : 'a) (t : 'a tree) : 'a tree =
  let rec find_insert t =
    match t with
    | (_, E) -> (R, T((B, E), x, (B, E)))
    | (c, T(l, y, r)) ->
      if x = y then (c, T(l, x, r))
      else if x > y
             then fixRR (c, T(l, y, (find_insert r)))
             else fixRR (c, T((find_insert l), y, r))
  in let (_, n) = find_insert t in (B, n)

let rec member (x : 'a) (t : 'a tree) : bool =
  match t with
  | (_, E) -> false
  | (_, T(l, y, r)) -> x = y || member x l || member x r

let fixBB (t : 'a tree) : 'a tree =
  match t with
  | (R, T((BB, x), y, (B, T(c, z, d)))) -> fixRR (B, T((R, T((B, x), y, c)), z, d))
  | (R, T((B, T(a, x, b)), y, (BB, z))) -> fixRR (B, T(a, x, (R, T(b, y, (B, z)))))
  | (B, T((BB, x), y, (B, T(c, z, d)))) -> fixRR (BB, T((R, T((B, x), y, c)), z, d))
  | (B, T((B, T(a, x, b)), y, (BB, z))) -> fixRR (BB, T(a, x, (R, T(b, y, (B, z)))))
  | (B, T((BB, w), x, (R, T((B, T(c, y, d)), z, e)))) ->
    (B, T(fixRR (B, T((R, T((B, w), x, c)), y, d)), z, e))
  | (B, T((R, T(a, w, (B, T(b, x, c)))), y, (BB, z))) ->
    (B, T(a, w, fixRR (B, T(b, x, (R, T(c, y, (B, z)))))))
  | _ -> t

let remove (x : 'a) (t : 'a tree) : 'a tree =
  let rec min_remove t =
    match t with
    | (R, T((B, E), x, (B, E))) -> (x, (B, E))
    | (B, T((B, E), x, (B, E))) -> (x, (BB, E))
    | (B, T((B, E), x, (R, T(a, y, b)))) -> (x, (B, T(a, y, b)))
    | (c, T(l, x, r)) ->
      let min, l' = min_remove l in (min, fixBB (c, T(l', x, r)))
    | _ -> failwith "Impossible"
  in
  let rec find_remove t =
    match t with
    | (_, E) -> t
    | (R, T((B, E), y, (B, E))) when x = y -> (B, E)
    | (B, T((R, l), y, (B, E))) when x = y -> (B, l)
    | (B, T((B, E), y, (B, E))) when x = y -> (BB, E)
    | (c, T(l, y, r)) ->
      if x = y
        then let (m, r') = min_remove r in fixBB (c, T(l, m, r'))
        else if x > y
               then fixBB (c, T(l, y, find_remove r))
               else fixBB (c, T(find_remove l, y, r))
  in let (_, n) = find_remove t in (B, n)
