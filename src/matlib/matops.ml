open Owl
open Dense
open Matrix.Z
module C = Complex

let rounding_error_bound = 0.0000000001

let zero p = C.zero $* p

let id = eye 2

let rec ( ^^ ) m n =
  if Int.equal (compare n 0) 1 (* n > 0 *) then kron m (m ^^ pred n)
  else of_array [|C.one|] 1 1

let padding m g q n =
  (* if m >= (n + q) && n > 0 && q > 0 then *)
  kron (id ^^ q) (kron g (id ^^ Int.sub (Int.sub m q) n))

let rearrange m g_glob permut =
  let invp = Utils.Array.argsort permut in
  let permut' =
    Array.append invp (Array.map (fun index -> Int.add index m) invp)
  in
  ( Ndarray.Z.reshape g_glob (Array.make (Int.mul m 2) 2)
  |> Ndarray.Z.transpose ~axis:permut'
  |> Ndarray.Z.reshape )
    [|Int.shift_left 1 m; Int.shift_left 1 m|]

let rearrange_local m n op_mat permut =
  let op_mat_glob = padding m op_mat 0 n in
  rearrange m op_mat_glob (Utils.Array.of_list permut)

let apply op rho = op *@ rho *@ ctranspose op

let seq_list n = 
  let rec seq_list_rev n =
    if Int.equal (compare 0 n) 1 then [] else n :: seq_list_rev (pred n)
  in List.rev (seq_list_rev n)

let supp m =
  let eigvecs, eigvals = Linalg.Z.eig m in
  let nonzeros =
    List.filter
      (fun i ->
        Int.equal
          (Float.compare (C.norm2 (get eigvals 0 i)) rounding_error_bound)
          1 )
      (seq_list (pred (col_num eigvals)))
  in
  if Int.equal (List.length nonzeros) 0 then zero m
  else
    let v = get_fancy [R []; L nonzeros] eigvecs in
    v *@ ctranspose v

let orth p =
  let n, _ = Matrix.Z.shape p in
  let idn = Matrix.Z.eye n in
  Matrix.Z.(idn - p)

let vee p1 p2 = supp Matrix.Z.(p1 + supp (orth p1 *@ p2 *@ conj (orth p1)))
