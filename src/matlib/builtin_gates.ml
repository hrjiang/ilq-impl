open Owl
open Dense
open Matops
module C = Complex

let pi = 2.0 *. asin 1.0

open Matrix.Z

(* Single qubit gates *)
let x = of_array [|C.zero; C.one; C.one; C.zero|] 2 2

let y = of_array [|C.zero; C.conj C.i; C.i; C.zero|] 2 2

let z = of_array [|C.one; C.zero; C.zero; C.neg C.one|] 2 2

let h =
  of_array [|C.one; C.one; C.one; C.neg C.one|] 2 2
  /$ C.sqrt {C.re= 2.; C.im= 0.}

let t =
  of_array [|C.one; C.zero; C.zero; C.exp {C.re= 0.; C.im= pi /. 4.}|] 2 2

(* The gate for RUS example: (I + i√2X)/√3 *)
let v = (id + (C.sqrt {re= -2.; im= 0.} $* x)) /$ C.sqrt {re= 3.; im= 0.}

(* A function mapping gate names (strings) to the corresponding matrix. 
 * We may update the function to extend the gate-set. *)
let gates s =
  match s with
  | "X" -> Some x
  | "Y" -> Some y
  | "Z" -> Some z
  | "H" -> Some h
  | "T" -> Some t
  | "V" -> Some v
  | _   -> None

(* Basis of 2x2 matrices *)
(* |0><0| *)
let b0 = of_array [|C.one; C.zero; C.zero; C.zero|] 2 2

(* |0><1| *)
let b1 = of_array [|C.zero; C.one; C.zero; C.zero|] 2 2

(* |1><0| *)
let b2 = of_array [|C.zero; C.zero; C.one; C.zero|] 2 2

(* |1><1| *)
let b3 = of_array [|C.zero; C.zero; C.zero; C.one|] 2 2

(* Controlled gates *)
(* We build controlled gate ignoring the actural qubit order, then rearrange
   the matrix w.r.t. the qubit order *)
let controlled_gate g =
  let n, _ = shape g in
  kron b0 (eye n) + kron b3 g
