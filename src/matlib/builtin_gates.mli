open Owl
open Dense
open Matrix.Z
module C = Complex

val pi : float

(* Single qubit gates *)
val x : mat

val y : mat

val z : mat

val h : mat

val t : mat

(* The gate for RUS example: (I + i√2X)/√3 *)
val v : mat

(* A function mapping gate names (strings) to the corresponding matrix. 
 * We may update the function to extend the gate-set. *)
val gates : string -> mat option

(* Basis of 2x2 matrices *)
(* |0><0| *)
val b0 : mat

(* |0><1| *)
val b1 : mat

(* |1><0| *)
val b2 : mat

(* |1><1| *)
val b3 : mat

(* Controlled gates *)
val controlled_gate : mat -> mat
