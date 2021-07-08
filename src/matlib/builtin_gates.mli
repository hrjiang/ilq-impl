open Owl
open Dense
open Matrix.Z
module C = Complex

val pi : float

(** Single qubit gates *)

val x : mat

val y : mat

val z : mat

val h : mat

val s : mat

val t : mat

val v : mat
(** The gate for RUS example: (I + i√2X)/√3 *)

val v3 : mat
(** The gate for RUS v3 example: (I + 2iZ)/√5 *)

val gates : string -> mat option
(** Map gate names (strings) to the corresponding matrix. We may update the
    function to extend the gate-set. *)

(* Basis of 2x2 matrices *)
(* |0><0| *)
val b0 : mat

(* |0><1| *)
val b1 : mat

(* |1><0| *)
val b2 : mat

(* |1><1| *)
val b3 : mat

(** Controlled gate *)

val controlled_gate : mat -> mat
(** [controlled_gate g]: construct controlled-g using a single qubit as
    control. I.e., I \oplus g *)
