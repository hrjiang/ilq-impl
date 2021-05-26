open Owl
open Dense.Matrix.Z
module C = Complex

val zero : mat -> mat
(** Maps arbitrary matrix to 0-matrix with the same dimension. *)

val id : mat
(** Identity matrix of dim 2. *)

val ( ^^ ) : mat -> int -> mat
(** [kron m n]: Kron matrix [m] for [n] times, if n <= 0 return 1. *)

val rearrange_local : int -> int -> mat -> int list -> mat
(** [rearrange_local m n g permut]: Rearrange the matrix of a [n]-qubit gate
    [g] w.r.t a permutated qubit order [permut] over the entire [m]-qubit
    system. E.g., if we apply X over qubit number 1, on top of a system
    consisting of qubits [\[0;1;2\]], then we get the corresponding operator
    over the entire 3-qubit system by [rearrange_local 3 1 X \[1\]@\[0;2\]] *)

val apply : mat -> mat -> mat
(** Applying an operator O on rho, i.e., O \rho O^dagger *)

val supp : mat -> mat
(** Support of a projection, an eigen value < rounding_error_bound is treated
    as zero*)

val orth : mat -> mat
(** Orthogonal projection of P, i.e., I - P *)

val vee : mat -> mat -> mat
(** Logical or of P1 and P2, i.e., P1 \/ P2 = supp(P1 + (I -
    P1)P2(I-P1)^dagger) *)
