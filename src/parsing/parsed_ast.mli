(** The parsed AST of qwhile + assertion *)

(* Square matrices are used for defining assertions, i.e., projections. These
   matrices are stored as array of complex numbers, along with the dimension. *)

type parsed_matrix = Pmat of int * Complex.t list

(* Projections in assertsions must be defined and bind with an identifier at
   the beginning of a program. *)

type proj_defn = Proj of string * parsed_matrix

(* Qubit identifiers are parsed simply as strings *)

type qvar = string

(* We implement a restricted gate set, consisting of single qubit gates, and
   controlled gates *)

type gate = Gate of string | Controlled of gate

(* We allow only one-qubit measurements on computational basis *)

type comm =
  | Skip
  | Assert  of qvar list * string
  | Init    of qvar
  | Unitary of gate * qvar list
  | Seq     of comm * comm
  | If      of qvar * comm * comm
  | While   of qvar * comm

type program = Prog of proj_defn list * comm
