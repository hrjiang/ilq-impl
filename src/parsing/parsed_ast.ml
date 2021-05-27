(** The parsed AST of qwhile + assertion *)

(* Square matrices are used for defining assertions, i.e., projections. These
   matrices are stored as list of complex numbers, along with the dimension. *)

type parsed_matrix = Pmat of int * Complex.t list

(* Projections in assertsions must be defined and bind with an identifier at
   the beginning of a program. *)

type proj_defn = Proj of string * parsed_matrix

(* Qubit identifiers are parsed simply as strings *)

type qvar = string

(* Presumtion is a projection identifier + arguments *)
type presumtion = Presume of (qvar list * string)

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

type program = Prog of proj_defn list * presumtion * comm

(* Free variables in the command *)

let addvar a l = if List.mem a l then l else l @ [a]

let rec addvars ql l =
  match ql with [] -> l | a :: ql' -> addvars ql' (addvar a l)

let rec fv c =
  match c with
  | Skip            -> []
  | Assert (ql, _)  -> ql
  | Init q          -> [q]
  | Unitary (_, ql) -> ql
  | Seq (c1, c2)    -> addvars (fv c2) (fv c1)
  | If (q, c1, c2)  -> addvars (fv c1 @ fv c2) [q]
  | While (q, c)    -> addvars (fv c) [q]
