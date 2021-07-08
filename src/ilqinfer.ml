open Printf
open Matlib.Matops
open Matlib.Builtin_gates
open Parsing.Parsed_ast
open Owl
open Dense

let index_of l e =
  let rec index_rec i = function
    | []       ->
        printf "\nVar %s not found.\n" e ;
        raise Not_found
    | hd :: tl -> if hd = e then i else index_rec (i + 1) tl
  in
  index_rec 0 l

let proj_mat pm =
  match pm with
  | Pmat (n, content) -> Matrix.Z.of_array (Utils.Array.of_list content) n n

let rec get_proj name pdefs =
  match pdefs with
  | [] ->
      printf "\nProjection %s not found.\n" name ;
      None
  | Proj (name', pm) :: pdefs' ->
      if name = name' then Some (proj_mat pm) else get_proj name pdefs'

(* We bind each quantum variable with a natural number according to their
   time of appearence. We use this number as the index of the qubit for the
   variable. *)

let return a = Some a

let bind o f = match o with None -> None | Some a -> f a

let (let*) x f = bind x f

let rec gate_mat g =
  match g with
  | Gate s       -> (
    match gates s with
    | Some _ -> gates s
    | None   ->
        printf "\nGate %s not found.\n" s ;
        None )
  | Controlled g ->
     let* m = gate_mat g in
     return (controlled_gate m)

(* require args subseteq vars, ensure result = args ++ tail /\ result is a
   permutation of vars *)
let rec pad_args args vars =
  match args with
  | []      -> vars
  | q :: ql ->
      let vars' = List.filter (fun a -> not (a = q)) vars in
      q :: pad_args ql vars'

(* rearrange operator w.r.t. arguments *)
(* [op_mat]: op matrix *)
(* [args]: op arguments *)
(* [vars]: all variables of the system *)
let rearrange_wrt_args op_mat args vars =
  let n = List.length args in
  let m = List.length vars in
  let permut = List.map (index_of vars) (pad_args args vars) in
  rearrange_local m n op_mat permut

let init q vars rho =
  let op0 = rearrange_wrt_args b0 [q] vars in
  let op1 = rearrange_wrt_args b1 [q] vars in
  Matrix.Z.(apply op0 rho + apply op1 rho)

let sem_gate g args vars =
  let* g_mat = gate_mat g in
  return (rearrange_wrt_args g_mat args vars)

let sem_meas b q vars =
  let meas_op_mat = if b then b3 else b0 in
  rearrange_wrt_args meas_op_mat [q] vars

let sem_proj p args vars = rearrange_wrt_args p args vars

let apply_gate g args vars rho =
  let* op = sem_gate g args vars in
  return (apply op rho)

let apply_proj_orth p args pdefs vars rho =
  let* pmat = get_proj p pdefs in
  let rc = sem_proj (orth pmat) args vars in
  return (apply rc rho)

(**********************************************************)
(* We model post using a pair of projections. I.e., (P_ok, P_er). When we
   reach a program point where P_er is not 0, we stop and report a violation
   of assertion. Otherwise we carry on the inferenc procedure using P_ok as
   presumtion. *)

let tolerable p eps = (Matrix.Z.trace p).re <= eps

(* merge the branches if q_er is zero, otherwise return the error *)
let merge r1 r2 =
  let o1, e1 = r1 in
  let o2, e2 = r2 in
  if tolerable e1 0. && tolerable e2 0. then (vee o1 o2, zero e1)
  else (zero o1, vee e1 e2)

let rec args_to_string args =
  match args with [] -> "" | a :: args' -> a ^ " " ^ args_to_string args'

(* b: bound for loop unrolling *)
(* vars: all variables of the system of interest *)
(* proj_defs: projection definitions *)
(* p: presumption *)
(* c: code *)
(* eps: tolerated error *)
let rec infer_aux b vars proj_defs p c eps =
  let pzero = zero p in
  match c with
  | Skip              -> return (p, pzero)
  | Assert (args, r)  ->
     let* q = apply_proj_orth r args proj_defs vars p in 
      return
        ( if tolerable q eps then (p, pzero)
        else (
          Printf.printf "\nAsertion failed: %s[ %s].\n" r
            (args_to_string args) ;
          Printf.printf "\nPredicate before assertion:" ;
          Matrix.Z.print p ;
          Printf.printf "\nThe erronous post:" ;
          Matrix.Z.print q ;
          (pzero, supp q) ) )
  | Init q            -> return (supp (init q vars p), pzero)
  | Unitary (g, args) ->
     let* q = apply_gate g args vars p in
     return (q, pzero)
  | Seq (c1, c2)      ->
     let* post = infer_aux b vars proj_defs p c1 eps in
     let q_ok, q_er = post in
     if tolerable q_er 0. then infer_aux b vars proj_defs q_ok c2 eps
     else return (q_ok, q_er)
  | If (q, c1, c0)    ->
     let* q1 = infer_aux b vars proj_defs
                 (supp (apply (sem_meas true q vars) p))
                 c1 eps in
     let* q0 = 
       infer_aux b vars proj_defs
         (supp (apply (sem_meas false q vars) p))
         c0 eps in
     return (merge q1 q0)
  | While (q, c)      ->
     let q0 = (supp (apply (sem_meas false q vars) p), zero p) in
     if b <= 0 then return q0
     else
       let* q1 = infer_aux (b - 1) vars proj_defs
                   (supp (apply (sem_meas true q vars) p))
                   (Seq (c, While (q, c)))
                   eps in
       return (merge q0 q1)

let infer b prog eps =
  let (Prog (pdefs, Presume (args, p), c)) = prog in
  let vars = fv c in
  Printf.printf "\nVariables: %s\n\n" (args_to_string vars) ;
  let* pmat = get_proj p pdefs in
  let pre = sem_proj pmat args vars in
  let* post = infer_aux b vars pdefs pre c eps in
  let _, q_er = post in
  return
    ( if tolerable q_er 0. then Printf.printf "OK\n"
    else Printf.printf "Error\n" )
