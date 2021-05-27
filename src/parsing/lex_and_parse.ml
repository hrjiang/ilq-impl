open Printf
open Complex
open Lexer
open Lexing
open Parsed_ast

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.read_token lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg ;
      None
  | Parser.Error    ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf ;
      exit (-1)

let print_complex z = printf "%.3f+%.3fi" z.re z.im

let rec print_complex_mat n i zl =
  match zl with
  | []       -> ()
  | z :: zl' ->
      if i >= n then (
        print_complex z ; printf "\n" ; print_complex_mat n 1 zl' )
      else (
        print_complex z ;
        printf "\t" ;
        print_complex_mat n (i + 1) zl' )

let print_mat m = match m with Pmat (n, zl) -> print_complex_mat n 1 zl

let print_proj proj =
  match proj with
  | Proj (p, m) -> printf "%s := \n" p ; print_mat m ; printf "\n"

let rec print_projs projs =
  match projs with
  | []             -> ()
  | proj :: projs' -> print_proj proj ; print_projs projs'

let rec print_qvars ql =
  match ql with
  | []       -> ()
  | [q]      -> printf "%s" q
  | q :: ql' -> printf "%s, " q ; print_qvars ql'

let print_presumption pre =
  match pre with
  | Presume (args, proj) ->
      printf "presume([" ; print_qvars args ; printf "], %s)" proj

let rec print_gate g =
  match g with
  | Gate a        -> printf "%s" a
  | Controlled g' -> printf "C-" ; print_gate g'

let rec print_indent indent =
  if indent <= 0 then ()
  else (
    printf "  " ;
    print_indent (indent - 1) )

let rec print_comm c indent =
  print_indent indent ;
  match c with
  | Skip              -> printf "skip"
  | Assert (ql, proj) ->
      printf "assert([" ; print_qvars ql ; printf "], %s)" proj
  | Init q            -> printf "init(%s)" q
  | Unitary (g, ql)   ->
      print_gate g ; printf "[" ; print_qvars ql ; printf "]"
  | Seq (c1, c2)      -> print_comm c1 0 ; printf ";\n" ; print_comm c2 indent
  | If (q, c1, Skip)  ->
      printf "if (%s) {\n" q ;
      print_comm c1 (indent + 1) ;
      printf "\n" ;
      print_indent indent ;
      printf "}\n"
  | If (q, c1, c2)    ->
      printf "if (%s) {\n" q ;
      print_comm c1 (indent + 1) ;
      printf "\n" ;
      print_indent indent ;
      printf "} else {\n" ;
      print_comm c2 (indent + 1) ;
      printf "\n" ;
      print_indent indent ;
      printf "}"
  | While (q, c')     ->
      printf "while (%s) {\n" q ;
      print_comm c' (indent + 1) ;
      printf "\n" ;
      print_indent indent ;
      printf "}"

let print_parsed_ast p =
  match p with
  | Prog (proj_defns, pre, c) ->
      print_projs proj_defns ;
      printf "\n" ;
      print_presumption pre ;
      printf "\n" ;
      print_comm c 0

let print_fv ql =
  printf "\n\nFree variables are:\n" ;
  List.iter (printf "%s ") ql

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some p -> (
      print_parsed_ast p ;
      match p with
      | Prog (_, _, c) ->
          print_fv (fv c) ;
          printf "\n" )
  | None   -> ()
