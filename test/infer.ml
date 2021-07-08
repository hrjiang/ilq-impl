open Parsing.Lex_and_parse
open Ilqinfer

let usage_msg = "parse_and_print <file> [-l <int>] [-e <float>]"

let input_file = ref ""

let loop_bound = ref 5

let err_bound = ref 0.005

let anon_fun filename = input_file := filename

let speclist =
  [ ("-l", Arg.Set_int loop_bound, "Bound for loop unrolling, default 5")
  ; ( "-e"
    , Arg.Set_float err_bound
    , "Bound for tolerable error, default 0.005" ) ]

let infer_and_return b prog e = match infer b prog e with _ -> ()

let () =
  Arg.parse speclist anon_fun usage_msg ;
  let ic = open_in !input_file in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= !input_file} ;
  let op = parse_with_error lexbuf in
  close_in ic ;
  match op with
  | Some prog -> infer_and_return !loop_bound prog !err_bound
  | _         ->
      Printf.printf "Unknown error" ;
      exit (-1)
