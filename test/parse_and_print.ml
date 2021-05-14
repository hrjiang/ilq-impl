open Parsing.Lex_and_parse

let usage_msg = "parse_and_print <file1>"

let input_files = ref []

let anon_fun filename = input_files := filename :: !input_files

let speclist = []

let () =
  Arg.parse speclist anon_fun usage_msg ;
  match !input_files with
  | []            -> exit (-1)
  | filename :: _ ->
      let ic = open_in filename in
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse_and_print lexbuf ;
      close_in ic
