(** Executes the lexer and parser *)

val parse_with_error : Lexing.lexbuf -> Parsed_ast.program option

val print_parsed_ast : Parsed_ast.program -> unit

val parse_and_print : Lexing.lexbuf -> unit
