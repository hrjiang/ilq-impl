{
  (* Libraries and helper functions for the parser *)
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

(* Regexes for tokens *)
let digit = ['0'-'9']
let frac = '.' digit*
let int = '-'? digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Lexer Rules *)
rule read_token = parse
  | whitespace { read_token lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "," { COMMA }
  | ":=" { COLONEQ }
  | ";" { SEMICOLON }
  | "skip" { SKIP }
  | "assert" { ASSERT }
  | "init" { INIT }
  | "gate" { GATE }
  | "controlled" { CONTROLLED }
  | "if" { IF }
  | "then" { THEN }
  | "else" {ELSE}
  | "while" { WHILE }
  | "proj" { PROJ }
  | "presume" { PRESUME }
  | "square" { SQUARE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "i" { IM }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
    
                  
