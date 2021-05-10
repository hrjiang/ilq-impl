%{
  open Parsed_ast
%}

(* Token definitions *)
%token <string> ID
%token <float> FLOAT
%token <int> INT
%token PLUS
%token IM
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token COMMA
%token COLONEQ
%token SEMICOLON
%token PROJ
%token SQUARE
%token CONTROLLED
%token SKIP
%token ASSERT
%token INIT
%token GATE
%token IF
%token WHILE
%token EOF

%right SEMICOLON

(* Starting production *)
%start <Parsed_ast.program option> program
%type <comm> comm
%type <gate> gate
%type <proj_defn> proj_defn
%type <Complex.t> complex
%% 
  
program:
  | EOF { None }
  | proj_defns = separated_list(SEMICOLON, proj_defn); c = comm; EOF { Some (Prog (proj_defns, c)) }
;

proj_defn:
  | PROJ; name = ID; COLONEQ; SQUARE; size=INT; LBRACKET; content = separated_list(COMMA, complex); RBRACKET { Proj (name, Pmat (size, content)) } 
;

complex:
  | f1 = FLOAT; PLUS; f2 = FLOAT; IM { {re = f1; im = f2} }
  | f1 = FLOAT { {re=f1; im=0.0} }
;

comm:
  | SKIP { Skip }
  | ASSERT; LPAREN; LBRACKET; qnames = separated_list(COMMA, ID); RBRACKET; COMMA; proj_name = ID; RPAREN { Assert (qnames, proj_name) }
  | INIT; name = ID { Init name }
  | g = gate; LBRACKET; qnames = separated_list(COMMA, ID); RBRACKET { Unitary (g, qnames) }
  | c1 = comm; SEMICOLON; c2 = comm { Seq (c1, c2) }
  | IF; LPAREN; name = ID; RPAREN; LBRACE; c1 = comm; RBRACE; LBRACE; c2 = comm; RBRACE { If (name, c1, c2) }
  | WHILE; LPAREN; name = ID; RPAREN; LBRACE; c = comm; RBRACE { While (name, c) }
;

gate:
  | GATE; name = ID { Gate name }
  | CONTROLLED; g = gate { Controlled g }
;












