%{
  open Parsed_ast
%}

(* Token definitions *)
%token <string> ID
%token <float> FLOAT
%token <int> INT
%token PLUS
%token MINUS
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
%token PRESUME
%token SQUARE
%token CONTROLLED
%token SKIP
%token ASSERT
%token INIT
%token GATE
%token IF
%token THEN
%token ELSE
%token WHILE
%token EOF

%right SEMICOLON

(* Starting production *)
%start <Parsed_ast.program option> program
%type <comm> comm
%type <gate> gate
%type <proj_defn> proj_defn
%type <parsed_matrix> parsed_matrix
%type <Complex.t> complex
%% 
  
program:
  | EOF { None }
  | proj_defns = separated_list(SEMICOLON, proj_defn); pre = presumption; c = comm; EOF { Some (Prog (proj_defns, pre, c)) }
;

proj_defn:
  | PROJ; name = ID; COLONEQ; mat = parsed_matrix { Proj (name, mat) } 
;

presumption:
  | PRESUME; LPAREN; LBRACKET; qnames = separated_list(COMMA, ID); RBRACKET; COMMA; name = ID; RPAREN { Presume (qnames, name) }

parsed_matrix:
  | SQUARE; size=INT; LBRACKET; content = separated_list(COMMA, complex); RBRACKET { Pmat (size, content) }

complex:
  | f1 = FLOAT; PLUS; f2 = FLOAT; IM { {re = f1; im = f2} }
  | f1 = FLOAT; MINUS; f2 = FLOAT; IM { {re = f1; im = Float.neg f2} }
  | MINUS; f1 = FLOAT; PLUS; f2 = FLOAT; IM { {re = Float.neg f1; im = f2} }
  | MINUS; f1 = FLOAT; MINUS; f2 = FLOAT; IM { {re = Float.neg f1; im = Float.neg f2} }
  | f1 = FLOAT { {re=f1; im=0.0} }
  | MINUS; f1 = FLOAT {{re=Float.neg f1; im=0.0}}
  ;

comm:
  | SKIP { Skip }
  | ASSERT; LPAREN; LBRACKET; qnames = separated_list(COMMA, ID); RBRACKET; COMMA; proj_name = ID; RPAREN { Assert (qnames, proj_name) }
  | INIT; name = ID { Init name }
  | g = gate; LBRACKET; qnames = separated_list(COMMA, ID); RBRACKET { Unitary (g, qnames) }
  | c1 = comm; SEMICOLON; c2 = comm { Seq (c1, c2) }
  | IF; LPAREN; name = ID; RPAREN; THEN; LBRACE; c1 = comm; RBRACE; ELSE; LBRACE; c0 = comm; RBRACE { If (name, c1, c0) }
  | IF; LPAREN; name = ID; RPAREN; THEN; LBRACE; c1 = comm; RBRACE { If (name, c1, Skip) }
  | WHILE; LPAREN; name = ID; RPAREN; LBRACE; c = comm; RBRACE { While (name, c) }
;

gate:
  | GATE; name = ID { Gate name }
  | CONTROLLED; g = gate { Controlled g }
;
