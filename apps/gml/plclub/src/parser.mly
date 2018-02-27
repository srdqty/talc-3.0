%{
%}

%token <string> IDENT
%token  BIND INCLUDE
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token EOF

%start program
%type <string list * Program.t list> program

%%

program:
  includelist tokenlist EOF
     { (List.rev $1,List.rev $2) }
;

includelist:
    includelist INCLUDE STRING { $3 :: $1 }
  | /* empty */ { [] }

tokenlist:
    tokenlist tokengroup
      { $2 :: $1 }
  | /* empty */
      { [] }
;

tokengroup:      
    token
      { $1 }
  | LBRACKET tokenlist RBRACKET
      { Program.Arr (List.rev $2) }
  | LBRACE tokenlist RBRACE
      { Program.Fun (List.rev $2) }
;

token:
    IDENT
      { Program.translate $1 }
  | BIND IDENT
      { Program.Binder $2 }
  | INT
      { Program.Int $1 }
  | FLOAT
      { Program.Float $1 }
  | STRING
      { Program.String $1 }
;
