%{
extern void print_string(string);
extern int print_int(int);
%}

%union {
  int    foo;
  short    x;
}

%token NUM 
%type <foo> exp NUM
%type <x> line input

%% /* Grammar rules and actions follow */

input:   /*empty*/      { $$ = ^$(3); }
        | input line   { $$ = ^$(4); }
;

line:   '\n'       { $$ = ^$(6); }
        | exp '\n' { print_string ("RESULT=");
	             print_int($1);
		     $$ = ^$(5); }
;

exp:    NUM { $$ = ^$($1); }
        | exp exp '+' { $$ = ^$($1 + $2); }
        | exp exp '-' { $$ = ^$($1 - $2); }
        | exp exp '*' { $$ = ^$($1 - $2); }

;
%%
