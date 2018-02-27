
Popocamllex README
Dan Grossman
April 13 1999

SUMMARY: I took the ocamllex sources and hacked them to spit out
Popcorn.  This file describes how to use the resulting program.

INSTALLATION: This directory has a Makefile.  It uses ocamlc,
ocamllex, and ocamlyacc to build an executable popocamllex.exe.

USAGE: Use popocamllex.exe exactly as you would use ocamllex except:
 * Give your files extension .popl (cute, eh?)
 * Result file will be .pop
 * Make your header, trailer, and actions popcorn code.
 * // style comments aren't supported so be careful with them (basically don't
   put troubling characters like unmatched braces and " in them).  /* */ style
   should be supported.
 * Actions should _return_ a value of type int.  Yes, you should use the
   Popcorn keyword return.
 * Files lexing.pop and lexing.h are in the Popcorn standard library and must
   be used with all generated lexers.  This recipe works:
   1. Put #include "lexing.h" in your .popl file.
   2. Link in lexing.pop

Note that all of the lex-specific syntax (for rules, patters, etc) is
as in ocamllex.

BISON: ocamllex and the bison Greg hacked to spit out Popcorn don't
quite speak the same language.  Here's how you can adapt for this:

1. Have some main function create a lexbuf by calling the from_function function
in the lexing module.  Put the result in a global variable (use an Opt
so it can be initialized).

2. Write a yylex() function which calls a lexer entry point with the lexbuf.
(Hint: each rule is translated to a function of the same name.)

3. Bison creates a header file you'll want to include.  Note this
header file may refer to other types that it doesn't define -- you
currently must extern them by other means.

4. In your actions, put values of type YYSTYPE in yylval.  

5. Define and manipulate int yyline and void yyerror(string) appropriately.

6. Make sure the eof actions return a negative integer or you will get parse
   errors.

EXAMPLE: The scheme compiler written in popcorn now uses as sources
slex.popl (converted by popocmallex to slex.pop) and sparse.y
(converted by the hacked bison to sparse.pop).  The Makefile includes
the lex engine.

TO DO: 

1. Only the "from_file" style of lexing has been tested, but
everything was ported.  

2. Currently the actions must return ints, but this will be easy to
generalize.  Since bison requires this anyway (and who uses lexers
without parsers), I'll do it later.  (Note: ocamllex has it easier b/c
it just relies on ocaml type inference.  We actually need to know
the type of the actions.)

3. Use prefix and open appropriately, such as with lexing.pop.
