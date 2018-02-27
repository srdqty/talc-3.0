This directory contains the beginnings of the "new" safe-C language
which we are calling "Cyclone".  (Not to be confused with the runtime-
code-generation facilities added to the original safe-C language
Popcorn.)  

See doc/notes.txt for some notes on the language -- these are out of
date with the current syntax but should get many of the ideas across.
See lib/ for examples of Cyclone code that should run through the
current processor and are hence up to date.  Porting the rest of the
Popcorn library to the Cyclone syntax would be a useful task for
someone to do.  See also doc/todo.txt for a list of problems with the
current system that should be addressed.

Currently, the Makefile assumes that you've built the Ocaml version
of the Popcorn compiler.  If you want to re-do the dependency analysis
for the sources, then you'll also need to build the Popcorn-in-Popcorn 
compiler.  The Makefile builds the Popcorn versions of bison and lex
for building the parser and lexer of Cyclone respectively.  It then
builds bin/cyclone.exe which can parse cyclone files.

