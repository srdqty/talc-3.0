#############################################################
# The below is my usual latex makefile, plus some stuff for #
# running latex2html (on my system, anyway) and for keeping #
# the tex file in the parent directory to minimize clutter. #
#############################################################


## Double-hash comments are specific to this latex2html/talc makefile




# Makefile for compiling LaTeX.  By Jeff Vinocur.
# Last modified 30 October 2000
#

## Stuff the below paragraphs refer to is not really applicable here

# This works provided the .tex file has the same name as the directory
# containing it.  To use another name, set the environment variable
# TEX-MAKEFILE-FILENAME to the filename minus the .tex part.
#
# In your directory, do `ln -s /wherever/this/file/is/Makefile` to
# make ./Makefile be a symlink, or copy it in if you prefer.
#
# Valid targets are:

## You should know that for complicated files that require multiple
## passes of latex, you should only use this file if that is all
## taken care of already, since it is too stupid to recover.  But 
## once you have run latex several times, you can use this for pdf
## or whatever.

# - "dvi", "ps", "pdf" - file formats, pdf requires ps, ps requires dvi
# - "world"            - an alias for "pdf"
# - "done"             - does "world" and then deletes the latex
#                        temporary files (.aux, .log)
# - "clean"            - deletes everything generated (.aux, .log,
#                        .dvi, .ps, .pdf)
# - "spell"            - runs ispell on the .tex file

##- "html"             - runs latex2html


##ifdef TEX-MAKEFILE-FILENAME
##  NAME=$(TEX-MAKEFILE-FILENAME)
##else
##  NAME=$(basename $(notdir $(PWD)))
##endif

## Ok, hardcode this
NAME=popcorn


world: pdf
dvi: $(NAME).dvi
ps: $(NAME).ps
pdf: $(NAME).pdf

## ---------------------------------------------------------------------
## BEGIN NEW STUFF

tex: $(NAME).tex
$(NAME).tex: ../$(NAME).tex
	cp ../$(NAME).tex .

## Yes, at the end is rm -f.  Note that the file is copied in above.
## Try not to be stupid.
html: $(NAME).tex
	latex2html \
	  -auto_navigation \
	  -dir html \
	  -mkdir \
	  -noaddress \
	  -local_icons \
	  -white \
	  -transparent \
	  -up_url "http://www.cs.cornell.edu/talc/" \
	  -up_title "Typed Assembly Language" \
	  $(NAME).tex
	rm -f $(NAME).tex



## END NEW STUFF (well, added "../" in a few places later on)
## ---------------------------------------------------------------------

$(NAME).dvi: ../$(NAME).tex
	latex ../$(NAME).tex

$(NAME).ps: $(NAME).dvi
	dvips $(NAME).dvi -o $(NAME).ps

$(NAME).pdf: $(NAME).ps
	ps2pdf $(NAME).ps $(NAME).pdf

.PHONY: clean done notemp spell dvi ps pdf

spell:
	ispell ../$(NAME).tex

clean: notemp
	rm -f $(NAME).dvi $(NAME).ps $(NAME).pdf

done: world notemp

notemp:
	rm -f $(NAME).log $(NAME).aux $(NAME).toc
