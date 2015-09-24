# default rule
.PHONY: all

all: tyrme

# building object files

IOBJS=ast.cmi parser.cmi

$(IOBJS): %.cmi: %.mli
	ocamlc -c $<


# lexical analyzer


parser.cmo: parser.ml parser.mli parser.cmi
	ocamlc -c $<

lexer.cmo: lexer.ml
	ocamlc -c $<

parser.mli parser.ml: parser.mly ast.cmi
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll


# compiling the code

tyrme: parser.cmo lexer.cmo tyrme.ml
	ocamlc -o tyrme parser.cmo lexer.cmo tyrme.ml

# Cleaning

.PHONY: clean

clean:
	rm -f *.cmi *.cmo
	rm -f lexer.ml parser.mli parser.ml
	rm -f tyrme
