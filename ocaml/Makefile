.PHONY: build clean clean-all dist

LIB=str.cma

build: csf

csf: csf.ml lexstack.cmo domain.cmo sflexer.cmo sfparser.cmo
	ocamlc -c csf.ml
	ocamlc -o csf $(LIB) lexstack.cmo domain.cmo sflexer.cmo sfparser.cmo csf.cmo

sfparser.cmo: domain.cmo
	ocamlyacc sfparser.mly
	ocamlc -c sfparser.mli
	ocamlc -c sfparser.ml

sflexer.cmo: sflexer.mll sfparser.cmo
	ocamllex sflexer.mll
	ocamlc -c sflexer.ml

domain.cmo: domain.ml
	ocamlc -c domain.ml

lexstack.cmo: lexstack.ml sfparser.cmo sflexer.cmo
	ocamlc -c lexstack.ml

clean:
	@rm -f *.{cmo,mli,cmi} sfparser.ml sflexer.ml

clean-all: clean
	@rm -f csf

dist: build clean

clean-build: clean-all build

test: clean-all build
	@./test.sh
