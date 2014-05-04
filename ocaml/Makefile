.PHONY: build clean clean-all dist

LIB=str.cma

build: csf

csf: csf.ml sf.cmo domain.cmo sflexer.cmo sfparser.cmo
	ocamlc -c csf.ml
	ocamlc -o csf $(LIB) domain.cmo sflexer.cmo sfparser.cmo sf.cmo csf.cmo

sfparser.cmo: domain.cmo
	ocamlyacc sfparser.mly
	ocamlc -c sfparser.mli
	ocamlc -c sfparser.ml

sflexer.cmo: sflexer.mll sfparser.cmo
	ocamllex sflexer.mll
	ocamlc -c sflexer.ml

domain.cmo: domain.ml
	ocamlc -c domain.ml

sf.cmo: sf.ml sfparser.cmo sflexer.cmo
	ocamlc -c sf.ml

clean:
	@rm -f *.{cmo,mli,cmi} sfparser.ml sflexer.ml

clean-all: clean
	@rm -f csf

dist: build clean

clean-build: clean-all build

test: clean-all build
	@./test.sh
