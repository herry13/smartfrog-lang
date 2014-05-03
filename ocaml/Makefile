.PHONY: build clean clean-all dist

build: domain.cmo sflexer.cmo sfparser.cmo csf.cmo
	ocamlc -o csf domain.cmo sflexer.cmo sfparser.cmo csf.cmo

csf.cmo: csf.ml
	ocamlc -c csf.ml

sfparser.cmo: domain.cmo
	ocamlyacc sfparser.mly
	ocamlc -c sfparser.mli
	ocamlc -c sfparser.ml

sflexer.cmo: sflexer.mll sfparser.cmo
	ocamllex sflexer.mll
	ocamlc -c sflexer.ml

domain.cmo: domain.ml
	ocamlc -c domain.ml

clean:
	@rm -f *.{cmo,mli,cmi} sfparser.ml sflexer.ml

clean-all: clean
	@rm -f csf

dist: build clean

clean-build: clean-all build

test: clean-all build
	@./test.sh
