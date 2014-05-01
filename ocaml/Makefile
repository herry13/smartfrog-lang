.PHONY: build clean clean-all dist

build: semantics sflexer.cmo sfparser.cmo csf.cmo
	ocamlc -o csf domain.cmo sflexer.cmo sfparser.cmo csf.cmo

csf.cmo: csf.ml
	ocamlc -c csf.ml

sfparser.cmo: sfparser.ml
	ocamlc -c sfparser.ml

sfparser.ml: sfparser.mli
	ocamlc -c sfparser.mli

sfparser.mli: sfparser.mly
	ocamlyacc sfparser.mly

sflexer.cmo: sflexer.ml sfparser.cmo
	ocamlc -c sflexer.ml

sflexer.ml: sflexer.mll
	ocamllex sflexer.mll

semantics: domain.ml
	ocamlc -c domain.ml

clean:
	@rm -f *.{cmo,mli,cmi} sfparser.ml sflexer.ml

clean-all: clean
	@rm -f csf

dist: build clean

clean-build: clean-all build

test: clean-all build
	@./csf comp.sf
