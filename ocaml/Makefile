.PHONY: build clean clean-all dist

LIB=str.cma

build: csf

csf: csf.ml sfhelper.cmo sfdomain.cmo sflexer.cmo sfparser.cmo
	ocamlc -c csf.ml
	ocamlc -o csf $(LIB) sfdomain.cmo sflexer.cmo sfparser.cmo sfhelper.cmo csf.cmo

sfparser.cmo: sfdomain.cmo
	ocamlyacc sfparser.mly
	ocamlc -c sfparser.mli
	ocamlc -c sfparser.ml

sflexer.cmo: sflexer.mll sfparser.cmo
	ocamllex sflexer.mll
	ocamlc -c sflexer.ml

sfdomain.cmo: sfdomain.ml
	ocamlc -c sfdomain.ml

sfhelper.cmo: sfhelper.ml sfparser.cmo sflexer.cmo
	ocamlc -c sfhelper.ml

clean:
	@rm -f *.cmo *.mli *.cmi sfparser.ml sflexer.ml

clean-all: clean
	@rm -f csf

dist: build clean

clean-build: clean-all build

test: clean-all build
	@./test.sh

darwin: dist
	@mv -f csf dist/darwin

linux: dist
	@mv -f csf dist/linux
