.PHONY: build clean clean-all dist

build: csf

LIB=sfast.cmo sflexer.cmo sfparser.cmo sfhelper.cmo sfdomain.cmo sfdomainhelper.cmo sfvaluation.cmo sftype.cmo csf.cmo

csf: csf.ml sfhelper.cmo sfvaluation.cmo sftype.cmo
	ocamlc -c csf.ml
	ocamlc -o csf $(LIB)

sfast.cmo: sfast.ml
	ocamlc -c sfast.ml

sfparser.cmo: sfparser.mly sfast.cmo
	ocamlyacc sfparser.mly
	ocamlc -c sfparser.mli
	ocamlc -c sfparser.ml

sflexer.cmo: sflexer.mll sfparser.cmo
	ocamllex sflexer.mll
	ocamlc -c sflexer.ml

sfhelper.cmo: sfhelper.ml sfparser.cmo sflexer.cmo
	ocamlc -c sfhelper.ml

sfdomain.cmo: sfdomain.ml sfdomainhelper.ml
	ocamlc -c sfdomain.ml
	ocamlc -c sfdomainhelper.ml

sfvaluation.cmo: sfvaluation.ml sfdomain.cmo
	ocamlc -c sfvaluation.ml

sftype.cmo: sftype.ml
	ocamlc -c sftype.ml

clean:
	@rm -f *.cmo *.mli *.cmi sfparser.ml sflexer.ml

clean-all: clean
	@rm -f csf

dist: build clean

clean-build: clean-all build

darwin: dist
	@mv -f csf dist/darwin

linux: dist
	@mv -f csf dist/linux
