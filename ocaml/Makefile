.PHONY: build clean clean-all dist

NATIVE=0

build: csf

ifeq ($(NATIVE),1)
# native compiler
COMPILER=ocamlopt
else
# byte code compiler
COMPILER=ocamlc
endif

BYTECODELIB=sfsyntax.cmo sflexer.cmo sfparser.cmo sfhelper.cmo sfdomain.cmo sfdomainhelper.cmo sfvaluation.cmo sftype.cmo csf.cmo

NATIVELIB=sfsyntax.cmx sflexer.cmx sfparser.cmx sfhelper.cmx sfdomain.cmx sfdomainhelper.cmx sfvaluation.cmx sftype.cmx csf.cmx

csf: csf.ml sfhelper.cmo sfvaluation.cmo sftype.cmo
	$(COMPILER) -c csf.ml
ifeq ($(COMPILER),ocamlc)
	$(COMPILER) -o csf $(BYTECODELIB)
else
	$(COMPILER) -o csf $(NATIVELIB)
endif

sfsyntax.cmo: sfsyntax.ml
	$(COMPILER) -c sfsyntax.ml

sfparser.cmo: sfparser.mly sfsyntax.cmo
	ocamlyacc sfparser.mly
	$(COMPILER) -c sfparser.mli
	$(COMPILER) -c sfparser.ml

sflexer.cmo: sflexer.mll sfparser.cmo
	ocamllex sflexer.mll
	$(COMPILER) -c sflexer.ml

sfhelper.cmo: sfhelper.ml sfparser.cmo sflexer.cmo
	$(COMPILER) -c sfhelper.ml

sfdomain.cmo: sfdomain.ml sfdomainhelper.ml
	$(COMPILER) -c sfdomain.ml
	$(COMPILER) -c sfdomainhelper.ml

sfvaluation.cmo: sfvaluation.ml sfdomain.cmo
	$(COMPILER) -c sfvaluation.ml

sftype.cmo: sftype.ml
	$(COMPILER) -c sftype.ml

clean:
	@rm -f *.cmo *.mli *.cmi *.o *.cmx sfparser.ml sflexer.ml

clean-all: clean
	@rm -f csf

dist: build clean

clean-build: clean-all build

darwin: dist
	@mv -f csf dist/darwin

linux: dist
	@mv -f csf dist/linux
