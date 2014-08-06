.PHONY: build clean clean-all dist

# 0 = OCaml bytecode
# 1 = native
NATIVE=0

# binary name
NAME=csfp

build: csf

ifeq ($(NATIVE),1)
# native compiler
COMPILER=ocamlopt
else
# byte code compiler
COMPILER=ocamlc
endif

BYTECODELIB=str.cma sfsyntax.cmo sflexer.cmo sfparser.cmo sfhelper.cmo sfdomain.cmo sfdomainhelper.cmo sfvaluation.cmo sftype.cmo csf.cmo

NATIVELIB=str.cmxa sfsyntax.cmx sflexer.cmx sfparser.cmx sfhelper.cmx sfdomain.cmx sfdomainhelper.cmx sfvaluation.cmx sftype.cmx csf.cmx

csf: csf.ml sfhelper.cmo sfvaluation.cmo sftype.cmo
	$(COMPILER) -c csf.ml
ifeq ($(COMPILER),ocamlc)
	$(COMPILER) -o $(NAME) $(BYTECODELIB)
else
	$(COMPILER) -o $(NAME) $(NATIVELIB)
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
	@rm -f *.cmo *.mli *.cmi *.o *.cmx sfparser.ml sflexer.ml sfparser.output

clean-all: clean
	@rm -f $(NAME)

dist: build clean

clean-build: clean-all build

darwin: dist
	@mv -f csf dist/darwin

linux: dist
	@mv -f csf dist/linux

install-ocaml:
	# install OCaml from apt
	sudo apt-get update -qq
	sudo apt-get install -qq ocaml

travis: install-ocaml dist
