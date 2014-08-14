.PHONY: build clean clean-all dist

# 0 = OCaml bytecode
# 1 = native
NATIVE=0

# binary name
NAME=csfp

ifeq ($(NATIVE),1)
# native compiler
COMPILER=ocamlopt -S
else
# byte code compiler
COMPILER=ocamlc
endif

# byte code libraries
BYTECODELIB= \
	str.cma \
	common.cmo \
	sfsyntax.cmo \
	sflexer.cmo \
	sfparser.cmo \
	sfhelper.cmo \
	sfdomain.cmo \
	sfdomainhelper.cmo \
	variable.cmo \
	sfvaluation.cmo \
	sftype.cmo \
	sastranslator.cmo \
	csf.cmo

# native code libraries
NATIVELIB= \
	str.cmxa \
	common.cmx \
	sfsyntax.cmx \
	sflexer.cmx \
	sfparser.cmx \
	sfhelper.cmx \
	sfdomain.cmx \
	sfdomainhelper.cmx \
	variable.cmx \
	sfvaluation.cmx \
	sftype.cmx \
	sastranslator.cmx \
	csf.cmx

# default
build: csf

csf: csf.ml variable sfhelper.cmo sfvaluation.cmo sftype.cmo sastranslator.cmo
	$(COMPILER) -c csf.ml
ifeq ($(NATIVE),1)
	$(COMPILER) -o $(NAME) $(NATIVELIB)
else
	$(COMPILER) -o $(NAME) $(BYTECODELIB)
endif

sfsyntax: sfsyntax.ml
	$(COMPILER) -c sfsyntax.ml

sfparser.cmo: sfparser.mly sfsyntax
	ocamlyacc sfparser.mly
	$(COMPILER) -c sfparser.mli
	$(COMPILER) -c sfparser.ml

sflexer.cmo: sflexer.mll sfparser.cmo
	ocamllex sflexer.mll
	$(COMPILER) -c sflexer.ml

sfhelper.cmo: sfhelper.ml sfparser.cmo sflexer.cmo
	$(COMPILER) -c sfhelper.ml

sfdomain: sfsyntax sfdomain.mli sfdomain.ml sfdomainhelper.ml
	$(COMPILER) -c sfdomain.mli
	$(COMPILER) -c sfdomain.ml
	$(COMPILER) -c sfdomainhelper.ml

sfvaluation.cmo: sfvaluation.ml sfdomain
	$(COMPILER) -c sfvaluation.ml

sftype.cmo: sftype.ml
	$(COMPILER) -c sftype.mli
	$(COMPILER) -c sftype.ml

sastranslator.cmo: sastranslator.ml sfdomain sftype.cmo
	$(COMPILER) -c sastranslator.ml

common: common.mli common.ml
	$(COMPILER) -c common.mli
	$(COMPILER) -c common.ml

variable: common sfdomain variable.mli variable.ml
	$(COMPILER) -c variable.mli
	$(COMPILER) -c variable.ml

clean:
	@rm -f *.s *.cmo sfparser.mli sflexer.mli *.cmi *.o *.cmx sfparser.ml sflexer.ml sfparser.output ocamlprof.dump

clean-all: clean
	@rm -f $(NAME)

dist: build clean

clean-build: clean-all build

darwin: dist
	@mv -f $(NAME) dist/darwin

linux: dist
	@mv -f $(NAME) dist/linux

install-ocaml:
	# install OCaml from apt
	sudo apt-get update -qq
	sudo apt-get install -qq ocaml

travis: install-ocaml dist
