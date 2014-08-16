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
	syntax.cmo \
	lexer.cmo \
	parser.cmo \
	helper.cmo \
	domain.cmo \
	type.cmo \
	variable.cmo \
	constraint.cmo \
	action.cmo \
	valuation.cmo \
	fdr.cmo \
	csfp.cmo

# native code libraries
NATIVELIB= \
	str.cmxa \
	common.cmx \
	syntax.cmx \
	lexer.cmx \
	parser.cmx \
	helper.cmx \
	domain.cmx \
	type.cmx \
	variable.cmx \
	constraint.cmx \
	action.cmx \
	valuation.cmx \
	fdr.cmx \
	csfp.cmx

# default
build: csfp

csfp: csfp.ml action helper valuation type fdr
	$(COMPILER) -c csfp.ml
ifeq ($(NATIVE),1)
	$(COMPILER) -o $(NAME) $(NATIVELIB)
else
	$(COMPILER) -o $(NAME) $(BYTECODELIB)
endif

syntax: syntax.mli syntax.ml
	$(COMPILER) -c syntax.mli
	$(COMPILER) -c syntax.ml

parser: parser.mly syntax
	ocamlyacc parser.mly
	$(COMPILER) -c parser.mli
	$(COMPILER) -c parser.ml

lexer: lexer.mll parser
	ocamllex lexer.mll
	$(COMPILER) -c lexer.ml

helper: helper.ml parser lexer
	$(COMPILER) -c helper.ml

domain: common syntax domain.mli domain.ml
	$(COMPILER) -c domain.mli
	$(COMPILER) -c domain.ml

valuation: valuation.mli valuation.ml domain
	$(COMPILER) -c valuation.mli
	$(COMPILER) -c valuation.ml

type: type.ml
	$(COMPILER) -c type.mli
	$(COMPILER) -c type.ml

common: common.mli common.ml
	$(COMPILER) -c common.mli
	$(COMPILER) -c common.ml

variable: variable.mli variable.ml common domain type
	$(COMPILER) -c variable.mli
	$(COMPILER) -c variable.ml

constraint: variable common domain type constraint.mli constraint.ml
	$(COMPILER) -c constraint.mli
	$(COMPILER) -c constraint.ml

action: common domain type variable constraint action.mli action.ml
	$(COMPILER) -c action.mli
	$(COMPILER) -c action.ml

fdr: common domain type variable constraint action fdr.mli fdr.ml
	$(COMPILER) -c fdr.mli
	$(COMPILER) -c fdr.ml

clean:
	@rm -f *.s *.cmo parser.mli lexer.mli *.cmi *.o *.cmx parser.ml lexer.ml parser.output ocamlprof.dump

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
