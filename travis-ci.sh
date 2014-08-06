#!/bin/bash

#######################################
#
# Bash script file for travis-ci.org.
# This only supports OCaml compiler.
#
#######################################

#######################################
# csf : ocaml compiler
#######################################

# compile csf
cd ocaml
make travis
cd ..

# run test script for csf
cd test/sfp
bash -ex travis-ocaml.sh
cd ../..

#######################################
# sfParser: scala compiler
#######################################

#cd test
#bash -ex travis-scala.sh
#cd ..
