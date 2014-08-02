#!/bin/bash

#######################################
#
# Bash script file for travis-ci.org.
# This only supports OCaml compiler.
#
#######################################

#########
# OCaml
#########

# install OCaml from apt
sudo apt-get update -qq
sudo apt-get install -qq ocaml

# compile OCaml compiler
cd ocaml
make dist
cd ..
cd test
./runCsf

