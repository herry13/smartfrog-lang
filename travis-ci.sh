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

# install OCaml from apt
sudo apt-get update -qq
sudo apt-get install -qq ocaml

# compile csf
echo "=> compiling csf..."
cd ocaml
make dist
cd ..

# run test script for csf
echo "=> run csf test script"
cd test
bash -ex travis-ocaml.sh


#######################################
# sfParser: scala compiler
#######################################

bash -ex travis-scala.sh


#######################################
# hsf: haskell compiler
#######################################

bash -ex travis-haskell.sh

