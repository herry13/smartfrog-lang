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
cd test
bash -ex travis-ocaml.sh
cd ..

#######################################
# sfParser: scala compiler
#######################################

cd test
bash -ex travis-scala.sh
cd ..


#######################################
# hsf: haskell compiler
#######################################

# install Haskell from apt
sudo apt-get install -qq ghc
cabal install Parsec MissingH Safe QuickCheck
cd haskell
make
cd ..

# run test script for hsf
cd test
bash -ex travis-haskell.sh
cd ..
