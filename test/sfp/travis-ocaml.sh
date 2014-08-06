#!/bin/bash

BASEDIR="$(dirname "$0")"
OPT_TYPE="-type"
OPT_JSON="-json"
OPT_YAML="-yaml"
OPT_AST="-ast"
BIN="$BASEDIR/../../ocaml/csfp"
EXT="sfp"

function test {
	if [[ -f $1 && "${1##*.}" = $EXT ]]; then
		result=$($BIN $2 $1 2>&1 1>/dev/null)
		if [[ $result != "" ]]; then
			echo "$1 [Failed]"
		else
			echo "$1 [OK]"
		fi
	fi
}

echo "=== running tests ==="
filelist="good-test-files.txt"
for file in $(cat $filelist); do
	# AST: -ast
	test $file $OPT_AST
	# type: -type
	test $file $OPT_TYPE
	# JSON: -json
	test $file $OPT_JSON
	# YAML: -yaml
	test $file $OPT_YAML
done
echo "=== done ==="
