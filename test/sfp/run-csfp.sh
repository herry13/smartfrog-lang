#!/bin/bash

BASEDIR="$(dirname "$0")"
OPT="-type"
BIN="$BASEDIR/../../ocaml/csfp $OPT"
EXT="sfp"

function test {
	if [[ -f $1 && "${1##*.}" = $EXT ]]; then
		result=$($BIN $1 2>&1 1>/dev/null)
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
	test $file
done
echo "=== done ==="
