#!/bin/bash

BASEDIR="$(dirname "$0")"
LISADIR="$BASEDIR/LISA2014paper"
OPT="-type"
BIN="$BASEDIR/../ocaml/csf $OPT"

function test {
	if [[ -f $1 && "${1##*.}" = "sf" ]]; then
		echo -n "$1 "
		result=$($BIN $1 2>&1 1>/dev/null)
		if [[ $result != "" ]]; then
			echo "[Failed]"
		else
			echo "[OK]"
		fi
	fi
}

echo "=== running tests ==="
filelist="good-test-files.txt"
for file in $(cat $filelist); do
	test $file
done

#for file in $BASEDIR/*; do
#	test $file
#done
#
#for file in $LISADIR/*; do
#	test $file
#done

echo "=== done ==="
