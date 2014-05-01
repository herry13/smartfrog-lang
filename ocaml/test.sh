#!/bin/bash

BASEDIR="$(dirname "$0")"
TESTDIR="$BASEDIR/../test"
LISADIR="$TESTDIR/LISA2014paper"
BIN="$BASEDIR/csf"

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

echo "Running tests..."

for file in $TESTDIR/*; do
	test $file
done

for file in $LISADIR/*; do
	test $file
done

echo "...done!"
