#!/bin/bash

BASEDIR="$(dirname "$0")"
BIN="$BASEDIR/../../sfp"
EXT="sfp"

echo "Running tests..."
for file in $BASEDIR/*
do
	if [[ -f $file && "${file##*.}" = "$EXT" ]]; then
		echo -n "$file "
		$BIN $file
#		result=$($BIN $file 2>&1 1>/dev/null)
#		if [[ $result != "" ]]; then
#			echo "[Failed]"
#		else
#			echo "[OK]"
#		fi
	fi
done
echo "...done!"
