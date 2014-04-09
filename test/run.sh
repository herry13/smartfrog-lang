#!/bin/bash

BASEDIR="$(dirname "$0")"
BIN="$BASEDIR/../sfParser"

echo "Running tests..."
for file in $BASEDIR/*
do
	if [[ -f $file && "${file##*.}" = "sf" ]]; then
		echo -n "$file "
		#result=$($BIN $file 1>/dev/null)
		result=$($BIN $file 2>&1 1>/dev/null)
		if [[ $result != "" ]]; then
			echo "[Failed]"
		else
			echo "[OK]"
		fi
	fi
done
echo "...done!"
