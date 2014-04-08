#!/bin/sh

BASEDIR="$(dirname "$0")"
BIN="$BASEDIR/../sfConfig"

echo "Running tests..."
for FILE in "$BASEDIR/*"
do
	if [[ -e $FILE ]]; then
		$BIN $FILE
	fi
done
echo "...done!"
