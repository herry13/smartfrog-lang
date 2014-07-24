#!/bin/bash

BASEDIR=$(dirname $0)
CSF="$BASEDIR/../../ocaml/csf -sf"
SF="/Users/admin/Documents/softwares/smartfrog/smartfrog.3.18.016/dist/bin/sfParse -d"
OUTDIR="output"

function test {
	if [[ -f $1 && "${1##*.}" = "sf" ]]; then
		# csf
		csffile="$2.csf"
		# sfparser
		sffile="$2.sf"
		
		# compare by 'diff'
		result=$(diff "$csffile" "$sffile")
		if [ "$result" = "" ]; then
			echo "[ok]"
		else
			echo "[diff]"
		fi
		#echo "done"
	else
		echo "[not-exist]"
	fi
}

files=$(cat files.txt)
mkdir -p $OUTDIR

echo "Running tests..."
counter=1
matched=0
for file in $files; do
	outfile=$(basename $file)
	echo -n "$counter) Processing $file..."
	result=$(test $file "$OUTDIR/$counter-$outfile")
	echo $result
	if [ "$result" = "[ok]" ]; then
		(( matched = matched + 1 ))
	fi
	(( counter = counter + 1 ))
done
echo "...done!"
echo "Matched: $matched files"
