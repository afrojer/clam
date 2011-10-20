#!/bin/bash

function err() {
	echo -e "\033[0;31m[E] $1\033[0m"
}

function checkoutput() {
	E=$(echo -e "$1" | diff --strip-trailing-cr -au - "$2")
	if [ ! -z "$E" ]; then
		err "$E"
		return 1
	else
		echo -e "\033[032m  [OK]\033[0m"
		return 0
	fi
}

function cleanup() {
	rm bin.clam 2>/dev/null
	rm .output 2>/dev/null
	rm temp 2>/dev/null
}

divider='==============================================='
for file in tests/*
do
	if [ -f "${file}.res" ] 
	then
		echo $divider
		echo "Testing $file:"
		. "${file}.res"
		echo -n "    compiling..."
		echo "" > .output
		cat "$file" | ./clam >> .output 2>&1
		checkoutput "$COMPILE" ".output"
		if [ $? -ne 0 ]; then
			cleanup
			continue
		fi
	
		if [ ! -f bin.clam ]; then
			cleanup
			continue
		fi
		echo -n "    checking... "
		echo "" > .output
		./bin.clam >> .output 2>&1
		checkoutput "$OUTPUT" ".output"
		cleanup
	fi
done
echo $divider 
