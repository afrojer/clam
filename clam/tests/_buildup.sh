#!/bin/bash

# do this automagically :-)
TEST_NAME=$(basename ${0%\.*})
TEST_SRC=${TEST_NAME}.clam
TEST_BIN=${TEST_NAME}.bin

function msgok() {
	echo -e "\033[00;34mOK $1\033[00m"
}

function error() {
	echo -e "\033[00;33mERROR $1\033[00m"
}

# Check that necessary unit test variables exist
if [ -z "$TEST_DESC" ]; then
	error "Unit test not properly formatted. Description required!"
	exit 1
fi

# Check if we're asking for help. If so, provide usage
if [ $# == 1 ] && ([ "$1" == "-help" ] || [ "$1" == "--help" ] || [ "$1" == "-h" ]); then
	echo
	echo "CLAM Unit Test: $TEST_NAME"
	echo "Description: $TEST_DESC"
	echo "Usage: $0 [optional/path/to/clam/binary]"
	echo
	exit 0
fi
		
# Find CLAM, verify it exists
CLAM_BINARY=${1:-../clam}
if [ ! -x $CLAM_BINARY ]; then
	error "Could not find CLAM binary at: $CLAM_BINARY"
	exit 1
fi

function compare() {
	cmp -s "$1" "$2"
	return $?
}

function compile_it() {
	if [ ! -f "$TEST_SRC" ]; then
		error "Could not find $TEST_NAME source file: '$TEST_SRC'"
		exit 1
	fi
	echo -n "Compiling '$TEST_SRC'..."
	COMPILE_OUTPUT=$(${CLAM_BINARY} -i $TEST_SRC -o ./$TEST_BIN 2>&1)
	ERRORS=$?
	if [ ! $ERRORS -eq 0 ]; then error "."; else msgok "."; fi
}

function run_it() {
	if [ ! -x "$TEST_BIN" ]; then
		RUN_OUTPUT="Missing binary!"
		ERRORS=1
	fi
	RUN_OUTPUT=$(./$TEST_BIN 2>&1)
	ERRORS=$?
}

echo "======================================="
echo "** $TEST_NAME"
echo "** $TEST_DESC"

# Make sure a test can't pass by accident
ERRORS=100000
