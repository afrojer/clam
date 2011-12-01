#!/bin/bash

function error() {
	echo -e "\e[00;33m$1\e[00m"
}

function compare {
	cmp -s "$1" "$2"
	return $?
}

# Check that necessary unit test variables exist
if [ -z "$TEST_NAME" ] || [ -z "$TEST_DESC" ]; then
	error "Error: Unit test not properly formatted. One or more required"
	error "parameters were undefined."
	exit 1
fi

# Check if we're asking for help. If so, provide usage
if [ $# == 1 ] && ([ "$1" == "-help" ] || [ "$1" == "--help" ]); then
	echo
	echo "CLAM Unit Test: $TEST_NAME"
	echo "Description: $TEST_DESC"
	echo "Usage: $0 [optional/path/to/clam/binary]"
	echo
	exit 0
fi
		
# Find CLAM, verify it exists
CLAM_BINARY="../clam"
if [ ! -x $CLAM_BINARY ]; then
	error "Error: could not find CLAM binary at: $CLAM_BINARY"
	exit 1
fi

echo "======================================="
echo "** $TEST_NAME ** [$( basename $0)]"
echo "( $TEST_DESC )"
echo "---------------------------------------"

# Make sure a test can't pass by accident
ERRORS=100000
