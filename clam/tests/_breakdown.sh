#!/bin/bash
# _breakdown.sh

rm -f ${TEST_NAME}.bin 2>&1

echo "---------------------------------------"
if [ $ERRORS -eq 0 ]; then
	echo -e "$TEST_NAME: [ \033[00;32mPASS\033[00m ]"
	echo
	exit 0
else
	echo -e "$TEST_NAME: [ \033[00;31mFAIL\033[00m ]"
	echo
	exit 1
fi
