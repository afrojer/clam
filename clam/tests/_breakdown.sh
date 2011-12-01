#!/bin/bash
echo "---------------------------------------"
if [ $ERRORS -eq 0 ]; then
	echo -e "$TEST_NAME: [ \e[00;32mPASS\e[00m ]"
	echo
	exit 0
else
	echo -e "$TEST_NAME: [ \e[00;31mFAIL\e[00m ]"
	echo
	exit 1
fi
