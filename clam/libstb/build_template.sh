#!/bin/bash
T=${1:-template.c}
O=${2:-template}
gcc -I. -c -o .t.o "$T"
gcc -o "$O" ../clam.a .t.o
rm -f .t.o
echo "$T -> $O"

