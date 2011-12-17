#!/bin/bash
T=${1:-template.c}
O=${2:-template}
g++ -D_DEBUG -I. -c -o .t.o "$T"
g++ -o "$O" ../clam.a .t.o
rm -f .t.o
echo "$T -> $O"

