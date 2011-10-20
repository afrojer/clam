
test : test.ml dummy.cmi wrap.o
	ocamlc -custom -o test test.ml wrap.o

wrap.o : wrap.c
	ocamlc -ccopt -ljpeg -c wrap.c

dummy.cmi : dummy.mli
	ocamlc -c dummy.mli




.PHONY : clean

clean :
	rm -f *.o *.a *.so *.cmi *.cmo
