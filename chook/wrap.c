
#include <stdio.h>
#include <stdlib.h>
#include <caml/mlvalues.h>


CAMLprim value dummy_inc_c(value vnum)
{
	int num = Int_val(vnum);
	++num;
	return Val_int(num);
}

/*
CAMLprim value dummy_print_int_c(value vnum)
{
	int num = Int_val(vnum);
	printf("Number: %d\n", num);
	return Val_unit;
}

*/
