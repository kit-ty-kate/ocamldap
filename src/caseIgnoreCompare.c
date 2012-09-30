#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

#define LESS -1
#define GREATER 1
#define EQUAL 0

char to_lowercase(long c) {
    if (((c >= 'A' && c <= 'Z') ||
	 (c >= 192 && c <= 214)) ||
	(c >= 216 && c <= 222))
	return (char) c + 32;
    else return (char) c;
}

CAMLprim value caseIgnoreCompare(value v1, value v2)
{
    /* based on byterun/compare.c */
    long res = 0;
    mlsize_t len1, len2, len;
    unsigned char * p1, * p2;

    if (v1 != v2) { /* if they don't point to the same thing */
	len1 = caml_string_length(v1);
	len2 = caml_string_length(v2);
	if (len1 != len2) res = len1 - len2;
	else { /* if the length is the same for both */
	    for (len = len1,
		     p1 = (unsigned char *) String_val(v1),
		     p2 = (unsigned char *) String_val(v2);
		 len > 0;
		 len--, p1++, p2++)
		if (*p1 != *p2)
		    if (to_lowercase (*p1) != to_lowercase(*p2)) {
			res = (long)*p1 - (long)*p2;
			break;
		    }
	}
    }

    if (res < 0)
	return Val_int(LESS);
    else if (res > 0)
	return Val_int(GREATER);
    else
	return Val_int(EQUAL);
}
