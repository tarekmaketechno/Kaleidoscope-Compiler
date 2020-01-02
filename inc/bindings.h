#include <stdio.h>
#include "types.h"

extern int print_int(int i);

extern int print_float(float i);

extern int print_bool(int i);

extern int print_string(char * i);

extern int check_overflow(int op1, int op2, arithop_type at);
