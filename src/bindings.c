#include <stdio.h>
#include <stdlib.h>
#include "bindings.h"
#include "types.h"
#include <limits.h>

extern int g_argc;
extern int g_optind;
extern char ** g_argv;

extern int print_int(int i) {
	printf("%d\n", i);
	return 0;
}

extern int print_float(float i) {
	printf("%f\n", i);
	return 0;
}

extern int print_bool(int i) {
	if (i) {
		printf("True\n");
	} else {
		printf("False\n");
	}
	return 0;
}

extern int print_string(char * i) {
	printf("%s\n", i);
	return 0;
}


extern int getarg(int i) {
	int arg_idx = i + g_optind;
	if(arg_idx >= g_argc) {
		printf("Error! Argument %d not found!\n", i);
		exit(EXIT_FAILURE);
	}
	return atoi(g_argv[arg_idx]);
}


extern float getargf(int i) {
	int arg_idx = i + g_optind;
	if(arg_idx >= g_argc) {
		printf("Error! Argument %d not found!\n", i);
		exit(EXIT_FAILURE);
	}
	return atof(g_argv[arg_idx]);
}


extern int check_overflow(int op1, int op2, arithop_type at) {
	int j;
	int o;
	switch(at) {
		case ArithTimesOp:
			if(__builtin_mul_overflow(op1, op2, &j))
				goto err;
			goto ok;
		case ArithPlusOp:
			if(__builtin_add_overflow(op1, op2, &j))
				goto err;
			goto ok;
		case ArithMinusOp:
			if(__builtin_sub_overflow(op1, op2, &j))
				goto err;
			goto ok;
		case ArithDivOp:
			if(op2 == 0 || (op1 == INT_MIN && op2 == -1))
				goto err;
			j = op1 / op2;
			goto ok;
	}
err:
	printf("OH NO!! OVERFLOW!!\n");	
	exit(EXIT_FAILURE);
ok:
	return j;
}
