#include <getopt.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

#include "generator.h"
#include "bindings.h"
#include "ekcc.tab.h"
#include "printer.h"
#include "ast.h"
#include "optimizer.h"

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>
#include <time.h>

extern int yylex();
extern int yyparse();
extern FILE *yyin;

void yyerror(ast_node_t ** prog, const char *s);

int g_argc;
int g_optind;
char ** g_argv;

int print_help() {
	printf("Kaleidoscope Compiler by Tarek Sami\n");
	printf("Usage:\n");
	printf("/ekcc [-h] [-v] [-O] [-emit-ast|-emit-llvm] <input-file>\n");
	return 0;
}

int64_t timestamp_now (void) {
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return (int64_t) tv.tv_sec * CLOCKS_PER_SEC + tv.tv_usec;
}

double timestamp_to_seconds (int64_t timestamp) {
	return timestamp / (double) CLOCKS_PER_SEC;
}

int main(int argc, char ** argv) {

	// Parse Options
	int emit_ast = 0;
	int emit_llvm = 0;
	int optimize_level = 0;
	int opt_idx = 0;
	char c;

	struct option opts[] = {
		{"emit-ast", optional_argument, &emit_ast, 1},
		{"emit-llvm", optional_argument, &emit_llvm, 1},
		{0, 0, 0, 0}
	};

	while(-1 != (c = getopt_long(argc, argv, "Ol:hf::", opts, &opt_idx))){
		switch(c){
			case 'O':
				printf("Turning on optimizer!\n");
				if(optimize_level == 0)
					optimize_level = 1;
				// optimize_level = atoi(optarg);
				// printf("Optimization level set at %d\n", optimize_level);
				break;
			case 'l':
				optimize_level = atoi(optarg);
				printf("Setting optimization level to %d\n", optimize_level);
				break;
			case 'h':
				print_help();
				break;
			default:
				break;
		}
	}

	// Open File
	char * filename;
	
	if (optind >= argc) {
		printf("No filename specified!\n");
		exit(EXIT_FAILURE);
	} else {
		filename = strdup(argv[optind]);
		optind++;
	}

	FILE * input_file = fopen(filename, "r");
	if(!input_file) {
		printf("Error! I can't open your file!\n");
		exit(EXIT_FAILURE);
	}

	yyin = input_file;

	ast_node_t ** p = 
		(ast_node_t **) malloc(sizeof(void *));

	yyparse(p);
	fclose(input_file);
	free(filename);

	// Send remaining options to gvars
	g_argc = argc;
	g_argv = argv;
	g_optind = optind;

	// Build symbol table (and do type checks)
	(*p)->vtable->symbol(*p, NULL);

	// Set up LLVM
	generator_t * gen = initialize_llvm();

	// Generate Code
	(*p)->vtable->codegen(*p, gen); 

	if(emit_ast) {
		(*p)->vtable->print((*p), 0);
	}

	if(emit_llvm) {
		LLVMDumpModule(gen->module);
	}


	// Create an empty buffer to store any error message
	char *error = NULL;

	// Verify module (not entirely sure what checks this does)
	LLVMVerifyModule(gen->module, LLVMAbortProcessAction, &error);

	// Dispose of error message?
	LLVMDisposeMessage(error);


	int64_t opt_dur = 0;
	if(optimize_level) {
		int64_t opt_start = timestamp_now();
		optimize(gen, optimize_level);
		opt_dur = timestamp_now() - opt_start;
	}
	printf("OPTIMIZATIONS TOOK %f SECONDS\n", timestamp_to_seconds(opt_dur));

	int64_t exec_start = timestamp_now();
	int rtn = validate_and_run(gen);
	int64_t exec_dur = timestamp_now() - exec_start;
	printf("EXECUTION TOOK %f SECONDS\n", timestamp_to_seconds(exec_dur));

	(*p)->vtable->destroy((*p));
	free(p);

	return rtn;
}


