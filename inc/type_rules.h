#pragma once

#ifndef MAX_NAME_LENGTH
#define MAX_NAME_LENGTH 128
#endif

#include "ast.h"


typedef enum type_error_type {
	void_vdecl, func_not_decl, void_ref, ref_func,
	ref_var_init, multi_run_func, run_func_sig,
	no_run_func, undecl_func, run_func_args
} type_error_type;

typedef struct type_error_t {
	type_error_type t;
	const char * msg;
} type_error_t;

int check_vdecl_type_not_void(ast_node_t * node);

int check_ref_type_not_void(ast_node_t * node);

int check_functions_declared_before_use(ast_node_t * node);

int check_functions_do_not_return_ref(ast_node_t * node);

int check_ref_var_initializer_is_variable(ast_node_t * node);

int check_run_function(ast_node_t * node);

int report_type_error(type_error_t * err);

