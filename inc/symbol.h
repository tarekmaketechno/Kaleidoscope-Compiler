#pragma once 

#include <string.h>
#include "hashtable.h"
#include "types.h"

typedef struct symbol_table_t symbol_table_t;
typedef struct symbol_t symbol_t;
typedef struct var_symbol_t var_symbol_t;
typedef struct func_symbol_t func_symbol_t;

struct symbol_table_t {
	HashTable * current_scope;
	symbol_table_t * parent_scope;
};

struct symbol_t {
	char name[MAX_NAME_LENGTH];
	symbol_type st;
	data_type_type dt;
	symbol_table_t * context;
	void * value;
};

struct var_symbol_t {
	symbol_t super;
	// data_type duplicated here for convenience
	data_type_type dt;
	type_node_type tnt;
};

struct func_symbol_t {
	symbol_t super;
	ast_type_node_t ** params;
	// ret_type duplicated here for convenience
	data_type_type ret_type;
	int n_args;
};

symbol_t * create_anon_symbol(data_type_type dt);

symbol_t * create_var_symbol(const char * name, 
		data_type_type dt,
		type_node_type tnt);

symbol_t * create_func_symbol(data_type_type ret_type,
		const char * name, 
		ast_type_node_t ** params, int n_args);

void destroy_symbol_table(symbol_table_t * table);

void add_symbol(symbol_t * symbol, symbol_table_t * table);

symbol_t * lookup_symbol(const char * name, symbol_table_t * table);

symbol_t * copy_symbol(symbol_t * s);

symbol_table_t * create_symbol_table(symbol_table_t * parent);


