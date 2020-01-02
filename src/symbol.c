#include "symbol.h"
#include <stdio.h>
#include <stdlib.h>

static unsigned long sdbm(const char * str) {

	unsigned long hash = 0;
	int c;

	while ((c = *str++))
		hash = c + (hash << 6) + (hash << 16) - hash;

	return hash;
}


symbol_t * create_anon_symbol(data_type_type dt) {
	symbol_t * symbol = 
		(symbol_t *) malloc(sizeof(symbol_t));

	symbol->st = Anon_Symb;
	symbol->dt = dt;
	symbol->value = NULL;
	return symbol;
}


symbol_t * create_var_symbol(const char * name, 
		data_type_type dt,
		type_node_type tnt) {

	symbol_t * symbol = 
		(symbol_t *) malloc(sizeof(var_symbol_t));

	strncpy(symbol->name, name, MAX_NAME_LENGTH);
	symbol->st = Varid_Symb;
	symbol->dt = dt;
	symbol->value = NULL;

	var_symbol_t * var_symb = (var_symbol_t *) symbol;
	var_symb->dt = dt;
	var_symb->tnt = tnt;

	return symbol;
}

symbol_t * create_func_symbol(data_type_type ret_type,
		const char * name, 
		ast_type_node_t ** params, int n_args) {

	symbol_t * symbol = 
		(symbol_t *) malloc(sizeof(func_symbol_t));

	symbol->st = Globid_Symb;
	symbol->dt = ret_type;
	symbol->value = NULL;
	strncpy(symbol->name, name, MAX_NAME_LENGTH);

	func_symbol_t * func_symb = (func_symbol_t *) symbol;
	func_symb->ret_type = ret_type;

	func_symb->params = params;
	func_symb->n_args = n_args;

	return symbol;
}


void add_symbol(symbol_t * symbol, symbol_table_t * table) {
	unsigned long key = sdbm(symbol->name);
	ht_insert(table->current_scope, &key, &symbol);
	symbol->context = table;
}


/********************************************************
 * There is something weird going on with the ht...
 * that's why all of the weird pointer arithmetic
 *******************************************************/

symbol_t * lookup_symbol(const char * name, symbol_table_t * table) {
	unsigned long key = sdbm(name);

	symbol_t ** rtn;
	while (table->current_scope != NULL) {
		if ((rtn = (symbol_t **) ht_lookup(table->current_scope, &key)) != NULL) {
			return *rtn;
		}
		if (table->parent_scope == NULL) {
			return NULL;
		}
		table = table->parent_scope;
	}

	// IDK How you could get here, but 
	// this should shut up the warning
	return NULL;

}


symbol_table_t * create_symbol_table(symbol_table_t * parent) {
	symbol_table_t * st = (symbol_table_t *) malloc(sizeof(symbol_table_t));
	HashTable * ht = (HashTable *) malloc(sizeof(HashTable));
	ht_setup(ht, sizeof(unsigned long), sizeof(symbol_t *), 30);
	ht_reserve(ht, 300);

	st->current_scope = ht;
	st->parent_scope = parent;
	return st;
}


void destroy_symbol_table(symbol_table_t * table) {
	if (table->parent_scope != NULL) {
		destroy_symbol_table(table->parent_scope);
	}
	ht_destroy(table->current_scope);
}
