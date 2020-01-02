#pragma  once

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdbool.h>
#include "types.h"
#include "symbol.h"
#include "generator.h"
#include "bindings.h"


/****************************
 *
 * Public struct definitions
 *
 ****************************/

struct ast_node_vtable {
	void (*destroy)(ast_node_t *);
	void (*print)(ast_node_t *, int);
	symbol_t * (*symbol)(ast_node_t *, symbol_table_t *);
	void (*codegen)(ast_node_t *, generator_t *);
};

struct ast_node_t {
	node_type type;
	ast_node_vtable * vtable;
	ast_node_t ** children;
	int n_children;
	symbol_t * s;
};

struct ast_list_node_t {
	ast_node_t super;
	ast_node_t * current;
	ast_list_node_t * next;
}; 

/************************************
 * Misc function prototypes
 * for public functions consumed
 * in the parser
 ************************************/

ast_program_node_t * construct_program_node(
		ast_list_node_t * externs,
		ast_list_node_t * funcs);

ast_extern_node_t * construct_extern_node(
		ast_type_node_t * data_type,
		ast_globid_node_t * globid,
		ast_list_node_t * tdecls);

ast_func_node_t * construct_func_node(
		ast_type_node_t * data_type,
		ast_globid_node_t * globid,
		ast_list_node_t * vdecls,
		ast_blk_node_t * blk);

ast_blk_node_t * construct_blk_node(
		ast_list_node_t * statements);

ast_stmt_node_t * construct_stmt_node(
		statement_type t,
		int count, ...);

ast_expression_node_t * construct_expression_node(
		expression_type et,
		int count, ...);

ast_binop_node_t * construct_binop_node(
		binop_type bo_type, 
		int count, ...);

ast_arithop_node_t *  construct_arithop_node(
		arithop_type at,
		ast_expression_node_t * op1,
		ast_expression_node_t * op2);

ast_logicop_node_t * construct_logicop_node(
		logicop_type lot,
		ast_expression_node_t * op1,
		ast_expression_node_t * op2);

ast_unop_node_t * construct_unop_node(
		unop_type ut,
		ast_expression_node_t * operand);

ast_type_node_t * construct_type_node(
		type_node_type tnt,
		ast_typename_node_t * tnn);

ast_vdecl_node_t * construct_vdecl_node(
		ast_type_node_t * type,
		ast_varid_node_t * varid);

ast_literal_node_t * construct_literal_node(
		literal_node_type lnt, ...);

ast_typename_node_t * construct_typename_node(
		data_type_type t);

ast_string_node_t * construct_string_node(
		const char * charstar);

ast_globid_node_t * construct_globid_node(
		const char * charstar);

ast_varid_node_t * construct_varid_node(
		const char * charstar);

ast_integer_node_t * construct_integer_node(
		int integer_value);

ast_float_node_t * construct_float_node(
		float float_value);

ast_bool_node_t * construct_bool_node(
		int bool_value);

ast_list_node_t * construct_list_node(
		ast_node_t * current);

void append_ast_list(ast_list_node_t * list, 
		ast_node_t * newthing);

void walk_tree_pre (void (*func)(ast_node_t *), 
		ast_node_t * node);

void walk_tree_pre_w_data(void (*func)(ast_node_t *, void *), 
		void * data, ast_node_t * node);

