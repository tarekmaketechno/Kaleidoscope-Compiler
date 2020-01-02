/**************************************
 *
 * Private types that are only to be
 * referenced inside this file
 *
 * Typedef'ing everything up here 
 * for organization / clarity (and 
 * because some types have circular
 * dependencies).
 *
 *************************************/

typedef struct ast_if_stmt_node_t ast_if_stmt_node_t;
typedef struct ast_while_stmt_node_t ast_while_stmt_node_t;
typedef struct ast_return_stmt_node_t ast_return_stmt_node_t;
typedef struct ast_assign_stmt_node_t ast_assign_stmt_node_t;
typedef struct ast_blk_stmt_node_t ast_blk_stmt_node_t;
typedef struct ast_expression_stmt_node_t ast_expression_stmt_node_t;
typedef struct ast_print_stmt_node_t ast_print_stmt_node_t;

typedef struct ast_list_expression_node_t ast_list_expression_node_t; 
typedef struct ast_list_expression_node_t ast_list_expression_node_t;
typedef struct ast_binop_expression_node_t ast_binop_expression_node_t;
typedef struct ast_unop_expression_node_t ast_unop_expression_node_t;
typedef struct ast_literal_expression_node_t ast_literal_expression_node_t; 
typedef struct ast_varid_expression_node_t ast_varid_expression_node_t;
typedef struct ast_globid_expression_node_t ast_globid_expression_node_t;

typedef struct ast_binop_arithop_node_t ast_binop_arithop_node_t;
typedef struct ast_binop_logicop_node_t ast_binop_logicop_node_t;
typedef struct ast_binop_assignop_node_t ast_binop_assignop_node_t;
typedef struct ast_binop_castop_node_t ast_binop_castop_node_t;

typedef struct ast_string_literal_node_t ast_string_literal_node_t;
typedef struct ast_integer_literal_node_t ast_integer_literal_node_t;
typedef struct ast_float_literal_node_t ast_float_literal_node_t;
typedef struct ast_bool_literal_node_t ast_bool_literal_node_t;
typedef struct ast_tdecls_literal_node_t ast_tdecls_literal_node_t;

struct ast_program_node_t {
	ast_node_t super;
	ast_list_node_t * externs;
	ast_list_node_t * funcs;
};

struct ast_extern_node_t {
	ast_node_t super;
	ast_type_node_t * data_type;
	ast_globid_node_t * globid;
	ast_list_node_t * tdecls;
};

struct ast_func_node_t {
	ast_node_t super;
	ast_type_node_t * data_type;
	ast_globid_node_t * globid;
	ast_blk_node_t * blk;
	ast_list_node_t * vdecls;
};

struct ast_blk_node_t {
	ast_node_t super;
	ast_list_node_t * statements;
};

struct ast_stmt_node_t {
	ast_node_t super;
	statement_type stmt_type;
};

struct ast_if_stmt_node_t{
	ast_stmt_node_t super;
	ast_expression_node_t * condition;
	ast_stmt_node_t * body;
	ast_stmt_node_t * else_clause;
};

struct ast_while_stmt_node_t{
	ast_stmt_node_t super;
	ast_expression_node_t * condition;
	ast_stmt_node_t * body;
};

struct ast_return_stmt_node_t{
	ast_stmt_node_t super;
	ast_expression_node_t * body;
};

struct ast_assign_stmt_node_t{
	ast_stmt_node_t super;
	ast_vdecl_node_t * vdecl;
	ast_expression_node_t * body;
};

struct ast_blk_stmt_node_t {
	ast_stmt_node_t super;
	ast_blk_node_t * blk;
};

struct ast_expression_stmt_node_t{
	ast_stmt_node_t super;
	ast_expression_node_t * body;
};

struct ast_print_stmt_node_t {
	ast_stmt_node_t super;
	ast_expression_node_t * expression;
	ast_string_node_t * string;
};

struct ast_expression_node_t {
	ast_node_t super;
	expression_type exp_type;
};

struct ast_list_expression_node_t {
	ast_expression_node_t super;
	ast_list_node_t * list;
};

struct ast_binop_expression_node_t {
	ast_expression_node_t super;
	ast_binop_node_t * binop;
};

struct ast_unop_expression_node_t {
	ast_expression_node_t super;
	ast_unop_node_t * unop;
};

struct ast_literal_expression_node_t {
	ast_expression_node_t super;
	ast_literal_node_t * literal;
};

struct ast_varid_expression_node_t {
	ast_expression_node_t super;
	ast_varid_node_t * varid;
};

struct ast_globid_expression_node_t {
	ast_expression_node_t super;
	ast_globid_node_t * globid;
	ast_list_node_t * expressions;
};

struct ast_binop_node_t {
	ast_node_t super;
	binop_type bt;
};

struct ast_binop_arithop_node_t {
	ast_binop_node_t super;
	ast_arithop_node_t * arithop;
};

struct ast_binop_logicop_node_t {
	ast_binop_node_t super;
	ast_logicop_node_t * logicop;
};

struct ast_binop_assignop_node_t {
	ast_binop_node_t super;
	ast_varid_node_t * varid;
	ast_expression_node_t * expression;
};

struct ast_binop_castop_node_t {
	ast_binop_node_t super;
	ast_typename_node_t * typename;
	ast_expression_node_t * expression;
};

struct ast_arithop_node_t {
	ast_node_t super;
	arithop_type at;
	ast_expression_node_t * op1;
	ast_expression_node_t * op2;
};

struct ast_logicop_node_t {
	ast_node_t super;
	logicop_type lot;
	ast_expression_node_t * op1;
	ast_expression_node_t * op2;
};

struct ast_unop_node_t{
	ast_node_t super;
	unop_type ut;
	ast_expression_node_t * operand;
};

struct ast_type_node_t{
	ast_node_t super;
	type_node_type tnt;
	ast_typename_node_t * tnn;
};

struct ast_vdecl_node_t{
	ast_node_t super;
	ast_type_node_t * type_node;
	ast_varid_node_t * varid;
};

struct ast_literal_node_t{
	ast_node_t super;
	literal_node_type lnt;
};

struct ast_string_literal_node_t {
	ast_literal_node_t super;
	ast_string_node_t * string_node;
};

struct ast_integer_literal_node_t {
	ast_literal_node_t super;
	ast_integer_node_t * integer_node;
};

struct ast_float_literal_node_t {
	ast_literal_node_t super;
	ast_float_node_t * float_node;
};

struct ast_bool_literal_node_t {
	ast_literal_node_t super;
	ast_bool_node_t * bool_node;
};

struct ast_tdecls_literal_node_t {
	ast_literal_node_t super;
	ast_list_node_t * tdecls_list;
};

struct ast_typename_node_t{
	ast_node_t super;
	data_type_type t;
};

struct ast_string_node_t{
	ast_node_t super;
	const char * charstar;
};

struct ast_globid_node_t{
	ast_node_t super;
	const char * charstar;
};

struct ast_varid_node_t{
	ast_node_t super;
	const char * charstar;
};

struct ast_integer_node_t{
	ast_node_t super;
	int integer_value;
};

struct ast_float_node_t{
	ast_node_t super;
	float float_value;
};

struct ast_bool_node_t{
	ast_node_t super;
	int bool_value;
};


