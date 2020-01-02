#include "../inc/ast.h"
#include "../inc/list.h"


typedef struct ekcc_symbol_table_t {
	list_item_t * externs;
	list_item_t * funcs;
	list_item_t * vars;
} ekcc_symbol_table_t;





