#include "../inc/type_rules.h"
#include "ast.h"
#include <string.h>


int report_type_error(type_error_t * err) {
	printf("Error: %s\n", err->msg);
	exit(-1);
}

void vdecl_type_not_void(ast_node_t * node) {
	if(node->type != VDecl) {
		return;
	}
	ast_vdecl_node_t * vd = ((ast_vdecl_node_t *)node);
	data_type_type t = get_vdecl_data_type(vd);
	
	if(t == Void_type) {
		type_error_t err = {void_vdecl, "Variable declaration cannot have void type"}; 
		report_type_error(&err);
	}
	return;
}


int check_vdecl_type_not_void(ast_node_t * node) {
	walk_tree_pre(vdecl_type_not_void, node);
	return 0;
}


void ref_type_not_void(ast_node_t * node) {
	if(node->type != Type) {
		return;
	}
	ast_type_node_t * tn = ((ast_type_node_t *)node);
	type_node_type tnt = get_type_node_type(tn);
	data_type_type typ = get_type_node_datatype(tn);
	
	if(tnt == RefType && typ == Void_type) {
		type_error_t err = {void_ref, "Ref variable cannot have void type"}; 
		report_type_error(&err);
	}
	return;
}


int check_ref_type_not_void(ast_node_t * node) {
	walk_tree_pre(ref_type_not_void, node);
	return 0;
}


void function_no_return_ref(ast_node_t * node) {
	if(node->type != Func) {
		return;
	}
	ast_func_node_t * f = ((ast_func_node_t *)node);
	ast_type_node_t * t = get_func_type(f);
	type_node_type tnt = get_type_node_type(t);
	if(tnt == RefType) {
		type_error_t err = {ref_func, "Function cannot return a ref"}; 
		report_type_error(&err);
	}
	return;
}


int check_functions_do_not_return_ref(ast_node_t * node) {
	walk_tree_pre(function_no_return_ref, node);
	return 0;
}


void ref_var_initializer_is_var(ast_node_t * node) {
	if(ref_var_init_is_not_var(node)){
		type_error_t err = {ref_var_init, "Function cannot return a ref"}; 
		report_type_error(&err);
	}
}


int check_ref_var_initializer_is_variable(ast_node_t * node) {
	walk_tree_pre(ref_var_initializer_is_var, node);
	return 0;
}


typedef struct run_fn_struct {
	int found;
	int has_correct_sig;
} run_fn_struct;


void run_function_walker(ast_node_t * node, void * rfv) {
	if(node->type != Func) {
		return;
	}
	run_fn_struct * rfn = (run_fn_struct *)rfv;
	ast_func_node_t * fun = (ast_func_node_t *)node;

	char buffer[MAX_NAME_LENGTH];
	ast_globid_node_t * globid = get_func_globid(fun);
	get_globid_charstar(globid, buffer);
	if(strncmp(buffer, "run", MAX_NAME_LENGTH) == 0) {
		if(rfn->found == 1) {
			type_error_t err = 
				{multi_run_func, "Program must have only one run function"};
			report_type_error(&err);
		}
		rfn->found = 1;
	} else {
		return;
	}

	ast_type_node_t * t = get_func_type(fun);
	data_type_type dt = get_type_node_datatype(t);
	if(dt != Int_type) {
		type_error_t err = 
			{run_func_sig, "Run function signature must by \"int\""};
		report_type_error(&err);
	} else {
		rfn->has_correct_sig = 1;
	}

	ast_list_node_t * vd = get_func_node_vdecls(fun);
	if (vd != NULL) {
		type_error_t err =
			{run_func_args, "Run function must take no arguments"};

		report_type_error(&err);
	}

	return;
}

int check_run_function(ast_node_t * node){
	run_fn_struct rfn = {0, 0};
	walk_tree_pre_w_data(run_function_walker, (void*)&rfn, node);
	if(rfn.found == 0) {
		type_error_t err = 
			{no_run_func, "Kaleidoscope programs must each have one \"run\" function!"};
		report_type_error(&err);
	}
	return 0;
}


typedef struct function_count_struct {
	ast_list_node_t * globids;
} function_count_struct;


void globid_walker(ast_node_t * node, void * data) {
	function_count_struct * fls = ((function_count_struct *)data);
	ast_globid_node_t * globid; 
	
	ast_list_node_t * current = fls->globids;
	char buf1[MAX_NAME_LENGTH];
	char buf2[MAX_NAME_LENGTH];

	switch(node->type) {
		case Func:
			globid = get_func_globid((ast_func_node_t *)node);
			break;

		case Extern:
			globid = get_extern_globid((ast_extern_node_t *)node);
			break;

		case Exp:
		{
			if(get_expression_type((ast_expression_node_t *) node) 
					!= GlobIdExpression) return;

			globid = get_globid_expression_globid(node); 
			get_globid_charstar(globid, buf1);

			while(current != NULL) {
				get_globid_charstar(((ast_globid_node_t *)current->current), buf2);
				if(strncmp(buf1, buf2, MAX_NAME_LENGTH) == 0) return;
				current = current->next;
			}
			
			char msg[512];
			sprintf(msg, "Function definition not found: %s", buf1);
			type_error_t err =  { undecl_func, msg };
			report_type_error(&err);
			
			return;
		}
		default:
			return;
	}

	if(fls->globids == NULL) {
		fls->globids = construct_list_node((ast_node_t *)globid);
	} else {
		append_ast_list(fls->globids, ((ast_node_t *) globid));
	}


}


int check_functions_declared_before_use(ast_node_t * node) {
	function_count_struct fls = { NULL };
	walk_tree_pre_w_data(globid_walker, (void*)&fls, node);
	return 0;
}

