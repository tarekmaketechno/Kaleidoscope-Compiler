#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include "ast.h"
#include "block.hpp"
#include "printer.h"
#include "ast_private.c"
#include "bindings.h"

#include <llvm-c/Core.h>

/*******************************************
 *
 * Misc. Utility Functions
 *
 * (There used to be more stuff here)
 *
 ******************************************/

int is_valid_cast(data_type_type op1, data_type_type op2) {
	switch(op1) {
		case Int_type:
			{
				switch(op2) {
					case Int_type:
					case Cint_type:
					case Float_type:
						return 1;
					case Bool_type:
						printf("Cannot cast int to bool!\n");
					case Void_type:
						printf("Cannot cast int to void!\n");
					case Char_type:
						printf("Cannot cast int to char!\n");
						return 0;
					default:
						printf("Unrecognized type!\n");
						return 0;
				}

			}
		case Cint_type:
			{
				switch(op2) {
					case Int_type:
					case Cint_type:
					case Float_type:
						return 1;
					case Bool_type:
					case Void_type:
					case Char_type:
						return 0;
					default:
						printf("ERROR! Unrecognized type!\n");
						return 1;
				}

			}
		case Float_type:
			{
				switch(op2) {
					case Int_type:
					case Cint_type:
					case Float_type:
						return 1;
					case Bool_type:
					case Void_type:
					case Char_type:
						return 0;
					default:
						printf("ERROR! Unrecognized type!\n");
						return 0;
				}

			}
		case Bool_type:
			{
				switch(op2) {
					case Int_type:
					case Cint_type:
					case Void_type:
					case Float_type:
					case Char_type:
						return 0;
					case Bool_type:
						return 1;
					default:
						printf("ERROR! Unrecognized type!\n");
						return 0;
				}

			}
		case Void_type:
		case Char_type:
			printf("ERROR! Invalid type for cast\n");
			exit(EXIT_FAILURE);
	}
}


void add_children(ast_node_t * node, int count, ...) {
	if(count == 0) {
		node->n_children = 0;
		node->children = NULL;
		return;
	}

	va_list ap;
	va_start(ap, count);

	ast_node_t ** pr_children = 
		(ast_node_t **) 
		calloc(count, sizeof(ast_node_t **));

	ast_node_t ** copy = pr_children;

	int n_children = 0;
	for(int i = 0; i < count; i++) {
		ast_node_t * child = va_arg(ap, ast_node_t*); 
		if(child != NULL) {
			*pr_children = child;
			pr_children++;
			n_children++;
		}
	}

	ast_node_t ** children = 
		(ast_node_t **) 
		calloc(n_children, sizeof(ast_node_t *));

	memcpy(children, copy, sizeof(ast_node_t **) * n_children);
	free(copy);

	node->n_children = n_children;
	node->children = children;
	va_end(ap);
	return;
}


/*********************************
 * Generic vtable 
 *
 * vtable functions for plain-
 * Jane node types
 *
 ********************************/

void type_aware_delete(ast_node_t * node);

void delete_tree(ast_node_t * node) {
	if(node == NULL) return;

	ast_node_t * child;
	for (int i = 0; i < node->n_children; i++) {
		child = node->children[i];

		if(child != NULL)
			child->vtable->destroy(child);
	}
	free(node->children);
	type_aware_delete(node);
	return;
}

void print_ast_node(ast_node_t * node, int depth) {
	if(node == NULL) return;

	for(int i = 0; i < depth; i++) {
		printf("  ");
	}
	char buffer[128];
	translate_type(node, buffer);
	printf("%s\n", buffer);


	if(node->type == List) {

		ast_node_t * current = 
			((ast_node_t *)((ast_list_node_t *)node)->current);
		if(current != NULL) current
			->vtable->print(current, depth+1);

		ast_list_node_t * next = 
			((ast_list_node_t *)((ast_list_node_t *)node)->next);
		if(next != NULL) ((ast_node_t *)next)
			->vtable->print((ast_node_t *)next, depth);

		return;
	}

	ast_node_t * child;
	for (int i = 0; i < node->n_children; i++) {
		child = node->children[i];

		if(child != NULL)
			child->vtable->print(child, depth+1);
	}

	return;
}


/**********************************************************************|
 *
 * This is the generic symbol implemenation, which should work for
 * most node types... It returns the symbol for the last child it
 * call on, to provide an easy-ish way to pass symbols back
 * up the tree, and removes the need to write out a symbol function
 * for each node that would essentially do the same.
 *
 * Keep in mind then, that children should always be added so the thing
 * that should be returned comes last in the array.
 *
 **********************************************************************/

symbol_t * ast_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_node_t * child;
	symbol_t * symb = NULL;
	for (int i = 0; i < node->n_children; i++) {
		child = node->children[i];

		if(child != NULL)
			symb = child->vtable->symbol(child, st);
	}

	if (symb != NULL) {
		node->s = create_anon_symbol(symb->dt);
		node->s->context = st;
	}
	return node->s;
}


/******************************************************************|
 * Generic codegen=================================================|
 * For nodes that don't really do anything special... can't 
 * imagine what those would be though...
 ******************************************************************/

void ast_codegen(ast_node_t * node, generator_t * gen) {
	ast_node_t * child = NULL;
	for (int i = 0; i < node->n_children; i++) {
		child = node->children[i];

		if(child != NULL)
			child->vtable->codegen(child, gen);
	}
	if(child != NULL && child->s != NULL) {
		node->s->value = child->s->value; 
	}
	return;
}

// Generic vtable instance
ast_node_vtable ast_vtable = {
	delete_tree,
	print_ast_node,
	ast_symbol,
	ast_codegen
};

/*******************************************
 *
 * Constructors and type-specific functions
 *
 ******************************************/

// Program Node

symbol_t * program_symbol(ast_node_t * node, symbol_table_t * st) {
	if(st == NULL) {
		st = create_symbol_table(NULL);
	}

	ast_node_t * child;
	for (int i = 0; i < node->n_children; i++) {
		child = node->children[i];

		if(child != NULL)
			child->vtable->symbol(child, st);
	}
	node->s = create_anon_symbol(Int_type);
	node->s->context = st;
	return node->s;
}


void program_codegen(ast_node_t * node, 
		generator_t * gen) {

	/* Construct Print Functions */
	LLVMTypeRef param_types[1] = {llvm_data_type(Int_type)};

	LLVMTypeRef ret_type = LLVMFunctionType(
			llvm_data_type(Int_type),
			param_types, 1, 0);

	LLVMValueRef l_func = LLVMAddFunction(
			gen->module, 
			"print_int",
			ret_type);

	LLVMSetLinkage(l_func, LLVMExternalLinkage);

	// Print Float
	param_types[0] = llvm_data_type(Float_type);

	ret_type = LLVMFunctionType(
			llvm_data_type(Float_type),
			param_types, 1, 0);

	l_func = LLVMAddFunction(
			gen->module, 
			"print_float",
			ret_type);

	LLVMSetLinkage(l_func, LLVMExternalLinkage);

	// Print Bool 
	param_types[0] = llvm_data_type(Bool_type);

	ret_type = LLVMFunctionType(
			llvm_data_type(Bool_type),
			param_types, 1, 0);

	l_func = LLVMAddFunction(
			gen->module, 
			"print_bool",
			ret_type);

	LLVMSetLinkage(l_func, LLVMExternalLinkage);

	// Print String 
	param_types[0] = llvm_data_type(Char_type);

	ret_type = LLVMFunctionType(
			llvm_data_type(Int_type),
			param_types, 1, 0);

	l_func = LLVMAddFunction(
			gen->module, 
			"print_string",
			ret_type);

	LLVMSetLinkage(l_func, LLVMExternalLinkage);

	// 
	// Add Overflow Check
	//
	LLVMTypeRef oparam_types[3] = {
		llvm_data_type(Int_type),
		llvm_data_type(Int_type),
		llvm_data_type(Int_type),
	};

	ret_type = LLVMFunctionType(
			llvm_data_type(Int_type),
			oparam_types, 3, 0);

	l_func = LLVMAddFunction(
			gen->module,
			"check_overflow",
			ret_type);

	/* Call childrens' codegen functions */
	ast_node_t * child = NULL;
	for (int i = 0; i < node->n_children; i++) {
		child = node->children[i];

		if(child != NULL)
			child->vtable->codegen(child, gen);
	}
	if(child != NULL && child->s != NULL) {
		node->s->value = child->s->value; 
	}

	return;
}


ast_node_vtable program_vtable = {
	delete_tree,
	print_ast_node,
	program_symbol,
	program_codegen

};

ast_program_node_t * construct_program_node(
				ast_list_node_t * externs,
				ast_list_node_t * funcs) {

	ast_program_node_t * node = 
		(ast_program_node_t *) 
		malloc(sizeof(ast_program_node_t));

	((ast_node_t *)node)->type = Program;
	((ast_node_t *)node)->vtable = &program_vtable;
	((ast_node_t *)node)->s = NULL;


	node->externs = externs;
	node->funcs = funcs;
	add_children(((ast_node_t *)node), 2, externs, funcs);
	return node;
}


// Extern node

symbol_t * extern_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_extern_node_t * ext = (ast_extern_node_t *)node;
	symbol_t * ext_symb = lookup_symbol(ext->globid->charstar, st);

	if (ext_symb != NULL) {
		// An error has occurred - can't reuse func names
		printf("ERROR: Attempted to redeclare function %s! "
				"Exiting...\n", ext->globid->charstar);
		exit(EXIT_FAILURE);

	}

	int n_args = 0;
	ast_list_node_t * cur = ext->tdecls;
	while (cur != NULL) {
		n_args ++;
		cur = cur->next;
	}

	ast_type_node_t ** params = (ast_type_node_t **) 
		malloc(sizeof(void *) * n_args);

	cur = ext->tdecls;
	int i = 0;
	while (cur != NULL) {
		ast_type_node_t * tdecl = 
			(ast_type_node_t *)(cur->current);
		params[i] = tdecl;
		i++;
		cur = cur->next;
	}

	data_type_type ret_type = ext->data_type->tnn->t;
	// TODO: check that return type is not ref
	ext_symb = create_func_symbol(ret_type,
			ext->globid->charstar,
			params, n_args);

	add_symbol(ext_symb, st);
	node->s = ext_symb;
	return ext_symb;
}

void extern_codegen(ast_node_t * node, generator_t * gen) {
	ast_extern_node_t * ext = (ast_extern_node_t *)node;
	func_symbol_t * ext_symb = 
		(func_symbol_t *) 
		lookup_symbol(ext->globid->charstar, node->s->context);


	// Get array of param types to build function prototype
	LLVMTypeRef param_types[ext_symb->n_args];
	for (int i = 0; i < ext_symb->n_args; i++) {
		switch(ext_symb->params[i]->tnt) {
			case PlainType:
				param_types[i] = 
					llvm_data_type(ext_symb->params[i]->tnn->t);
				break;

			case RefType: 
			case NoaRefType:
				param_types[i] = 
					LLVMPointerType(
						llvm_data_type(
							ext_symb->params[i]->tnn->t), 0);
				break;
		}

	}

	LLVMTypeRef ret_type = LLVMFunctionType(
			llvm_data_type(ext_symb->ret_type), 
			param_types, ext_symb->n_args, 0);

	LLVMValueRef l_func = LLVMAddFunction(
			gen->module, 
			ext->globid->charstar,
			ret_type);

	LLVMSetLinkage(l_func, LLVMExternalLinkage);

}

ast_node_vtable extern_vtable = {
	delete_tree,
	print_ast_node,
	extern_symbol,
	extern_codegen

};

ast_extern_node_t * construct_extern_node(
		ast_type_node_t * data_type,
		ast_globid_node_t * globid,
		ast_list_node_t * tdecls) {

	ast_extern_node_t * node = (ast_extern_node_t *) 
		malloc(sizeof(ast_extern_node_t));

	((ast_node_t *) node)->type = Extern;
	((ast_node_t *)node)->vtable = &extern_vtable;
	((ast_node_t *)node)->s = NULL;
	node->data_type = data_type;
	node->globid = globid;
	node->tdecls = tdecls;
	add_children(((ast_node_t *)node), 3, 
			data_type, globid, tdecls);
	return node;
}


// Function node

symbol_t * func_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_func_node_t * func = (ast_func_node_t *)node;
	symbol_t * func_symb = lookup_symbol(func->globid->charstar, st);

	if (func_symb != NULL) {
		printf("ERROR: Attempted to redeclare function %s! "
				"Exiting...\n", func->globid->charstar);
		exit(EXIT_FAILURE);
	}

	int n_args = 0;
	ast_list_node_t * cur = func->vdecls;
	while (cur != NULL) {
		n_args ++;
		cur = cur->next;
	}

	ast_type_node_t ** params = (ast_type_node_t **) 
		malloc(sizeof(void *) * n_args);

	cur = func->vdecls;

	int i = 0;

	while (cur != NULL) {
		ast_vdecl_node_t * vdecl = 
			(ast_vdecl_node_t *)(cur->current);
		params[i] = vdecl->type_node;
		i++;
		cur = cur->next;
	}

	data_type_type ret_type = func->data_type->tnn->t;
	func_symb = create_func_symbol(ret_type,
			func->globid->charstar,
			params, n_args);

	add_symbol(func_symb, st);

	// Call symbol for all of the variable 
	// declarations, put them in a new scope
	symbol_table_t * cst = create_symbol_table(st);
	cur = func->vdecls;
	while(cur != NULL) {
		cur->current->vtable->symbol(cur->current, cst);
		cur = cur->next;
	}

	ast_node_t * blk = (ast_node_t *)(func->blk);
	blk->vtable->symbol(blk, cst);

	node->s = func_symb;
	node->s->context = cst;
	return func_symb;
}


void func_codegen(ast_node_t * node, generator_t * gen) {
	ast_func_node_t * func = (ast_func_node_t *)node;
	func_symbol_t * func_symb = 
		(func_symbol_t *) 
		lookup_symbol(func->globid->charstar, node->s->context);


	// Get array of param types to build function prototype
	LLVMTypeRef param_types[func_symb->n_args];
	for (int i = 0; i < func_symb->n_args; i++) {

		switch(func_symb->params[i]->tnt) {
			case PlainType:
				param_types[i] = 
					llvm_data_type(func_symb->params[i]->tnn->t);
				break;

			case RefType: 
			case NoaRefType:
				param_types[i] = 
					LLVMPointerType(
						llvm_data_type(
							func_symb->params[i]->tnn->t), 0);
				break;
		}
	}

	LLVMTypeRef ret_type = LLVMFunctionType(
			llvm_data_type(func_symb->ret_type), 
			param_types, func_symb->n_args, 0);

	LLVMValueRef l_func = LLVMAddFunction(
			gen->module, 
			func->globid->charstar,
			ret_type);

	LLVMBasicBlockRef entry = 
		LLVMAppendBasicBlock(l_func, "entry");

	LLVMPositionBuilderAtEnd(gen->builder, entry);

	ast_list_node_t * cur = func->vdecls;
	symbol_t * vdecl_symb = NULL;
	ast_node_t * vdecl_node = NULL;
	for (int i = 0; i < func_symb->n_args; i++) {

		vdecl_node = (ast_node_t *) cur->current;
		vdecl_node->vtable->codegen(vdecl_node, gen);

		var_symbol_t * vdecl_var_symb = 
			(var_symbol_t *) cur->current->s;

		symbol_t * vdecl_symb = cur->current->s;

		switch(vdecl_var_symb->tnt) {
			case PlainType:
				{
					LLVMBuildStore(gen->builder, 
							LLVMGetParam(l_func, i), 
							vdecl_symb->value);
					break;
				}
			case RefType: 
			case NoaRefType:
				{
					vdecl_symb->value = 
						LLVMGetParam(l_func, i);
					break;
				}
		}

		cur = cur->next;
	}

	ast_node_t * blk = (ast_node_t *)(func->blk);

	LLVMPositionBuilderAtEnd(gen->builder, entry);

	blk->vtable->codegen(blk, gen);

	LLVMPositionBuilderAtEnd(gen->builder, blk->s->value);
	LLVMBuildUnreachable(gen->builder);

	return;
}


ast_node_vtable func_vtable = {
	delete_tree,
	print_ast_node,
	func_symbol,
	func_codegen

};

ast_func_node_t * construct_func_node(
		ast_type_node_t * data_type,
		ast_globid_node_t * globid,
		ast_list_node_t * vdecls,
		ast_blk_node_t * blk) {

	ast_func_node_t * node = (ast_func_node_t *) 
		malloc(sizeof(ast_func_node_t));

	((ast_node_t *)node)->type = Func;
	((ast_node_t *)node)->vtable = &func_vtable;
	((ast_node_t *)node)->s = NULL;
	node->data_type = data_type;
	node->globid = globid;
	node->vdecls = vdecls;
	node->blk = blk;
	add_children(((ast_node_t *)node), 4, 
			data_type, globid, vdecls, blk);
	return node;
}

// Block node

symbol_t * blk_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_blk_node_t * blk = (ast_blk_node_t *)node;

	symbol_table_t * cst = create_symbol_table(st);
	
	ast_list_node_t * cur = blk->statements;
	symbol_t * symb = NULL;
	while(cur != NULL) {
		symb = cur->current->vtable->symbol(cur->current, cst);
		cur = cur->next;
	}

	node->s = symb;
	return symb;
}


void blk_codegen(ast_node_t * node, generator_t * gen) {
	ast_blk_node_t * blk = (ast_blk_node_t *)node;
	
	LLVMBasicBlockRef insert = 
		LLVMGetInsertBlock(gen->builder);
	
	LLVMValueRef func = LLVMGetBasicBlockParent(
			insert);

	static int ctr = 0;
	char label[128];
	sprintf(label, "generic_blk_start-%d", ctr++);
	LLVMBasicBlockRef new_blk = 
		LLVMAppendBasicBlock(func, label);

	LLVMBuildBr(gen->builder, new_blk);
	LLVMPositionBuilderAtEnd(gen->builder, new_blk);

	ast_list_node_t * cur = blk->statements;
	while(cur != NULL) {
		
		cur->current->vtable
			->codegen(cur->current, gen);
		cur = cur->next;
	}

	sprintf(label, "generic_blk_end-%d", ctr++);
	LLVMBasicBlockRef end_blk = 
		LLVMAppendBasicBlock(func, label);

	LLVMBuildBr(gen->builder, end_blk);
	LLVMPositionBuilderAtEnd(gen->builder, end_blk);

	node->s->value = end_blk;
}


ast_node_vtable blk_vtable = {
	delete_tree,
	print_ast_node,
	blk_symbol,
	blk_codegen
};

ast_blk_node_t * construct_blk_node(
	ast_list_node_t * statements) {

	ast_blk_node_t * node = 
		(ast_blk_node_t *) malloc(sizeof(ast_blk_node_t));

	((ast_node_t *)node)->type = Blk;
	((ast_node_t *)node)->vtable = &blk_vtable;
	((ast_node_t *)node)->s = NULL;
	node->statements = statements;
	add_children(((ast_node_t *)node), 1, statements);
	return node;
}

/***********************************************|
 * Statement nodes==============================|
 *
 * Again, this follows the pattern of using a 
 * variadic constructor, which passes control
 * (along with its unpacked arguments) to the
 * appropriate constructor for a more specific 
 * type.
 ***********************************************/

void if_stmt_codegen(ast_node_t * node, generator_t * gen) {

	ast_if_stmt_node_t * ifs = 
		((ast_if_stmt_node_t *)node);

	ast_node_t * condition = (ast_node_t *) 
		ifs->condition;

	// Get the insert block & calling function
	LLVMBasicBlockRef insert = 
		LLVMGetInsertBlock(gen->builder);

	LLVMValueRef func = 
		LLVMGetBasicBlockParent(insert);

	condition->vtable->codegen(condition, gen);

	LLVMValueRef zero = LLVMConstInt(
			LLVMInt1Type(), 0, 0);

	LLVMValueRef new_cond = LLVMBuildICmp(gen->builder, LLVMIntNE, 
			condition->s->value, zero, "ifcond");

	LLVMBasicBlockRef if_start = 
		LLVMAppendBasicBlock(func, "if_start");

	LLVMPositionBuilderAtEnd(gen->builder, if_start);

	ast_node_t * body = (ast_node_t *) ifs->body;
	body->vtable->codegen(body, gen);

	if(!LLVMValueIsBasicBlock(body->s->value)) goto err;

	LLVMPositionBuilderAtEnd(gen->builder, 
			body->s->value);

	// Process the Else Clause
	// add the else block or a dummy
	ast_node_t * else_clause = 
		(ast_node_t *) ifs->else_clause;

	LLVMBasicBlockRef else_blk;

	if(else_clause == NULL) {
		else_blk = LLVMAppendBasicBlock(func, 
				"dummy_else");
	} else {
		else_clause->vtable->codegen(else_clause, gen);
		else_blk = else_clause->s->value;
	}

	// Wire everything together
	
	LLVMBasicBlockRef exit_block = 
		LLVMAppendBasicBlock(func, "exit");

	LLVMPositionBuilderAtEnd(gen->builder, 
			insert);

	LLVMBuildCondBr(gen->builder, 
			condition->s->value,
			if_start,
			else_blk);

	LLVMPositionBuilderAtEnd(
			gen->builder, body->s->value);

	LLVMBuildBr(gen->builder, exit_block);

	LLVMPositionBuilderAtEnd(gen->builder, 
			else_blk);

	LLVMBuildBr(gen->builder, exit_block);

	LLVMPositionBuilderAtEnd(gen->builder, 
			exit_block);
	return;
err:
	printf("LLVMValueRef is not a basic block!\n");
	exit(EXIT_FAILURE);
}


ast_node_vtable if_stmt_vtable = {
	delete_tree, 
	print_ast_node,
	ast_symbol,
	if_stmt_codegen
	// ast_codegen
};


ast_stmt_node_t * construct_if_stmt_node(
			ast_expression_node_t * condition,
			ast_stmt_node_t * body,
			ast_stmt_node_t * else_clause) {

	ast_if_stmt_node_t * node = 
		(ast_if_stmt_node_t *) malloc(sizeof(ast_if_stmt_node_t));

	((ast_node_t *)node)->type = Statement;
	((ast_node_t *)node)->vtable = &if_stmt_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_stmt_node_t *)node)->stmt_type = IfStatement;

	node->body = body;
	node->condition = condition;
	node->else_clause = else_clause;

	add_children(((ast_node_t *)node), 3, condition, 
			body, else_clause);

	return ((ast_stmt_node_t *)node);
}


void while_stmt_codegen(ast_node_t * node, generator_t * gen) {
	ast_while_stmt_node_t * ws =
			(ast_while_stmt_node_t *)node;

	ast_node_t * condition = (ast_node_t *) 
		ws->condition;

	// Get the insert block & calling function
	LLVMBasicBlockRef insert = 
		LLVMGetInsertBlock(gen->builder);

	LLVMValueRef func = 
		LLVMGetBasicBlockParent(insert);

	condition->vtable->codegen(condition, gen);

	LLVMValueRef zero = LLVMConstInt(
			LLVMInt1Type(), 0, 0);

	LLVMBasicBlockRef while_start = 
		LLVMAppendBasicBlock(func, "while_start");

	LLVMValueRef init_cond = LLVMBuildICmp(gen->builder, LLVMIntNE, 
			condition->s->value, zero, "while_cond");

	LLVMPositionBuilderAtEnd(gen->builder, while_start);

	ast_node_t * body = (ast_node_t *) ws->body;
	body->vtable->codegen(body, gen);

	LLVMPositionBuilderAtEnd(gen->builder, 
			body->s->value);

	// Wire everything together
	LLVMBasicBlockRef exit_block = 
		LLVMAppendBasicBlock(func, "exit");

	LLVMPositionBuilderAtEnd(gen->builder, 
			insert);

	LLVMBuildCondBr(gen->builder, 
			init_cond,
			while_start,
			exit_block);

	LLVMPositionBuilderAtEnd(
			gen->builder, body->s->value);

	condition->vtable->codegen(condition, gen);

	LLVMValueRef update_cond = LLVMBuildICmp(gen->builder, LLVMIntNE, 
			condition->s->value, zero, "while_cond_update");

	LLVMBuildCondBr(gen->builder, 
			update_cond,
			while_start,
			exit_block);

	LLVMPositionBuilderAtEnd(gen->builder, 
			exit_block);
	return;
}


ast_node_vtable while_stmt_vtable = {
		delete_tree,
		print_ast_node,
		ast_symbol,
		while_stmt_codegen
};


ast_stmt_node_t * construct_while_stmt_node(
			ast_expression_node_t * condition,
			ast_stmt_node_t * body) {

	ast_while_stmt_node_t * node = 
		(ast_while_stmt_node_t *) 
		malloc(sizeof(ast_while_stmt_node_t));

	((ast_node_t *)node)->type = Statement;
	((ast_stmt_node_t *)node)->stmt_type = WhileStatement;
	((ast_node_t *)node)->vtable = &while_stmt_vtable;
	((ast_node_t *)node)->s = NULL;

	node->condition = condition;
	node->body = body;
	add_children(((ast_node_t *)node), 2, condition, body);
	return ((ast_stmt_node_t *)node);
}


void return_stmt_codegen(ast_node_t * node, generator_t * gen) {
	ast_return_stmt_node_t * rtn = 
		(ast_return_stmt_node_t *)node;

	ast_node_t * exp = (ast_node_t *)rtn->body;
	exp->vtable->codegen(exp, gen);

	LLVMBasicBlockRef insert = 
		LLVMGetInsertBlock(gen->builder);

	LLVMValueRef func = 
		LLVMGetBasicBlockParent(insert);

	node->s->value = 
		LLVMBuildRet(gen->builder, exp->s->value);

	LLVMBasicBlockRef new_blk = 
		LLVMAppendBasicBlock(func, "post_return");

	LLVMPositionBuilderAtEnd(gen->builder, new_blk);

}


ast_node_vtable return_stmt_vtable = {
	delete_tree,
	print_ast_node,
	ast_symbol,
	return_stmt_codegen
};


ast_stmt_node_t * construct_return_stmt_node(
			ast_expression_node_t * body) {

	ast_return_stmt_node_t * node = 
		(ast_return_stmt_node_t *) 
		malloc(sizeof(ast_return_stmt_node_t));

	((ast_node_t *)node)->type = Statement;
	((ast_node_t *)node)->vtable = &return_stmt_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_stmt_node_t *)node)->stmt_type = ReturnStatement;

	node->body = body;
	add_children(((ast_node_t *)node), 1, body);
	return ((ast_stmt_node_t *)node);
}


void print_assign_stmt_node(ast_node_t * node, int depth) {
	if(node == NULL) return;

	for(int i = 0; i < depth; i++) {
		printf("  ");
	}

	printf("Assignment statement:\n");
	ast_node_t * child;
	for (int i = 0; i < node->n_children; i++) {
		child = node->children[i];

		if(child != NULL)
			child->vtable->print(child, depth+1);
	}

}


symbol_t * assign_stmt_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_assign_stmt_node_t * assign = 
		((ast_assign_stmt_node_t *)node);

	// Run the vdecl's symbol function first
	ast_node_t * a_vdecl = (ast_node_t *)(assign->vdecl);
	symbol_t * vdecl_symb = 
		a_vdecl->vtable->symbol(a_vdecl, st);

	// Then do the body...
	// this way we know both types
	ast_node_t * a_body = (ast_node_t *)(assign->body);
	symbol_t * body_symb = 
		a_body->vtable->symbol(a_body, st);

	// Check that the types match
	if (vdecl_symb->dt != body_symb->dt && 
			!is_valid_cast(vdecl_symb->dt, body_symb->dt)) {
		printf("ERROR! Invalid assignment! Exiting...\n");
		printf("LHS: %d, RHS: %d\n", vdecl_symb->dt, body_symb->dt);
		exit(EXIT_FAILURE);
	}

	// Return vdecl_symb, since it's actually been added
	// to the symbol table, whereas the body has not
	node->s = vdecl_symb;
	return vdecl_symb;

}


void assign_stmt_codegen(ast_node_t * node, generator_t * gen) {

	ast_assign_stmt_node_t * assign = 
		((ast_assign_stmt_node_t *)node);

	ast_node_t * a_vdecl = (ast_node_t *)(assign->vdecl);
	a_vdecl->vtable->codegen(a_vdecl, gen);

	ast_node_t * a_body = (ast_node_t *)(assign->body);
	a_body->vtable->codegen(a_body, gen);

	switch(assign->vdecl->type_node->tnt) {
		case PlainType:
			{
				LLVMBuildStore(gen->builder, a_body->s->value,
						a_vdecl->s->value);
				break;
			}
		case RefType: 
		case NoaRefType:
			{
				if(assign->body->exp_type != VarIdExpression) 
					goto err;

				ast_varid_expression_node_t * var_exp = 
					(ast_varid_expression_node_t *) assign->body;

				ast_node_t * varid = (ast_node_t *) (var_exp->varid);

				symbol_t * varid_symbol = lookup_symbol(var_exp->varid->charstar, 
						node->s->context);

				a_vdecl->s->value = varid_symbol->value;

				break;
			}
	}
	node->s->value = a_vdecl->s->value;
	return;
err:
	printf("ERROR!\n");
	printf("Can only assign to ref from a var of referred"
			" to type\n");
	exit(EXIT_FAILURE);
}


ast_node_vtable assign_stmt_vtable = {
	delete_tree, 
	print_assign_stmt_node,
	assign_stmt_symbol,
	assign_stmt_codegen

};

ast_stmt_node_t * construct_assign_stmt_node(
			ast_vdecl_node_t * vdecl,
			ast_expression_node_t * body) {

	ast_assign_stmt_node_t * node = 
		(ast_assign_stmt_node_t *) 
		malloc(sizeof(ast_assign_stmt_node_t));

	((ast_node_t *)node)->type = Statement;
	((ast_node_t *)node)->vtable = &assign_stmt_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_stmt_node_t *)node)->stmt_type = AssignStatement;

	node->vdecl = vdecl;
	node->body = body;
	add_children(((ast_node_t *)node), 2, body, vdecl);
	return ((ast_stmt_node_t *)node);
}


ast_stmt_node_t * construct_blk_stmt_node(
		ast_blk_node_t * blk) {

	ast_blk_stmt_node_t * node = 
		(ast_blk_stmt_node_t *) 
		malloc(sizeof(ast_blk_stmt_node_t));
	
	((ast_node_t *)node)->type = Statement;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_stmt_node_t *)node)->stmt_type = BlkStatement;
	node->blk = blk;
	add_children(((ast_node_t *)node), 1, blk);
	return ((ast_stmt_node_t *)node);
}


ast_stmt_node_t * construct_expression_stmt_node(
			ast_expression_node_t * body) {

	ast_expression_stmt_node_t * node = 
		(ast_expression_stmt_node_t *) 
		malloc(sizeof(ast_expression_stmt_node_t));

	((ast_node_t *)node)->type = Statement;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_stmt_node_t *)node)->stmt_type = ExpressionStatement;
	node->body = body;
	add_children(((ast_node_t *)node), 1, body);
	return ((ast_stmt_node_t *)node);
}


void destroy_print_stmt_node(ast_node_t * node) {
	free((char *)((ast_print_stmt_node_t *)node)->string);

	ast_print_stmt_node_t * ps_node = 
		((ast_print_stmt_node_t *)node);
	ast_node_t * exp_node = (ast_node_t *)(ps_node->expression);
	if(exp_node != NULL) exp_node->vtable->destroy(exp_node);

	free(node->children);
	free((ast_print_stmt_node_t *)node);
}


void print_stmt_codegen(ast_node_t * node, generator_t * gen) {
	ast_print_stmt_node_t * psn = (ast_print_stmt_node_t *) node;

	LLVMValueRef p;
	LLVMValueRef arg_array[1];
	if(psn->expression != NULL) {
		ast_node_t * exp_node = (ast_node_t *) psn->expression;
		exp_node->vtable->codegen(exp_node, gen);

		LLVMValueRef val = exp_node->s->value;
		arg_array[0] =  val;

		switch(exp_node->s->dt) {
			case Int_type:
			case Cint_type:
				p = LLVMGetNamedFunction(gen->module, 
						"print_int");
				break;
			case Float_type:
				p = LLVMGetNamedFunction(gen->module, 
						"print_float");
				break;
			case Bool_type:
				p = LLVMGetNamedFunction(gen->module, 
						"print_bool");
				break;
			case Char_type:
				p = LLVMGetNamedFunction(gen->module, 
						"print_string");
				break;
			case Void_type:
				printf("Error - cannot print a void type!");
				exit(EXIT_FAILURE);
		}
	}

	node->s->value = LLVMBuildCall(gen->builder, 
			p, arg_array, 1, "Print Statement");

	return;
}


ast_node_vtable print_stmt_vtable = {
	destroy_print_stmt_node, 
	print_ast_node,
	ast_symbol,
	print_stmt_codegen
};


ast_stmt_node_t * construct_print_stmt_node(
		ast_expression_node_t * exp,
		ast_string_node_t * string) {

	ast_print_stmt_node_t * node = 
		(ast_print_stmt_node_t *) 
		malloc(sizeof(ast_print_stmt_node_t));

	((ast_node_t *)node)->type = Statement;
	((ast_node_t *)node)->vtable = &print_stmt_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_stmt_node_t *)node)->stmt_type = PrintStatement;

	node->expression = exp;
	node->string = string;

	add_children(((ast_node_t *)node), 2, exp, string);
	return ((ast_stmt_node_t *)node);
}


ast_stmt_node_t * construct_stmt_node(
		statement_type t,
		int count, ...) {

	ast_stmt_node_t * node;
	va_list ap;
	va_start(ap, count);

	ast_vdecl_node_t * vdecl;
	ast_expression_node_t * exp;
	ast_stmt_node_t * stmt1;
	ast_stmt_node_t * stmt2;
	ast_blk_node_t * blk;
	ast_string_node_t * str;

	switch(t)
	{
		case IfStatement:
			exp = va_arg(ap, ast_expression_node_t*);
			stmt1 = va_arg(ap, ast_stmt_node_t*); 
			stmt2 = va_arg(ap, ast_stmt_node_t*); 
			node = construct_if_stmt_node(exp, stmt1, stmt2);
			break;

		case WhileStatement:
			exp = va_arg(ap, ast_expression_node_t*);
			stmt1 = va_arg(ap, ast_stmt_node_t*); 
			node = construct_while_stmt_node(exp, stmt1);
			break;

		case ReturnStatement:
			exp = va_arg(ap, ast_expression_node_t*);
			node = construct_return_stmt_node(exp);
			break;

		case ExpressionStatement:
			exp = va_arg(ap, ast_expression_node_t*);
			node = construct_expression_stmt_node(exp);
			break;

		case AssignStatement:
			vdecl = va_arg(ap, ast_vdecl_node_t*);
			exp = va_arg(ap, ast_expression_node_t*);
			node = construct_assign_stmt_node(vdecl, exp); 
			break;

		case BlkStatement:
			blk = va_arg(ap, ast_blk_node_t*);
			node = construct_blk_stmt_node(blk);
			break;

		case PrintStatement:
			exp = va_arg(ap, ast_expression_node_t*),
			str = va_arg(ap, ast_string_node_t*);
			node = construct_print_stmt_node(exp, str);
			break;
	}
	va_end(ap);
	return node;
}


/******************************************************|
 * Expression Nodes====================================|
 * 
 * Similar to statements, these have a generic type +
 * variadic constructor, which routes control to the
 * appropriate (private) type + constructor.
 ******************************************************/

ast_expression_node_t * construct_list_expression_node(
		ast_list_node_t * list) {

	ast_list_expression_node_t * node = 
		(ast_list_expression_node_t *) 
		malloc(sizeof(ast_list_expression_node_t));

	((ast_node_t *)node)->type = Exp;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_expression_node_t *)node)->exp_type = ExpressionList;
	node->list = list;

	add_children(((ast_node_t *)node), 1, list);
	return ((ast_expression_node_t *)node); 
}


ast_expression_node_t *	construct_binop_expression_node(
		ast_binop_node_t * binop) {

	ast_binop_expression_node_t * node = 
		(ast_binop_expression_node_t *) 
		malloc(sizeof(ast_binop_expression_node_t));

	((ast_node_t *)node)->type = Exp;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	node->binop = binop;
	((ast_expression_node_t *)node)->exp_type = BinOpExpression;

	add_children(((ast_node_t *)node), 1, binop);
	return ((ast_expression_node_t *)node); 
}


ast_expression_node_t *	construct_unop_expression_node(
		ast_unop_node_t * unop) {
	ast_unop_expression_node_t * node = 
		(ast_unop_expression_node_t *) 
		malloc(sizeof(ast_unop_expression_node_t));

	((ast_node_t *)node)->type = Exp;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_expression_node_t *)node)->exp_type = UnOpExpression;

	node->unop = unop;
	add_children(((ast_node_t *)node), 1, unop);

	return ((ast_expression_node_t *)node); 
}


ast_expression_node_t *	construct_literal_expression_node(
		ast_literal_node_t * literal) {

	ast_literal_expression_node_t * node = 
		(ast_literal_expression_node_t *) 
		malloc(sizeof(ast_literal_expression_node_t));

	((ast_node_t *)node)->type = Exp;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_expression_node_t *)node)->exp_type = LiteralExpression;

	node->literal = literal;
	add_children(((ast_node_t *)node), 1, literal);

	return ((ast_expression_node_t *)node); 
}


// Varid expressions - just look up the varid and return its symbol
symbol_t * varid_expression_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_varid_expression_node_t * varid_exp = 
		(ast_varid_expression_node_t *) node;

	symbol_t * varid_symbol = lookup_symbol(varid_exp->varid->charstar, st);
	if(varid_symbol == NULL) {
		printf("ERROR - Varid %s not found! "
				"Exiting...\n", varid_exp->varid->charstar);
		exit(EXIT_FAILURE);
	}
	node->s = create_anon_symbol(varid_symbol->dt);
	node->s->context = st;
	return node->s;
}


void varid_expression_codegen(ast_node_t * node, generator_t * gen) {
	ast_varid_expression_node_t * varid_exp = 
		(ast_varid_expression_node_t *) node;

	symbol_t * varid_symbol = lookup_symbol(
			varid_exp->varid->charstar, node->s->context);

	var_symbol_t * var_s = (var_symbol_t *) varid_symbol;

	node->s->value = LLVMBuildLoad(gen->builder, 
			varid_symbol->value,
			varid_exp->varid->charstar);

}


ast_node_vtable varid_expression_vtable = {
	delete_tree,
	print_ast_node,
	varid_expression_symbol,
	varid_expression_codegen
};


ast_expression_node_t *	construct_varid_expression_node(
		ast_varid_node_t * varid) {

	ast_varid_expression_node_t * node = 
		(ast_varid_expression_node_t *) 
		malloc(sizeof(ast_varid_expression_node_t));

	((ast_node_t *)node)->type = Exp;
	((ast_node_t *)node)->vtable = &varid_expression_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_expression_node_t *)node)->exp_type = VarIdExpression;

	node->varid = varid;
	add_children(((ast_node_t *)node), 1, varid);

	return ((ast_expression_node_t *)node); 
}


symbol_t * globid_expression_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_globid_expression_node_t * globid_exp = 
		(ast_globid_expression_node_t *) node;

	symbol_t * globid_symbol = lookup_symbol(
			globid_exp->globid->charstar, st);

	if(globid_symbol == NULL) {
		printf("ERROR - Globid %s not found! "
				"Exiting...\n", 
				globid_exp->globid->charstar);
		exit(EXIT_FAILURE);
	}

	ast_list_node_t * args = globid_exp->expressions;
	symbol_t * arg_symb;
	int actual_count = 0;
	func_symbol_t * func = (func_symbol_t *) globid_symbol;
	for (int i = 0; i < func->n_args; i++) {

		if(args == NULL) {
			printf("ERROR! Not enough arguments for function %s.\n"
					"Expected %d args, got %d.\n"
					"Exiting.\n", ((symbol_t *)func)->name
					, func->n_args, actual_count);
			exit(EXIT_FAILURE);
		}

		arg_symb = args->current->vtable->symbol(args->current, st);

		if(arg_symb->dt != func->params[i]->tnn->t) {

			printf("ERROR! Mismatched function parameter types... "
					"In function: %s\n"
					"Expected: %d\n"
					"Actual: %d\n"
					"Exiting.\n", ((symbol_t *)func)->name
					, func->params[i]->tnn->t, arg_symb->dt);
			exit(EXIT_FAILURE);
		}
		args = args->next;
		actual_count ++;
	}
	node->s = globid_symbol;
	node->s->context = st;
	return globid_symbol;
}


void globid_exp_codegen(ast_node_t * node, generator_t * gen) {
	ast_globid_expression_node_t * globid_exp = 
		(ast_globid_expression_node_t *) node;

	symbol_t * globid_symbol = lookup_symbol(
			globid_exp->globid->charstar, 
			node->s->context);

	if(globid_symbol == NULL) {
		printf("GLOBID SYMBOL WAS NULL\n");
		goto err;
	}

	func_symbol_t * func = (func_symbol_t *) globid_symbol;
	LLVMValueRef * arg_array = 
		(LLVMValueRef *) malloc(func->n_args * sizeof(void *));

	ast_list_node_t * cur_arg = globid_exp->expressions;
	ast_node_t * arg_exp = NULL;
	for (int i = 0; i < func->n_args; i++) {

		if(cur_arg == NULL) {
			printf("CURRENT ARG IS NULL\n");

			goto err;
		}

		arg_exp = cur_arg->current;

		switch(func->params[i]->tnt) {
			case PlainType:
				{
					arg_exp->vtable->codegen(arg_exp, gen);

					arg_array[i] = arg_exp->s->value;
					break;
				}
			case RefType: 
			case NoaRefType:
				{
					arg_exp->vtable->codegen(arg_exp, gen);
					ast_expression_node_t * exp_node = 
						(ast_expression_node_t *) arg_exp;

					if(exp_node->exp_type != VarIdExpression) 
						goto err;

					ast_varid_node_t * varid = 
						((ast_varid_expression_node_t *)exp_node)->varid;

					symbol_t * arg_symbol = lookup_symbol(
							varid->charstar,node->s->context);

					arg_array[i] = arg_symbol->value;

					break;
				}
		}

		cur_arg = cur_arg->next;
	}

	LLVMValueRef func_l = LLVMGetNamedFunction(
			gen->module, 
			globid_exp->globid->charstar);

	node->s->value = LLVMBuildCall(gen->builder, 
			func_l, arg_array, 
			func->n_args, "func_call");

	return;

err:
	printf("ERROR: Exiting...\n");
	exit(EXIT_FAILURE);
		
}


ast_node_vtable globid_expression_vtable = {
	delete_tree,
	print_ast_node,
	globid_expression_symbol,
	globid_exp_codegen

};


ast_expression_node_t *	construct_globid_expression_node(
		ast_globid_node_t * globid,
		ast_list_node_t * expressions) {

	ast_globid_expression_node_t * node = 
		(ast_globid_expression_node_t *) 
		malloc(sizeof(ast_globid_expression_node_t));

	((ast_node_t *)node)->type = Exp;
	((ast_node_t *)node)->vtable = &globid_expression_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_expression_node_t *)node)->exp_type = GlobIdExpression;

	node->globid = globid;
	node->expressions = expressions;
	add_children(((ast_node_t *)node), 2, globid, expressions);

	return ((ast_expression_node_t *)node); 
}


ast_expression_node_t * construct_expression_node(
		expression_type et,
		int count, ...) {

	ast_expression_node_t * node;
	va_list ap;
	va_start(ap, count);

	switch(et) {
		case ExpressionList:
			node = construct_list_expression_node(
					va_arg(ap, ast_list_node_t *));
			break;

		case BinOpExpression:
			node = construct_binop_expression_node(
					va_arg(ap, ast_binop_node_t*));
			break;
			
		case UnOpExpression:
			node = construct_unop_expression_node(
					va_arg(ap, ast_unop_node_t*));
			break;
		
		case LiteralExpression: 
			node = construct_literal_expression_node(
					va_arg(ap, ast_literal_node_t*));
			break;
			
		case VarIdExpression:
			node = construct_varid_expression_node(
					va_arg(ap, ast_varid_node_t*));
			break;
			
		case GlobIdExpression:
		{

			ast_globid_node_t * g = va_arg(ap, ast_globid_node_t*);
			ast_list_node_t * l = va_arg(ap, ast_list_node_t *);
			node = construct_globid_expression_node(g, l);
			break;
		}
	}

	va_end(ap);
	return node;
}


/***************************************************|
 * Binary operators=================================|
 *
 * Note: Binops have several different constructors
 * depending on what type of operation is being
 * performed... The generic constructor is a
 * variadic function which unpacks its arguments
 * and sends them to the appropriate specific 
 * constructor... This pattern is used repeatedly
 * throughout this code.
 ***************************************************/

ast_binop_node_t * construct_binop_arithop_node(
		ast_arithop_node_t * arithop) {
	
	ast_binop_arithop_node_t * node = 
		(ast_binop_arithop_node_t *)
		malloc(sizeof(ast_binop_arithop_node_t));

	((ast_node_t *)node)->type = BinOp;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_binop_node_t *)node)->bt = BinaryArithOp;

	node->arithop = arithop;
	add_children(((ast_node_t *)node), 1, arithop);

	return ((ast_binop_node_t *)node);
}

ast_binop_node_t * construct_binop_logicop_node(
		ast_logicop_node_t * logicop) {
	
	ast_binop_logicop_node_t * node = 
		(ast_binop_logicop_node_t *)
		malloc(sizeof(ast_binop_logicop_node_t));

	((ast_node_t *)node)->type = BinOp;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_binop_node_t *)node)->bt = BinaryLogicOp;

	node->logicop = logicop;
	add_children(((ast_node_t *)node), 1, logicop);

	return ((ast_binop_node_t *)node);
}

symbol_t * binop_assignop_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_binop_assignop_node_t * binop_assignop = 
		(ast_binop_assignop_node_t *) node;

	ast_node_t * op1 = (ast_node_t *)(binop_assignop->varid);
	symbol_t * op1_symb = op1->vtable->symbol(op1, st);

	ast_node_t * op2 = (ast_node_t *)(binop_assignop->expression);
	symbol_t * op2_symb = op2->vtable->symbol(op2, st);

	if(op1_symb->dt != op2_symb->dt) {
		printf("ERROR! Assignment operands have "
				"mismatched types! Exiting...\n");
		exit(EXIT_FAILURE);
	}

	symbol_t * rtn_symb = create_anon_symbol(op1_symb->dt);
	node->s = rtn_symb;
	node->s->context = st;

	return rtn_symb;
}



void binop_assignop_codegen(ast_node_t * node, generator_t * gen) {
	ast_binop_assignop_node_t * binop_assignop = 
		(ast_binop_assignop_node_t *) node;

	ast_node_t * op1 = 
		(ast_node_t *)(binop_assignop->varid);
	op1->vtable->codegen(op1, gen);

	ast_node_t * op2 = 
		(ast_node_t *)(binop_assignop->expression);
	op2->vtable->codegen(op2, gen);


	var_symbol_t * var_symbol = (var_symbol_t *) lookup_symbol(
			binop_assignop->varid->charstar, 
			node->s->context);

	node->s->value = LLVMBuildStore(gen->builder, 
			op2->s->value, 
			op1->s->value);

	return;
}


ast_node_vtable binop_assignop_vtable = {
	delete_tree, 
	print_ast_node,
	binop_assignop_symbol,
	binop_assignop_codegen

};

ast_binop_node_t * construct_binop_assignop_node(
		ast_varid_node_t * varid,
		ast_expression_node_t * expression) {
	
	ast_binop_assignop_node_t * node = 
		(ast_binop_assignop_node_t *)
		malloc(sizeof(ast_binop_assignop_node_t));

	((ast_node_t *)node)->type = BinOp;
	((ast_node_t *)node)->vtable = &binop_assignop_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_binop_node_t *)node)->bt = BinaryAssignOp;

	node->varid = varid;
	node->expression = expression;
	add_children(((ast_node_t *)node), 2, varid, expression);

	return ((ast_binop_node_t *)node);
}


symbol_t * binop_castop_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_binop_castop_node_t * binop_castop = (ast_binop_castop_node_t *) node;

	ast_typename_node_t * op1 = binop_castop->typename;

	ast_node_t * op2 = (ast_node_t *)(binop_castop->expression);
	symbol_t * op2_symb = op2->vtable->symbol(op2, st);

	symbol_t * rtn_symb;

	if(!is_valid_cast(op1->t, op2_symb->dt)) goto err;


ok:
	rtn_symb = create_anon_symbol(op2_symb->dt);
	rtn_symb->context = st;
	node->s = rtn_symb;
	return rtn_symb;
err:
	printf("ERROR! Invalid cast!");
	printf("Exiting...\n");
	exit(EXIT_FAILURE);
}


void binop_castop_codegen(ast_node_t * node, generator_t * gen) {
	ast_binop_castop_node_t * binop_castop = 
		 (ast_binop_castop_node_t *) node;

	ast_typename_node_t * op1 = binop_castop->typename;

	ast_node_t * op2 = (ast_node_t *)(binop_castop->expression);
	op2->vtable->codegen(op2, gen);

	switch(op1->t) {
		case Cint_type:
		case Int_type:
			{
				switch(op2->s->dt) {
					case Int_type:
					case Cint_type:
						node->s->value = op2->s->value;
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFPCast(
								gen->builder,
								op2->s->value,
								LLVMInt32Type(),
								"tmp");

						goto ok;
					case Bool_type:
					case Char_type:
					case Void_type:
						goto err;
				}

			}
		case Float_type:
			{
				switch(op2->s->dt) {
					case Int_type:
					case Cint_type:
						node->s->value = LLVMBuildIntCast(
								gen->builder,
								op2->s->value,
								LLVMFloatType(),
								"tmp");
					case Float_type:
						node->s->value = op2->s->value;
						goto ok;
					case Bool_type:
					case Void_type:
					case Char_type:
						goto err;
				}

			}
		case Bool_type:
			{
				switch(op2->s->dt) {
					case Bool_type:
						node->s->value = op2->s->value;
						goto ok;
					case Int_type:
					case Cint_type:
					case Void_type:
					case Float_type:
					case Char_type:
						goto err;
				}

			}
		case Char_type:
		case Void_type:
			goto err;
	}
ok:
	return;
err:
	printf("ERROR! Invalid cast! Exiting...\n");
	exit(EXIT_FAILURE);
}

ast_node_vtable binop_castop_vtable = {
	delete_tree, 
	print_ast_node,
	binop_castop_symbol,
	binop_castop_codegen

};

ast_binop_node_t * construct_binop_castop_node(
		ast_typename_node_t * typename,
		ast_expression_node_t * expression) {
	
	ast_binop_castop_node_t * node = 
		(ast_binop_castop_node_t *)
		malloc(sizeof(ast_binop_castop_node_t));

	((ast_node_t *)node)->type = BinOp;
	((ast_node_t *)node)->vtable = &binop_castop_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_binop_node_t *)node)->bt = BinaryAssignOp;

	node->typename = typename;
	((ast_node_t *)node)->vtable = &ast_vtable;
	node->expression = expression;
	add_children(((ast_node_t *)node), 2, typename, expression);

	return ((ast_binop_node_t *)node);
}

ast_binop_node_t * construct_binop_node(
		binop_type bt, int count, ...) {

	ast_binop_node_t * node;
	va_list ap;
	va_start(ap, count);

	switch(bt)
	{
		case BinaryArithOp:
			node = construct_binop_arithop_node(
					va_arg(ap, ast_arithop_node_t*));
			break;

		case BinaryLogicOp:
			node = construct_binop_logicop_node(
					va_arg(ap, ast_logicop_node_t*));
			break;

		case BinaryAssignOp:
			{

				ast_varid_node_t * varid = 
					va_arg(ap, ast_varid_node_t*);
				ast_expression_node_t * exp = 
					va_arg(ap, ast_expression_node_t*);

				node = construct_binop_assignop_node(varid, exp);
				break;
			}

		case BinaryCastOp:
			{

				ast_typename_node_t * typename = 
					va_arg(ap, ast_typename_node_t*);
				ast_expression_node_t * exp = 
					va_arg(ap, ast_expression_node_t*);

				node = construct_binop_castop_node(typename, exp);
				break;
			}
	}

	va_end(ap);
	return node;
}


// Arithmetic operators

symbol_t * arithop_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_arithop_node_t * arithop = (ast_arithop_node_t *) node;

	ast_node_t * op1 = (ast_node_t *)(arithop->op1);
	symbol_t * op1_symb = op1->vtable->symbol(op1, st);

	ast_node_t * op2 = (ast_node_t *)(arithop->op2);
	symbol_t * op2_symb = op2->vtable->symbol(op2, st);

	if(op1_symb->dt != op2_symb->dt) {
		printf("ERROR! Arithmetic operands have"
				" mismatched types! Exiting...\n");
		exit(EXIT_FAILURE);
	}

	symbol_t * rtn_symb = create_anon_symbol(op1_symb->dt);
	rtn_symb->context = st;
	node->s = rtn_symb;
	return rtn_symb;
}


void arithop_codegen(ast_node_t * node, generator_t * gen) {
	if (node->s == NULL) {
		printf("ERROR! Nonexistent symbol for unop node."
				" Exiting...\n");
		exit(EXIT_FAILURE);
	}

	ast_arithop_node_t * arithop = (ast_arithop_node_t *) node;

	ast_node_t * op1 = (ast_node_t *)(arithop->op1);
	op1->vtable->codegen(op1, gen);

	ast_node_t * op2 = (ast_node_t *)(arithop->op2);
	op2->vtable->codegen(op2, gen);

	switch(arithop->at) {
		case ArithTimesOp:
			{
				switch(op1->s->dt) {
					case Cint_type:
						{
							LLVMValueRef optype = LLVMConstInt(
									LLVMInt32Type(), ArithTimesOp, 1);

							LLVMValueRef arg_array[3] = {
								op1->s->value, op2->s->value, optype
							};

							LLVMValueRef o = LLVMGetNamedFunction(gen->module, 
									"check_overflow");
							
							node->s->value = LLVMBuildCall(gen->builder, o, 
									arg_array, 3, "overflow add check");

							goto ok;
						}
					case Int_type:
						node->s->value = LLVMBuildMul(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFMul(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Bool_type:
					case Void_type:
					case Char_type:
						goto err;
					default:
						goto err;

				}
			}
		case ArithDivOp:
			{
				switch(op1->s->dt) {
					case Cint_type:
						{
							LLVMValueRef optype = LLVMConstInt(
									LLVMInt32Type(), ArithDivOp, 1);

							LLVMValueRef arg_array[3] = {
								op1->s->value, op2->s->value, optype
							};

							LLVMValueRef o = LLVMGetNamedFunction(gen->module, 
									"check_overflow");
							
							node->s->value = LLVMBuildCall(gen->builder, o, 
									arg_array, 3, "overflow add check");

							goto ok;
						}
					case Int_type:
						node->s->value = LLVMBuildSDiv(
								  gen->builder,
								  op1->s->value,
								  op2->s->value, "tmp");
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFDiv(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;

					case Bool_type:
					case Void_type:
					case Char_type:
						goto err;
					default:
						goto err;
				}
			}
		case ArithPlusOp:
			{
				switch(op1->s->dt) {
					case Cint_type:
						{
							LLVMValueRef optype = LLVMConstInt(
									LLVMInt32Type(), ArithPlusOp, 1);

							LLVMValueRef arg_array[3] = {
								op1->s->value, op2->s->value, optype
							};

							LLVMValueRef o = LLVMGetNamedFunction(gen->module, 
									"check_overflow");
							
							node->s->value = LLVMBuildCall(gen->builder, o, 
									arg_array, 3, "overflow add check");

							goto ok;
						}
					case Int_type:
						node->s->value = LLVMBuildAdd(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFAdd(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;

					case Bool_type:
					case Void_type:
					case Char_type:
						goto err;
					default:
						goto err;
				}
			}
		case ArithMinusOp:
			{
				switch(op1->s->dt) {
					case Cint_type:
						{
							LLVMValueRef optype = LLVMConstInt(
									LLVMInt32Type(), ArithMinusOp, 1);

							LLVMValueRef arg_array[3] = {
								op1->s->value, op2->s->value, optype
							};

							LLVMValueRef o = LLVMGetNamedFunction(gen->module, 
									"check_overflow");
							
							node->s->value = LLVMBuildCall(gen->builder, o, 
									arg_array, 3, "overflow add check");

							goto ok;
						}
					case Int_type:
						node->s->value = LLVMBuildSub(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFSub(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;

					case Bool_type:
					case Void_type:
					case Char_type:
						goto err;
					default:
						goto err;
				}
			}
	}
ok:
	return;
err:
	printf("ERROR! Invalid operand type"
			". Exiting...\n");
	exit(EXIT_FAILURE);

}

ast_node_vtable arithop_vtable = {
	delete_tree, 
	print_ast_node,
	arithop_symbol,
	arithop_codegen

};


ast_arithop_node_t *  construct_arithop_node(
		arithop_type at,
		ast_expression_node_t * op1,
		ast_expression_node_t * op2) { 

	ast_arithop_node_t * node = 
		(ast_arithop_node_t *)
		malloc(sizeof(ast_arithop_node_t));

	((ast_node_t *)node)->type = ArithOp;
	((ast_node_t *)node)->vtable = &arithop_vtable;
	((ast_node_t *)node)->s = NULL;
	node->at = at;
	node->op1 = op1;
	node->op2 = op2;
	add_children(((ast_node_t *)node), 2, op1, op2);
	return node;
}



// Logical operator

symbol_t * logicop_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_logicop_node_t * logicop = (ast_logicop_node_t *) node;

	ast_node_t * op1 = (ast_node_t *)(logicop->op1);
	symbol_t * op1_symb = op1->vtable->symbol(op1, st);

	ast_node_t * op2 = (ast_node_t *)(logicop->op2);
	symbol_t * op2_symb = op2->vtable->symbol(op2, st);

	if(op1_symb->dt != op2_symb->dt) {
		printf("ERROR! Logical operands"
				" have mismatched types!\n");
		printf("Types were %i and %i\n", 
				op1_symb->dt, op2_symb->dt);
		exit(EXIT_FAILURE);
	}

	symbol_t * rtn_symb = create_anon_symbol(Bool_type);
	rtn_symb->context = st;
	node->s = rtn_symb;
	return rtn_symb;
}


void logicop_codegen(ast_node_t * node, generator_t * gen) {
	if (node->s == NULL) {
		printf("ERROR! Nonexistent symbol for unop node."
				" Exiting...\n");
		exit(EXIT_FAILURE);
	}

	ast_logicop_node_t * logicop = (ast_logicop_node_t *) node;

	ast_node_t * op1 = (ast_node_t *)(logicop->op1);
	op1->vtable->codegen(op1, gen);

	ast_node_t * op2 = (ast_node_t *)(logicop->op2);
	op2->vtable->codegen(op2, gen);
	
	switch(logicop->lot) {
		case EqualityOp:
			{
				switch(op1->s->dt) {
					case Int_type:
					case Cint_type:
					case Bool_type:
						node->s->value = LLVMBuildICmp(
								gen->builder,
								LLVMIntEQ,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFCmp(
								gen->builder,
								LLVMRealOEQ,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Char_type:
						goto err;
					default:
						goto err;
				}

			}
		case LessThanOp:
			{
				switch(op1->s->dt) {
					case Int_type:
					case Cint_type:
						node->s->value = LLVMBuildICmp(
								gen->builder,
								LLVMIntSLT,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFCmp(
								gen->builder,
								LLVMRealOLT,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Bool_type:
					case Char_type:
					case Void_type:
						goto err;
				}
			}
		case GreaterOp:
			{
				switch(op1->s->dt) {
					case Int_type:
					case Cint_type:
						node->s->value = LLVMBuildICmp(
								gen->builder,
								LLVMIntSGT,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFCmp(
								gen->builder,
								LLVMRealOGT,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Bool_type:
					case Void_type:
					case Char_type:
						goto err;
				}
			}
		case LogicAndOp:
			{
				switch(op1->s->dt) {
					case Bool_type:
						node->s->value = LLVMBuildAnd(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Int_type:
					case Cint_type:
					case Float_type:
					case Void_type:
					case Char_type:
						goto err;
				}
			}
		case LogicOrOp:
			{
				switch(op1->s->dt) {
					case Bool_type:
						node->s->value = LLVMBuildOr(
								gen->builder,
								op1->s->value,
								op2->s->value, "tmp");
						goto ok;
					case Int_type:
					case Cint_type:
					case Float_type:
					case Void_type:
					case Char_type:
						goto err;
				}
			}
	}
ok:
	return;
err:
	printf("ERROR! Invalid operand type"
			". Exiting...\n");
	exit(EXIT_FAILURE);
}


ast_node_vtable logicop_vtable = {
	delete_tree, 
	print_ast_node,
	logicop_symbol,
	logicop_codegen

};


ast_logicop_node_t * construct_logicop_node(
		logicop_type lot,
		ast_expression_node_t * op1,
		ast_expression_node_t * op2) {

	ast_logicop_node_t * node = 
		(ast_logicop_node_t *)
		malloc(sizeof(ast_logicop_node_t));

	((ast_node_t *)node)->type = LogicOp;
	((ast_node_t *)node)->vtable = &logicop_vtable;
	((ast_node_t *)node)->s = NULL;
	node->lot = lot;
	node->op1 = op1;
	node->op2 = op2;
	add_children(((ast_node_t *)node), 2, op1, op2);
	return node;
}


// Unary operators

symbol_t * unop_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_unop_node_t * unop = (ast_unop_node_t *) node;
	ast_node_t * operand = (ast_node_t *)(unop->operand);
	symbol_t * op_symb = operand->vtable->symbol(operand, st);

	switch(unop->ut) {
		case NotUnOp:
			if(op_symb->dt != Bool_type) {
				printf("ERROR: Can only use 'not' with boolean "
						"expressions!\n");
				goto err;
			}
			break;
		case NegateOp:
			{
				switch(op_symb->dt) {
					case Int_type:
					case Cint_type:
					case Float_type:
						goto ok;
					case Bool_type:
					case Void_type:
					case Char_type:
						goto err;
					default:
						goto err;
				}
				break;
			}
	}
ok:
	node->s = create_anon_symbol(op_symb->dt);
	node->s->context = st;
	return op_symb;
err:
	printf("ERROR! Invalid unary operation! Exiting....\n");
	exit(EXIT_FAILURE);
}


void unop_codegen(ast_node_t * node, generator_t * gen) {
	if (node->s == NULL) {
		printf("ERROR! Nonexistent symbol for unop node."
				" Exiting...\n");
		exit(EXIT_FAILURE);
	}

	ast_unop_node_t * unop = (ast_unop_node_t *) node;
	ast_node_t * operand = (ast_node_t *)(unop->operand);
	operand->vtable->codegen(operand, gen);

	switch(unop->ut) {
		case NotUnOp:
			node->s->value = LLVMBuildNot(gen->builder, 
					operand->s->value, "tmp");
			goto ok;
		case NegateOp:
			{
				switch(operand->s->dt) {
					case Cint_type:
						{
							LLVMValueRef optype = LLVMConstInt(
									LLVMInt32Type(), ArithTimesOp, 1);

							LLVMValueRef minusone = LLVMConstInt(
									LLVMInt32Type(), -1, 1);

							LLVMValueRef arg_array[3] = {
								operand->s->value, minusone, optype

							};

							LLVMValueRef o = LLVMGetNamedFunction(gen->module, 
									"check_overflow");
							
							node->s->value = LLVMBuildCall(gen->builder, o, 
									arg_array, 3, "overflow negation");
							goto ok;
						}
					case Int_type:
						node->s->value = LLVMBuildNeg(gen->builder, 
								operand->s->value, "tmp");
						goto ok;
					case Float_type:
						node->s->value = LLVMBuildFNeg(gen->builder, 
								operand->s->value, "tmp");
						goto ok;
					case Bool_type:
					case Void_type:
					case Char_type:
						goto err;
				}

			}
	}
ok:
	return;
err:
	printf("ERROR! Invalid operand for negation!\n");
	exit(EXIT_FAILURE);
}


ast_node_vtable unop_node_vtable = {
	delete_tree, 
	print_ast_node,
	unop_symbol,
	unop_codegen
};

ast_unop_node_t * construct_unop_node(
		unop_type ut,
		ast_expression_node_t * operand) {

	ast_unop_node_t * node = 
		(ast_unop_node_t *)
		malloc(sizeof(ast_unop_node_t));

	((ast_node_t *)node)->type = UnOp;
	((ast_node_t *)node)->vtable = &unop_node_vtable;
	((ast_node_t *)node)->s = NULL;

	node->ut = ut;
	node->operand = operand;
	add_children(((ast_node_t *)node), 1, operand);

	return node;
}


// Type node

void print_type_node(ast_node_t * node, int depth) {
	for (int i = 0; i < depth; i++) {
		printf("  ");
	}

	char * typename;
	ast_type_node_t * typenode = (((ast_type_node_t *)node));

	switch(typenode->tnt) {
		case RefType:
			typename = "ref";
			break;
		case NoaRefType:
			typename = "noalias ref";
			break;
		case PlainType:
			typename = "plain";
			break;
	}

	printf("Type: %s\n", typename);
	((ast_node_t *)typenode->tnn)
		->vtable->print((ast_node_t *)typenode->tnn, depth);
	return;
}


ast_node_vtable type_node_vtable = {
	delete_tree, 
	print_type_node,
	ast_symbol,
	ast_codegen

};


ast_type_node_t * construct_type_node(
		type_node_type tnt,
		ast_typename_node_t * tnn) {

	ast_type_node_t * node = 
		(ast_type_node_t *)
		malloc(sizeof(ast_type_node_t));

	((ast_node_t *)node)->type = Type;
	((ast_node_t *)node)->vtable = &type_node_vtable;
	((ast_node_t *)node)->s = NULL;

	node->tnt = tnt;
	node->tnn = tnn;

	add_children(((ast_node_t *)node), 1, tnn);
	return node;
}

// Variable declaration node

symbol_t * vdecl_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_vdecl_node_t * vdecl = (ast_vdecl_node_t *)node;
	symbol_t * vdecl_symb = lookup_symbol(vdecl->varid->charstar, st);

	if (vdecl_symb != NULL) {
		printf("ERROR: Cannot redeclare variable %s! " 
				"Exiting...\n", vdecl->varid->charstar);
		exit(EXIT_FAILURE);
	} 

	vdecl_symb = create_var_symbol(vdecl->varid->charstar, 
			vdecl->type_node->tnn->t,
			vdecl->type_node->tnt);

	add_symbol(vdecl_symb, st);
	node->s = vdecl_symb;
	node->s->context = st;
	return vdecl_symb;
}


void vdecl_codegen(ast_node_t * node, generator_t * gen) {

	ast_vdecl_node_t * vdecl = (ast_vdecl_node_t *)node;

	LLVMBasicBlockRef insert = LLVMGetInsertBlock(gen->builder);
	LLVMValueRef func = LLVMGetBasicBlockParent(insert);

	LLVMBasicBlockRef entry = LLVMGetEntryBasicBlock(func);
	LLVMValueRef term = LLVMGetBasicBlockTerminator(entry);
	
	if(term) {
		LLVMPositionBuilderBefore(gen->builder, term);
	} else {
		LLVMPositionBuilderAtEnd(gen->builder, entry);
	}

	switch(vdecl->type_node->tnt) {
		case PlainType:
			node->s->value = LLVMBuildAlloca(
					gen->builder,
					llvm_data_type(node->s->dt),
					vdecl->varid->charstar);
			break;
		case RefType: 
		case NoaRefType:
			{

				break;
			}
	}
	LLVMPositionBuilderAtEnd(gen->builder, insert);
}


ast_node_vtable vdecl_vtable = {
	delete_tree,
	print_ast_node,
	vdecl_symbol,
	vdecl_codegen
};


ast_vdecl_node_t * construct_vdecl_node(
		ast_type_node_t * type_node,
		ast_varid_node_t * varid) {

	ast_vdecl_node_t * node = 
		(ast_vdecl_node_t *)
		malloc(sizeof(ast_vdecl_node_t));

	((ast_node_t *)node)->type = VDecl;
	((ast_node_t *)node)->vtable = &vdecl_vtable;
	((ast_node_t *)node)->s = NULL;

	node->type_node = type_node;

	node->varid = varid;
	add_children(((ast_node_t *)node), 2, type_node, varid);

	return node;
}


// Literal node

ast_literal_node_t * construct_string_literal_node(
		ast_string_node_t * string_node) {

	ast_string_literal_node_t * node = 
		(ast_string_literal_node_t *)
		malloc(sizeof(ast_string_literal_node_t));

	((ast_node_t *)node)->type = Lit;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_literal_node_t *)node)->lnt = StringLiteral;

	node->string_node = string_node;
	add_children(((ast_node_t *)node), 1, string_node);

	return ((ast_literal_node_t *)node);
}


ast_literal_node_t * construct_integer_literal_node(
		ast_integer_node_t * integer_node) {

	ast_integer_literal_node_t * node = 
		(ast_integer_literal_node_t *)
		malloc(sizeof(ast_integer_literal_node_t));

	((ast_node_t *)node)->type = Lit;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_literal_node_t *)node)->lnt = IntegerLiteral;

	node->integer_node = integer_node;
	add_children(((ast_node_t *)node), 1, integer_node);
	return ((ast_literal_node_t *)node);
}


ast_literal_node_t * construct_float_literal_node(
		ast_float_node_t * float_node) {

	ast_float_literal_node_t * node = 
		(ast_float_literal_node_t *)
		malloc(sizeof(ast_float_literal_node_t));

	((ast_node_t *)node)->type = Lit;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_literal_node_t *)node)->lnt = FloatLiteral;

	node->float_node = float_node;
	add_children(((ast_node_t *)node), 1, float_node);
	return ((ast_literal_node_t *)node);
}


ast_literal_node_t * construct_bool_literal_node(
		ast_bool_node_t * bool_node) {

	ast_bool_literal_node_t * node = 
		(ast_bool_literal_node_t *)
		malloc(sizeof(ast_bool_literal_node_t));

	((ast_node_t *)node)->type = Lit;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_literal_node_t *)node)->lnt = BoolLiteral;

	node->bool_node = bool_node;
	add_children(((ast_node_t *)node), 1, bool_node);

	return ((ast_literal_node_t *)node);
}


ast_literal_node_t * construct_tdecls_literal_node(
		ast_list_node_t * tdecls_list) {

	ast_tdecls_literal_node_t * node = 
		(ast_tdecls_literal_node_t *)
		malloc(sizeof(ast_tdecls_literal_node_t));

	((ast_node_t *)node)->type = Lit;
	((ast_node_t *)node)->vtable = &ast_vtable;
	((ast_node_t *)node)->s = NULL;
	((ast_literal_node_t *)node)->lnt = TdeclsLiteral;

	node->tdecls_list = tdecls_list;
	add_children(((ast_node_t *)node), 1, tdecls_list);
	return ((ast_literal_node_t *)node);
}


ast_literal_node_t * construct_literal_node(
		literal_node_type lnt, ...) {

	ast_literal_node_t * node;
	va_list ap;
	va_start(ap, lnt);

	switch(lnt)
	{
		case StringLiteral:
			node = construct_string_literal_node(
					va_arg(ap, ast_string_node_t*));
			break;

		case IntegerLiteral:
			node = construct_integer_literal_node(
					va_arg(ap, ast_integer_node_t*));
			break;

		case FloatLiteral:
			node = construct_float_literal_node(
					va_arg(ap, ast_float_node_t*));
			break;

		case BoolLiteral:
			node = construct_bool_literal_node(
					va_arg(ap, ast_bool_node_t*));
			break;

		case TdeclsLiteral:
			node = construct_tdecls_literal_node(
					va_arg(ap, ast_list_node_t *));
			break;
	}

	va_end(ap);
	return node;
}

// Type name node

void print_typename_node(ast_node_t * node, int depth) {
	for (int i = 0; i < depth; i++) {
		printf("  ");
	}

	char * typename;

	switch(((ast_typename_node_t *)node)->t){
		case Int_type:
			typename = "int";
			break;
		case Cint_type:
			typename = "cint";
			break;
		case Float_type:
			typename = "float";
			break;
		case Bool_type:
			typename = "bool";
			break;
		case Void_type:
			typename = "void";
			break;
		case Char_type:
			typename = "char";
			break;
	}

	printf("Typename: %s\n", typename);
}

ast_node_vtable typename_vtable = {
	delete_tree,
	print_typename_node,
	ast_symbol,
	ast_codegen
};

ast_typename_node_t * construct_typename_node(
		data_type_type t) {

	ast_typename_node_t * node = 
		(ast_typename_node_t *)
		malloc(sizeof(ast_typename_node_t));

	((ast_node_t *)node)->type = TypeName;
	((ast_node_t *)node)->vtable = &typename_vtable;
	((ast_node_t *)node)->s = NULL;
	node->t = t;
	add_children(((ast_node_t *)node), 0);
	return node;
}

// String node

void destroy_string_node(ast_node_t * node){
	free((char *)((ast_string_node_t *)node)->charstar);
	free(node->children);
	free((ast_string_node_t *)node);
}

void print_string_node(ast_node_t * node, int depth){
	for (int i = 0; i < depth; i++) {
		printf("  ");
	}
	printf("String Node: %s\n", 
			((ast_string_node_t *)node)->charstar);
}

symbol_t * string_symbol(ast_node_t * node, symbol_table_t * st) {
	// ast_string_node_t * string = (ast_string_node_t *) node;
	node->s = create_anon_symbol(Char_type);
	node->s->context = st;
	return node->s;
}

void string_codegen(ast_node_t * node, generator_t * gen) {
	ast_string_node_t * string = (ast_string_node_t *) node;

	node->s->value = LLVMBuildGlobalStringPtr(gen->builder, 
			string->charstar, "a string");
}

ast_node_vtable string_vtable = {
	destroy_string_node,
	print_string_node,
	string_symbol,
	string_codegen
};

ast_string_node_t * construct_string_node(
		const char * charstar) {

	ast_string_node_t * node = 
		(ast_string_node_t *)
		malloc(sizeof(ast_string_node_t));

	((ast_node_t *)node)->type = String;
	((ast_node_t *)node)->vtable = &string_vtable;
	((ast_node_t *)node)->s = NULL;
	node->charstar = charstar;
	add_children(((ast_node_t *)node), 0);
	return node;
}

// Global identifier node


void destroy_globid_node(ast_node_t * node){
	free((char *)((ast_globid_node_t *)node)->charstar);
	free(node->children);
	free((ast_globid_node_t *)node);
}

void print_globid_node(ast_node_t * node, int depth) {
	for (int i = 0; i < depth; i++) { 
		printf("  ");
	}

	printf("Global Identifier: %s\n", 
			(((ast_globid_node_t *)node)->charstar));
}

symbol_t * globid_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_globid_node_t * globid = (ast_globid_node_t *) node;
	symbol_t * globid_symb = lookup_symbol(globid->charstar, st);
	if (globid_symb == NULL) {
		printf("ERROR! Unable to find function %s\n"
				"Exiting...", globid->charstar);
		exit(EXIT_FAILURE);
	}

	node->s = create_anon_symbol(globid_symb->dt);
	node->s->context = st;
	return node->s;
}


ast_node_vtable globid_vtable = {
	destroy_globid_node,
	print_globid_node,
	globid_symbol,
	ast_codegen
};

ast_globid_node_t * construct_globid_node(
		const char * charstar) {

	ast_globid_node_t * node = 
		(ast_globid_node_t *) 
		malloc(sizeof(ast_globid_node_t));

	((ast_node_t *)node)->type = GlobId;
	((ast_node_t *)node)->vtable = &globid_vtable;
	((ast_node_t *)node)->s = NULL;
	node->charstar = charstar;
	add_children(((ast_node_t *)node), 0);
	return node;
}

// Variable identifier node

void destroy_varid_node(ast_node_t * node){
	free((char *)((ast_varid_node_t *)node)->charstar);
	free(node->children);
	free((ast_varid_node_t *)node);
}

void print_varid_node(ast_node_t * node, int depth) {
	for (int i = 0; i < depth; i++) { 
		printf("  ");
	}

	printf("Variable Identifier: %s\n", 
			(((ast_varid_node_t *)node)->charstar));
}

/*********************************************************|
 * Since vdecls don't call this function, this should fail
 * and exit if the symbol is not found... for now, vdecls' 
 * symbol functions deal with the varid node directly
 * without invoking symbol on the var.
 *********************************************************/
symbol_t * varid_symbol(ast_node_t * node, symbol_table_t * st) {
	ast_varid_node_t * varid = (ast_varid_node_t *) node;
	symbol_t * varid_symb = lookup_symbol(varid->charstar, st);
	if(varid_symb == NULL) {
		printf("ERROR! Variable identifier %s not found! "
				"Exiting...\n", varid->charstar);
		exit(EXIT_FAILURE);
	}
	node->s = create_anon_symbol(varid_symb->dt);
	node->s->context = st;
	return node->s;
}


void varid_codegen(ast_node_t * node, generator_t * gen) {
	if (node->s == NULL) {
		printf("ERROR! Nonexistent symbol for varid node."
				" Exiting...\n");
		exit(EXIT_FAILURE);
	}

	ast_varid_node_t * varid = 
		(ast_varid_node_t *) node;

	symbol_t * varid_symbol = 
		lookup_symbol(varid->charstar, node->s->context);

	var_symbol_t * var_symb = 
		(var_symbol_t *) var_symb;

	node->s->value = varid_symbol->value;
}


ast_node_vtable varid_vtable = {
	destroy_varid_node,
	print_varid_node,
	varid_symbol,
	varid_codegen

};

ast_varid_node_t * construct_varid_node(
		const char * charstar) {

	ast_varid_node_t * node = 
		(ast_varid_node_t *) 
		malloc(sizeof(ast_varid_node_t));

	((ast_node_t *)node)->type = VarId;
	((ast_node_t *)node)->vtable = &varid_vtable;
	((ast_node_t *)node)->s = NULL;
	node->charstar = charstar;
	add_children(((ast_node_t *)node), 0);
	return node;
}


// Integer node

void print_integer_node(ast_node_t * node, int depth) {
	for (int i = 0; i < depth; i++) { 
		printf("  ");
	}

	printf("Integer: %d\n", (((ast_integer_node_t *)node)->integer_value));
}


symbol_t * int_symbol(ast_node_t * node, symbol_table_t * st) {
	symbol_t * rtn_symb = create_anon_symbol(Int_type);
	rtn_symb->context = st;
	node->s = rtn_symb;
	return rtn_symb;
}


void int_codegen(ast_node_t * node, generator_t * gen) {
	if (node->s == NULL) {
		printf("ERROR! Nonexistent symbol for int node."
				" Exiting...\n");
		exit(EXIT_FAILURE);
	}
	ast_integer_node_t * inn = (ast_integer_node_t *) node;
	node->s->value = LLVMConstInt(LLVMInt32Type(), inn->integer_value, 1);
}


ast_node_vtable integer_vtable = {
	delete_tree,
	print_integer_node,
	int_symbol,
	int_codegen
};


ast_integer_node_t * construct_integer_node(
		int integer_value) {

	ast_integer_node_t * node = 
		(ast_integer_node_t *) 
		malloc(sizeof(ast_integer_node_t));

	((ast_node_t *)node)->type = VarId;
	((ast_node_t *)node)->vtable = &integer_vtable;
	((ast_node_t *)node)->s = NULL;
	node->integer_value = integer_value;
	add_children(((ast_node_t *)node), 0);
	return node;
}


// Floats

void print_float_node(ast_node_t * node, int depth) {
	for (int i = 0; i < depth; i++) { 
		printf("  ");
	}

	printf("Float: %f\n" , (((ast_float_node_t *)node)->float_value));
}


symbol_t * float_symbol(ast_node_t * node, symbol_table_t * st) {
	// ast_float_node_t * fl = (ast_float_node_t *) node;

	symbol_t * rtn_symb = create_anon_symbol(Float_type);
	rtn_symb->context = st;
	node->s = rtn_symb;
	return rtn_symb;
}	


void float_codegen(ast_node_t * node, generator_t * gen) {
	if (node->s == NULL) {
		printf("ERROR! Nonexistent symbol for float node."
				" Exiting...\n");
		exit(EXIT_FAILURE);
	}
	ast_float_node_t * flo = (ast_float_node_t *) node;
	node->s->value = LLVMConstReal(LLVMFloatType(), flo->float_value);
}


ast_node_vtable float_vtable = {
	delete_tree,
	print_float_node,
	float_symbol,
	float_codegen
};


ast_float_node_t * construct_float_node(
		float float_value) {

	ast_float_node_t * node = 
		(ast_float_node_t *) 
		malloc(sizeof(ast_float_node_t));

	((ast_node_t *)node)->type = VarId;
	((ast_node_t *)node)->vtable = &float_vtable;
	((ast_node_t *)node)->s = NULL;

	node->float_value = float_value;
	add_children(((ast_node_t *)node), 0);
	return node;
}

// Bools

void print_bool_node(ast_node_t * node, int depth) {
	for (int i = 0; i < depth; i++) { 
		printf("  ");
	}

	if((((ast_bool_node_t *)node)->bool_value) == 1)
		printf("%s\n", "true"); 

	if((((ast_bool_node_t *)node)->bool_value) == 0)
		printf("%s\n", "false"); 
}

symbol_t * bool_symbol(ast_node_t * node, symbol_table_t * st) {
	symbol_t * rtn_symb = create_anon_symbol(Bool_type);
	rtn_symb->context = st;
	node->s = rtn_symb;
	return rtn_symb;
}

void bool_codegen(ast_node_t * node, generator_t * gen) {
	if (node->s == NULL) {
		printf("ERROR! Nonexistent symbol for bool node."
				" Exiting...\n");
		exit(EXIT_FAILURE);
	}
	ast_bool_node_t * boo = (ast_bool_node_t *) node;
	node->s->value = LLVMConstInt(
			LLVMInt1Type(), boo->bool_value, 0);
}

ast_node_vtable bool_vtable = {
	delete_tree,
	print_bool_node,
	bool_symbol,
	bool_codegen
};

ast_bool_node_t * construct_bool_node(
		int bool_value) {

	ast_bool_node_t * node = 
		(ast_bool_node_t *) 
		malloc(sizeof(ast_bool_node_t));

	((ast_node_t *)node)->type = VarId;
	((ast_node_t *)node)->vtable = &bool_vtable;
	((ast_node_t *)node)->s = NULL;

	node->bool_value = bool_value;
	add_children(((ast_node_t *)node), 0);
	return node;
}


// Special List Node Type 

void print_list_node(ast_node_t * node, int depth) {
	if(node == NULL) return;

	ast_node_t * current = 
		((ast_node_t *)((ast_list_node_t *)node)->current);

	if(current != NULL) 
		current->vtable->print(current, depth);

	ast_list_node_t * next = 
		((ast_list_node_t *)((ast_list_node_t *)node)->next);

	if(next != NULL) 
		((ast_node_t *)next)->vtable->print((ast_node_t *)next, depth);

	return;
}

void delete_list(ast_node_t * node) {
	if(node == NULL) return;

	ast_node_t * current = 
		((ast_node_t *)((ast_list_node_t *)node)->current);

	if(current != NULL) 
		current->vtable->destroy(current);

	ast_list_node_t * next = 
		((ast_list_node_t *)((ast_list_node_t *)node)->next);

	if(next != NULL) 
		((ast_node_t *)next)->vtable->destroy((ast_node_t *)next);

	free(node->children);
	free(node);

	return;
}

symbol_t * list_symbol(ast_node_t * node, symbol_table_t * st) {
	if(node == NULL) return NULL;

	symbol_t * rtn = NULL;
	ast_node_t * current = ((ast_node_t *)((ast_list_node_t *)node)->current);
	if(current != NULL) {
		rtn = current->vtable->symbol(current, st);
	}

	ast_list_node_t * next = 
		((ast_list_node_t *)((ast_list_node_t *)node)->next);

	if(next != NULL) 
		((ast_node_t *)next)->vtable->symbol((ast_node_t *)next, st);

	node->s = create_anon_symbol(rtn->dt);
	node->s->context = st;
	return rtn;
}

void list_codegen(ast_node_t * node, generator_t * gen) {
	if(node == NULL) return;

	ast_node_t * current = ((ast_node_t *)
			((ast_list_node_t *)node)->current);

	if(current != NULL) {
		current->vtable->codegen(current, gen);
	}

	ast_list_node_t * next = 
		((ast_list_node_t *)((ast_list_node_t *)node)->next);

	if(next != NULL) {
		((ast_node_t *)next)->vtable
			->codegen((ast_node_t *)next, gen);
	}

	node->s->value = current->s->value;
}


ast_node_vtable list_vtable = {
	delete_list,
	print_list_node,
	list_symbol,
	list_codegen
};

ast_list_node_t * construct_list_node(
		ast_node_t * current) {

	ast_list_node_t * node =
		(ast_list_node_t *)
		malloc(sizeof(ast_list_node_t));

	((ast_node_t *)node)->type = List;
	((ast_node_t *)node)->vtable = &list_vtable;
	((ast_node_t *)node)->s = NULL;

	node->current = current;
	node->next = NULL;
	add_children(((ast_node_t *)node), 1, current);
	return node;
}

void append_ast_list(ast_list_node_t * list, 
		ast_node_t * newthing) {
	if (list == NULL) return;

	ast_list_node_t * new_list_item = 
		construct_list_node(newthing);

	while(list->next != NULL) {
		list = list->next;
	}

	list->next = new_list_item;
	return;
}

#include "deleter.c"


