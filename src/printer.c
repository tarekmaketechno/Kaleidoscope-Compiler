#include "../inc/printer.h"
#include <stdio.h>
#include <stdbool.h>

void translate_type(ast_node_t * node, char * buffer) {
		
	switch(node->type)
	{
		case Program:
			sprintf(buffer, "%s", "Program");
			break;
		case Extern:
			sprintf(buffer, "%s", "Extern");
			break;
		case Func:
			sprintf(buffer, "%s", "Function");
			break;
		case Blk:
			sprintf(buffer, "%s", "Block");
			break;
		case Statement:
			sprintf(buffer, "%s", "Statement");
			break;
		case Exp:
			sprintf(buffer, "%s", "Expression");
			break;
		case BinOp:
			sprintf(buffer, "%s", "Binary Operation");
			break;
		case ArithOp:
			sprintf(buffer, "%s", "ArithOp");
			break;
		case LogicOp:
			sprintf(buffer, "%s", "LogicOp");
			break;
		case UnOp:
			sprintf(buffer, "%s", "Unary Operation");
			break;
		case Lit:
			sprintf(buffer, "%s", "Literal Expression");
			break;
		case List:
			sprintf(buffer, "%s", "List Entry");
			break;
		case VarId:
			sprintf(buffer, "%s", "Variable Identifier");
			break;
		case String:
			sprintf(buffer, "%s", "String");
			break;
		case GlobId:
			sprintf(buffer, "%s", "Global Identifier");
			break;
		case Type:
			sprintf(buffer, "%s", "Type");
			break;
		case VDecl:
			sprintf(buffer, "%s", "Variable Declaration");
			break;
		case TypeName:
			sprintf(buffer, "%s", "Type Name");
			break;
		default:
			printf("Unrecognized node! Exiting...");
			exit(1);
	}

	return;
}

int print_ast_recursive(ast_node_t * node, int depth) {
	if(!node) return 0;

	char buffer[64];
	for (int j = 0; j < depth; j++) printf("  ");

	if(node->type == List) {
		printf("List entry:\n");
		print_ast_recursive(((ast_list_node_t *)node)->current, depth + 1);
		print_ast_recursive(
				((ast_node_t*)((ast_list_node_t *)node)->next), depth);

		return 0;
	}

	translate_type(node, buffer);
	printf("%s\n", buffer);

	ast_node_t ** children = node->children;
	for(int i = 0; i < node->n_children; i++) {
		print_ast_recursive(*children, depth + 1);
		children++;
	}
	return 0;
}

int print_ast_runner(ast_node_t * node) {

	// Recursively print AST
	if (!print_ast_recursive(node, 0)) return -1;

	return 0;
}

