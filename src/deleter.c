/************************************************|
 * Type-specific deletes=========================|
 *
 * Making delete functions for each type got to 
 * be a big chore, so I made this section instead
 ************************************************/

void delete_statement_node(ast_stmt_node_t * node) {
	switch(node->stmt_type) {
		case IfStatement:
			free((ast_if_stmt_node_t *)node);
			break;
		case ReturnStatement:
			free((ast_return_stmt_node_t *)node);
			break;
		case AssignStatement:
			free((ast_assign_stmt_node_t *)node);
			break;
		case WhileStatement:
			free((ast_while_stmt_node_t *)node);
			break;
		case ExpressionStatement:
			free((ast_expression_stmt_node_t *)node);
			break;
		case BlkStatement:
			free((ast_blk_stmt_node_t *)node);
			break;
		case PrintStatement:
			free((ast_print_stmt_node_t *)node);
			break;
		default:
			break;
	}
}

void delete_expression_node(ast_expression_node_t * node) {
	switch(node->exp_type) {
		case ExpressionList:
			free((ast_list_expression_node_t *)node);
			break;
		case BinOpExpression:
			free((ast_binop_expression_node_t *)node);
			break;
		case UnOpExpression:
			free((ast_unop_expression_node_t *)node);
			break;
		case LiteralExpression:
			free((ast_literal_expression_node_t *)node);
			break;
		case VarIdExpression:
			free((ast_varid_expression_node_t *)node);
			break;
		case GlobIdExpression:
			free((ast_globid_expression_node_t *)node);
			break;
	}

}

void delete_binop_node(ast_binop_node_t * node) {
	switch(node->bt) {
		case BinaryArithOp:
			free((ast_binop_arithop_node_t *)node);
			break;
		case BinaryLogicOp:
			free((ast_binop_logicop_node_t *)node);
			break;
		case BinaryAssignOp:
			free((ast_binop_assignop_node_t *)node);
			break;
		case BinaryCastOp:
			free((ast_binop_castop_node_t *)node);
			break;

	}
}


void delete_literal_node(ast_literal_node_t * node) {
	switch(node->lnt) {
		case StringLiteral:
			free((ast_string_literal_node_t *)node);
			break;
		case IntegerLiteral:
			free((ast_integer_literal_node_t *)node);
			break;
		case FloatLiteral:
			free((ast_float_literal_node_t *)node);
			break;
		case BoolLiteral:
			free((ast_bool_literal_node_t *)node);
			break;
		case TdeclsLiteral:
			free((ast_tdecls_literal_node_t *)node);
			break;
	}
}

void type_aware_delete(ast_node_t * node) {
	switch(node->type) {
		case Program:
			free(((ast_program_node_t *)node));
			break;

		case Extern:
			free(((ast_extern_node_t *)node));
			break;

		case Func:
			free(((ast_func_node_t *)node));
			break;

		case Blk:
			free(((ast_blk_node_t *)node));
			break;

		case Statement:
			delete_statement_node(((ast_stmt_node_t *)node));
			break;

		case Exp:
			delete_expression_node(((ast_expression_node_t *)node));
			break;

		case BinOp:
			delete_binop_node(((ast_binop_node_t *)node));
			break;

		case ArithOp:
			free(((ast_arithop_node_t *)node));
			break;

		case LogicOp:
			free(((ast_logicop_node_t *)node));
			break;

		case UnOp:
			free(((ast_unop_node_t *)node));
			break;

		case Lit:
			delete_literal_node(((ast_literal_node_t *)node));
			break;

		case VarId:
			free(((ast_varid_node_t *)node));
			break;

		case String:
			free(((ast_string_node_t *)node));
			break;

		case List:
			free(((ast_list_node_t *)node));
			break;

		case GlobId:
			free(((ast_globid_node_t *)node));
			break;

		case Type:
			free(((ast_type_node_t *)node));
			break;

		case VDecl:
			free(((ast_vdecl_node_t *)node));
			break;

		case TypeName:
			free(((ast_typename_node_t *)node));
			break;

		default:
			perror("Error! Unrecognized type... Exiting");
			exit(-1);

	}
}

