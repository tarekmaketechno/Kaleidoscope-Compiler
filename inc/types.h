#pragma once 

#ifndef MAX_NAME_LENGTH
#define MAX_NAME_LENGTH 128
#endif

/*
 *
 * Enums for various things
 *
 */

typedef enum data_type_type {
	Int_type,Cint_type,Char_type,
	Float_type,Bool_type,Void_type
} data_type_type;

typedef enum node_type {
	Program, Extern, Func, Blk, Statement, 
	Exp, BinOp, ArithOp, LogicOp, 
	UnOp, Lit, VarId, String, List,
	GlobId, Type, VDecl, TypeName
} node_type;


typedef enum statement_type {
	IfStatement, ReturnStatement, AssignStatement,
	WhileStatement, ExpressionStatement, BlkStatement,
	PrintStatement
} statement_type;


typedef enum expression_type {
	ExpressionList, BinOpExpression, UnOpExpression, 
	LiteralExpression, VarIdExpression, GlobIdExpression
} expression_type;


typedef enum logicop_type {
	EqualityOp, LessThanOp,
	GreaterOp, LogicAndOp, LogicOrOp          
} logicop_type;


typedef enum binop_type {
	BinaryArithOp, BinaryLogicOp,
	BinaryAssignOp, BinaryCastOp 
} binop_type;


typedef enum arithop_type {
	ArithTimesOp, ArithDivOp, 
	ArithPlusOp, ArithMinusOp
} arithop_type;

typedef enum unop_type {
	NotUnOp, NegateOp
} unop_type;

typedef enum type_node_type { 
	PlainType, RefType, NoaRefType
} type_node_type;


typedef enum literal_node_type {
	StringLiteral, IntegerLiteral,
	FloatLiteral, BoolLiteral, TdeclsLiteral, 
} literal_node_type;

typedef enum symbol_type {
	Globid_Symb, Varid_Symb, Anon_Symb
} symbol_type;


// Top-Level Stuff
typedef struct ast_node_vtable ast_node_vtable;
typedef struct ast_list_node_t ast_list_node_t;
typedef struct ast_node_t ast_node_t;

typedef struct ast_program_node_t ast_program_node_t;
typedef struct ast_extern_node_t ast_extern_node_t;
typedef struct ast_func_node_t ast_func_node_t;
typedef struct ast_blk_node_t ast_blk_node_t; 
typedef struct ast_stmt_node_t ast_stmt_node_t;
typedef struct ast_expression_node_t ast_expression_node_t;

// Operator node types
typedef struct ast_binop_node_t ast_binop_node_t;
typedef struct ast_arithop_node_t ast_arithop_node_t;
typedef struct ast_logicop_node_t ast_logicop_node_t;
typedef struct ast_unop_node_t ast_unop_node_t;


// Miscellaneous
typedef struct ast_type_node_t ast_type_node_t;
typedef struct ast_vdecl_node_t ast_vdecl_node_t;
typedef struct ast_literal_node_t ast_literal_node_t;
typedef struct ast_tdecl_node_t ast_tdecl_node_t;


// Things with concrete (POD) values
typedef struct ast_typename_node_t ast_typename_node_t;
typedef struct ast_string_node_t ast_string_node_t;
typedef struct ast_globid_node_t ast_globid_node_t;
typedef struct ast_varid_node_t ast_varid_node_t;
typedef struct ast_integer_node_t ast_integer_node_t;
typedef struct ast_float_node_t ast_float_node_t;
typedef struct ast_bool_node_t ast_bool_node_t;


