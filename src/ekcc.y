%{
	#include <stdbool.h>
	#include "ast.h"
	#include "type_rules.h"

	extern int yylex();
	extern int yyparse();
	void yyerror(ast_node_t ** prog, const char *s);
	extern int line_number;
%}

%union {
	int	ival;
	float	fval;
	bool 	bval;
	char*	sval;

	data_type_type			dtt;
	ast_node_t *			ast;
	ast_list_node_t *		lst;

	ast_program_node_t *		prognode;
	ast_extern_node_t *		externode;
	ast_func_node_t *		funcnode;
	ast_blk_node_t *		blknode;
	ast_stmt_node_t	*		stmtnode;
	ast_expression_node_t *		exprnode;

	ast_binop_node_t *		binopnode;
	ast_arithop_node_t *		arithopnode;
	ast_logicop_node_t *		logicopnode;
	ast_unop_node_t	*		unopnode;

	ast_type_node_t	*		typenode;
	ast_vdecl_node_t *		vdeclnode;
	ast_literal_node_t *		literalnode;

	ast_typename_node_t *		typenamenode;
	ast_string_node_t *		stringnode;
	ast_globid_node_t *		globidnode;
	ast_varid_node_t *		varidnode;
	ast_integer_node_t *		integernode;
	ast_float_node_t *		floatnode;
	ast_bool_node_t	*		boolnode;
}

%right EQ
%right NOT
%precedence NEG
%left LAND
%left LOR
%left LT GT
%left DBL_EQ
%left TIMES
%left DIV
%left PLUS MINUS

%token RETURN
%token WHILE
%token PRINT
%token IF
%token ELSE
%token SEMICOLON
%token NOALIAS
%token COMMA
%token ENDL
%token V_DECL
%token OPEN_PARENS
%token CLOSE_PARENS
%token OPEN_CURLY
%token CLOSE_CURLY
%token EXTERN
%token OPEN_SQ
%token CLOSE_SQ
%token REF
%token DEF
%token ALL

%token <ival> INT
%token <fval> FLOAT
%token <sval> IDENT 
%token <sval> VARID
%token <sval> SLIT
%token <bval> BOOL
%token <dtt> TYPENAME

%type <ast>		prog
%type <externode>	extern
%type <funcnode>	func
%type <blknode>		blk
%type <stmtnode>	statement
%type <exprnode>	expression
               
%type <logicopnode>	logicop
%type <binopnode>	binop
%type <arithopnode>	arithop
%type <unopnode>	unop
               
%type <vdeclnode>	vdecl
%type <literalnode>	literal
%type <typenamenode>	typename
%type <typenode>	type
%type <stringnode>	string
%type <globidnode>	globid
%type <varidnode>	varid
%type <integernode>	integer
%type <floatnode>	float
%type <boolnode>	bool

%type <lst> externs funcs statements expressions vdecls tdecls



%parse-param {ast_node_t ** p}

%%

prog:
	externs funcs  { *p = (ast_node_t *) construct_program_node($1, $2); }
	| funcs { *p = (ast_node_t *) construct_program_node(NULL, $1); }

externs:
	externs extern { append_ast_list($1, ((ast_node_t *)$2)); }
	| extern { $$ = construct_list_node((ast_node_t *)$1); }

extern:
	EXTERN type globid OPEN_PARENS tdecls CLOSE_PARENS SEMICOLON
		{ $$ = construct_extern_node($2, $3, $5); }
	| EXTERN type globid OPEN_PARENS CLOSE_PARENS SEMICOLON 
		{ $$ = construct_extern_node($2, $3, NULL); }

funcs:
	funcs func 
		{ append_ast_list($1, ((ast_node_t *)$2)); }
	| func 
		{ $$ = construct_list_node((ast_node_t *)$1); }

func:
	DEF type globid OPEN_PARENS vdecls CLOSE_PARENS blk
		{ $$ = construct_func_node($2, $3, $5, $7); }
	| DEF type globid OPEN_PARENS CLOSE_PARENS blk
		{ $$ = construct_func_node($2, $3, NULL, $6); }

blk:
	OPEN_CURLY statements CLOSE_CURLY
		{ $$ = construct_blk_node($2); }
	| OPEN_CURLY CLOSE_CURLY
		{ $$ = construct_blk_node(NULL); }

statements:
	statements statement { append_ast_list($1, ((ast_node_t *)$2)); }
	| statement { $$ = construct_list_node((ast_node_t *)$1); }

statement:
	blk
		{ $$ = construct_stmt_node(BlkStatement, 1, $1); }
	| RETURN SEMICOLON 
		{ $$ = construct_stmt_node(ReturnStatement, 1, NULL); }
	| RETURN expression SEMICOLON 
		{ $$ = construct_stmt_node(ReturnStatement, 1, $2); }
	| vdecl EQ expression SEMICOLON 
		{ $$ = construct_stmt_node(AssignStatement, 2, $1, $3); }
	| expression SEMICOLON 
		{ $$ = construct_stmt_node(ExpressionStatement, 1, $1); }
	| WHILE OPEN_PARENS expression CLOSE_PARENS statement 
		{ $$ = construct_stmt_node(WhileStatement, 2, $3, $5); }
	| IF OPEN_PARENS expression CLOSE_PARENS statement 
		{ $$ = construct_stmt_node(IfStatement, 3, $3, $5, NULL); }
	| IF OPEN_PARENS expression CLOSE_PARENS statement ELSE statement 
		{ $$ = construct_stmt_node(IfStatement, 3, $3, $5, $7); }
	| PRINT string SEMICOLON
		{ $$ = construct_stmt_node(PrintStatement, 1, NULL, $2); }
	| PRINT expression SEMICOLON
		{ $$ = construct_stmt_node(PrintStatement, 1, $2, NULL); }

unop:
	NOT expression
		{ $$ = construct_unop_node(NotUnOp, $2); }
	| MINUS expression %prec NEG 
		{ $$ = construct_unop_node(NegateOp, $2); }

arithop:
	expression TIMES expression
		{ $$ = construct_arithop_node(ArithTimesOp, $1, $3); }
	| expression DIV expression
		{ $$ = construct_arithop_node(ArithDivOp, $1, $3); }
	| expression PLUS expression
		{ $$ = construct_arithop_node(ArithPlusOp, $1, $3); }
	| expression MINUS expression
		{ $$ = construct_arithop_node(ArithMinusOp, $1, $3); }

logicop:
	expression LT expression
		{ $$ = construct_logicop_node(LessThanOp, $1, $3); }
	| expression GT expression
		{ $$ = construct_logicop_node(GreaterOp, $1, $3); }
	| expression DBL_EQ expression
		{ $$ = construct_logicop_node(EqualityOp, $1, $3); }
	| expression LAND expression
		{ $$ = construct_logicop_node(LogicAndOp, $1, $3); }
	| expression LOR expression
		{ $$ = construct_logicop_node(LogicOrOp, $1, $3); }

binop:
	arithop 
		{ $$ = construct_binop_node(BinaryArithOp, 1, $1); }
	| logicop
		{ $$ = construct_binop_node(BinaryLogicOp, 1, $1); }
	| varid EQ expression
		{ $$ = construct_binop_node(BinaryAssignOp, 2, $1, $3); }
	| OPEN_SQ typename CLOSE_SQ expression
		{ $$ = construct_binop_node(BinaryCastOp, 2, $2, $4); }

expressions:
	expressions COMMA expression 
		{ append_ast_list($1, ((ast_node_t *)$3)); }
	| expression 
		{ $$ = construct_list_node((ast_node_t *)$1); }

expression:
	OPEN_PARENS CLOSE_PARENS 
		{ $$ =  construct_expression_node(ExpressionList, 1, NULL); }
	| OPEN_PARENS expressions CLOSE_PARENS 
		{ $$ =  construct_expression_node(ExpressionList, 1, $2); }
	| unop
		{ $$ =  construct_expression_node(UnOpExpression, 1, $1); }
	| binop
		{ $$ =  construct_expression_node(BinOpExpression, 1, $1); }
	| literal
		{ $$ =  construct_expression_node(LiteralExpression, 1, $1); }
	| varid
		{ $$ =  construct_expression_node(VarIdExpression, 1, $1); }
	| globid OPEN_PARENS CLOSE_PARENS
		{ $$ =  construct_expression_node(GlobIdExpression, 2, $1, NULL); } 
	| globid OPEN_PARENS expressions CLOSE_PARENS
		{ $$ =  construct_expression_node(GlobIdExpression, 2, $1, $3); }

type:
	NOALIAS REF typename
		{ $$ = construct_type_node(NoaRefType, $3); }
	| REF typename
		{ $$ = construct_type_node(RefType, $2); }
	| typename
		{ $$ = construct_type_node(PlainType, $1); }
	| REF REF typename
		{ yyerror(p, "A ref type cannot be ref"); }

vdecls:
	vdecl 
		{ $$ = construct_list_node((ast_node_t *)$1); }
	| vdecls COMMA vdecl 
		{ append_ast_list($1, ((ast_node_t *)$3)); }

vdecl:
	type varid 
		{ $$ = construct_vdecl_node($1, $2); }

literal:
	string
		{ $$ = construct_literal_node(StringLiteral, $1); }
	| integer
		{ $$ = construct_literal_node(IntegerLiteral, $1); }
	| float
		{ $$ = construct_literal_node(FloatLiteral, $1); }
	| bool
		{ $$ = construct_literal_node(BoolLiteral, $1); }
	| tdecls
		{ $$ = construct_literal_node(TdeclsLiteral, $1); }

tdecls:
	type
		{ $$ = construct_list_node((ast_node_t *)$1); }
	| tdecls COMMA type
		{ append_ast_list($1, ((ast_node_t *)$3)); }

typename:
	TYPENAME
		{ $$ = construct_typename_node($1);}

string:
	SLIT
		{ $$ = construct_string_node($1);}

globid:
	IDENT
		{ $$ = construct_globid_node($1);}

varid:
	VARID
		{ $$ = construct_varid_node($1);}

integer:
	INT
		{ $$ = construct_integer_node($1);}

float:
	FLOAT 
		{ $$ = construct_float_node($1); }

bool:
	BOOL
		{ $$ = construct_bool_node($1); }

%%


void yyerror(ast_node_t ** p, const char *s) {
	printf("Parse error on line %d: \n%s\n", line_number, s);
	exit(EXIT_FAILURE);
}
