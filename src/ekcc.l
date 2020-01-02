%{
	#include <string.h>
	#include <stdlib.h>
	#include <stdio.h>
	#include <stdbool.h>
	#include "ast.h"
	#include "ekcc.tab.h"  // to get the token types from Bison

	int line_number = 1;

	int parsetyp(char * raw_in) {
		data_type_type rtn;

		if(strncmp(raw_in, "int", 3) == 0) {
			rtn = Int_type;
			goto out;
		}
		else if(strncmp(raw_in, "void", 3) == 0) {
			rtn = Void_type;
			goto out;
		}
		else if(strncmp(raw_in, "cint", 3) == 0) {
			rtn = Cint_type;
			goto out;
		}
		else if(strncmp(raw_in, "float", 3) == 0) {
			rtn = Float_type;
			goto out;
		}
		else if(strncmp(raw_in, "bool", 3) == 0) {
			rtn = Bool_type;
			goto out;
		}
		else { perror("Error! Unrecognized type found... Exiting.");
			exit(-1); }

out:
		return rtn;
	}
%}

COMMENT		\#.*[\n\r]*
SLIT 		\"[^\"\n\r]*\"
WHITE		[ \t]
DIGIT		[0-9]
NONZERO		[1-9]
INT		{DIGIT}*
FLOAT		{DIGIT}*\.{DIGIT}*{DIGIT}+
EXTERN		extern
BOOL		true|false
TYPENAME	int|cint|float|bool|void
IDENT		[a-zA-Z_]+[a-zA-Z0-9_]*
NOALIAS 	noalias
RETURN		return
WHILE		while
PRINT		print
IF		if
ELSE		else
COMMA		\,
NEWLINE		\n
SEMICOLON	;
OPEN_PARENS	\(
CLOSE_PARENS	\)
OPEN_CURLY	\{
CLOSE_CURLY	\}
DOLLAR		\$
OPEN_SQ		\[
CLOSE_SQ	\]
PLUS		\+
MINUS		\-
TIMES		\*
DIV		\/
DEF		def
REF		ref
NOT		\!
ALL		.
LT		\<
GT		\>
LAND		&&
LOR		\|\|
EQ		\=

%option noyywrap
%option nounput
%option noinput

%%
{WHITE}				;
{COMMENT}			;
{TYPENAME}			{ yylval.dtt = parsetyp(yytext); return TYPENAME; }
{DOLLAR}{IDENT}			{ yylval.sval= strdup(yytext); return VARID; }
{SLIT}				{ yylval.sval =strdup(yytext); return SLIT; }
{FLOAT}				{ yylval.fval = atof(yytext); return FLOAT; }
{INT}				{ yylval.ival = atoi(yytext); return INT; }
{BOOL}				{ yylval.bval = !strcmp(yytext, "true"); return BOOL; }
{REF}				{ return REF; }
{DEF}				{ return DEF; }
{NOT}				{ return NOT; }
{NOALIAS}			{ return NOALIAS; }
{EXTERN}			{ return EXTERN; }
{WHILE}				{ return WHILE; }
{RETURN}			{ return RETURN; }
{PRINT}				{ return PRINT; }
{IF}				{ return IF; }
{ELSE}				{ return ELSE; }
{IDENT}				{ yylval.sval=strdup(yytext); return IDENT; }
{OPEN_PARENS}			{ return OPEN_PARENS; }
{CLOSE_PARENS}			{ return CLOSE_PARENS; }
{OPEN_CURLY}			{ return OPEN_CURLY; }
{CLOSE_CURLY}			{ return CLOSE_CURLY; }
{OPEN_SQ}			{ return OPEN_SQ; }
{CLOSE_SQ}			{ return CLOSE_SQ; }
{PLUS}				{ return PLUS; }
{MINUS}				{ return MINUS; }
{TIMES}				{ return TIMES; }
{DIV}				{ return DIV; }
{COMMA}				{ return COMMA; }
{LT}				{ return LT; }
{GT}				{ return GT; }
{LAND}				{ return LAND; }
{LOR}				{ return LOR; }
{EQ}{EQ}			{ return DBL_EQ; }
{EQ}				{ return EQ; }
{SEMICOLON}			{ return SEMICOLON; }
{NEWLINE}			{ ++line_number; }
{ALL}				;
%%

