%{
#include <stdio.h>
#include "tree.h"

extern char *yytext;
extern PROGRAM *mainProgram;

void yyerror() {
   printf ("syntax error before %s\n", yytext); 
}
%}

%union {
   int intconst;
   char *stringconst;
   struct EXP *exp;
    struct PROGRAM *program;
    struct STMT *stmt;
    struct FUNC *func_call;
    struct ARG_LIST *arg_list;
    struct FUNC_DECL *func_decl;

}

%token <intconst> tINTCONST
%token <stringconst> tIDENTIFIER 
%token IF ELSE RETURN ASSERT EQ UNKNOWN NEQ


%type	<program> program
%type 	<exp> exp
%type	<stmt>		stmt
%type	<func_call>	func_call
%type	<arg_list>	next_args
%type	<arg_list>	arg_list
%type	<arg_list>	arg_decl_list
%type	<arg_list>	next_decl_args
%type	<func_decl>	func_decl


%start top

%left '+' '-'
%left '*' '/'

%% 

top:	   	program {mainProgram = $1;}
	;

program:	     {$$ = makeEmptyProgram();  }
			 | program stmt {$$ = makeProgram($1, $2);}


;

stmt: exp ';' {$$ = makeStmtExp($1); }
//			|	func_call ';' {$$ = makeStmtFuncCall($1); }
	|	func_decl {$$ = makeStmtFuncDecl($1); }
        |	'[' arg_decl_list ']' '[' arg_list ']' '[' arg_list ']' func_decl
		{$$ = makeStmtRWFuncDecl($2, $5, $8, $10);}
        |	tIDENTIFIER tIDENTIFIER ';' {$$ = makeStmtDecl($1, $2); }
	|	tIDENTIFIER tIDENTIFIER '=' exp ';' {$$ = makeStmtDeclAssign($1, $2, $4); }
	|	tIDENTIFIER '=' exp ';' { $$ = makeStmtAssign($1, $3); }
        |	IF '(' exp ')' '{' program '}' {$$ = makeStmtIf($3, $6);}
	|	IF '(' exp ')' '{' program '}' ELSE '{' program '}'
			{$$ = makeStmtIfElse($3, $6, $10);}
	|	RETURN exp ';' {$$ = makeStmtReturn($2);}
        |	ASSERT exp ';' {$$ = makeStmtAssert($2);}

;

func_decl: tIDENTIFIER tIDENTIFIER '(' arg_decl_list ')' '{' program '}'
	       {$$ = makeFuncDecl($1, $2, $4, $7); }
		;

	 
func_call: tIDENTIFIER '(' arg_list ')' {$$ = makeFuncCall($1, $3); };


arg_decl_list: tIDENTIFIER tIDENTIFIER next_decl_args {$$ = makeSingleArgDeclList($1, $2, $3);};

next_decl_args: {$$ = makeEmptyArg();}
|	next_decl_args ',' tIDENTIFIER tIDENTIFIER {$$ = makeArgDeclList($1, $3, $4); };

arg_list: exp next_args {$$ = makeSingleArgList($1, $2);};

next_args: {$$ = makeEmptyArg();}
	       |	next_args ',' exp {$$ = makeArgList($1, $3); };

exp : tIDENTIFIER
      { $$ = makeEXPid ($1); }
| UNKNOWN { $$ = makeExpUnknown(); }	
    | tINTCONST	
      { $$ = makeEXPintconst ($1); }
    | exp '*' exp
      { $$ = makeEXPtimes ($1, $3); }
    | exp '/' exp
      { $$ = makeEXPdiv ($1, $3); }
    | exp '+' exp
      { $$ = makeEXPplus ($1, $3); }
    | exp '-' exp
      { $$ = makeEXPminus ($1, $3); }

	|	'-' exp { $$ = NULL; } %prec '*'

    | '(' exp ')'
      { $$ = $2; }
|	func_call {$$ = makeExpFuncCall($1);}
	|	tIDENTIFIER '.' tIDENTIFIER
	{$$ = makeExpDotAccess($1, $3);}
|	exp EQ exp {$$ = makeExpEqCheck($1, $3); }
|	exp NEQ exp {$$ = makeExpNeqCheck($1, $3); }
;
%%
