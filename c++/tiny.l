%{
#include "y.tab.h"
#include <string.h>
#include <stdlib.h>

extern int lineno;
%}

%%
[ \t]+    /* ignore */;
\n        lineno++;

"*"       return '*';
"/"       return '/';
"+"       return '+';
"-"       return '-';
"("       return '(';
")"       return ')';
";"       return ';';
","       return ',';
"{"       return '{';
"}"       return '}';
"["       return '[';
"]"       return ']';
"="       return '=';
"."       return '.';
"if"      return IF;
"else"    return ELSE;
"assert"  return ASSERT;
"return"  return RETURN;
"=="      return EQ;
"!="      return NEQ;
"??"      return UNKNOWN;


0|([1-9][0-9]*) {
  yylval.intconst = atoi (yytext);
  return tINTCONST;
}

[a-zA-Z_][a-zA-Z0-9_]* { 
  yylval.stringconst = 
    (char *) malloc (strlen (yytext) + 1);
  sprintf (yylval.stringconst, "%s", yytext); 
  return tIDENTIFIER; 
}

.         /* ignore */;
%%
