#include "tree.h"

#include "eval.h"
#include "stdio.h"
#include "actualMain.h"

void yyparse();

PROGRAM *mainProgram;

int lineno;

int main()
{ lineno = 1;
  /* printf("Type in a tiny exp folowed by one or two Ctrl-d's:\n"); */
  yyparse();
  /* printf("\nThe result of evaluating:\n"); */
  /* prettyEXP(theexpression); */
  /* printf("\n"); */
  /* printf("is: %d\n",evalEXP(theexpression)); */
  start(mainProgram);
  return(1);
}
