#include <stdio.h>
#include "eval.h"

int evalEXP(EXP *e)
{ switch (e->kind) {
    case idK:
         printf("I can't evaluate the value of an identifier!");
	 return(0);
         break;
    case intconstK:
	 return(e->val.intconstE);
         break;
    case timesK:
	 return(evalEXP(e->val.timesE.left) * 
	        evalEXP(e->val.timesE.right));
         break;
    case divK:
     return (evalEXP(e->val.divE.left)/
             evalEXP(e->val.divE.right));
         break;
    case plusK:
	 return(evalEXP(e->val.plusE.left) + 
	        evalEXP(e->val.plusE.right));
         break;
    case minusK:
	 return(evalEXP(e->val.minusE.left) -
	         evalEXP(e->val.minusE.right));
         break;  
    default: 
	 printf("ERROR: Impossible type for an expression node.");
	 return(0);
  }
}