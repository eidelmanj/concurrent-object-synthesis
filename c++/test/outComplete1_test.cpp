#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <iostream>
#include "vops.h"
#include "outComplete1.h"

using namespace std;

void blah__Wrapper_ANONYMOUSTest(Parameters& _p_) {
  for(int _test_=0;_test_< _p_.niters ;_test_++) {
    try{
      ANONYMOUS::blah__WrapperNospec();
      ANONYMOUS::blah__Wrapper();
    }catch(AssumptionFailedException& afe){  }
  }
}

int main(int argc, char** argv) {
  Parameters p(argc, argv);
  srand(time(0));
  blah__Wrapper_ANONYMOUSTest(p);
  printf("Automated testing passed for outComplete1\n");
  return 0;
}
