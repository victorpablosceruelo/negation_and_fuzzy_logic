#include <stdio.h>
#include <assert.h>
#include <limits.h>

#define CHAR_T 8

int main(){
  int i = 2;
  
  while(i-- > 0) {
    printf("i is %d\n",i);
  }
  return 0;
}
