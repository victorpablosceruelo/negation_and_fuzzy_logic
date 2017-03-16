#include <stdio.h>
#include <assert.h>
#include <limits.h>

#define CHAR_T 8

int main(){
  int len = 32; 
  unsigned int pc = (1 << len)-1;
  int pcm = (2 ^ len)  - 1;  /* use pow */
  int pcs = ~(1 << (len - 1));
  int intsz = (sizeof(long) << 3);
  unsigned int allones = ~ 0;
  int mask = (1 << len) - 1;

  printf("for len %d, pc is <%d>, pcm is <%d>, pcs is <%d>, int size is <%d>,max int is <%d>\n",
         len,pc,pcm,pcs,intsz,INT_MAX);

  printf("all ones is <%u>, max uint is <%u>, mask is <%u>\n",
         allones,UINT_MAX,mask);

#if HELLOWORLDDD
  printf("hello world\n");
#endif

#ifdef HELLOWORLD
  printf("hello deaf world\n");
#endif

  return 0;
}
