#include <stdlib.h>
#include <stdio.h>

void obtain_list(int n, long *l, long **s) {
  long i;
  if (n < 0) n = 0;
  *l = n;
  *s = (long *)malloc((*l) * sizeof(long));
  for (i = 0; i < *l; i++) {
    (*s)[i] = i;
  }
}

void show_list(long l, long *s) {
  if (s) {
    long n;
    printf("From C:");
    for (n = 0; n < l; n++) {
      printf(" %ld", s[n]);
    }
    printf(".\n");
  } else {
    printf("From C: []\n");
  }
}







