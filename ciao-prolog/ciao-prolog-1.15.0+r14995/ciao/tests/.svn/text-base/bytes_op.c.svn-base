#include <stdlib.h>
#include <stdio.h>

void obtain_list(int n, int *l, char **s) {
  int i;
  int c;
  if (n < 0) n = 0;
  *l = n;
  *s = (char *)malloc(*l);
  for (i = 0; i < *l; i++) {
    (*s)[i] = i;
  }
}

void show_list(int l, char *s) {
  printf("Printing the byte list from C:");
  if (s) {
    int n;
    for (n = 0; n < l; n++) {
      printf(" %d", s[n]);
    }
    printf("\n");
  } else {
    printf("[]\n");
  }
}







