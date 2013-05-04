#include <stdio.h>
#include <time.h>

time_t init_time;

void init() {
  printf("This is an initialization function written in C.\n");
  init_time = time(NULL);
}

void print_time() {
  printf("This module is written in C and was loaded at %s\n", 
    ctime(&init_time));
}






