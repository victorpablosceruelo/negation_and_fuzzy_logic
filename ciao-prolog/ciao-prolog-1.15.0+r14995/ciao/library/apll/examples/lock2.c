#include <pthread.h>
#include <stdio.h>

main(int argc, char **argv){
  pthread_cond_t lck;
  pthread_mutex_t mtx;
  int ret;

  pthread_mutex_init(&mtx, NULL);

  pthread_cond_init(&lck, NULL);

  pthread_cond_wait(&lck,&mtx);
}

