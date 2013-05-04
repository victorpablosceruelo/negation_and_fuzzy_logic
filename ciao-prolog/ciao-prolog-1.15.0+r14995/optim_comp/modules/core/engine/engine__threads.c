#include <engine/basiccontrol.native.h>

#if defined(USE_THREADS) && defined(USE_POSIX_THREADS)
pthread_attr_t detached_thread;
pthread_attr_t joinable_thread;
#endif
