#include <engine/basiccontrol.native.h>

#if defined(USE_OWN_MALLOC)
char *own_malloc(intmach_t size);
char *own_realloc(char *ptr, intmach_t size_in_chars);
void own_free(char *ptr);
void init_own_malloc();
#  define Malloc(p)     own_malloc((p))
#  define Free(p)       own_free((char *)(p))
#  define Realloc(p, s) own_realloc((char *)(p), (s))
#elif defined(sequent) 
#  include <stdlib.h>
#  define Malloc(p) shmalloc(p)
#  define Free(p)   free(p)
#  define Realloc(p, s) realloc(p, s)
#else
#if defined(SunOS4)
#  include <malloc.h>
#else
#  include <stdlib.h>
#endif
#  define Malloc(p)     malloc(p)
#  define Free(p)       free((char *)p)
#  define Realloc(p, s) realloc((char *)p, s)
#endif                                                       

#if defined(Solaris) || defined(LINUX) || defined(DARWIN)
#include <string.h>
#else
#include <memory.h>
#endif
#include <unistd.h>

/* Memory management lock (for internal structures) */
SLOCK mem_mng_l;

/* Debug */
/* TODO: move the real debug code here... debug.h only handles common stuff? */
#if defined(DEBUG_TRACE)
/* condition to show memory operation */
/* TODO: make configurable */
bool_t dump_memory_cond(char *p, intmach_t size) {
  return debug_mem && size > 100000; /* do not dump small blocks */
}
/* alloc */
#define DEBUG__TRACE_ALLOC(P, SIZE) { \
  dump_memory_alloc((char *)(P), (SIZE)); \
}
void dump_memory_alloc(char *p, intmach_t size) {
  if (!dump_memory_cond(p, size)) return;
  TRACE_PRINTF("alloc %p-%p (0x%lx bytes)\n", p, (char *)p+size, (long)size);
}
/* realloc */
#define DEBUG__TRACE_REALLOC(PTR, DECR, P, SIZE) { \
  dump_memory_realloc((char *)(PTR), (DECR), (char *)(P), (SIZE)); \
}
void dump_memory_realloc(char *p, intmach_t size, char *p2, intmach_t size2) {
  if (!dump_memory_cond(p, size)) return;
  TRACE_PRINTF("realloc %p-%p (0x%lx bytes) to %p-%p (0x%lx bytes)\n", p, (char *)p+size, (long)size, p2, (char *)p2+size2, (long)size2);
}
/* free */
#define DEBUG__TRACE_FREE(PTR, DECR) { \
  dump_memory_free((char *)(PTR), (DECR)); \
}
void dump_memory_free(char *p, intmach_t size) {
  if (!dump_memory_cond(p, size)) return;
  TRACE_PRINTF("free %p-%p (0x%lx bytes)\n", p, (char *)p+size, (long)size);
}
#else
#define DEBUG__TRACE_ALLOC(P, SIZE)
#define DEBUG__TRACE_REALLOC(PTR, DECR, P, SIZE)
#define DEBUG__TRACE_FREE(PTR, DECR)
#endif

/* total # bytes grabbed by malloc/realloc/free . Not accurately
   measured -- it depends on the mem. mang. own data structures
   (MCL) */
intmach_t total_mem_count = 0;                                 /* Shared */

/* # bytes used by the Prolog program & database code.  Probably not
   accurately measured (patched here and there) (MCL).  */
intmach_t mem_prog_count = 0;                                     /* Shared */

/* locks for accessing the shared memory.  Although malloc() is
   MT-safe in Solaris (how 'bout Linux?), the engine uses several
   internal data structures for its own memory management which should
   be correctly updated.  So we use our own lock for providing an
   atomic access to checkalloc/checkrealloc/checkdealloc (MCL).
*/

#define USE_TINY_BLOCKS 1
#if defined(USE_TINY_BLOCKS)
/* From an execution profile, 24 seems a good threshhold (MCL) */
#define THRESHHOLD 24
#define NTINY 682
#endif

#if defined(USE_TINY_BLOCKS)
static char *tiny_blocks = NULL;                     /* Shared & locked */
#endif

#if defined(USE_TINY_BLOCKS)
static char *get_tiny_blocks() {
  intmach_t i;
  char *ptr;
  char *p;
  char *tail = NULL;

 /* prt was a call to checkalloc, but I made a direct call to Malloc()
    because of recursive locks stopping the engine.  Thus, the total amount
    of memory is also increased here. */

  ptr = Malloc(NTINY*THRESHHOLD);
  p = ptr+NTINY*THRESHHOLD;

  if (!ptr) {
    print_syserror("% Malloc");
    Release_slock(mem_mng_l);
    PANIC_FAULT("Memory allocation failed");
  }

  total_mem_count += NTINY*THRESHHOLD;

  for (i=NTINY; i>0; i--) {
    p -= THRESHHOLD;
    *((char **)p) = tail;
    tail = p;
  }
  tiny_blocks = ptr+THRESHHOLD;
  return ptr;
}
#endif

char *checkalloc(intmach_t size) {
  char *p;

  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (size<=THRESHHOLD) {
    p = tiny_blocks;
    if (p) {
      tiny_blocks = *((char **)p);
    } else {
      p = get_tiny_blocks();
    }
  } else { /* size > THRESHHOLD */
#endif
    p = Malloc(size);

    if (!p) {
      print_syserror("% Malloc");
      Release_slock(mem_mng_l);
      PANIC_FAULT("Memory allocation failed");
    }
    if (!ENSURE_ADDRESSABLE(p, size)) {
      Release_slock(mem_mng_l);
      PANIC_FAULT("Memory allocated out of addressable bounds!");
    }
      
    /* mem_prog_count += size+sizeof(char *); */
    total_mem_count += size;
#if defined(USE_TINY_BLOCKS)
  }
#endif
  DEBUG__TRACE_ALLOC(p, size);
  Release_slock(mem_mng_l);
  return p;
}

void checkdealloc(char *ptr, intmach_t decr) {
  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (decr<=THRESHHOLD) {
    *((char **)ptr) = tiny_blocks;
    tiny_blocks = ptr;
  } else {
#endif
    total_mem_count -= decr;
    Free(ptr);
#if defined(USE_TINY_BLOCKS)
  }
#endif
  DEBUG__TRACE_FREE(ptr, decr);
  Release_slock(mem_mng_l);
}

char *checkrealloc(char *ptr, intmach_t decr, intmach_t size) {
  char *p;

  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (decr<=THRESHHOLD) {
    if (size<=THRESHHOLD) {
      /* leave in the same tiny block */
      p = ptr;
    } else {
      /* move from a tiny block to a new tiny block */
      p = Malloc(size);
      total_mem_count += size;
      if (!p) {
        print_syserror("% Malloc");
        Release_slock(mem_mng_l);
        PANIC_FAULT("Memory allocation failed");
      }
      if (!ENSURE_ADDRESSABLE(p, size)) {
	Release_slock(mem_mng_l);
	PANIC_FAULT("Memory allocated out of addressable bounds!");
      }
      // todo: alignment was not necessary, right?
      //memcpy(p, ptr, ALIGN_TO(intmach_t, decr));
      memcpy(p, ptr, decr);
      *((char **)ptr) = tiny_blocks;
      tiny_blocks = ptr;
    }
  } else { /* decr > THRESHHOLD */
    if (size<=THRESHHOLD) {
      /* move from a big block to a new tiny block */
      p = tiny_blocks;
      if (!p) {
	p = get_tiny_blocks();
      }
      // todo: alignment was not necessary, right?
      //memcpy(p, ptr, ALIGN_TO(intmach_t, size));
      memcpy(p, ptr, size);
    } else {
#endif
      /* leave in a big block */
      p = Realloc(ptr, size);
      if (!p) {
        print_syserror("% realloc");
        Release_slock(mem_mng_l);
        PANIC_FAULT("Memory allocation failed");
      }
      if (!ENSURE_ADDRESSABLE(p, size)) {
	Release_slock(mem_mng_l);
	PANIC_FAULT("Memory allocated out of addressable bounds!");
      }
      /* mem_prog_count  += (size-decr); */
      total_mem_count += (size-decr);
#if defined(USE_TINY_BLOCKS)
    }
  }
#endif
  DEBUG__TRACE_REALLOC(ptr, decr, p, size);
  Release_slock(mem_mng_l);
  return p;
}

void init_alloc() {
  Init_slock(mem_mng_l);
#if defined(USE_OWN_MALLOC)
  init_own_malloc();
#endif
}

