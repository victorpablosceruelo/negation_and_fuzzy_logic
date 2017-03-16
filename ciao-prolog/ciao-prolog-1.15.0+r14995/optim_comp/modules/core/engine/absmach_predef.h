
/*****************************************************************************/
/* Run-time configuration options */

/* ------------------------------------------------------------------------- */

/* Use __thread global variables to store the worker instead of 
   passing it as an argument (i386-only) */
/* TODO: it run slower!!! without __thread it run even slower! */
//#define USE_GLOBAL_WORKER 1

/* When USE_GLOBAL_WORKER is not enabled, this option assigns a
   register to the worker parameter (i386-only) */
/* TODO: does not work because a gcc bug (cast to function forgets
   attributes), which has been reported to be fixed in 3.4.0 */
//#define USE_REGWORKERARG 1

//#define DEBUG_TRACE 1
//#define DEBUG_INSTRACE 1

/* Collect garbage collection's statistics */
#define USE_GCSTATS 1

/* TODO: include as a DEBUG_TRACE option */
//#define DEBUG__TRACE__LOW_STACKS 1

/* TODO: hmmm ... makes abort much slower */
//#define WAIT_THREAD_CANCELLED 1

/* Enable dynamic link of native code */
#if defined(FOREIGN_FILES)
#define USE_DYNLINK_NATIVE 1
#endif

/* Enable disassembler */
#define USE_DISASSEMBLER 1

/* Enable term compilation */
/* TODO: disable when not needed */
#define USE_CTERM 1

/* Automatically generated configuration and basic definitions */
#include <engine/engine__configuration.h>

/*****************************************************************************/
/* Variable attributes (for GCC) */

/* Mark variables that may be assigned and not used, but that are legitimate
   code (e.g., in macros). */
#define MAYBE_UNUSED __attribute__((unused))

/*****************************************************************************/
/* System type definitions */

/* TODO: use stdint.h or inttypes.h */

/* TODO: clean this code, in most cases it should be defined by the guest OS */
#if defined(Solaris)
#include <sys/int_types.h>
#elif defined(DARWIN)
#include <stdint.h>
#else
/* TODO: check that definitions are correct for all supported 64 bit systems */
typedef unsigned char uint8_t;
typedef signed char int8_t;
typedef unsigned short uint16_t;
typedef signed short int16_t;
typedef unsigned int uint32_t;
typedef signed int int32_t;
/* is __WORDSIZE standard? */
/* #if __WORDSIZE == 64 */
#if defined(x86_64)
typedef long int int64_t;
typedef unsigned long int uint64_t;
#else
typedef long long int64_t;
typedef unsigned long long uint64_t;
#endif
#endif

/* 32 bits in 32 bit architectures, 64 bits in 64 bit architectures */
typedef unsigned long intp_t; /* integer that can contain pointers */

typedef double flt64_t;

/*****************************************************************************/
/* Common interface to OS dependant features: locks, threads, etc. */
/* TODO: needed everywhere? */

#include <engine/engine__os.h>
#include <stdio.h>
#include <setjmp.h>

/*****************************************************************************/
/* Worker argument abstraction */

#define CHANGE_WORKER(NEW_WORKER, CODE) { \
  worker_t *old_w; \
  old_w = w; \
  SET_WORKER((NEW_WORKER)); \
  CODE; \
  SET_WORKER(old_w); \
}
#define LOCAL_WORKER(NEW_WORKER, CODE) { \
  worker_t *w; \
  SET_WORKER((NEW_WORKER)); \
  CODE; \
}

#if defined(USE_GLOBAL_WORKER)
/* TODO: worker_t not yet defined? */
extern __thread worker_t *w;
#define DECL_WORKER_ARG
#define DECL_WORKER_ARGC
#define WORKER_ARG
#define WORKER_ARGC
#define WORKERATTR
/* note: in both macros, if CODE jumps somewhere else (goto, return or
   longjmp) then the old worker will not be restored  */
#define SET_WORKER(NEW_WORKER) w = (NEW_WORKER) 
/* note: use from CVOID CINSN CBOOL etc code */
#define CVOID__WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE) 
/* note: use from normal C functions */
#define WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE)
#else
#define DECL_WORKER_ARG worker_t *w
#define DECL_WORKER_ARGC DECL_WORKER_ARG,
#define WORKER_ARG w
#define WORKER_ARGC WORKER_ARG,
#if defined(USE_REGWORKERARG)
/* TODO: does not work in gcc prior to 3.4.0 (CAST) */
#define WORKERATTR __attribute__ ((regparm(1)))
#else
#define WORKERATTR
#endif
#define SET_WORKER(NEW_WORKER) w = (NEW_WORKER)
/* note: in both macros, if CODE jumps somewhere else (goto, return or
   longjmp) then the old worker will not be restored  */
/* note: use from CVOID CINSN CBOOL etc code */
#define CVOID__WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE)
/* note: use from normal C functions */
#define WITH_WORKER(NEW_WORKER, CODE) LOCAL_WORKER(NEW_WORKER, CODE)
#endif

/*****************************************************************************/
/* Calling protocols */

#define CINSNP__PROTO(NAME) bcp_t WORKERATTR NAME(DECL_WORKER_ARG)
#define CINSNP__PROTO_N(NAME, ...) bcp_t WORKERATTR NAME(DECL_WORKER_ARGC __VA_ARGS__)
#define CINSNP__INSNCONT(NAME) NAME(WORKER_ARG)
#define CINSNP__INSNCONT_N(NAME, ...) NAME(WORKER_ARGC __VA_ARGS__)
#define CINSNP__LASTCALL(NAME) return NAME(WORKER_ARG)
#define CINSNP__LASTCALL_N(NAME, ...) return NAME(WORKER_ARGC __VA_ARGS__)
#define CINSNP__GOTO(NAME) return NAME
#define CINSNP__PROCEED return SUCCESS_INSNP
#define CINSNP__FAIL return FAIL_INSNP

#define CFUN__PROTO(NAME, TYPE) TYPE WORKERATTR NAME(DECL_WORKER_ARG)
#define CFUN__PROTO_N(NAME, TYPE, ...) TYPE WORKERATTR NAME(DECL_WORKER_ARGC __VA_ARGS__)
#define CFUN__EVAL(NAME) NAME(WORKER_ARG)
#define CFUN__EVAL_N(NAME, ...) NAME(WORKER_ARGC __VA_ARGS__)
#define CFUN__LASTCALL(NAME) return NAME(WORKER_ARG)
#define CFUN__LASTCALL_N(NAME, ...) return NAME(WORKER_ARGC __VA_ARGS__)
#define CFUN__PROCEED(X) return (X)

#define CBOOL__PROTO(NAME) bool_t WORKERATTR NAME(DECL_WORKER_ARG)
#define CBOOL__PROTO_N(NAME, ...) bool_t WORKERATTR NAME(DECL_WORKER_ARGC __VA_ARGS__)
#define CBOOL__SUCCEED(NAME) NAME(WORKER_ARG)
#define CBOOL__SUCCEED_N(NAME, ...) NAME(WORKER_ARGC __VA_ARGS__)
#define CBOOL__CALL(NAME) if (!NAME(WORKER_ARG)) return FALSE
#define CBOOL__CALL_N(NAME, ...) if (!NAME(WORKER_ARGC __VA_ARGS__)) return FALSE
#define CBOOL__LASTCALL(NAME) return NAME(WORKER_ARG)
#define CBOOL__LASTCALL_N(NAME, ...) return NAME(WORKER_ARGC __VA_ARGS__)
#define CBOOL__TEST(X) if (!(X)) return FALSE
#define CBOOL__LASTTEST(X) return (X)
#define CBOOL__PROCEED return TRUE
#define CBOOL__FAIL return FALSE

#define CVOID__PROTO(NAME) void WORKERATTR NAME(DECL_WORKER_ARG)
#define CVOID__PROTO_N(NAME, ...) void WORKERATTR NAME(DECL_WORKER_ARGC __VA_ARGS__)
#define CVOID__CALL(NAME) NAME(WORKER_ARG)
#define CVOID__CALL_N(NAME, ...) NAME(WORKER_ARGC __VA_ARGS__)
#define CVOID__PROCEED return

/*****************************************************************************/
/* Types definition (section typedef) */
/* todo[ts]: move everything to absmach definition */

#if !defined(TRUE)
#define FALSE 0
#define TRUE 1
#endif

/* States a worker can be in */
typedef enum {
  IDLE,      /* The memory areas are available for being used by a thread */
  WORKING,                 /* The memory areas are being used by a thread */
  PENDING_SOLS,               /* Frozen --  backtracking can be requested */
  FAILED	             /* Frozen -- but no more solutions available */
} thread_state_t;

typedef enum {DYNAMIC, CONC_OPEN, CONC_CLOSED} Behavior;

/* ------------------------------------------------------------------------- */

#define AddrBCOp(B,T,X) ({T* __t = (T*)((char *)(B)+(X)); __t; }) 
#define BCOp(B,T,X) (*AddrBCOp(B,T,X))
#define BCoff(P,X) ((bcp_t)((char *)(P)+(X)))

/*
#define BCOp(B,T,X) (*((T*)((char *)(B)+(X))))
#define BCoff(P,X) ((bcp_t)((char *)(P)+(X)))
*/
