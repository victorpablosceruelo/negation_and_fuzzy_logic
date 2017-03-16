/***************************************************************************/
/* Common interface to OS dependant features: signals, locks, threads...   */

/* TODO: is this line required? */
#if defined(Solaris) && !defined(__svr4__)
#define __svr4__
#endif

/***************************************************************************/
/* signals */

#if !defined(__svr4__)
#define SIGNAL signal
#define SIGBLOCK(SIG) sigblock(sigmask(SIG))
#define SIGPAUSE_0() sigpause(0)
#else                                                         /* __svr4__ */
#define SIGNAL(SIG,HDL)				\
do {						\
  struct sigaction act;				\
						\
  sigemptyset(&act.sa_mask);			\
  act.sa_flags = 0 | SA_NODEFER;		\
  act.sa_handler = HDL;				\
						\
  sigaction(SIG,&act,(struct sigaction *)NULL);	\
} while (0)

#define SIGBLOCK(SIG)					\
do {							\
  sigset_t block_set;					\
							\
  sigemptyset(&block_set);				\
  sigaddset(&block_set, SIG);				\
  sigprocmask(SIG_BLOCK, &block_set, (sigset_t *)NULL);	\
} while (0)

#define SIGPAUSE_0()	\
do {			\
  sigset_t set;		\
			\
  sigemptyset(&set);	\
  sigsuspend(&set);	\
} while (0)

#endif /* __svr4__ */

#if defined(__svr4__) || defined(LINUX)
#define UNBLOCK_SIG(Sig)                 \
{ sigset_t mask;                         \
  sigemptyset(&mask);                    \
  sigaddset(&mask, (Sig));               \
  sigprocmask(SIG_UNBLOCK, &mask, NULL); \
}
#else
#define UNBLOCK_SIG(Sig)
#endif

/***************************************************************************/
/* longjmp/setjmp */

#if defined(SYMM)
# define LONGJMP(Env, Val) longjmp(Env, Val)
# define SETJMP(Env) setjmp(Env)
# define JMP_BUF jmp_buf
#else                                                         /* Not SYMM */
# if defined(Solaris)
#  if defined(i86)                                      /* Solaris && i86 */
#   define JMP_BUF int
#   define SETJMP(Env)  0
#   define LONGJMP(Env, Val) TRACE_PRINTF("{Serious error recovery not available in this architecture}\n"); engine_exit(-1);
#  else                                               /* Solaris && sparc */
#   define JMP_BUF           jmp_buf
#   define SETJMP(Env)       setjmp(Env)
#   define LONGJMP(Env, Val) longjmp(Env, Val)
#  endif
# else 
#  if defined(crossWin32i86)            
#  define JMP_BUF           jmp_buf
#  define SETJMP(Env)       setjmp(Env)
#  define LONGJMP(Env, Val) longjmp(Env, Val)
#  else
#  define JMP_BUF           sigjmp_buf
#  define SETJMP(Env)       sigsetjmp(Env, 1)
#  define LONGJMP(Env, Val) siglongjmp(Env, Val)
#  endif
# endif
#endif

/***************************************************************************/
/* Max path name */

#if !defined(MAXPATHLEN)
# if defined(PATH_MAX)
#  define MAXPATHLEN PATH_MAX
# else
#  define MAXPATHLEN 1024
# endif
#endif

#if !defined(MAX_FILENAME)
# if defined(FILENAME_MAX)
#  define MAX_FILENAME FILENAME_MAX
# else
#  define MAX_FILENAME MAXPATHLEN
# endif
#endif

/*****************************************************************************/
/* locks */

#if !defined(NULL)
#define NULL (void *)0
#endif

#if !defined(USE_THREADS) /* Empty macros */
/* Spin locks: fast, but make a busy-wait */
#define Init_slock(p)          
#define Wait_Acquire_slock(p)  
#define Release_slock(p)       
#define Destroy_slock(p)       
#define SLock_is_unset(p)      1

/* non spin locks: should suspend the thread, but are possibly slower */
#define Init_lock(p)          
#define Wait_Acquire_lock(p)  
#define Release_lock(p)       
#define Destroy_lock(p)       
#define Lock_is_unset(p)      1

/* Conditional vars */
#define Init_Cond(cond)
#define Cond_Var_Init(cond)
#define Cond_Var_Wait(cond, lock)
#define Cond_Var_Broadcast(cond)
#define Wait_Acquire_Cond_lock(Cond)
#define Release_Cond_lock(Cond)
#define Cond_Begin(Cond)
#define Broadcast_Cond(Cond)
#define Wait_For_Cond_End(Cond)
#define Wait_For_Cond_Begin(Predicate, Cond)

#if defined(USE_RTCHECKS)
#define Cond_Lock_is_unset(Cond) FALSE
#endif

typedef char CONDITION;                          /* Avoid compiler errors */
typedef char SLOCK;
typedef char LOCK;

#else  /* We have THREADS!!!!! */

#define USE_LOCKS 1

/* First, we look at the spin locks.  We know how to do them in i86 and in
   Sparc architectures.  I am not really sure about the Sequent: could not
   try it, actually.  This is heavily influenced by Kish Shen and Roland
   Karlsson (thanks to both). */

#if (defined(i86) || defined(Sparc) || defined(Sparc64) || defined(Sequent)) && defined(__GNUC__)
#define HAVE_NATIVE_SLOCKS 1

/*
#if defined(Sparc) || defined(Sequent) 
#define DESTRUCTIVE  
#endif
*/

typedef volatile unsigned long SLOCK_ST;
typedef volatile struct {
  SLOCK_ST  lock_st;
  SLOCK_ST *lock_pt;
} SLOCK;


#if defined(DESTRUCTIVE)
#define lock_offset 1 /*lock is offset from start of structure by one word*/
#define LockOffSet(p) (p.lock_pt + lock_offset)
#else                                                     /* !DESTRUCTIVE */
#define LockOffSet(p) (p.lock_pt)
#endif                                                     /* DESTRUCTIVE */


# if defined(Sparc) || defined(Sparc64)     /* Macro definition for sparc */
#   define aswap(addr,reg)                                      \
({ int _ret;                                                    \
   asm volatile ("swap %1,%0"                                   \
        : "=r" (_ret), "=m" (*(addr))    /* Output %0,%1 */     \
        : "m"  (*(addr)), "0" (reg));    /* Input (%2),%0 */    \
   _ret;                                                        \
})
# endif                                                          /* Sparc */

/* Was      asm volatile ("xchgw %0,%1"                                */

# if defined(i86) || defined(Sequent)          /* Now, Intel 80x86 stuff */
#   define aswap(adr,reg)                                       \
  ({ long _ret;                                             \
     asm volatile ("xchgl %0,%1"                                \
        : "=q" (_ret), "=m" (*(adr))    /* Output %0,%1 */      \
        : "m"  (*(adr)), "0"  (reg));   /* Input (%2),%0 */     \
     _ret;                                                      \
  })
#  endif

 /* Untested for MIPS! */

#if defined(__STDC__)
#define VOLATILE volatile
#else
#define VOLATILE
#endif

# if defined(mips)
#   define asm_ll(adr)						\
  ({ int _ret;							\
     asm VOLATILE ("ll %0,%1"					\
	: "=r" (_ret), "=m" (*(adr))	/* Output %0,%1 */	\
	: "m"  (*(adr)));		/* Input (%2) */	\
     _ret;							\
  })

#   define asm_sc(adr,reg)					\
  ({ int _ret;							\
     asm VOLATILE ("sc %0,%1"					\
	: "=r" (_ret), "=m" (*(adr))	/* Output %0,%1 */	\
	: "m"  (*(adr)), "0" (reg));	/* Input (%2),%0 */	\
     _ret;							\
  })

#  define aswap(p,v)						\
  ({ int _ret;							\
     do { _ret = asm_ll(p); } while(! asm_sc(p, v));		\
     _ret;							\
  })

#define mips_try_lock(p) (*(LockOffset(p))=1)

#define mips_lock(p)						\
do { if (mips_try_lock(p)) break;			        \
     {{ t_lock_wait(); }}					\
     {{ t_debug(__FILE__,__LINE__); }}				\
     while(*(LockOffset*p)==1) continue;		        \
     {{ t_lock_ready(); }}					\
   } while(1);
#endif

/* Initialize a spin lock */
#define Init_slock(p)  { \
        p.lock_pt = &p.lock_st; \
        *(LockOffSet(p))=0; \
       }

/*
  From alan@lxorguk.ukuu.org.uk: 
  Whenever the lock grab fails the CPU will spin sampling the cache
  line until another CPU writes it back which invalidates our MESI
  cache line for the data. This avoids the code spin-generating lock
  cycles on the bus.
*/

#if defined(mips)
#define Wait_Acquire_slock(p) mips_lock(p)
#else
#define Wait_Acquire_slock(p) \
     while (aswap((LockOffSet(p)),1)) { \
        while(*(LockOffSet(p)));\
    }
#endif

#define Release_slock(p)          *(LockOffSet(p))=0
#define Destroy_slock(p)          p.lock_pt = NULL /* Force error afterwards */
#define SLock_is_unset(p)         *(LockOffSet(p))==0

/*
#define Acquire_slock_or_fail(p)  aswap((LockOffSet(p)),1)==0
#define Try_Acquire_self_slock(p) (aswap(*(LOCK_ST **)p, *(LOCK_ST *)p))
#define Reset_lock(p, value)     (aswap(*(LOCK_ST **)p, value))
#define Release_self_lock(p)
*/

#endif

/* Now, the O.S. for which we know there are locks provided by a library.
   These presumably put the thread to sleep and do not consume any
   resources, but also are more expensive to call. */

#if defined(LINUX) || defined(IRIX) || defined (Solaris) || defined (DARWIN)
#define HAVE_LIB_LOCKS 1

#include <pthread.h>
#include <unistd.h>
#include <errno.h>

/* IRIX machines seem to have a fair scheduling without having to relinquish
   anything.  Some Solaris machines define _POSIX_PRIORITY_SCHEDULING*/

#if defined(_POSIX_PRIORITY_SCHEDULING) && defined(LINUX)
/* 
   Unlike other implementation of threads, pthreads for Linux implements
   threads as user-space processes.
*/
#include <sched.h>
#define RELINQUISH_PROCESSOR sched_yield();
#else
#define RELINQUISH_PROCESSOR 
#endif

typedef pthread_mutex_t LOCK;

#define Init_lock(p)         pthread_mutex_init(&p, NULL)
#define Wait_Acquire_lock(p) pthread_mutex_lock(&p)
#define Release_lock(p)      pthread_mutex_unlock(&p)
#define Destroy_lock(p)      pthread_mutex_destroy(&p)
#define Lock_is_unset(p)     lock_is_unset(&p)

#define CONDITIONAL_VARS 1

typedef pthread_cond_t COND_VAR;

#define Cond_Var_Init(cond)    pthread_cond_init(&cond, NULL)
#define Cond_Var_Wait(cond, lock) pthread_cond_wait(&cond, &lock)
/*#define Cond_Var_Signal(cond) pthread_cond_signal(&cond)*/
#define Cond_Var_Broadcast(cond) pthread_cond_broadcast(&cond)
#endif

#if defined(Win32)
#define HAVE_LIB_LOCKS 1

#include <windows.h>

 /* I am using Critical Section Objects instead of mutexes because,
 according t the MS documentation, CSO are faster, although they cannot be
 used to synchronize objects belonging to different processes.  */

typedef CRITICAL_SECTION LOCK;

#define Init_lock(p)         InitializeCriticalSection(&p)
#define Wait_Acquire_lock(p) EnterCriticalSection(&p)
#define Release_lock(p)      LeaveCriticalSection(&p)
#define Destroy_lock(p)      DeleteCriticalSection(&p)
#define Lock_is_unset(p)     lock_is_unset_win32(&p)

#define CONDITIONAL_VARS 1
#define RELINQUISH_PROCESSOR 1

typedef HANDLE COND_VAR;

#define Cond_Var_Init(cond) cond = CreateEvent(NULL, FALSE, FALSE, NULL)

/* put a timeout; we might miss a wakeup otherwise, since we do not */
/* have atomic Wait and Unlock in Win95/98 */

#define Cond_Var_Wait(cond, lock) \
        Release_lock(lock); \
        WaitForSingleObject(cond, 10); \
        Wait_Acquire_lock(lock)
/*#define Cond_Var_Signal(cond)     SetEvent(cond)*/
#define Cond_Var_Broadcast(cond)  PulseEvent(cond)
#endif

#if !defined(HAVE_LIB_LOCKS) && defined(HAVE_NATIVE_SLOCKS)
/* Define the non-spin ones in terms of the spin ones */

typedef SLOCK LOCK;

#define Init_lock(p)          Init_slock(p)
#define Wait_Acquire_lock(p)  Wait_Acquire_slock(p)  
#define Release_lock(p)       Release_slock(p)       
#define Destroy_lock(p)       Destroy_slock(p)       
#define Lock_is_unset(p)      SLock_is_unset(p)      
#endif

#if defined(HAVE_LIB_LOCKS) && !defined(HAVE_NATIVE_SLOCKS)
/* Do it the other way around */

typedef LOCK SLOCK;

#define Init_slock(p)          Init_lock(p)
#define Wait_Acquire_slock(p)  Wait_Acquire_lock(p)  
#define Release_slock(p)       Release_lock(p)       
#define Destroy_slock(p)       Destroy_lock(p)       
#define SLock_is_unset(p)      Lock_is_unset(p)      
#endif

#if !defined(HAVE_LIB_LOCKS) && !defined(HAVE_NATIVE_SLOCKS)

typedef SLOCK int;
typedef LOCK  int;

#define Init_slock(p)          
#define Wait_Acquire_slock(p)  
#define Release_slock(p)       
#define Destroy_slock(p)       
#define SLock_is_unset(p)      1

#define Init_lock(p)          
#define Wait_Acquire_lock(p)  
#define Release_lock(p)       
#define Destroy_lock(p)       
#define Lock_is_unset(p)      1
#endif

/* The two below should not be used in user code, only here. */
#define Wait_Acquire_Cond_lock(Cond) Wait_Acquire_lock(Cond.cond_lock)
#define Release_Cond_lock(Cond) Release_lock(Cond.cond_lock)



/* Use of the conditional vars: 

A conditional variable is inited using Init_Cond(cond_var)

The variables involved in a condition which is expressed by cond_var are
changed using:

Cond_Begin(cond_var)
... change the variables ...
Broadcast_Cond(cond_var)

which releases all the threads waiting on the condition.

When a thread has to check that / wait for a condition to hold, 

Wait_For_Cond_Begin(pred(...), cond_var)
...do something...
Wait_For_Cond_End(cond_var)

where pred() determines when the condition *does not hold*, i.e., the 
execution continues when pred() is false.

*/


#define Wait_For_Cond_End(Cond)  Release_Cond_lock(Cond)
#define Cond_Lock_is_unset(Cond) Lock_is_unset(Cond.cond_lock)
#define Cond_Begin(Cond)  Wait_Acquire_Cond_lock(Cond)

#if defined(CONDITIONAL_VARS)

#define Init_Cond(Cond) {                                            \
                          Init_lock(Cond.cond_lock);                 \
                          Cond_Var_Init(Cond.cond_var);   \
                        }
#define Wait_For_Cond_Begin(Predicate, Cond) {                             \
                     Wait_Acquire_Cond_lock(Cond);                \
                     RELINQUISH_PROCESSOR                                  \
                     while (Predicate) {                                   \
                       Cond_Var_Wait(Cond.cond_var, Cond.cond_lock); \
                   }                                                       \
                }
/*
#define Signal_Cond(Cond) {                             \
                     Cond_Var_Signal(Cond.cond_var);   \
                     Release_Cond_lock(Cond);      \
                 }
*/
#define Broadcast_Cond(Cond) {                             \
                     Cond_Var_Broadcast(Cond.cond_var);   \
                     Release_Cond_lock(Cond);      \
                 }
#else

#define Init_Cond(Cond) Init_lock(Cond.cond_lock)
#define Wait_For_Cond_Begin(Predicate, Cond) {                     \
                       int pred_is_signaled;                      \
                       Wait_Acquire_Cond_lock(Cond);      \
                       pred_is_signaled = (Predicate);             \
                       while(pred_is_signaled) {                   \
                         Release_Cond_lock(Cond);         \
                         RELINQUISH_PROCESSOR                      \
                         Wait_Acquire_Cond_lock(Cond);    \
                         pred_is_signaled = (Predicate);           \
                       }                                           \
                     }
/*#define Signal_Cond(Cond) Release_Cond_lock(Cond)*/
#define Broadcast_Cond(Cond) Release_Cond_lock(Cond)

#endif

typedef struct conditionstruct {
   LOCK     cond_lock;
#if defined(CONDITIONAL_VARS)
   COND_VAR cond_var;
#endif
} CONDITION;

#endif /* USE_THREADS */

#if defined(USE_RTCHECKS) || defined(DEBUG_TRACE)
#if defined(Win32)
int lock_is_unset_win32(LOCK *p);
#else
int lock_is_unset(LOCK *p);
#endif
#endif

#if defined(DEBUG_TRACE)
int get_inc_counter(void);
void reset_counter(void);
#endif

#if defined(DEBUG_TRACE)
#define GET_INC_COUNTER get_inc_counter()
#define RESET_COUNTER reset_counter()
#else
#define GET_INC_COUNTER 0
#define RESET_COUNTER 
#endif

/*****************************************************************************/
/* threads */

/* Disallow threads in selected cases, even if set at the general settings */
#if defined(USE_THREADS)

/* 
   These macros allows the same macros to be use with the POSIX and Solaris
   threads packages.  Some code taken from Kish's DASWAM
*/

#if defined(DARWIN) || defined(LINUX) || defined(IRIX) || defined(Solaris)
#define USE_POSIX_THREADS 1
#endif

#if defined(Win32) 
#define USE_WIN32_THREADS 1
#endif

#if defined(USE_POSIX_THREADS)
#include <pthread.h>

extern pthread_attr_t detached_thread;
extern pthread_attr_t joinable_thread;

#define INIT_THREADS \
  pthread_attr_init(&detached_thread); \
  pthread_attr_setdetachstate(&detached_thread, PTHREAD_CREATE_DETACHED); \
  pthread_attr_setscope(&detached_thread, PTHREAD_SCOPE_SYSTEM); \
  pthread_attr_init(&joinable_thread); \
  pthread_attr_setdetachstate(&joinable_thread, PTHREAD_CREATE_JOINABLE); \
  pthread_attr_setscope(&joinable_thread, PTHREAD_SCOPE_SYSTEM);

typedef pthread_t THREAD_T;	/* The type of a thread */
typedef pthread_t THREAD_ID;	/* The unique identifier of a thread */
typedef void *    THREAD_ARG;
typedef void *    THREAD_RES_T;
typedef void *(*THREAD_START)(void *);

/*
#define Thread_Create_NoGoalId(Process, Arg, Id, Handle) { \
    pthread_create(&(Id), &detached_thread, Process, Arg); \
    Handle = Id; \
}
*/

#define Thread_Create_GoalId(Process, Arg, Id, Handle) { \
    pthread_create(&(Id), &joinable_thread, Process, Arg); \
    Handle = Id; \
}

#define Thread_Join(Id)     pthread_join(Id, NULL)
#define Thread_Exit(Status) pthread_exit(Status)
#define Thread_Id           pthread_self()
#define Allow_Thread_Cancel \
     pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL); \
     pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
#define Disallow_Thread_Cancel \
     pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL); \
     pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
#define Thread_Cancel(Id) pthread_cancel(Id)
#define Thread_Equal(thr1, thr2) pthread_equal(thr1, thr2)
#define Thread_Dispose(ThrH) pthread_join(ThrH)
#else
#define INIT_THREADS
#endif  

#if defined(USE_WIN32_THREADS)

#include <windows.h>

typedef HANDLE THREAD_T;	/* The thread object */
typedef DWORD  THREAD_ID;	/* The unique thread identifier */
typedef LPVOID THREAD_ARG;	/* The argument to the initial thread call */
typedef DWORD  THREAD_RES_T;
typedef LPTHREAD_START_ROUTINE  THREAD_START; /* The type of the routine */

/* 
   Every Win32 thread remains in memory until it has finished and all
   references to it have been removed by using CloseHandle(); I am
   wrapping it inside the Thread_Dispose() macro.
*/

/*
#define Thread_Create_NoGoalId(Process, Arg, Id, Handle) \
        Handle = CreateThread(NULL, 0, Process, Arg, 0, Id)
*/

#define Thread_Create_GoalId(Process, Arg, Id, Handle) \
        Handle = CreateThread(NULL, 0, Process, Arg, 0, &(Id))

/* Thread_Join en Win32 needs a handle, which is not the same as a ThrId! */
#define Thread_Join(Handle) { \
        WaitForSingleObject(Handle, INFINITE);\
        CloseHandle(Handle); \
      }

#define Thread_Exit(Status)  ExitThread(Status)
#define Thread_Id            GetCurrentThreadId()
/* We are not giving security access to the threads, so everybody
   can cancel other threads. */
#define Allow_Thread_Cancel 
#define Disallow_Thread_Cancel  
#define Thread_Cancel(ThrH)  TerminateThread(ThrH, 0) /* Last arg exit code */
#define Thread_Dispose(ThrH) CloseHandle(ThrH) /* Not in other APIs */
#define Thread_Equal(thrid1, thrid2) (thrid1 == thrid2) /* needs Ids */

#endif /* USE_WIN32_THREADS */
#endif /* USE_THREADS */

#if !defined(USE_THREADS) || (!defined(USE_WIN32_THREADS) && !defined(USE_POSIX_THREADS))

#include <unistd.h>
#include <signal.h>


#if defined(IRIX)
# include <sys/types.h>
#endif

#if defined(SunOS4)
#include <sys/wait.h>
#endif

#if defined(Solaris) || defined(LINUX) || defined(Win32)
#include <sys/types.h>
#include <sys/wait.h>
#endif

typedef int THREAD_T;
typedef int THREAD_ID;	/* The unique identifier of a thread */
typedef void *THREAD_ARG;
typedef void *THREAD_RES_T;
typedef void *(*THREAD_START)(void *);


#define Thread_Id getpid()
#define Thread_Create_GoalId(Process, Arg, Id, Handle) \
      if ((void *)Id != NULL) Handle = Id = getpid(); Process (Arg)
#define Thread_Create_no_Id(Process, Arg) Process (Arg)
#define Thread_Join(i)
#define Thread_Exit(status)    exit(0)           /* Not correct, actually */
#define Allow_Thread_Cancel
#define Disallow_Thread_Cancel
#define Thread_Cancel(Id) kill(Thread_Id, SIGTERM)
#define Thread_Equal(thr1, thr2) (thr1 == thr2)
#endif /* USE_THREADS */

