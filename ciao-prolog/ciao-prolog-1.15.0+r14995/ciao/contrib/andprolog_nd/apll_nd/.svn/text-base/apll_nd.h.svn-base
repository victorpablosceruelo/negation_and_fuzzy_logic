#include "datadefs.h"
#include "support.h"
#include "support_defs.h"
#include "term_support_defs.h"
#include "threads.h"
#include "locks.h"
#include "initial.h"
#include "task_areas.h"
#include "wam.h"
#include "wam_defs.h"
#include "tasks_defs.h"
#include "startgoal_defs.h"
#include "nondet_defs.h"
#include "start_defs.h"
#include "alloc_defs.h"
#include "ciao_prolog.h"
#include "streams_defs.h"
#include "predtyp.h"

#include "math.h"
#include <time.h>
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>

#include <string.h> //memcpy



#if defined(ANDPARALLEL)
/* stats */
int total_goals;
int trapped_goals;
double trapped_time;
double trapped_fake_time;
SLOCK stats_l;

/* local atoms */
tagged_t fifo;
tagged_t lifo;
tagged_t det;

/* state of execution */
#define NOTEXECUTED    0
#define LOCAL_EXEC     1
#define REM_EXECUTING  2
#define FINISHED       3
#define FAILED         4
#define PENDING_SOLS   5
#define REEXECUTE      6
#define CANCELLED      7

#define NUMBER_STATES   8
#endif

#if defined(THREADS) && defined(USE_POSIX_THREADS)
extern pthread_attr_t joinable_thread;
#endif

#if defined(ANDPARALLEL)
#define PREV_CP(NODE) ChoiceCharOffset(NODE,-(NODE)->next_alt->node_offset)
#define H_INIT_CP(H) ((H)->exec_limits)->init
#define H_END_CP(H) PREV_CP((H)->exec_limits->end)
#define H_FRONT_CP(H) (H)->exec_limits->end
#endif

/* ****************************************** */
/*   ANDPROLOG LOW-LEVEL SUPPORT PRIMITIVES   */
/* ****************************************** */
extern bool_t apll_nd_start_thread(Argdecl);
extern bool_t apll_nd_number_agents(Argdecl);
extern bool_t apll_nd_push_goal(Argdecl);
extern bool_t apll_nd_find_goal(Argdecl);
extern bool_t apll_nd_goal_available(Argdecl);
extern bool_t apll_nd_cancellation(Argdecl);
extern bool_t apll_nd_free_memory(Argdecl);
extern bool_t apll_nd_retrieve_goal(Argdecl);
extern bool_t apll_nd_goal_det(Argdecl);
extern bool_t apll_nd_set_goal_det(Argdecl);
extern bool_t apll_nd_set_goal_nondet(Argdecl);
extern bool_t apll_nd_goal_not_executed(Argdecl);
extern bool_t apll_nd_set_goal_not_executed(Argdecl);
extern bool_t apll_nd_goal_rem_executing(Argdecl);
extern bool_t apll_nd_set_goal_rem_executing(Argdecl);
extern bool_t apll_nd_goal_finished(Argdecl);
extern bool_t apll_nd_set_goal_finished(Argdecl);
extern bool_t apll_nd_goal_tobacktrack(Argdecl);
extern bool_t apll_nd_set_goal_tobacktrack(Argdecl);
extern bool_t apll_nd_goal_toreexecute(Argdecl);
extern bool_t apll_nd_set_goal_toreexecute(Argdecl);
extern bool_t apll_nd_goal_failed(Argdecl);
extern bool_t apll_nd_set_goal_failed(Argdecl);
extern bool_t apll_nd_goal_cancelled(Argdecl);
extern bool_t apll_nd_set_goal_cancelled(Argdecl);

extern bool_t apll_nd_send_event(Argdecl);
extern bool_t apll_nd_read_event(Argdecl);
extern bool_t apll_nd_save_init_execution(Argdecl);
extern bool_t apll_nd_save_end_execution(Argdecl);
extern bool_t apll_nd_more_solutions(Argdecl);
extern bool_t apll_nd_move_execution_top(Argdecl);
extern bool_t apll_nd_move_pointers_down(Argdecl);
extern bool_t apll_nd_metacut_garbage_slots(Argdecl);

extern bool_t apll_nd_waiting(Argdecl);
extern bool_t apll_nd_suspend(Argdecl);
extern bool_t apll_nd_release(Argdecl);
extern bool_t apll_nd_release_remote(Argdecl);
extern bool_t apll_nd_release_some_suspended_thread(Argdecl);
extern bool_t apll_nd_release_all_for_unwinding(Argdecl);
extern bool_t apll_nd_enter_mutex(Argdecl);
extern bool_t apll_nd_enter_mutex_self(Argdecl);
extern bool_t apll_nd_enter_mutex_remote(Argdecl);
extern bool_t apll_nd_exit_mutex(Argdecl);
extern bool_t apll_nd_exit_mutex_self(Argdecl);
extern bool_t apll_nd_exit_mutex_remote(Argdecl);

extern bool_t apll_nd_clean_measures(Argdecl);
extern bool_t apll_nd_print_measures(Argdecl);
extern bool_t apll_nd_new_measure(Argdecl);
extern bool_t apll_nd_not_measure(Argdecl);
extern bool_t apll_nd_incr_num_local_backtr(Argdecl);

extern bool_t init(Argdecl);

double timeval_diff(struct timeval *a, struct timeval *b)
{
  return (double)(a->tv_sec + (double)a->tv_usec/CLOCKS_PER_SEC) -
    (double)(b->tv_sec + (double)b->tv_usec/CLOCKS_PER_SEC);
}

#define Prev_Wam_Of(W,Prev) {				\
    worker_t *tmp = W;					\
    do							\
      {							\
	tmp = Next_Wam_Of(tmp);				\
      }							\
    while(Next_Wam_Of(tmp) != W);			\
    Prev = tmp;						\
  }

/* Adds handler to the goal list */
#define INIT_HANDLER(ARG, H, GOAL, DETERM, STR)				\
  {									\
    DEREF(GOAL,GOAL);							\
    DEREF(DETERM,DETERM);						\
    DEREF(STR,STR);							\
    (*H)->goal = GOAL;							\
    (*H)->det = (DETERM==det)?TRUE:FALSE;				\
    (*H)->exec_state = NOTEXECUTED;					\
    (*H)->agent = ARG;							\
    (*H)->remote_agent = NULL;						\
    (*H)->exec_limits = NULL;						\
    (*H)->gle = (handler_entry_t *)checkalloc(sizeof(handler_entry_t));	\
    (*H)->gle->handler = STR;						\
  }
