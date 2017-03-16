/* Starts a new thread in the system */
bool_t apll_parback_start_thread(Arg)
     Argdecl;
{
#if defined(PARBACK)
  goal_descriptor_t *gd = NULL;

  // Make sure we are calling a callable term!
  DEREF(X(0),X(0));
  ENSURE_CALLABLE(X(0),1);

  gd = gimme_a_new_gd();

  gd->goal = X(0);
  gd->action = CREATE_WAM | KEEP_STACKS | CREATE_THREAD;
  {  // Copy goal to remote thread
    Argdecl = gd->worker_registers;
    DEREF(X(0), cross_copy_term(Arg, gd->goal));
  }
  gd->action |= NEEDS_FREEING;
  Thread_Create_GoalId(startgoal, gd, gd->thread_id, gd->thread_handle);

  // Increase the number of agents in the system
  Wait_Acquire_slock(nagents_l);
  nagents++;
  Release_slock(nagents_l);
#endif

  return TRUE;
}

/* Returns the number of agents available in the system */
bool_t apll_parback_number_agents(Arg)
     Argdecl;
{
#if defined(PARBACK)
  Wait_Acquire_slock(nagents_l);
  Unify(X(0),MakeInteger(Arg, nagents));
  Release_slock(nagents_l);
#endif

  return TRUE;
}

///* Copyright (C) 2007,2008 UPM-CLIP */
//
//#include "datadefs.h"
//#include "support.h"
//#include "support_defs.h"
//#include "term_support_defs.h"
//#include "threads.h"
//#include "locks.h"
//#include "initial.h"
//#include "task_areas.h"
//#include "wam.h"
//#include "wam_defs.h"
//#include "tasks_defs.h"
//#include "startgoal_defs.h"
//#include "nondet_defs.h"
//#include "start_defs.h"
//#include "alloc_defs.h"
//#include "ciao_prolog.h"
//#include "streams_defs.h"
//#include "predtyp.h"
//
//#include "math.h"
//#include <sys/time.h>
//#include <stdlib.h>
//#include <stdio.h>
//#include <string.h>
//
//#include "apll.h"
//#include "visandor.h"
//
//
//#if defined(ANDPARALLEL)
///* local atoms */
//tagged_t fifo;
//tagged_t lifo;
//tagged_t det;
//
///* state of execution */
//#define NOTEXECUTED    0
//#define REM_EXECUTING  1
//#define FINISHED       2
//#define PENDING_SOLS   4
//#define REEXECUTE      5
//#define CANCELLED      6
//
//#define NUMBER_STATES   7
//#endif
//
///* Checks whether a particular variable is a callable structure or not */
//#define NOT_CALLABLE(What) IsVar(What) || TagIsSmall(What) || TagIsLarge(What)
//#define ENSURE_CALLABLE(What, ArgNum)   \
//if (NOT_CALLABLE(What))  {printf("\nNot Callable\n"); }
////  BUILTIN_ERROR(TYPE_ERROR(CALLABLE), What, ArgNum)
//
//
///*****************************************************************/
///*            ANDPROLOG LOW-LEVEL SUPPORT PRIMITIVES             */
///*****************************************************************/
//
//
///* Starts a new thread in the system */
//bool_t apll_start_thread(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  goal_descriptor_t *gd = NULL;
//
//  // Make sure we are calling a callable term!
//  DEREF(X(0),X(0));
//  ENSURE_CALLABLE(X(0),1);
//
//  gd = gimme_a_new_gd();
//
//  gd->goal = X(0);
//  gd->action = CREATE_WAM | KEEP_STACKS | CREATE_THREAD;
//  {  // Copy goal to remote thread
//    Argdecl = gd->worker_registers;
//    DEREF(X(0), cross_copy_term(Arg, gd->goal));
//  }
//  gd->action |= NEEDS_FREEING;
//  Thread_Create_GoalId(startgoal, gd, gd->thread_id, gd->thread_handle);
//
//  // Increase the number of agents in the system
//  Wait_Acquire_slock(nagents_l);
//  nagents++;
//  Release_slock(nagents_l);
//#endif
//
//  return TRUE;
//}
//
//
///* Returns the number of agents available in the system */
//bool_t apll_number_agents(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  Wait_Acquire_slock(nagents_l);
//  cunify(Arg, X(0), MakeInteger(w, (int)nagents));
//  Release_slock(nagents_l);
//#endif
//
//  return TRUE;
//}
//
//
//#if defined(ANDPARALLEL)
///* Reads a handler */
//par_handler_t *read_handler(hdler)
//     tagged_t hdler;
//{
//  CIAO_REGISTER tagged_t x1 = (tagged_t)NULL;
//
//  // Obtain the handler struct
//  DEREF(hdler,hdler);
//  DerefSwitch(hdler,x1,;);
//  if (TagIsSTR(hdler) && (TagToHeadfunctor(hdler) == functor_Dhandler)) {
//    DerefArg(x1,hdler,1);
//    return (par_handler_t *)TermToPointer(x1);
//  }
//  else
//    return NULL;
//}
//
//
///* Adds handler to the goal list */
//void init_handler(Arg, h, goal, determ, str)
//     Argdecl;
//     par_handler_t *(*h);
//     tagged_t goal;
//     tagged_t determ;
//     tagged_t str;
//{
//  DEREF(goal,goal);
//  DEREF(determ,determ);
//  DEREF(str,str);
//
//  if (!IsVar(goal))
//    (*h)->goal = goal;
//  if (!IsVar(determ))
//    (*h)->det = (determ==det)?TRUE:FALSE;
//  (*h)->exec_state = NOTEXECUTED;
//  (*h)->agent = w;
//  (*h)->remote_agent = NULL;
//  (*h)->exec_limits = NULL;
//  (*h)->gle = (handler_entry_t *)checkalloc(sizeof(handler_entry_t));
//  ((*h)->gle)->handler = str;
//
//#if defined(VISANDOR)
//  // VisAndOr initialization
//  (Pcall_Level(w))++;
//  (*h)->ppf = Pcall_Level(w);
//  EVENT(FORK, Pcall_Level(w), 2);
//#endif
//}
//
//
///* Adds handler to the goal list */
//void add_handler_to_goal_list(Arg, gle)
//     Argdecl;
//     handler_entry_t *(*gle);
//{
//  if (Goal_List_Top != NULL) {
//    (*gle)->prev = Goal_List_Top;
//    Goal_List_Top->next = (*gle);
//    Goal_List_Start->prev = (*gle);
//    Goal_List_Top = (*gle);
//  }
//  else {
//    Goal_List_Start = (*gle);
//    Goal_List_Top = (*gle);
//    (*gle)->prev = Goal_List_Top;
//  }
//  (*gle)->next = Goal_List_Start;
//}
//
//
///* Removes handler from the goal list */
//void remove_handler_from_goal_list(Arg, gle, free)
//     Argdecl;
//     handler_entry_t *(*gle);
//     bool_t free;
//{
//  if ((*gle) != NULL) {
//    if (Goal_List_Start == Goal_List_Top) {
//      Goal_List_Start = NULL;
//      Goal_List_Top = NULL;
//    }
//    else {
//      ((*gle)->next)->prev = (*gle)->prev;
//      ((*gle)->prev)->next = (*gle)->next;
//      if (*gle == Goal_List_Top)
//	Goal_List_Top = (*gle)->prev;
//      if (*gle == Goal_List_Start)
//	Goal_List_Start = (*gle)->next;
//    }
//    if (free) {
//      checkdealloc((tagged_t *)(*gle),sizeof(handler_entry_t));
//      (*gle) = NULL;
//    }
//  }
//}
//
//
//void send_event(w,tagged_h,canc)
//     Argdecl;
//     tagged_t tagged_h;
//     bool_t canc;
//{
//  par_handler_t *h = read_handler(tagged_h);
//
//  if (h != NULL) {
//    worker_t *remote_a = (worker_t *)(h->remote_agent);
//    if ((remote_a != NULL) && (remote_a != w)) {
//#if defined(DEBUG)
//      if (debug_threads) {
//	printf("send_event("); prolog_display(Arg);
//	printf(",%d) from %x to %x\n",canc,w,remote_a);
//      }
//#endif
//
//      // Create a new entry for the event queue
//      DEREF(X(0),X(0));
//      event_entry_t *new_eqe =
//	(event_entry_t *)checkalloc(sizeof(event_entry_t));
//      new_eqe->handler = read_handler(X(0));
//      new_eqe->canc = canc;
//
//      // Add entry to the remote event queue
//      Wait_Acquire_slock(Event_Queue_Lock_Of(remote_a));
//      if (Event_Queue_Top_Of(remote_a) != NULL) {
//	new_eqe->prev = Event_Queue_Top_Of(remote_a);
//	Event_Queue_Top_Of(remote_a)->next = new_eqe;
//	Event_Queue_Start_Of(remote_a)->prev = new_eqe;
//	Event_Queue_Top_Of(remote_a) = new_eqe;
//      }
//      else {
//	Event_Queue_Start_Of(remote_a) = new_eqe;
//	Event_Queue_Top_Of(remote_a) = new_eqe;
//	new_eqe->prev = Event_Queue_Top_Of(remote_a);
//      }
//      new_eqe->next = Event_Queue_Start_Of(remote_a);
//      Release_slock(Event_Queue_Lock_Of(remote_a));
//    }
//  }
//}
//#endif
//
//
///* Pushes the handler associated to a goal on to the goal list */
//bool_t apll_push_goal(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = NULL;
//
//  DEREF(X(0),X(0));
//  DEREF(X(1),X(1));
//  DEREF(X(2),X(2));
//  if (!IsVar(X(0))) ENSURE_CALLABLE(X(0),1);
//
//  // Create or read the handler
//  if (IsVar(X(2))) {  // Create
//    // Allocating memory for the handler
//    h = (par_handler_t *)checkalloc(sizeof(par_handler_t));
//
//    // Create the structure in the heap
//    CIAO_REGISTER tagged_t *pt1 = w->global_top;
//    HeapPush(pt1,functor_Dhandler);
//    HeapPush(pt1,PointerToTerm((int)h));
//    w->global_top = pt1;
//    CTagToPointer(X(2)) = Tag(STR, HeapOffset(pt1,-2));
//
//    // Initialize handler and add it to the goal list
//    DEREF(X(2),X(2));
//    Wait_Acquire_slock(Goal_List_Lock);
//    init_handler(w,&h,X(0),X(1),X(2));
//    add_handler_to_goal_list(w,&(h->gle));
//    unwinding_done = FALSE;
//  }
//  else {  // Read
//    // Initialize handler and add it to the goal list, if not in it yet
//    h = read_handler(X(2));
//    if (h != NULL) {
//      Wait_Acquire_slock(Goal_List_Lock);
//      init_handler(w,&h,X(0),X(1),X(2));
//      add_handler_to_goal_list(w,&(h->gle));
//    }
//  }
//
//  // Incrementing the number of parallel goals in the system
//
//  Release_slock(Goal_List_Lock);
//
//#if defined(DEBUG)
//  if (debug_threads) {
//    tagged_t aux_x0 = X(0); X(0) = X(2);
//    printf("Goal pushed: "); prolog_display(Arg); printf(" by %x\n",w);
//    X(0) = aux_x0;
//  }
//#endif
//#endif
//
//  return TRUE;
//}
//
//
///* Finds a handler associated to a goal from the goal list of some agent */
//bool_t apll_find_goal(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  bool_t found_goal = FALSE;
//  worker_t *aux = Next_Wam_Of(w);
//  handler_entry_t *gle = NULL;
//
//  DEREF(X(0),X(0));
//  DEREF(X(1),X(1));
//  if (aux != w) {
//    do {
//      Wait_Acquire_slock(Goal_List_Lock_Of(aux));
//      if (Goal_List_Top_Of(aux) != NULL && Goal_List_Start_Of(aux) != NULL) {
//
//	// Pick up the handler
//	if (X(0) == fifo)  // FIFO scheduling
//	  gle = Goal_List_Start_Of(aux);
//	else if (X(0) == lifo)  // LIFO scheduling
//	  gle = Goal_List_Top_Of(aux);
//	else
//	  MAJOR_FAULT("Goal scheduling error in apll_find_goal.");
//
//	// Incrementing the number of parallel goals that have been stoled
//	Wait_Acquire_slock(npargoalstaken_l);
//	if (npargoalstaken != NULL && measure) (npargoalstaken->value)++;
//	Release_slock(npargoalstaken_l);
//
//	par_handler_t *h = read_handler(gle->handler);
//	if (h != NULL) {
//	  if ((X(1) != det) || ((X(1) == det) && (h->det))) {
//	    // Give value to remote_agent in the handler
//	    h->remote_agent = w;
//
//	    // Return a pointer to the remote goal in X(2)
//	    cunify(Arg, X(2), gle->handler);
//
//#if defined(DEBUG)
//	    if (debug_threads) {
//	      tagged_t aux_x0 = X(0); X(0) = X(2);
//	      printf("Handler found: "); prolog_display(Arg);
//	      printf(" by %x\n",w);
//	      X(0) = aux_x0;
//	    }
//#endif
//
//	    // Remove handler from goal list in remote agent
//	    remove_handler_from_goal_list(aux,&(h->gle),TRUE);
//	    h->exec_state = REM_EXECUTING;
//	    h->gle = NULL;
//
//	    // Exit
//	    found_goal = TRUE;
//	  }
//	}
//	else {
//	  remove_handler_from_goal_list(aux,&gle,TRUE);
//	  gle = NULL;
//	}
//      }
//      Release_slock(Goal_List_Lock_Of(aux));
//      aux = Next_Wam_Of(aux);
//    }
//    while ((!found_goal) && (aux != w));
//  }
//
//  return found_goal;
//#else
//  return TRUE;
//#endif
//}
//
//
///* Succeeds if the handler has not been picked up yet by another agent
//   and fails otherwise */
//bool_t apll_goal_available(Arg)
//     Argdecl;
//{
//  bool_t result = TRUE;
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  // Check whether the goal has been taken by another agent or not
//  if (h != NULL) {
//    Wait_Acquire_slock(Goal_List_Lock);
//    if ((h->gle == NULL) && (h->remote_agent != w))
//      result = FALSE;
//    else {
//      h->remote_agent = w;
//      remove_handler_from_goal_list(w,&(h->gle),TRUE);
//      h->gle = NULL;
//    }
//    Release_slock(Goal_List_Lock);
//  }
//  else
//    result = FALSE;
//#endif
//
//  return result;
//}
//
//
///* Frees the memory used by the handler */
//bool_t apll_cancellation(Arg)
//     Argdecl;
//{
//  bool_t ret = TRUE;
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    Wait_Acquire_slock(Goal_List_Lock);
//    if (h->gle != NULL) {  // Handler still in goal list
//      printf("H->GLE!=NULL\n");
//      remove_handler_from_goal_list(Arg,&(h->gle),TRUE);
//      h->gle = NULL;
//      Release_slock(Goal_List_Lock);
//    }
//    else {
//      if (!h->det) {
//	if ((h->remote_agent != NULL) && (h->remote_agent != Arg) &&
//            (h->agent == Arg)) {
//	  printf("PREVIEW\n");
//	  Release_slock(Goal_List_Lock);
//	  Wait_Acquire_slock(Mutex_Lock);
//	  worker_t *rem_a = h->remote_agent;
//	  printf("PREVIEW_DONE\n");
//	  if (h->exec_state == NOTEXECUTED) {  // done
//	    printf("NOTEXECUTED\n");
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == REM_EXECUTING) {  // done
//	    printf("REMEXECUTING\n");
//	    if (Cancel_Goal_Exec_Handler_Of(h->remote_agent) != NULL) printf("\nPROBLEMAS CANCEL I\n");
//	    Cancel_Goal_Exec_Handler_Of(h->remote_agent) = h;
//	    h->exec_state = CANCELLED;
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == FINISHED) {  // done
//	    printf("FINISHED\n");
//	    printf("\nPROBLEMAS I\n"); fflush(stdout);
//	    h->exec_state = CANCELLED;
//	    Release_slock(Mutex_Lock);
//	    Wait_Acquire_slock(Mutex_Lock_Of(rem_a));
//	    send_event(Arg,X(0),TRUE);
//	    if (Suspended_Waiting_For_Work_Of(rem_a)) {
//	      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(rem_a));
//	      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(rem_a));
//	      Release_lock(Waiting_For_Work_Lock_Of(rem_a));
//	    }
//	    Release_slock(Mutex_Lock_Of(rem_a));
//	  }
//	  else if (h->exec_state == FAILED) {  // done
//	    printf("FAILED\n");
//	    Release_slock(Mutex_Lock);
//	    ret = FALSE;
//	  }
//	  else if (h->exec_state == PENDING_SOLS) {  // done
//	    printf("PENDING_SOLS\n");
//	    if (Cancel_Goal_Exec_Handler_Of(h->remote_agent) != NULL) printf("\nPROBLEMAS CANCEL III\n");
//	    Cancel_Goal_Exec_Handler_Of(h->remote_agent) = h;
//	    h->exec_state = CANCELLED;
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == REEXECUTE) {  // done
//	    printf("REEXECUTED\n");
//	    h->exec_state = CANCELLED;
//	    Release_slock(Mutex_Lock);
//	  }
//	  else { Release_slock(Goal_List_Lock); printf("ELSE\n"); ret = FALSE; }
//	}
//	else
//	  {
//	    printf("h_>REMOTE=NULL\n");
//	    Release_slock(Goal_List_Lock);
//	    ret = FALSE;
//	  }
//      }
//      else {
//	printf("DETERMINISTIC\n");
//	Release_slock(Goal_List_Lock);
//      }
//    }
//
//    // Free memory used for handler
//        printf("\nLIBERAR BIEN despues de que el remoto cancele\n");
//    //checkdealloc((tagged_t *)h,sizeof(par_handler_t));
//
//#if defined(DEBUG)
//    if (debug_threads) {
//      printf("Cancellation done: "); prolog_display(Arg); printf(" by %x\n",Arg);
//    }
//#endif
//  }
//#endif
//
//  return ret;
//}
//
//
///* Returns the goal associated to the handler */
//bool_t apll_retrieve_goal(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return cunify(Arg, X(1), h->goal);
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether the goal is deterministic or not */
//bool_t apll_goal_det(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return h->det;
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Sets the goal to deterministic */
//bool_t apll_set_goal_det(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    if (h->agent == w || h->remote_agent == w)
//      h->det = TRUE;
//#endif
//
//  return TRUE;
//}
//
//
///* Sets the goal to non-deterministic */
//bool_t apll_set_goal_nondet(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    if (h->agent == w || h->remote_agent == w)
//      h->det = FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether the goal has not been executed yet */
//bool_t apll_goal_not_executed(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return (h->exec_state == NOTEXECUTED);
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Sets the goal execution to never executed */
//bool_t apll_set_goal_not_executed(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    if (h->agent == w || h->remote_agent == w)
//      h->exec_state = NOTEXECUTED;
//    else
//      return FALSE;
//  }
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether the goal is remotely executing */
//bool_t apll_goal_rem_executing(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return (h->exec_state == REM_EXECUTING);
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Sets the goal execution to being remotely executing */
//bool_t apll_set_goal_rem_executing(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    if (h->agent == w || h->remote_agent == w)
//      h->exec_state = REM_EXECUTING;
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether the goal execution has finished or not */
//bool_t apll_goal_finished(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return (h->exec_state == FINISHED);
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Sets the goal execution to finished */
//bool_t apll_set_goal_finished(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    if (h->agent == w || h->remote_agent == w)
//      h->exec_state = FINISHED;
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether the goal execution has to backtrack or not */
//bool_t apll_goal_tobacktrack(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return (h->exec_state == PENDING_SOLS);
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
///* Sets the goal execution to be backtracked */
//bool_t apll_set_goal_tobacktrack(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    if (h->agent == w || h->remote_agent == w)
//      h->exec_state = PENDING_SOLS;
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether the goal execution has to be reexecuted or not */
//bool_t apll_goal_toreexecute(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    {
//      return (h->exec_state == REEXECUTE);
//    }
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Sets the goal execution to be reexecuted */
//bool_t apll_set_goal_toreexecute(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    if (h->agent == w || h->remote_agent == w)
//      h->exec_state = REEXECUTE;
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether the goal execution has failed or not */
//bool_t apll_goal_failed(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return (h->exec_state == FAILED);
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Sets the goal execution to finished */
//bool_t apll_set_goal_failed(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    if (h->agent == w || h->remote_agent == w)
//      h->exec_state = FAILED;
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether the goal execution has been cancelled or not */
//bool_t apll_goal_cancelled(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return (Cancel_Goal_Exec_Handler_Of(h->remote_agent) != h);
//  else
//    return FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* Sets the goal execution to cancelled */
//bool_t apll_set_goal_cancelled(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    if (h->agent == w || h->remote_agent == w)
//      h->exec_state = CANCELLED;
//#endif
//
//  return TRUE;
//}
//
//
///* Sends the goal to the agent that picked it up in order to perform
//   backtracking over it */
//bool_t apll_send_event(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  send_event(w,X(0),FALSE);
//#endif
//
//  return TRUE;
//}
//
//
///* Reads an event from the event queue and fails if it is empty */
//bool_t apll_read_event(Arg)
//     Argdecl;
//{
//  bool_t ret = TRUE;
//
//#if defined(ANDPARALLEL)
//  Wait_Acquire_slock(Event_Queue_Lock);
//  if ((Event_Queue_Start == NULL) && (Event_Queue_Top == NULL)) {
//    ret = FALSE;
//  }
//  else {
//    if (Event_Queue_Start->canc)
//      {
//	Event_Queue_Start->handler->exec_state = CANCELLED;
//	printf("READ_EVENT - CANCELLED HANDLER\n");
//      }
//
//    // Create the structure in the heap
//    CIAO_REGISTER tagged_t *pt1 = w->global_top;
//    HeapPush(pt1,functor_Dhandler);
//    HeapPush(pt1,PointerToTerm((int)Event_Queue_Start->handler));
//    w->global_top = pt1;
//    CTagToPointer(X(0)) = Tag(STR, HeapOffset(pt1,-2));
//
//#if defined(DEBUG)
//    if (debug_threads) {
//      printf("read_event("); prolog_display(Arg); printf(") by %x\n",w);
//    }
//#endif
//
//    // Remove entry from the event queue
//    event_entry_t *eqe = NULL;
//    if (Event_Queue_Start == Event_Queue_Top) {
//      eqe = Event_Queue_Start;
//      Event_Queue_Start = NULL;
//      Event_Queue_Top = NULL;
//    }
//    else {
//      (Event_Queue_Start->next)->prev = Event_Queue_Start->prev;
//      (Event_Queue_Start->prev)->next = Event_Queue_Start->next;
//      eqe = Event_Queue_Start;
//      Event_Queue_Start = Event_Queue_Start->next;
//    }
//    checkdealloc((tagged_t *)eqe, sizeof(event_entry_t));
//  }
//  Release_slock(Event_Queue_Lock);
//#endif
//
//  return ret;
//}
//
//
///* Saves a pointer to the initial choice point of the goal execution
//   in the handler */
//bool_t apll_save_init_execution(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    if (h->exec_limits == NULL) {
//      (h->exec_limits) = (parallel_exec_entry_t *)
//                         checkalloc(sizeof(parallel_exec_entry_t));
//      (h->exec_limits)->init = w->node;
//      (h->exec_limits)->end = w->node;
//    }
//    else {
//      (h->exec_limits)->init = w->node;
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
///* Saves a pointer to the final choice point of the goal execution in
//   the handler */
//bool_t apll_save_end_execution(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    if (h->exec_limits != NULL) {
//      (h->exec_limits)->end = w->node;
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
///* Returns whether there are more solutions to be computed or not */
//bool_t apll_more_solutions(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    if (h->exec_limits != NULL) {
//      if (h->det)
//	return (h->exec_limits)->init == (h->exec_limits)->end;
//      else
//	return !((h->exec_limits)->init == 
//		 ChoiceCharOffset((h->exec_limits)->end,
//		   -((h->exec_limits)->end)->next_alt->node_offset));
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
//bool_t apll_estado_pilas(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//    worker_t *aux = w;
//    do {
//      printf("AGENTE %p",aux);
//      printf("  node:%p\n",w->node);
//      printf("  global_top:%p\n",w->global_top);
//      printf("  local_top:%p\n",w->local_top);
//      printf("  trail_top:%p\n",w->trail_top);
//      aux = Next_Wam_Of(aux);
//    }
//    while ((aux != w));
//#endif
//}
//
//
///* Moves the choice point of the parallel goal to the top of the
//   stack */
//bool_t apll_move_execution_top(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//  
//  node_t *node = Arg->node;
//  
//  // Move choice points of current parallel goal execution to top of stack
//  Arg->node = ChoiceCharOffset(Arg->node,-Arg->node->next_alt->node_offset);
//  ComputeA(Arg->local_top,Arg->node);
//  if (h != NULL) {
//    if (h->exec_limits != NULL) {
//      Wait_Acquire_slock(Parallel_Exec_Lock);
//      CIAO_REGISTER node_t *init = (h->exec_limits)->init;
//      CIAO_REGISTER node_t *end = (h->exec_limits)->end;
//      printf("***********************Arg->node=%p     h->exec_limits->end=%p\n",Arg->node,h->exec_limits->end);
//      if (ChoiceYounger(Arg->node, (h->exec_limits)->end)) {
//
//	Release_slock(Parallel_Exec_Lock);
//	CIAO_REGISTER node_t *node, *node2, *pargoal_node, *younger_nodes;
//	CIAO_REGISTER tagged_t *pargoal_trail, *younger_trail;
//
///* #if defined(DEBUG) */
///*     if (debug_threads) { */
//	printf("Entering apll_move_execution_top...\n");
///*     } */
///* #endif */
//
//	printf("OLD Arg->node=%x\n",(unsigned int)Arg->node);
//
//	node = Arg->node;
//	while (!ChoiceYounger(init, node)) {
//	  printf("node=%x\n", (unsigned int)node);
//	  printf("  node->trail_top=%x\n", (unsigned int)node->trail_top);
//	  printf("  node->global_top=%x\n", (unsigned int)node->global_top);
//	  //	  printf("  node->next_insn=%x\n", (unsigned int)node->next_insn);
//	  printf("  node->local_top=%x\n", (unsigned int)node->local_top);
//	  int i;
//	  for (i = 0; i < OffsetToArity(node->next_alt->node_offset) ; i++)
//	    printf("  node->term[%d]=%x\n", i,(unsigned int)node->term[i]);
//	  node = ChoiceCharOffset(node,-node->next_alt->node_offset);
//	}
//	printf("1) init=%x   end=%x Arg->node=%p\n", (unsigned int)(h->exec_limits)->init,
//	       (unsigned int)(h->exec_limits)->end,Arg->node);
//
///* 	tagged_t *iT = Arg->trail_top; */
///* 	while(iT >= init->trail_top) */
///* 	  { */
///* 	    //	    printf("\nTRAIL %p = %p\n",iT,*iT); */
///* 	    iT--; */
///* 	  } */
//	printf("\nArg->trail_top:%x\n",(unsigned int)Arg->trail_top); fflush(stdout);
//	// STEP ZERO: Calculate sizes of auxiliary data structures.
//	// Stack and trail grow in different directions
//	int size_pargoal_node =
//          ChoiceCharDifference(ChoiceCharOffset(init, -init->next_alt->node_offset), end);
//	int size_younger_nodes =
//	  ChoiceCharDifference(end,Arg->node);
//	
//	printf("\nNODE %p - END %p\n",Arg->node,end);
//	for(node = Arg->node; node != end; node = ChoiceCharOffset(node, -node->next_alt->node_offset))
//	  node2 = node;
//
//	node2->global_top = init->global_top;
//	node2->local_top = init->local_top;
//
//	printf("\nprimer node de youngers %p\n",node2);
//
//	int size_pargoal_trail =
//          TrailCharDifference(//init->trail_top, //end->trail_top);
//			  init->trail_top, node2->trail_top);
//	    //ChoiceCharOffset(end, end->next_alt->node_offset+4)->trail_top);
//	//			  end->trail_top);
//	int size_younger_trail =
//	  TrailCharDifference(node2->trail_top, Arg->trail_top);
//			  //	    ChoiceCharOffset(end, end->next_alt->node_offset+4)->trail_top,
//			  //end_trail_top,
//	//            Arg->trail_top);
///* 	int size_pargoal_trail = */
///*           TrailDifference(init->trail_top, //end->trail_top); */
///* 	    ChoiceCharOffset(end, end->next_alt->node_offset+4)->trail_top); */
///* 	int size_younger_trail = */
///* 	  TrailDifference(//end->trail_top, Arg->trail_top); */
///* 	    ChoiceCharOffset(end, end->next_alt->node_offset+4)->trail_top, */
///*             Arg->trail_top); */
//
//	printf("size_pargoal_node=%d\n", (unsigned int)size_pargoal_node);
//	printf("size_younger_nodes=%d\n", (unsigned int)size_younger_nodes);
//	printf("size_pargoal_trail=%d\n", (unsigned int)size_pargoal_trail);
//	printf("size_younger_trail=%d\n", (unsigned int)size_younger_trail);
//
//	// STEP ONE: save memory for auxiliary data structures
//	// Behavior of memcpy when overlapping is undetermined.
//	// Therefore, we need to move younger section of stack to another
//        // auxiliary place in memory before moving it down on the stack
//        pargoal_node = (node_t *) checkalloc(size_pargoal_node);
//        younger_nodes = (node_t *) checkalloc(size_younger_nodes);
//	pargoal_trail = (tagged_t *) checkalloc(size_pargoal_trail);
//	younger_trail = (tagged_t *) checkalloc(size_younger_trail);
//
//        // STEP TWO: Copy choice points of goal execution and trail to
//        // auxiliary data structures
//       	memcpy(pargoal_node, end, size_pargoal_node);
//       	memcpy(younger_nodes, Arg->node, size_younger_nodes);
//	memcpy(pargoal_trail, init->trail_top, size_pargoal_trail);
//	memcpy(younger_trail, node2->trail_top, size_younger_trail);
//
//
//	// STEP THREE: Move younger choice points and trail section down
//	//	printf("Arg->node before=%x\n",(unsigned int)Arg->node);
//	Arg->node = ChoiceCharOffset(Arg->node, -size_pargoal_node);
//	//printf("Arg->node after=%x\n",(unsigned int)Arg->node);
//	memcpy(init->trail_top, younger_trail, size_younger_trail);
//	memcpy(Arg->node, younger_nodes, size_younger_nodes);
//	//printf("Arg->trail_top before=%x\n",(unsigned int)Arg->trail_top);
//	    //Arg->trail_top = TrailOffset(Arg->trail_top, -size_pargoal_trail);
//	//printf("Arg->trail_top after=%x\n",(unsigned int)Arg->trail_top);
//
//	//printf("size_younger_trail=%d\n", (unsigned int)size_younger_trail);
//
//	// STEP FOUR: Update trail pointers of choice points that are
//	// moved down the stack
//	node = node2 = Arg->node;
//	while (!ChoiceYounger(init, node2))
//	  {
//	    node2->trail_top = TrailCharOffset(node2->trail_top, -size_pargoal_trail);
//	    node2 = ChoiceCharOffset(node2,-node2->next_alt->node_offset);
//	  }
//	
//	// STEP FIVE: Update top of heap, local stack of oldest choice point
//        //node->local_top = par_goal_node->local_top;
//	// node->global_top = init->global_top;
//
//	// STEP SIX: Move pargoal_node to top of stack and
//	// pargoal_trail to top of trail
//      	Arg->node = ChoiceCharOffset(Arg->node, size_pargoal_node);
//	memcpy(Arg->node, pargoal_node, size_pargoal_node);
//	//(h->exec_limits)->end = Arg->node;
//	//Arg->trail_top = TrailOffset(Arg->trail_top, size_pargoal_trail);
//	memcpy(TrailCharOffset(Arg->trail_top, -size_pargoal_trail), pargoal_trail, size_pargoal_trail);
//	//	Arg->node->trail_top = Arg->trail_top;
//	//(h->exec_limits)->trail_top = Arg->trail_top;
///* 	ComputeA(Arg->local_top,Arg->node); */
///* 	NewShadowregs(Arg->global_top); */
///* 	SaveGtop(Arg->node, Arg->global_top); */
///* 	SaveLtop(Arg->node); */
///* 	SetShadowregs(Arg->node); */
//
//	// STEP NINE: Update trail pointers of choice points that are
//	// moved on top of the stack
//	node2 = Arg->node;
//	while (ChoiceYounger(node2, node)) {
//	  node2->global_top = Arg->global_top;
//	  node2->local_top = Arg->local_top;
//	  node2->trail_top = TrailCharOffset(node2->trail_top, size_younger_trail);
//	  node2 = ChoiceCharOffset(node2,-node2->next_alt->node_offset);
//	}
//
//	node2 = Arg->node;
//	while (ChoiceYounger(node2, Choice_Start)) {
//	    int i;
//	    for (i = 0; i < OffsetToArity(node2->next_alt->node_offset); i++)
//	      {
//		par_handler_t *ht = read_handler(node2->term[i]);
//		if ((ht != NULL) && !(ht->goal & 1) && (ht->exec_limits != NULL))
//		  {
//		    ht->goal = ht->goal | 1;
//		    //remote handler??
//		    if ((ChoiceYounger(Choice_Start,ht->exec_limits->init)) || (ChoiceYounger(ht->exec_limits->end, Choice_End)))
//		      {
//			printf("\nPuntero a handler remoto\n");
//			printf("\nHay que actualizar limites de otra manera\n");
//			continue;
//		      }
//		    //is init in the affected choice segment?
//		    if (!ChoiceYounger(init,ht->exec_limits->init))
//		      {
//			//is init in the younger choice segment?
//			if (ChoiceYounger(ht->exec_limits->init,ChoiceCharOffset(Arg->node,-size_younger_nodes)))
//			  ht->exec_limits->init = ChoiceCharOffset(ht->exec_limits->init,-size_pargoal_node);
//			else 
//			  ht->exec_limits->init = ChoiceCharOffset(ht->exec_limits->init,size_younger_nodes);			
//		      }
//		    //is end in the affected choice segment?
//		    if (!ChoiceYounger(init,ht->exec_limits->end))
//		      {
//			//is end in the younger choice segment?
//			if (ChoiceYounger(ht->exec_limits->end,ChoiceCharOffset(Arg->node,-size_younger_nodes)))
//			  {
//			    ht->exec_limits->end = ChoiceCharOffset(ht->exec_limits->end,-size_pargoal_node);
//			  }
//			else
//			  {
//			    ht->exec_limits->end = ChoiceCharOffset(ht->exec_limits->end,size_younger_nodes);	
//			  }
//		      }
//		  }
//	      }
//	    node2 = ChoiceCharOffset(node2,-node2->next_alt->node_offset);
//	}
//
//	node2 = Arg->node;
//	while (ChoiceYounger(node2, Choice_Start)) {
//	  int i;
//	  for (i = 0; i < OffsetToArity(node2->next_alt->node_offset); i++)
//	    {
//	      par_handler_t *ht = read_handler(node2->term[i]);
//	      if (ht != NULL) ht->goal = ht->goal & 0xfffffffe;
//	    }
//	  node2 = ChoiceCharOffset(node2,-node2->next_alt->node_offset);
//	}
//
//	// CLEANUP? Increment the number of remote trapped backwards executions
//	Wait_Acquire_slock(nrembacktr_trapped_l);
//	if (nrembacktr_trapped != NULL && measure)
//	  (nrembacktr_trapped->value)++;
//	Release_slock(nrembacktr_trapped_l);
//
///* #if defined(DEBUG) */
///*       if (debug_threads) { */
//        printf("Exiting apll_move_execution_top!\n");
///*       } */
///* #endif */
//
//	node = Arg->node;
//	while (!ChoiceYounger(init, node)) {
//	  printf("node=%x\n", (unsigned int)node);
//	  printf("  node->trail_top=%x\n", (unsigned int)node->trail_top);
//	  printf("  node->global_top=%x\n", (unsigned int)node->global_top);
//	  //	  printf("  node->next_insn=%x\n", (unsigned int)node->next_insn);
//	  printf("  node->local_top=%x\n", (unsigned int)node->local_top);
///* 	  int i; */
///* 	  for (i = 0; i < OffsetToArity(node->next_alt->node_offset) ; i++) */
///* 	    printf("  node->term[%d]=%x\n", i,(unsigned int)node->term[i]); */
//	  node = ChoiceCharOffset(node,-node->next_alt->node_offset);
//	}
//	printf("2) init=%x   end=%x Arg->node=%p\n", (unsigned int)(h->exec_limits)->init,
//	       (unsigned int)(h->exec_limits)->end,Arg->node);
//
//	printf("\nArg->trail_top:%x\n",(unsigned int)Arg->trail_top); fflush(stdout);
//
///* 	iT = Arg->trail_top; */
///* 	while(iT >= init->trail_top) */
///* 	  { */
///* 	    //	    printf("\nTRAIL %p = %p\n",iT,*iT); */
///* 	    iT--; */
///* 	  } */
//
///* 	node = Arg->node; */
///* 	while (!ChoiceYounger(init, node)) { */
///* 	  printf("\nnode=%x\n", (unsigned int)node); */
///* 	  printf("  node->trail_top=%x\n", (unsigned int)node->trail_top); */
///* 	  printf("  node->global_top=%x\n", (unsigned int)node->global_top); */
///* 	  printf("  node->next_alt=%x\n", (unsigned int)node->next_alt); */
///* 	  printf("  node->frame=%x\n", (unsigned int)node->frame); */
///* 	  printf("  node->next_insn=%x\n", (unsigned int)node->next_insn); */
///* 	  printf("  node->local_top=%x\n", (unsigned int)node->local_top); */
///* 	  printf("  node->term[0]=%x\n", (unsigned int)node->term[0]); */
///* 	  node = ChoiceCharOffset(node,-node->next_alt->node_offset); */
///* 	} */
///* 	printf("init=%x   end=%x\n", (unsigned int)(h->exec_limits)->init, (unsigned int)(h->exec_limits)->end); */
//
//	checkdealloc((tagged_t*)pargoal_node,size_pargoal_node);
//	checkdealloc((tagged_t*)younger_nodes,size_younger_nodes);
//	checkdealloc(pargoal_trail,size_pargoal_trail);
//	checkdealloc(younger_trail,size_younger_trail);
//      }
//      else {
//	SetShadowregs(Arg->node);
//	Release_slock(Parallel_Exec_Lock);
//
//	// CLEANUP? Increment the number of remote backwards executions on top
//	Wait_Acquire_slock(nrembacktr_top_l);
//	if (nrembacktr_top != NULL && measure)
//	  (nrembacktr_top->value)++;
//	Release_slock(nrembacktr_top_l);
//      }
//
///*       // If no more solutions of a goal are needed then the goal execution */
///*       // section is removed from the stack */
///*       if (h->exec_state == CANCELLED) { */
///* 	printf("------------------------------CANCELLED\n"); */
///* 	Arg->node = ChoiceCharOffset(init, -init->next_alt->node_offset); */
///* 	SetShadowregs(Arg->node); */
///*       } */
//
//      
//    }
//    
//    if (h->exec_state == CANCELLED)
//      {
//	if (Cancel_Goal_Exec_Handler != NULL) printf("\nPROBLEMAS CANCEL II\n");
//	printf("NEW CANCEL_GOAL_EXEC_HANDLER\n");
//	Cancel_Goal_Exec_Handler = h;
//      }
//    else h->exec_state = REM_EXECUTING;
//  }
//  
//#endif
//
//  printf("\nFIN  %x\n",(unsigned int)Arg->node);
//  return TRUE;
//}
//
//
///* Returns whether the agent associated to the input handler is
//   waiting for some work or not */
//bool_t apll_waiting(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL)
//    return Suspended_Waiting_For_Work_Of(h->agent);
//#endif
//
//  return TRUE;
//}
//
//
///* Suspends the execution of the current thread */
//bool_t apll_suspend(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//#if defined(VISANDOR)
//  EVENT(AGENT_IDLE,0,nacagents);
//#endif
//
//  printf("\nSUSPENDEMOS %p\n",w);
//  Wait_Acquire_lock(Waiting_For_Work_Lock);
//#if defined(DEBUG)
//  if (debug_threads)
//    printf("Suspending: %x\n",w);
//#endif
//  if (Mode == UNWIND) Mode = FORWARD_EXEC;
//  Suspended_Waiting_For_Work = TRUE;
//  Release_slock(Mutex_Lock);
//  Suspend = WAITING;
//
//#if defined(Solaris)
//  //Measuring the active time of the agent
//  if (measure)
//    Suspending_Time_Cont = gethrtime();
//  if (Total_Suspending_Time != NULL && measure)
//    Total_Suspending_Time->n++;
//#endif
//
//  printf("\nA CASCARLA %p\n",w);
//  Cond_Var_Wait(Waiting_For_Work_Cond_Var,Waiting_For_Work_Lock);
//  printf("\nSALIMOS %p\n",w);
//
//#if defined(Solaris)
//  // Measuring the active time of the agent
//  if (Total_Suspending_Time != NULL && measure)
//    Total_Suspending_Time->time = Total_Suspending_Time->time +
//      (double)(gethrtime() - Suspending_Time_Cont);
//#endif
//  
//  Suspend = RELEASED;
//#if defined(DEBUG)
//  if (debug_threads)
//    printf("Releasing: %x\n",w);
//#endif
//  printf("AAA\n");
//  Release_lock(Waiting_For_Work_Lock);
//  Wait_Acquire_slock(Mutex_Lock);
//  Suspended_Waiting_For_Work = FALSE;
//  printf("BBB\n");
//
//#if defined(VISANDOR)
//  EVENT(AGENT_BUSY,0,nacagents);
//#endif
//
//  return (Mode == FORWARD_EXEC);
//#endif
//
//  return TRUE;
//}
//
//
///* Releases the execution of the publishing agent */
//bool_t apll_release(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    if (h->remote_agent == w && h->exec_state >= 0 &&
//        h->exec_state < NUMBER_STATES) {
//      worker_t *ag = h->agent;
//      if ((ag != NULL) && (ag != w) && (Suspended_Waiting_For_Work_Of(ag))) {
//	Wait_Acquire_lock(Waiting_For_Work_Lock_Of(ag));
//	Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(ag));
//	Release_lock(Waiting_For_Work_Lock_Of(ag));
//      }
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
///* Releases the execution agent that picked up the parallel goal */
//bool_t apll_release_remote(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    if (h->agent == w) {
//      worker_t *ag = h->remote_agent;
//      if ((ag != NULL) && (ag != w) && (Suspended_Waiting_For_Work_Of(ag))) {
//	Wait_Acquire_lock(Waiting_For_Work_Lock_Of(ag));
//	Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(ag));
//	Release_lock(Waiting_For_Work_Lock_Of(ag));
//      }
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
///* Releases some suspended agent */
//bool_t apll_release_some_suspended_thread(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  bool_t released_agent = FALSE;
//  worker_t *aux = Next_Wam_Of(w);
//
//  // Release a suspended agent, if there is one
//  do {
//    Wait_Acquire_slock(Mutex_Lock_Of(aux));
//    if (Suspended_Waiting_For_Work_Of(aux)) {
//      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
//      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
//      Release_lock(Waiting_For_Work_Lock_Of(aux));
//      released_agent = TRUE;
//    }
//    Release_slock(Mutex_Lock_Of(aux));
//    aux = Next_Wam_Of(aux);
//  }
//  while (!released_agent && (aux != w));
//#endif
//
//  return TRUE;
//}
//
//
///* Releases the execution of the agents to perform stack unwinding */
//bool_t apll_release_all_for_unwinding(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  if ((main_worker == w) && (!unwinding_done)) {
//    worker_t *aux = w;
//    for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux))
//      Mode_Of(aux) = UNWIND;
//    for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux)) {
//      if (Suspended_Waiting_For_Work_Of(aux)) {
//	Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
//	Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
//	Release_lock(Waiting_For_Work_Lock_Of(aux));
//      }
//    }
//    unwinding_done = TRUE;
//  }
//#endif
//
//  return TRUE;
//}
//
//
///* Attemps to enter into the mutual exclusion */
//bool_t apll_enter_mutex(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL && h->exec_state >= 0 && h->exec_state < NUMBER_STATES) {
//    if ((h->agent != NULL) && (h->agent != w)) {
//      Safe_To_Cancel = FALSE;
//      Wait_Acquire_slock(Mutex_Lock_Of(h->agent));
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
///* Attemps to enter into the local mutual exclusion */
//bool_t apll_enter_mutex_self(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  Safe_To_Cancel = FALSE;
//  Wait_Acquire_slock(Mutex_Lock);
//#endif
//
//  return TRUE;
//}
//
//
///* Attemps to enter into the remote mutual exclusion */
//bool_t apll_enter_mutex_remote(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    if ((h->remote_agent != NULL) && (h->remote_agent != w)) {
//      Safe_To_Cancel = FALSE;
//      Wait_Acquire_slock(Mutex_Lock_Of(h->remote_agent));
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
///* Exits from the mutual exclusion */
//bool_t apll_exit_mutex(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL && h->exec_state >= 0 && h->exec_state < NUMBER_STATES) {
//    if ((h->agent != NULL) && (h->agent != w)) {
//      Release_slock(Mutex_Lock_Of(h->agent));
//      Safe_To_Cancel = TRUE;
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
///* Exits from the local mutual exclusion */
//bool_t apll_exit_mutex_self(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  Release_slock(Mutex_Lock);
//  Safe_To_Cancel = TRUE;
//#endif
//
//  return TRUE;
//}
//
//
///* Exits from the remote mutual exclusion */
//bool_t apll_exit_mutex_remote(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    if ((h->remote_agent != NULL) && (h->remote_agent != w)) {
//      Release_slock(Mutex_Lock_Of(h->remote_agent));
//      Safe_To_Cancel = TRUE;
//    }
//  }
//#endif
//
//  return TRUE;
//}
//
//
//#if defined(ANDPARALLEL)
//void clean_stats(l,p)
//     SLOCK l;
//     int_par_t *(*p);
//{
//  int_par_t *aux;
//
//  Wait_Acquire_slock(l);
//  while ((*p) != NULL) {
//    aux = (*p);
//    (*p) = (*p)->prev;
//    if ((*p) != NULL)
//      (*p)->next = NULL;
//    checkdealloc((tagged_t *)aux,sizeof(int_par_t));
//  }
//  Release_slock(l);
//}
//#endif
//
//
///* Resets the value of statistical measures for nondet parallel
//   programs */
//bool_t apll_clean_measures(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  npargoals = 0;
//  clean_stats(npargoalstaken_l,&npargoalstaken);
//  clean_stats(nlocalbacktr_l,&nlocalbacktr);
//  clean_stats(nrembacktr_top_l,&nrembacktr_top);
//  clean_stats(nrembacktr_trapped_l,&nrembacktr_trapped);
//
//#if defined(Solaris)
//  double_par_t *aux;
//  while (Total_Suspending_Time != NULL) {
//    aux = Total_Suspending_Time;
//    Total_Suspending_Time = Total_Suspending_Time->prev;
//    if (Total_Suspending_Time != NULL)
//      Total_Suspending_Time->next = NULL;
//    checkdealloc((tagged_t *)aux,sizeof(double_par_t));
//  }
//  Suspending_Time_Cont = gethrtime();
//  worker_t *aux_w = Next_Wam_Of(w);
//  while (aux_w != w) {
//    while (Total_Suspending_Time_Of(aux_w) != NULL) {
//      aux = Total_Suspending_Time_Of(aux_w);
//      Total_Suspending_Time_Of(aux_w) = Total_Suspending_Time_Of(aux_w)->prev;
//      if (Total_Suspending_Time != NULL)
//  	Total_Suspending_Time_Of(aux_w)->next = NULL;
//      checkdealloc((tagged_t *)aux,sizeof(double_par_t));
//    }
//    Suspending_Time_Cont_Of(aux_w) = gethrtime();
//    aux_w = Next_Wam_Of(aux_w);
//  }
//#endif
//#endif
//
//  return TRUE;
//}
//
//
//#if defined(ANDPARALLEL)
//void print_stats(l,p,average,sigma)
//     SLOCK l;
//     int_par_t *p;
//     float (*average);
//     float (*sigma);
//{
//  int_par_t *aux;
//  int n=0;
//
//  Wait_Acquire_slock(l);
//  aux = p;
//  (*average) = 0;
//  n = 0;
//  while (aux != NULL) {
//    n++;
//    (*average)+=aux->value;
//    aux = aux->prev;
//  }
//  if (n>0) (*average) = (*average)/n; else (*average) = 0;
//  aux = p;
//  (*sigma) = 0;
//  n = 0;
//  while (aux != NULL) {
//    n++;
//    (*sigma)+=(aux->value-(*average))*(aux->value-(*average));
//    aux = aux->prev;
//  }
//  if (n>0) (*sigma) = sqrt((*sigma)/n); else (*sigma) = 0;
//  Release_slock(l);
//}
//#endif
//
//
///* Prints the value of statistical measures for nondet parallel
//   programs */
//bool_t apll_print_measures(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  float average = 0;
//  float sigma = 0;
//
//  // Parallel goals taken
//  print_stats(npargoalstaken_l,npargoalstaken,&average,&sigma);
//  printf("Parallel goals taken by other agents:\n");
//  printf("   Average: %lf\n",average);
//  printf("   sigma:   %lf\n",sigma);
//
//  // Times of local backtracking performed
//  print_stats(nlocalbacktr_l,nlocalbacktr,&average,&sigma);
//  printf("Parallel goals backtracked locally:\n");
//  printf("   Average: %lf\n",average);
//  printf("   sigma:   %lf\n",sigma);
//
//  // Times of remote backtracking performed on top of stack
//  print_stats(nrembacktr_top_l,nrembacktr_top,&average,&sigma);
//  printf("Parallel goals backtracked remotely on top of stack:\n");
//  printf("   Average: %lf\n",average);
//  printf("   sigma:   %lf\n",sigma);
//
//  // Times of remote backtracking over trapped goals
//  print_stats(nrembacktr_trapped_l,nrembacktr_trapped,&average,&sigma);
//  printf("Parallel goals backtracked remotely and trapped:\n");
//  printf("   Average: %lf\n",average);
//  printf("   sigma:   %lf\n",sigma);
//
//#if defined(Solaris)
//  // Suspending time of each agent
//  double_par_t *aux = Total_Suspending_Time;
//  int n = 0;
//  double av_time = 0.0, av_n = 0.0;
//  while (aux != NULL) {
//    n++;
//    av_time+=aux->time;
//    av_n+=aux->n;
//    aux = aux->prev;
//  }
//  if (n>0) {
//    av_time = av_time/n;
//    av_n = av_n/n;
//  }
//  else {
//    av_time = 0.0;
//    av_n = 0.0;
//  }
//  printf("Suspending time of WAM %x: %lf\n",(int)w,av_time);
//  printf("   Number of times suspended: %lf\n",av_n);
//  worker_t *aux_w = Next_Wam_Of(w);
//  while (aux_w != w) {
//    aux = Total_Suspending_Time_Of(aux_w);
//    n = 0;
//    av_time = 0.0;
//    av_n = 0.0;
//    while (aux != NULL) {
//      n++;
//      av_time+=aux->time;
//      av_n+=aux->n;
//      aux = aux->prev;
//    }
//    if (n>0) {
//      av_time = av_time/n;
//      av_n = av_n/n;
//    }
//    else {
//      av_time = 0.0;
//      av_n = 0.0;
//    }
//    printf("Suspending time of WAM %x: %lf\n",(int)aux_w,av_time);
//    printf("   Number of times suspended: %lf\n",av_n);
//    aux_w = Next_Wam_Of(aux_w);
//  }
//#endif
//#endif
//  
//  return TRUE;
//}
//
//
//#if defined(ANDPARALLEL)
//void new_stats(l,p)
//     SLOCK l;
//     int_par_t *(*p);
//{
//  int_par_t *aux;
//
//  Wait_Acquire_slock(l);
//  aux = (int_par_t *)checkalloc(sizeof(int_par_t));
//  aux->prev = (*p);
//  aux->next = NULL;
//  aux->value = 0;
//  if ((*p) != NULL)
//    (*p)->next = aux;
//  (*p) = aux;
//  aux = NULL;
//  Release_slock(l);
//}
//#endif
//
//
///* Prepares the statistical values for a new execution */
//bool_t apll_new_measure(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  measure = TRUE;
//
//  npargoals = 0;
//  new_stats(npargoalstaken_l,&npargoalstaken);
//  new_stats(nlocalbacktr_l,&nlocalbacktr);
//  new_stats(nrembacktr_top_l,&nrembacktr_top);
//  new_stats(nrembacktr_trapped_l,&nrembacktr_trapped);
//
//#if defined(Solaris)
//  worker_t *aux_w = Next_Wam_Of(w);
//  double_par_t *aux =
//    (double_par_t *)checkalloc(sizeof(double_par_t));
//  Suspending_Time_Cont = gethrtime();
//  aux->prev = Total_Suspending_Time;
//  aux->next = NULL;
//  aux->time = 0.0;
//  aux->n = 0;
//  if (Total_Suspending_Time != NULL)
//    Total_Suspending_Time->next = aux;
//  Total_Suspending_Time = aux;
//  while (aux_w != w) {
//    Suspending_Time_Cont_Of(aux_w) = gethrtime();
//    aux = (double_par_t *)checkalloc(sizeof(double_par_t));
//    aux->prev = Total_Suspending_Time_Of(aux_w);
//    aux->next = NULL;
//    aux->time = 0.0;
//    aux->n = 0;
//    if (Total_Suspending_Time_Of(aux_w) != NULL)
//      Total_Suspending_Time_Of(aux_w)->next = aux;
//    Total_Suspending_Time_Of(aux_w) = aux;
//    aux_w = Next_Wam_Of(aux_w);
//  }
//#endif
//#endif
//
//  return TRUE;
//}
//
//
///* Increments the number of times that local backtracking has been
//   performed over nondeterministic parallel goals */
//bool_t apll_incr_num_local_backtr(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  Wait_Acquire_slock(nlocalbacktr_l);
//  if (nlocalbacktr != NULL && measure) (nlocalbacktr->value)++;
//  Release_slock(nlocalbacktr_l);
//#endif
//
//  return TRUE;
//}
//
//
///* Avoids measuring */
//bool_t apll_not_measure(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  measure = FALSE;
//#endif
//
//  return TRUE;
//}
//
//
///* ***************************************************************** */
//
//
///* Initialization procedure */
//bool_t init(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  fifo             = init_atom_check("fifo");
//  lifo             = init_atom_check("lifo");
//  det              = init_atom_check("det");
//#endif
//
//  return TRUE;
//}
