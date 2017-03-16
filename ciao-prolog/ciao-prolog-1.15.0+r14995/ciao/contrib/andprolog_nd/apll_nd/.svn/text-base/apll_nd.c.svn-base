#include "apll_nd.h"
#include <tabling.h>

/* Checks whether a particular variable is a callable structure or not */
#define NOT_CALLABLE(What) IsVar(What) || TagIsSmall(What) || TagIsLarge(What)

/*****************************************************************/
/*            ANDPROLOG LOW-LEVEL SUPPORT PRIMITIVES             */
/*****************************************************************/


/* Starts a new thread in the system */
bool_t apll_nd_start_thread(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  goal_descriptor_t *gd = NULL;

  // Make sure we are calling a callable term!
  DEREF(X(0),X(0));
  if (NOT_CALLABLE(X(0))) return FALSE;

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
bool_t apll_nd_number_agents(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Wait_Acquire_slock(nagents_l);
  cunify(Arg, X(0), MakeInteger(w, (int)nagents));
  Release_slock(nagents_l);
#endif

  return TRUE;
}


#if defined(ANDPARALLEL)
/* Reads a handler */
par_handler_t *read_handler(hdler)
     tagged_t hdler;
{
  CIAO_REGISTER tagged_t x1 = (tagged_t)NULL;

  // Obtain the handler struct
  DEREF(hdler,hdler);
  DerefSwitch(hdler,x1,;);

  DerefArg(x1,hdler,1);
  return (par_handler_t *)TermToPointer(x1);
}


/* Adds handler to the goal list */
void add_handler_to_goal_list(Arg, gle)
     Argdecl;
     handler_entry_t *(*gle);
{
  if (Goal_List_Top_Of(Arg) != NULL) 
    {
      (*gle)->prev = Goal_List_Top_Of(Arg);
      Goal_List_Top_Of(Arg)->next = (*gle);
      Goal_List_Start_Of(Arg)->prev = (*gle);
      Goal_List_Top_Of(Arg) = (*gle);
    }
  else 
    {
      Goal_List_Start_Of(Arg) = (*gle);
      Goal_List_Top_Of(Arg) = (*gle);
      (*gle)->prev = Goal_List_Top_Of(Arg);
    }
  (*gle)->next = Goal_List_Start_Of(Arg);
}


/* Removes handler from the goal list */
void remove_handler_from_goal_list(Arg, gle, free)
     Argdecl;
     handler_entry_t *(*gle);
     bool_t free;
{
  if ((*gle) != NULL) 
    {
      if (Goal_List_Start_Of(Arg) == Goal_List_Top_Of(Arg)) 
	{
	  Goal_List_Start_Of(Arg) = NULL;
	  Goal_List_Top_Of(Arg) = NULL;
	}
      else 
	{
	  ((*gle)->next)->prev = (*gle)->prev;
	  ((*gle)->prev)->next = (*gle)->next;
	  if (*gle == Goal_List_Top_Of(Arg))
	    Goal_List_Top_Of(Arg) = (*gle)->prev;
	  if (*gle == Goal_List_Start_Of(Arg))
	    Goal_List_Start_Of(Arg) = (*gle)->next;
	}
      if (free) 
	{
	  checkdealloc((tagged_t *)(*gle),sizeof(handler_entry_t));
	  (*gle) = NULL;
	}
    }
}


/* Adds handler to the parallel executions list */
void add_handler_to_parallel_exec_list(Arg, limits)
     Argdecl;
     parallel_exec_entry_t *limits;
{
  if (Last_Parallel_Exec != NULL) 
    {
      limits->prev = Last_Parallel_Exec;
    }
  else limits->prev = NULL;

  Last_Parallel_Exec = limits;
}


///* Removes handler from the choice point list */
//void remove_handler_from_parallel_exec_list(Arg, limits, free)
//     Argdecl;
//     parallel_exec_entry_t *limits;
//     bool_t free;
//{
//  if (limits == NULL) return; 
//
//  if (Last_Parallel_Exec->prev == NULL)
//    {
//      Last_Parallel_Exec = NULL;
//    }
//  else 
//    {
//      if (limits->prev == NULL)
//	{
//	  (limits->next)->prev = NULL;
//	}
//      else if (limits->next == NULL) 
//	{
//	  Last_Parallel_Exec = Last_Parallel_Exec->prev;
//	  Last_Parallel_Exec->next = NULL;
//	}
//      else 
//	{
//	  (limits->next)->prev = limits->prev;
//	  (limits->prev)->next = limits->next;
//	}
//    }
//  if (free) checkdealloc((tagged_t *)limits, sizeof(parallel_exec_entry_t));
//}

///* Moves the handler to the top of the parallel executions list */
//void move_handler_to_top_of_parallel_exec_list(Arg, limits, init)
//     Argdecl;
//     parallel_exec_entry_t *(*limits);
//     node_t *init;
//{
//  if ((*limits) != NULL) {
//    parallel_exec_entry_t *next = (*limits);
//    parallel_exec_entry_t *last = (*limits);
//    int incr = ChoiceCharDifference((*limits)->init,init);
//
//    // Pointing to the last parallel_exec_entry which needs to be moved
//    while ((last->next != NULL) &&
//           (!ChoiceYounger((last->next)->end,(*limits)->end))) {
//      last = last->next;
//    }
//
//    while (next != NULL) {
//      // Remove handler from the parallel executions list
//      remove_handler_from_parallel_exec_list(w,next,FALSE);
//
//      // Update pointers
//      next->init = ChoiceCharOffset(next->init,incr);
//      next->end = ChoiceCharOffset(next->end,incr);
//
//      // Add the handler to the top of the parallel executions list
//      add_handler_to_parallel_exec_list(w,next);
//
//      // Next one or end it
//      if (next == last)
//	break;
//      else
//	next = next->next;
//    }
//  }
//}


void send_event(w,tagged_h,canc)
     Argdecl;
     tagged_t tagged_h;
     bool_t canc;
{
  par_handler_t *h = read_handler(tagged_h);

  worker_t *remote_a = (worker_t *)(h->remote_agent);
#if defined(DEBUG)
    if (debug_threads) {
      printf("send_event("); prolog_display(Arg);
      printf(",%d) from %p to %p\n",canc,w,remote_a);
    }
#endif

    // Create a new entry for the event queue
    event_entry_t *new_eqe =
      (event_entry_t *)checkalloc(sizeof(event_entry_t));
    new_eqe->handler = tagged_h;
    new_eqe->canc = canc;

    // Add entry to the remote event queue
    Wait_Acquire_slock(Event_Queue_Lock_Of(remote_a));
    if (Event_Queue_Top_Of(remote_a) != NULL) 
      {
	new_eqe->prev = Event_Queue_Top_Of(remote_a);
	Event_Queue_Top_Of(remote_a)->next = new_eqe;
	Event_Queue_Start_Of(remote_a)->prev = new_eqe;
	Event_Queue_Top_Of(remote_a) = new_eqe;
      }
    else 
      {
	Event_Queue_Start_Of(remote_a) = new_eqe;
	Event_Queue_Top_Of(remote_a) = new_eqe;
	new_eqe->prev = Event_Queue_Top_Of(remote_a);
      }
    new_eqe->next = Event_Queue_Start_Of(remote_a);
    Release_slock(Event_Queue_Lock_Of(remote_a));
}
#endif


/* Pushes the handler associated to a goal on to the goal list */
bool_t apll_nd_push_goal(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = NULL;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  if (NOT_CALLABLE(X(0))) return FALSE;

  // Allocating memory for the handler
  h = (par_handler_t *)checkalloc(sizeof(par_handler_t));

  // Create the structure in the heap
  CIAO_REGISTER tagged_t *pt1 = w->global_top;
  HeapPush(pt1,functor_Dhandler);
  HeapPush(pt1,PointerToTerm((int)h));
  w->global_top = pt1;
  CTagToPointer(X(2)) = Tag(STR, HeapOffset(pt1,-2));

  //For taking care of dependences
#if defined(DEP_TRAPPED)
  Dep_Size++;
  Dep_Id = (int*) realloc (Dep_Id, Dep_Size * sizeof(int));
  Dep_Id[Dep_Size-1] = 0;
  //Updating handler dependence number
  h->dep_size = Dep_Size;
  h->dep_id = (int*) malloc (Dep_Size*sizeof(int));
  int i;
  for (i = 0; i < Dep_Size; i++)
    h->dep_id[i] = Dep_Id[i];
  h->dep_id[Dep_Size-1] = 1;
#endif

  // Initialize handler and add it to the goal list
  INIT_HANDLER(w,&h,X(0),X(1),X(2));
  Wait_Acquire_slock(Goal_List_Lock);
  add_handler_to_goal_list(w,&(h->gle));  
  Release_slock(Goal_List_Lock);

#if defined(DEBUG)
  if (debug_threads) {
    tagged_t aux_x0 = X(0); X(0) = X(2);
    printf("Goal pushed: "); prolog_display(Arg); printf(" by %p\n",w);
    X(0) = aux_x0;
  }
#endif
#endif

//  Wait_Acquire_slock(stats_l);
//  total_goals++;
//  Release_slock(stats_l);

  return TRUE;
}

#if defined(DEP_TRAPPED)
//Checks if dependences indicate that h will trap other parallel goals
int imply_trapped(Arg, h)
     Argdecl;
     par_handler_t *h;
{
  int i, minSize;
  if (Dep_Size < h->dep_size) minSize = Dep_Size;
  else minSize = h->dep_size;

  for (i = 0; i < minSize; i++)
    {
      if (Dep_Id[i] < h->dep_id[i]) return TRUE;
      if (Dep_Id[i] > h->dep_id[i]) return FALSE;
    }
  return (minSize == h->dep_size);
}
#endif

/* Finds a handler associated to a goal from the goal list of some agent */
bool_t apll_nd_find_goal(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  worker_t *aux;
  handler_entry_t *gle = NULL;

  DEREF(X(0),X(0));

// if (Goal_Cache != NULL)
//   {
//     aux = Goal_Cache->agent;
//     Wait_Acquire_slock(Goal_List_Lock_Of(aux));
//     if (Goal_List_Top_Of(aux) != NULL && Goal_List_Start_Of(aux) != NULL)
//	{
//	  // Pick up the handler
//	  if (X(0) == fifo)  // FIFO scheduling
//	    gle = Goal_List_Start_Of(aux);
//	  else if (X(0) == lifo)  // LIFO scheduling
//	    gle = Goal_List_Top_Of(aux);
//	  else
//	    MAJOR_FAULT("Goal scheduling error in apll_nd_find_goal.");
//     
//	  par_handler_t *h = read_handler(gle->handler);
//	  if (h != NULL) 
//	    {
//	      if ((X(1) != det) || ((X(1) == det) && (h->det))) 
//		{
//		  // Give value to remote_agent in the handler
//		  h->remote_agent = w;
//		  
//		  // Return a pointer to the remote goal in X(2)
//		  cunify(Arg, X(2), gle->handler);
//		  
//		  // Remove handler from goal list in remote agent
//		  remove_handler_from_goal_list(aux,&(h->gle),TRUE);
//		  h->exec_state = REM_EXECUTING;
//		  h->gle = NULL;
//		  Release_slock(Goal_List_Lock_Of(aux));
//		  
//		  // Exit
//		  Goal_Cache = NULL;
//		  return TRUE;
//		}
//	    }
//	}
//     Release_slock(Goal_List_Lock_Of(aux));
//   }

//  aux = Next_Wam_Of(w);
  aux = main_worker;
  Goal_Cache = NULL;

  do 
    {
      Wait_Acquire_slock(Goal_List_Lock_Of(aux));
#if defined(DEP_TRAPPED)
      if (Goal_List_Start_Of(aux) != NULL &&
	  !imply_trapped(w,read_handler(Goal_List_Start_Of(aux)->handler)))
#else
	if (Goal_List_Start_Of(aux) != NULL)
#endif
	  {
	    gle = Goal_List_Start_Of(aux);
	    par_handler_t *h = read_handler(gle->handler);

	    // Give value to remote_agent in the handler
	    h->remote_agent = w;
	    // Return a pointer to the remote goal in X(2)
	    tagged_t h_tagged_t= gle->handler;
	    // Remove handler from goal list in remote agent
	    remove_handler_from_goal_list(aux,&(h->gle),TRUE);
	    h->exec_state = REM_EXECUTING;
	    h->gle = NULL;
	    Release_slock(Goal_List_Lock_Of(aux));

#if defined(DEP_TRAPPED)
	    Dep_Size = h->dep_size;
	    Dep_Id = h->dep_id;
#endif		      
	    return cunify(Arg, X(1), h_tagged_t);
	  }
      Release_slock(Goal_List_Lock_Of(aux));
      aux = Next_Wam_Of(aux);
    }
  while (aux != main_worker);
  //      while (aux != w);        
  
  return FALSE;
#else
  return TRUE;
#endif
}


/* Succeeds if the handler has not been picked up yet by another agent
   and fails otherwise */
bool_t apll_nd_goal_available(Arg)
     Argdecl;
{
  bool_t result = TRUE;
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  // Check whether the goal has been taken by another agent or not
  Wait_Acquire_slock(Goal_List_Lock);
  if ((h->gle == NULL) && (h->remote_agent != w)) result = FALSE;
  else 
    {
      h->remote_agent = w;
      h->exec_state = LOCAL_EXEC;
      remove_handler_from_goal_list(w,&(h->gle),TRUE);
      h->gle = NULL;
    }
    Release_slock(Goal_List_Lock);
#endif

  return result;
}

/* Frees the memory used by the handler */
bool_t apll_nd_reclaim_handler(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  Wait_Acquire_slock(Goal_List_Lock);
  if (h->gle != NULL) 
    {  // Handler still in goal list
      remove_handler_from_goal_list(w,&(h->gle),TRUE);
      h->gle = NULL;
    }
  Release_slock(Goal_List_Lock);
  
  //      if (h->exec_state == REM_EXECUTING) 
  //	printf("\nwarning %d\n",(int)h->exec_state);
  
  // Free memory used for handler
  checkdealloc((tagged_t *)h,sizeof(par_handler_t));
#endif

  return TRUE;
}


/* Frees the memory used by the handler */
//bool_t apll_nd_cancellation(Arg)
//     Argdecl;
//{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    Wait_Acquire_slock(Goal_List_Lock);
//    if (h->gle != NULL) {  // Handler still in goal list
//      remove_handler_from_goal_list(w,&(h->gle),TRUE);
//      h->gle = NULL;
//      Release_slock(Goal_List_Lock);
//    }
//    else {
//      if (!h->det) {
//	if ((h->remote_agent != NULL) && (h->remote_agent != w) &&
//            (h->agent == w)) {
//	  Release_slock(Goal_List_Lock);
//	  Wait_Acquire_slock(Mutex_Lock);
//	  if (h->exec_state == NOTEXECUTED) {  // done
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == REM_EXECUTING) {  // done
//	    Cancel_Goal_Exec_Of(h->remote_agent) = TRUE;
//	    h->exec_state = CANCELLED;
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == FINISHED) {  // done
//	    worker_t *rem_a = h->remote_agent;
//	    Release_slock(Mutex_Lock);
//	    Wait_Acquire_slock(Mutex_Lock_Of(rem_a));
//	    h->exec_state = CANCELLED;
//	    send_event(w,X(0),TRUE);
//	    if (Suspended_Waiting_For_Work_Of(rem_a)) {
//	      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(rem_a));
//	      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(rem_a));
//	      Release_lock(Waiting_For_Work_Lock_Of(rem_a));
//	    }
//	    Release_slock(Mutex_Lock_Of(rem_a));
//	  }
//	  else if (h->exec_state == FAILED) {  // done
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == PENDING_SOLS) {  // done
//	    Cancel_Goal_Exec_Of(h->remote_agent) = TRUE;
//	    h->exec_state = CANCELLED;
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == REEXECUTE) {  // done
//	    Release_slock(Mutex_Lock);
//	  }
//	}
//	else
//	  Release_slock(Goal_List_Lock);
//      }
//      else
//	Release_slock(Goal_List_Lock);
//    }
//
//    // Free memory used for handler
//    checkdealloc((tagged_t *)h,sizeof(par_handler_t));
//
//#if defined(DEBUG)
//    if (debug_threads) {
//      printf("Cancellation done: "); prolog_display(Arg); printf(" by %x\n",w);
//    }
//#endif
//  }
//#endif
//
//  return TRUE;
//}


/* Returns the goal associated to the handler */
bool_t apll_nd_retrieve_goal(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

#if defined(DEP_TRAPPED)
  Dep_Size = h->dep_size;
  Dep_Id = h->dep_id;
#endif

  return cunify(Arg, X(1), h->goal);
#endif

  return TRUE;
}


/* Returns whether the goal is deterministic or not */
bool_t apll_nd_goal_det(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  return read_handler(X(0))->det;
#endif

  return TRUE;
}


/* Sets the goal to deterministic */
bool_t apll_nd_set_goal_det(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  read_handler(X(0))->det = TRUE;
#endif

  return TRUE;
}


/* Sets the goal to non-deterministic */
bool_t apll_nd_set_goal_nondet(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  read_handler(X(0))->det = FALSE;
#endif

  return TRUE;
}


/* Returns whether the goal has not been executed yet */
bool_t apll_nd_goal_not_executed(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
    return (read_handler(X(0))->exec_state == NOTEXECUTED);
#endif

  return TRUE;
}


/* Sets the goal execution to never executed */
bool_t apll_nd_set_goal_not_executed(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  read_handler(X(0))->exec_state = NOTEXECUTED;
#endif

  return TRUE;
}


/* Returns whether the goal is remotely executing */
bool_t apll_nd_goal_rem_executing(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  return (read_handler(X(0))->exec_state == REM_EXECUTING);
#endif

  return TRUE;
}


/* Sets the goal execution to being remotely executing */
bool_t apll_nd_set_goal_rem_executing(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  read_handler(X(0))->exec_state = REM_EXECUTING;
#endif

  return TRUE;
}


/* Returns whether the goal execution has finished or not */
bool_t apll_nd_goal_finished(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  return (read_handler(X(0))->exec_state == FINISHED);
#endif

  return TRUE;
}


/* Sets the goal execution to finished */
bool_t apll_nd_set_goal_finished(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  read_handler(X(0))->exec_state = FINISHED;
#endif

  return TRUE;
}


/* Returns whether the goal execution has to backtrack or not */
bool_t apll_nd_goal_tobacktrack(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  return (read_handler(X(0))->exec_state == PENDING_SOLS);
#endif

  return TRUE;
}


/* Sets the goal execution to be backtracked */
bool_t apll_nd_set_goal_tobacktrack(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  read_handler(X(0))->exec_state = PENDING_SOLS;
#endif

  return TRUE;
}


/* Returns whether the goal execution has to be reexecuted or not */
bool_t apll_nd_goal_toreexecute(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  return (read_handler(X(0))->exec_state == REEXECUTE);
#endif

  return TRUE;
}


/* Sets the goal execution to be reexecuted */
bool_t apll_nd_set_goal_toreexecute(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  read_handler(X(0))->exec_state = REEXECUTE;
#endif

  return TRUE;
}


/* Returns whether the goal execution has failed or not */
bool_t apll_nd_goal_failed(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  return (read_handler(X(0))->exec_state == FAILED);
#endif

  return TRUE;
}


/* Sets the goal execution to finished */
bool_t apll_nd_set_goal_failed(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  h->exec_state = FAILED;
  
//  remove_handler_from_parallel_exec_list(w, h->exec_limits, TRUE);
  parallel_exec_entry_t *aux_tel = Last_Parallel_Exec->prev;
  checkdealloc((tagged_t *)Last_Parallel_Exec, sizeof(parallel_exec_entry_t));

  Last_Parallel_Exec = aux_tel;
  h->exec_limits = NULL;
#endif

  return TRUE;
}


/* Returns whether the goal execution has been cancelled or not */
bool_t apll_nd_goal_cancelled(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  return (read_handler(X(0))->exec_state == CANCELLED);
#endif

  return TRUE;
}


/* Sets the goal execution to cancelled */
bool_t apll_nd_set_goal_cancelled(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  read_handler(X(0))->exec_state = CANCELLED ;
#endif

  return TRUE;
}

/* Gets dependence info */
bool_t apll_nd_get_dep_info(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
#if defined(DEP_TRAPPED)
  if (!cunify(Arg, X(0), MakeInteger(w, Dep_Size))) return FALSE;
  return cunify(Arg, X(1), MakeInteger(w, (int)Dep_Id));
#else
  if (!cunify(Arg, X(0), MakeInteger(w, 1))) return FALSE;
  return cunify(Arg, X(1), MakeInteger(w, 1));  
#endif
#endif

  return TRUE;
}

/* Sets dependence info */
bool_t apll_nd_set_dep_info(Arg)
     Argdecl;
{
#if defined(DEP_TRAPPED)
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  Dep_Size = GetInteger(X(0));
  Dep_Id = (int*)GetInteger(X(1));
#endif

  return TRUE;
}


/* Sends the goal to the agent that picked it up in order to perform
   backtracking over it */
bool_t apll_nd_send_event(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  send_event(w,X(0),FALSE);
#endif

  return TRUE;
}


/* Reads an event from the event queue and fails if it is empty */
bool_t apll_nd_read_event(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Wait_Acquire_slock(Event_Queue_Lock);
  if (Event_Queue_Start == NULL) 
    {
      Release_slock(Event_Queue_Lock);
      return FALSE;
    }

  if (Event_Queue_Start->canc) 
    {
      printf("\nNo cancelation for the time being!!!\n");
//      Wait_Acquire_slock(Parallel_Exec_Lock);
//      if (Event_Queue_Start->limits != NULL) 
//	{
//	  if (Last_Parallel_Exec->end != (Event_Queue_Start->limits)->end) 
//	    {
//	      remove_handler_from_parallel_exec_list(w,
//						     Event_Queue_Start->limits,
//						     TRUE);
//	      Event_Queue_Start->limits = NULL;
//	      Release_slock(Parallel_Exec_Lock);
//	      ret = FALSE;
//	    }
//	  else 
//	    {
//	      w->node = (Event_Queue_Start->limits)->init;
//	      if (w->node != NULL)
//		SetShadowregs(w->node);
//	      Release_slock(Parallel_Exec_Lock);
//	      ret = TRUE;
//	    }
//	}
    }
    else CTagToPointer(X(0)) = Event_Queue_Start->handler;

  // Remove entry from the event queue
  event_entry_t *eqe = NULL;
 
  if (Event_Queue_Start == Event_Queue_Top) 
    {
      eqe = Event_Queue_Start;
      Event_Queue_Start = NULL;
      Event_Queue_Top = NULL;
    }
  else 
    {
      (Event_Queue_Start->next)->prev = Event_Queue_Start->prev;
      (Event_Queue_Start->prev)->next = Event_Queue_Start->next;
      eqe = Event_Queue_Start;
      Event_Queue_Start = Event_Queue_Start->next;
    }
  Release_slock(Event_Queue_Lock);
  checkdealloc((tagged_t *)eqe, sizeof(event_entry_t));
#endif

  return TRUE;
}


/* Saves a pointer to the initial choice point of the goal execution
   in the handler */
bool_t apll_nd_save_init_execution(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  (h->exec_limits) = (parallel_exec_entry_t *)
    checkalloc(sizeof(parallel_exec_entry_t));
  add_handler_to_parallel_exec_list(w,h->exec_limits);
  (h->exec_limits)->init = w->node;
  (h->exec_limits)->end = w->node;
#endif

  return TRUE;
}


/* Saves a pointer to the final choice point of the goal execution in
   the handler */
bool_t apll_nd_save_end_execution(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  if (Last_Parallel_Exec != NULL)
    Last_Parallel_Exec->end = w->node;
#endif

  return TRUE;
}

///* Moves the choice point of the parallel goal to the top of the
//   stack */
bool_t apll_nd_move_execution_top(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  CIAO_REGISTER node_t *node;
  CIAO_REGISTER par_handler_t *h = read_handler(X(0));
  tagged_t *aux_stack;
  int size_aux_stack;
  tagged_t *itrail;
  node_t *node_init = H_INIT_CP(h);
  node_t *node_end = H_END_CP(h);
  tagged_t *global_top_init = node_init->global_top;
  frame_t *local_top_init = node_init->local_top;
  parallel_exec_entry_t *l_exec_limits;
  parallel_exec_entry_t *last_tel;
  parallel_exec_entry_t *aux_tel;
  node_t *node_frontier;
  
  // Move choice points of current parallel goal execution to top of stack
#if defined(DEBUG_TRAPPED)
  printf("\nMoving exec top %p\n",w->node);
  printf("\ntrail node %p y trail %p\n",w->node->trail_top,w->trail_top);
#endif
  
  //  frame_t *tmp_local_top = NULL;
  //  ComputeA(tmp_local_top,w->node);
  //  Top most choice_pt is from the scheduler -> we take it out
  w->node = PREV_CP(w->node);
  ComputeA(w->local_top,w->node);
  
  //  if (tmp_local_top != w->local_top)
  //    printf("\nLocal Top value has to be analized\n");
  
#if defined(DEBUG_TRAPPED)
  printf("\nMoving exec top %p\n",w->node); fflush(stdout);
  printf("\ntrail node %p y trail %p\n",w->node->trail_top,w->trail_top);
#endif

  if (ChoiceYounger(w->node, H_END_CP(h))) 
    {
#if defined(DEP_TRAPPED)
      printf("\nTrapped goal --> BUG!!!\n");
#endif
      struct timeval t_ini, t_fin, t_fin_fake;
      gettimeofday(&t_ini,NULL);

#if defined(DEBUG_TRAPPED)
      printf("\nPRE\n");
      printf("\nMoving trapped goal node %p - ini %p - end %p\n",
	     w->node,H_INIT_CP(h),H_END_CP(h)); 	  
      for(node = w->node; !ChoiceYounger(H_INIT_CP(h),node); 
	  node = PREV_CP(node))
	{
	  printf("\nNode %p=%p trail value %p=%p\n",
		 node,node->next_alt,node->trail_top,
		 (void*)CTagToPointer(node->trail_top));
	  printf("\nProtects until %p %p\n",node->global_top,node->local_top);
	}
      tagged_t *trail_init = H_INIT_CP(h)->trail_top;	  
      //for(itrail = w->trail_top; !TrailYounger(trail_init,itrail); itrail--)
      //printf("\nitrail %p=%p\n",itrail,(void*)CTagToPointer(itrail));
      printf("\nTRAPPED %p - %p\n", node_init,node_end);
      for (l_exec_limits = Last_Parallel_Exec; l_exec_limits != NULL; 
	   l_exec_limits = l_exec_limits->prev)
	{
	  printf("\nG__EXEC %p - %p\n",l_exec_limits->init,l_exec_limits->end);
	  printf("\n    Node %p=%p trail value %p=%p\n",
		 l_exec_limits->init,l_exec_limits->init->next_alt,
		 l_exec_limits->init->trail_top,
		 (void*)CTagToPointer(l_exec_limits->init->trail_top));
	  printf("\n    Node %p=%p trail value %p=%p\n",l_exec_limits->end,
		 l_exec_limits->end->next_alt, l_exec_limits->end->trail_top,
		 (void*)CTagToPointer(l_exec_limits->end->trail_top));
	}
#endif

//      for (l_exec_limits = Last_Parallel_Exec; l_exec_limits != NULL; 
//	   l_exec_limits = l_exec_limits->prev)
//	{
//	  printf("\nG__EXEC %p = %p - %p\n",l_exec_limits,
//		 l_exec_limits->init,l_exec_limits->end);
//	}

      // STEP ZERO: Calculate sizes of auxiliary data structures.
      // Stack and trail grow in different directions
      int size_pargoal_node = ChoiceCharDifference
	(PREV_CP(H_INIT_CP(h)), H_END_CP(h));
      int size_younger_nodes = ChoiceCharDifference(H_END_CP(h),w->node);
      int size_pargoal_trail = TrailCharDifference
	(H_INIT_CP(h)->trail_top, H_FRONT_CP(h)->trail_top);
      int size_younger_trail = TrailCharDifference
	(H_FRONT_CP(h)->trail_top, w->trail_top);
	  
      // STEP ONE: save memory for auxiliary data structures
      // Behavior of memcpy when overlapping is undetermined.
      // Therefore, we need to move younger section of stack to another
      // auxiliary place in memory before moving it down on the stack

      node_frontier = ChoiceCharOffset(w->node, -size_pargoal_node);
      if (size_pargoal_trail > size_younger_trail)
	size_aux_stack = size_pargoal_trail;
      else 
	size_aux_stack = size_younger_trail;

      if (size_pargoal_node > size_aux_stack)
	size_aux_stack = size_pargoal_node;

      if (size_younger_nodes > size_aux_stack)
	size_aux_stack = size_younger_nodes;

      aux_stack = (tagged_t *) checkalloc(size_aux_stack);

      if (size_pargoal_trail > size_younger_trail)
	{
	  // STEP ONE: Copy trail cells of goal execution to
	  // auxiliary data structures
	  memcpy(aux_stack, H_INIT_CP(h)->trail_top, size_pargoal_trail);
	  // STEP TWO: Move younger trail cells down
	  memcpy(H_INIT_CP(h)->trail_top, H_FRONT_CP(h)->trail_top, 
		 size_younger_trail);
	  // STEP THREE: Move pargoal trail cells to top of stack
	  memcpy(TrailCharOffset(w->trail_top, -size_pargoal_trail), 
		 aux_stack, size_pargoal_trail);
	}
      else
	{
	  memcpy(aux_stack, H_FRONT_CP(h)->trail_top, size_younger_trail);
	  memcpy(TrailCharOffset(w->trail_top, -size_pargoal_trail), 
		 H_INIT_CP(h)->trail_top, size_pargoal_trail);
	  memcpy(H_INIT_CP(h)->trail_top, aux_stack, size_younger_trail);
	}
      if (size_pargoal_node > size_younger_nodes)
	{
	  // STEP ONE: Copy choice points of goal execution to
	  // auxiliary data structures
	  memcpy(aux_stack, H_END_CP(h), size_pargoal_node);
	  // STEP TWO: Move younger choice points down
	  memcpy(node_frontier, w->node, size_younger_nodes);
	  // STEP THREE: Move pargoal_node to top of stack
	  memcpy(w->node, aux_stack, size_pargoal_node);
	}
      else
	{
	  memcpy(aux_stack, w->node, size_younger_nodes);
	  memcpy(w->node, H_END_CP(h), size_pargoal_node);
	  memcpy(node_frontier, aux_stack, size_younger_nodes);
	}
      checkdealloc((tagged_t*)aux_stack,size_aux_stack);

      // STEP SIX: Update trail pointers of choice points that are
      // moved down the stack
      for (node = node_frontier; !ChoiceYounger(H_INIT_CP(h), node);
	   node = PREV_CP(node))
	{
	  node->trail_top = TrailCharOffset
	    (node->trail_top, -size_pargoal_trail);	  
	}
      node_init->global_top = global_top_init;
      node_init->local_top = local_top_init;
      
      // STEP SEVEN: Update trail pointers of choice points that are
      // moved on top of the stack
      for (node = w->node; ChoiceYounger(node, node_frontier); 
	   node = PREV_CP(node))
	{
	  node->global_top = w->global_top;
	  node->local_top = w->local_top;
	  node->trail_top = TrailCharOffset(node->trail_top, size_younger_trail);
	}
	  
      // STEP EIGHT: Update execution_limits
      last_tel = Last_Parallel_Exec;
      for (l_exec_limits = Last_Parallel_Exec; 
	   l_exec_limits != h->exec_limits;
	   l_exec_limits = l_exec_limits->prev)
	{
//	  printf("\nExec_limts init %p end %p\n",
//		 l_exec_limits->init,l_exec_limits->end);
	  //taking trapped handler limits
//	  if (l_exec_limits->end == NULL)
//	    printf("\nWARNING l_exec_limits->end I !!!!!!!!!\n");
//	  if (!ChoiceYounger(l_exec_limits->end,node_end))
//	    {
//	      if (l_exec_limits->end == NULL)
//		printf("\nWARNING l_exec_limits->end II !!!!!!!!!\n");
//	      //first_tel == previouos l_exec_limit
//	      if (last_tel == NULL)
//		{
//		    last_tel = first_tel;
//		  printf("\nAssignament of last_tel %p\n",last_tel);
//		}
//	    }
	  last_tel = l_exec_limits;

	  //is init in the affected choice segment?
//	  node_t *orig_init_exec_limit = l_exec_limits->init;
//	  if (!ChoiceYounger(node_init,l_exec_limits->init))
//	    {
//	      if (ChoiceYounger(l_exec_limits->init,node_end))
//		{
//		  if (!ChoiceYounger(l_exec_limits->end,node_end)) 
//		    printf("\nBUG in move_execution_top III\n");
	  l_exec_limits->init = ChoiceCharOffset
	    (l_exec_limits->init,-size_pargoal_node);
//		}
//	      else
//		{
////		  if (ChoiceYounger(l_exec_limits->end,node_frontier)) 
////		    printf("\nBUG in move_execution_top IV\n");
//		  l_exec_limits->init = ChoiceCharOffset
//		    (l_exec_limits->init,size_younger_nodes);
//		}
//	    }
////	  if (node_init == l_exec_limits->end) nothing!
//	  if (ChoiceYounger(l_exec_limits->end,node_init))
//	    {
//	      if (ChoiceYounger(l_exec_limits->end,node_end))
//		{
	  l_exec_limits->end = ChoiceCharOffset
	    (l_exec_limits->end,-size_pargoal_node);
//		}
//	      else
//		{
//		  if (ChoiceYounger(node_init,orig_init_exec_limit) || 
//		      ChoiceYounger(orig_init_exec_limit,node_end)) 
//		    { 
//		      printf("\nBUG in move_execution_top V\n");
//		    }
//		  l_exec_limits->end = ChoiceCharOffset
//		    (l_exec_limits->end,size_younger_nodes);
//		}
//	    }
	}
      
//      printf("\ntrapped goal %p - %p!!\n",h,h->exec_limits); fflush(stdout);

      h->exec_limits->init = ChoiceCharOffset
	(h->exec_limits->init,size_younger_nodes);
      h->exec_limits->end = NULL;

//      printf("\ntrapped goal 2!!\n"); fflush(stdout);
      if (h->exec_limits  == last_tel) 
	printf("\nWARINING -> no trapped!!\n");
      if (h->exec_limits  != last_tel->prev) 
	printf("\nWARINING -> more than one trapped!!\n");
//      else
//	printf("\nMAS DE UN TRAPPED\n");
//      fflush(stdout);
      //Reordering Parallel executions
      aux_tel = Last_Parallel_Exec;
      Last_Parallel_Exec = h->exec_limits;
      last_tel->prev = h->exec_limits->prev;
      if (last_tel->prev != NULL)
	last_tel->prev->end = last_tel->init;
      h->exec_limits->prev = aux_tel;
      aux_tel->end = h->exec_limits->init;
      
      gettimeofday(&t_fin,NULL);
      
      //STEP EIGHT: remove trail cells pointing out of my environment 
      // (I disordered this fake trail cells!!)
      for(itrail = w->trail_top; 
	  !TrailYounger(h->exec_limits->init->trail_top,itrail); 
	  itrail--)
	{
//	  if (TagIsHVA(CTagToPointer(itrail)))
//	    {
//	      if ((TagToPointer(CTagToPointer(itrail)) >= 
//		   H_END_CP(h)->global_top) ||
//		  (TagToPointer(CTagToPointer(itrail)) < 
//		   H_INIT_CP(h)->global_top))
//		{
//		  if (OnHeap(TagToPointer(CTagToPointer(itrail))))
//		    {
//		      printf("\nTHIS IS THE CASE!!!\n");
//		      CTagToPointer(itrail) = 0;
//		    }
//		}
//	    }  
	  if (TagIsSVA(CTagToPointer(itrail)))
	    {
	      if ((TagToPointer(CTagToPointer(itrail)) >= 
		    (tagged_t*)last_tel->init->local_top) 
//		   ||
//		   (TagToPointer(CTagToPointer(itrail)) < 
//		    (tagged_t*)H_INIT_CP(h)->local_top)) 
		  &&
		  OnStack(TagToPointer(CTagToPointer(itrail))))
		{
		  NullifyTrailEntry(itrail);
		}
	    }  
	}
      
//      for (l_exec_limits = Last_Parallel_Exec; l_exec_limits != NULL; 
//	   l_exec_limits = l_exec_limits->prev)
//	{
//	  printf("\nEND G__EXEC %p = %p - %p\n",l_exec_limits,
//		 l_exec_limits->init,l_exec_limits->end);
//	}

      //STEP TEN: incrementing the number of trapped computations
      gettimeofday(&t_fin_fake,NULL);
      
      Wait_Acquire_slock(stats_l);
      trapped_goals++;
      trapped_time = trapped_time + timeval_diff(&t_fin, &t_ini);
      trapped_fake_time = trapped_fake_time + timeval_diff(&t_fin_fake, &t_ini);
      Release_slock(stats_l);
            
#if defined(DEBUG_TRAPPED)
      printf("\nPOST\n");
      for(node = w->node; !ChoiceYounger(node_init,node); node = PREV_CP(node))
	{
	  printf("\nNode %p=%p trail value %p=%p\n",
		 node,node->next_alt,node->trail_top,
		 (void*)CTagToPointer(node->trail_top));
	  printf("\nProtects until %p %p\n",node->global_top,node->local_top);
	}
//      for(itrail = w->trail_top; !TrailYounger(trail_init,itrail); itrail--)
//	printf("\nitrail %p=%p\n",itrail,(void*)CTagToPointer(itrail));
      printf("\nTRAPPED %p - %p\n", node_init,node_end);
      for (l_exec_limits = Last_Parallel_Exec; l_exec_limits != NULL; 
	   l_exec_limits = l_exec_limits->prev)
	{
	  printf("\nG__EXEC %p - %p\n",l_exec_limits->init,l_exec_limits->end);
	  printf("\n    Node %p=%p trail value %p=%p\n",l_exec_limits->init,
		 l_exec_limits->init->next_alt, l_exec_limits->init->trail_top,
		 (void*)CTagToPointer(l_exec_limits->init->trail_top));
	  printf("\n    Node %p=%p trail value %p=%p\n",l_exec_limits->end,
		 l_exec_limits->end->next_alt, l_exec_limits->end->trail_top,
		 (void*)CTagToPointer(l_exec_limits->end->trail_top));
	}
#endif
    }

  SetShadowregs(w->node);

//  if (h->exec_state == CANCELLING)
//    {
//      if (Cancel_Goal_Exec_Handler != NULL) printf("\nBUG CANCEL II\n");
//      Wait_Acquire_slock(Mutex_Lock_Of(h->remote_agent));
//      Cancel_Goal_Exec_Handler = h;
//      Release_slock(Mutex_Lock_Of(h->remote_agent));
//    }
//  else
//    {
//      Wait_Acquire_slock(Mutex_Lock_Of(h->agent));

  h->exec_state = REM_EXECUTING;
  h->exec_limits->end = NULL;

//      Release_slock(Mutex_Lock_Of(h->agent));
//    }
      
  
#if defined(DEP_TRAPPED)
  Dep_Size = h->dep_size;
  Dep_Id = h->dep_id;
#endif		      
#endif

  return TRUE;
}

/* Returns whether the agent associated to the input handler is
   waiting for some work or not */
bool_t apll_nd_waiting(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  return Suspended_Waiting_For_Work_Of(read_handler(X(0))->agent);
#endif

  return TRUE;
}


/* Suspends the execution of the current thread */
bool_t apll_nd_suspend(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)

  Mode = FORWARD_EXEC; //UNWIND is finish

  Wait_Acquire_lock(Waiting_For_Work_Lock);
  Release_slock(Mutex_Lock);

#if defined(DEBUG)
  if (debug_threads)
    printf("Suspending: %p\n",w);
#endif
  Suspended_Waiting_For_Work = TRUE;
  Suspend = WAITING;

  //#if defined(Solaris)
  // Measuring the active time of the agent
  //if (measure)
  //  Suspending_Time_Cont = gethrtime();
  //if (Total_Suspending_Time != NULL && measure)
  //  Total_Suspending_Time->n++;
  //#endif

  Cond_Var_Wait(Waiting_For_Work_Cond_Var,Waiting_For_Work_Lock);

  //#if defined(Solaris)
  // Measuring the active time of the agent
  //if (Total_Suspending_Time != NULL && measure)
  //  Total_Suspending_Time->time = Total_Suspending_Time->time +
  //    (double)(gethrtime() - Suspending_Time_Cont);
  //#endif

  Suspend = RELEASED;
#if defined(DEBUG)
  if (debug_threads)
    printf("Releasing: %p\n",w);
#endif
  Suspended_Waiting_For_Work = FALSE;
  Release_lock(Waiting_For_Work_Lock);

  Wait_Acquire_slock(Mutex_Lock);

  return (Mode == FORWARD_EXEC);
#endif

  return TRUE;
}


/* Releases the execution of the publishing agent */
bool_t apll_nd_release(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  worker_t *ag = read_handler(X(0))->agent;
  
  if (Suspended_Waiting_For_Work_Of(ag)) 
    {
      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(ag));
      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(ag));
      Release_lock(Waiting_For_Work_Lock_Of(ag));
    }
#endif

  return TRUE;
}


/* Releases the execution agent that picked up the parallel goal */
bool_t apll_nd_release_remote(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  worker_t *ag = read_handler(X(0))->remote_agent;
  
  if (Suspended_Waiting_For_Work_Of(ag)) 
    {
      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(ag));
      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(ag));
      Release_lock(Waiting_For_Work_Lock_Of(ag));
    }
#endif

  return TRUE;
}


/* Releases some suspended agent */
bool_t apll_nd_release_some_suspended_thread(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));
  bool_t released_agent = FALSE;
  worker_t *aux = Next_Wam_Of(w);

  // Release a suspended agent, if there is one
  do {
    Wait_Acquire_slock(Mutex_Lock_Of(aux));
#if defined(DEP_TRAPPED)
    if (Suspended_Waiting_For_Work_Of(aux) && (Goal_Cache_Of(aux) == NULL) && 
	!imply_trapped(aux,h)) 
      {
#else
    if (Suspended_Waiting_For_Work_Of(aux) && (Goal_Cache_Of(aux) == NULL)) 
      {
#endif
	Goal_Cache_Of(aux) = h;    
	Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
	Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
	Release_lock(Waiting_For_Work_Lock_Of(aux));
	released_agent = TRUE;
      }
    Release_slock(Mutex_Lock_Of(aux));
    aux = Next_Wam_Of(aux);
  }
  while (!released_agent && (aux != w));
#endif

  return TRUE;
}


/* Releases the execution of the agents to perform stack unwinding */
bool_t apll_nd_release_all_for_unwinding(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)

#if defined(DEP_TRAPPED)
  Dep_Size = 0;
  Dep_Id = NULL;
#endif

  if (main_worker == w) 
    {
      worker_t *aux = w;
      for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux))
	while(!Suspended_Waiting_For_Work_Of(aux));
      for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux))
	{
	  Mode_Of(aux) = UNWIND;
#if defined(DEP_TRAPPED)
	  Dep_Size_Of(aux) = 0;
	  Dep_Id_Of(aux) = NULL;
#endif
	}
      for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux)) 
	{
	  if (Suspended_Waiting_For_Work_Of(aux)) 
	    {
	      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
	      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
	      Release_lock(Waiting_For_Work_Lock_Of(aux));
	    }
	  else printf("\nWarning\n");
	}
      for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux))
	while(Mode_Of(aux) == UNWIND);
      for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux))
	while(!Suspended_Waiting_For_Work_Of(aux));
    }
  
//  printf("\nPOST UNWIND\n");
//  worker_t *aux = w;
//  do
//    {
//      printf("\nWAM %p",aux);      
//      printf(" GOAL %p-%p",
//	     Goal_List_Start_Of(aux),Goal_List_Top_Of(aux));
//      printf(" EVENT %p-%p",
//	     Event_Queue_Start_Of(aux),Event_Queue_Top_Of(aux));
//      printf(" EXEC %p\n",Last_Parallel_Exec_Of(aux));
//      printf("Nodo %p(%p,%p,%p,%p)",aux->node,aux->node->global_top,aux->node->local_top,aux->node->frame,aux->node->trail_top);
//      printf(" Heap %p",aux->global_top);
//      printf(" Frame %p",aux->frame);
//      printf(" Trail %p\n",aux->trail_top);
//      aux=Next_Wam_Of(aux);
//    }
//  while (aux != w);

#endif

  return TRUE;
}


/* Attemps to enter into the mutual exclusion */
bool_t apll_nd_enter_mutex(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Safe_To_Cancel = FALSE;
  Wait_Acquire_slock(Mutex_Lock_Of(read_handler(X(0))->agent));
#endif

  return TRUE;
}


/* Attemps to enter into the local mutual exclusion */
bool_t apll_nd_enter_mutex_self(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Safe_To_Cancel = FALSE;
  Wait_Acquire_slock(Mutex_Lock);
#endif

  return TRUE;
}


/* Attemps to enter into the remote mutual exclusion */
bool_t apll_nd_enter_mutex_remote(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Safe_To_Cancel = FALSE;
  Wait_Acquire_slock(Mutex_Lock_Of(read_handler(X(0))->remote_agent));
#endif

  return TRUE;
}


/* Exits from the mutual exclusion */
bool_t apll_nd_exit_mutex(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Release_slock(Mutex_Lock_Of(read_handler(X(0))->agent));
  Safe_To_Cancel = TRUE;
#endif

  return TRUE;
}


/* Exits from the local mutual exclusion */
bool_t apll_nd_exit_mutex_self(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Release_slock(Mutex_Lock);
  Safe_To_Cancel = TRUE;
#endif

  return TRUE;
}


/* Exits from the remote mutual exclusion */
bool_t apll_nd_exit_mutex_remote(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Release_slock(Mutex_Lock_Of(read_handler(X(0))->remote_agent));
  Safe_To_Cancel = TRUE;
#endif

  return TRUE;
}


/* Resets the value of statistical measures for nondet parallel
   programs */
bool_t apll_nd_reset_stats(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  total_goals = 0;
  trapped_goals = 0;
  trapped_time = 0;
  trapped_fake_time = 0;
#endif

  return TRUE;
}

/* Show memory usage */
bool_t apll_nd_show_memory_usage(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  worker_t *aux = w;
  int heapTot = 0;
//  int localTot = 0;
  do
    {
//      ComputeA(aux->local_top,aux->node);
//      printf("\nHeap %d Local %d\n",
//	     HeapDifference(aux->heap_start,aux->global_top),
//	     StackDifference(aux->stack_start,aux->local_top));
      heapTot += HeapDifference(aux->heap_start,aux->global_top);
//      localTot += StackDifference(aux->stack_start,aux->local_top);
      aux = Next_Wam_Of(aux);
    }
  while (aux != w);
  printf("\nTotal Heap %d\n",heapTot);
  printf("\nTotal Choice %d - %d\n",ChoiceDifference(w->choice_start,w->node),sizeof(node_t));
//  printf("\nTotal Local %d\n",localTot);

#endif

  return TRUE;
}

/* Gets the value of statistical measures for nondet parallel
   programs */
bool_t apll_nd_get_stats(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  cunify(Arg, X(0), MakeFloat(w, ((double)trapped_goals)/total_goals));
  cunify(Arg, X(1), MakeFloat(w, trapped_time * 1000));
  cunify(Arg, X(2), MakeFloat(w, trapped_fake_time * 1000));
#endif

  return TRUE;
}

/* Initialization procedure */
bool_t init(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  fifo             = init_atom_check("fifo");
  lifo             = init_atom_check("lifo");
  det              = init_atom_check("det");
  total_goals = 0;
  trapped_goals = 0;
  trapped_time = 0;
  trapped_fake_time = 0;
  Init_slock(stats_l);
#endif

  return TRUE;
}

