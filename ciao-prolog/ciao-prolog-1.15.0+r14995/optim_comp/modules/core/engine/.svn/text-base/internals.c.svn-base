#include <engine/basiccontrol.native.h>

#include <stdio.h>
#include <unistd.h>
#if !defined(USE_THREADS) || defined(SunOS4)
#include <signal.h>
#endif
#if defined(USE_RTCHECKS)
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>                                    /* For getpid() */
#endif
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <setjmp.h>

CINSNP__PROTO(prolog_abort) {
  choice_t *b;
  /* cut all the way and fail, leaving wam with a return code */
  DEREF(X(0), X(0));
  w->misc->exit_code = GetSmall(X(0));
  w->previous_choice = b = InitialChoice;
  CODE_CUT(b);
  CINSNP__FAIL;
}

/* $atom_mode(+Atom, -Context)
 * Context = 2'000 for alpha
 * Context = 2'001 for quote
 * Context = 2'010 for other
 * Context = 2'100 for punct, depending on how Atom is printed.
 */
CBOOL__PROTO(prolog_atom_mode) {
  atom_t *atomptr;

  DEREF(X(0),X(0));
  atomptr = TaggedToAtom(X(0));
  if (atomptr->has_special) {
    CBOOL__UnifyCons(MakeSmall(1),X(1));
  } else if (atomptr->has_dquote) {
    CBOOL__UnifyCons(MakeSmall(2),X(1));
  } else if (atomptr->has_squote) {
    CBOOL__UnifyCons(MakeSmall(4),X(1));
  } else {
    CBOOL__UnifyCons(TaggedZero,X(1));
  }

  CBOOL__PROCEED;
}

/* termheap_usage: [sizeof_used_space, sizeof_free_space] */
CBOOL__PROTO(termheap_usage) {
  intmach_t used, free;
  tagged_t x;
  
  used = HeapCharDifference(Heap_Start,G->heap_top);
  free = HeapCharAvailable(G->heap_top);
  MakeLST(x,IntmachToTagged(free),atom_nil);
  MakeLST(x,IntmachToTagged(used),x);
  CBOOL__LASTUNIFY(X(0),x);
}

/* envstack_usage: [sizeof_used_space, sizeof_free_space] */
CBOOL__PROTO(envstack_usage) {
  intmach_t used, free;
  tagged_t x;
  frame_t *newa;

  GetFrameTop(newa,w->choice,G->frame);
  used = StackCharUsed(newa);
  free = StackCharAvailable(newa);
  MakeLST(x,IntmachToTagged(free),atom_nil);
  MakeLST(x,IntmachToTagged(used),x);
  CBOOL__LASTUNIFY(X(0),x);
}

/* choice_usage: [sizeof_used_space, sizeof_free_space] */
CBOOL__PROTO(choice_usage) {
  intmach_t used, free;
  tagged_t x;
  
  used = ChoiceCharDifference(Choice_Start,w->choice);
  free = ChoiceCharDifference(w->choice,G->trail_top)/2;
  MakeLST(x,IntmachToTagged(free),atom_nil);
  MakeLST(x,IntmachToTagged(used),x);
  CBOOL__LASTUNIFY(X(0),x);
}

/* trail_usage: [sizeof_used_space, sizeof_free_space] */
CBOOL__PROTO(trail_usage) {
  intmach_t used, free;
  tagged_t x;
  
  used = TrailCharDifference(Trail_Start,G->trail_top);
  free = TrailCharDifference(G->trail_top,w->choice)/2;
  MakeLST(x,IntmachToTagged(free),atom_nil);
  MakeLST(x,IntmachToTagged(used),x);
  CBOOL__LASTUNIFY(X(0),x);
}

/* stack_shift_usage: [global shifts,local+control/trail shifts,time spent] */

CBOOL__PROTO(stack_shift_usage) {
  tagged_t x;
  int64_t time = (stats.ss_click*1000)/stats.userclockfreq;
  
  MakeLST(x,IntmachToTagged(time),atom_nil);
  time = stats.ss_local+stats.ss_control;
  MakeLST(x,IntmachToTagged(time),x);
  time = stats.ss_global;
  MakeLST(x,IntmachToTagged(time),x);
  CBOOL__LASTUNIFY(X(0),x);
}

/**********************************
 *  GARBAGE COLLECTION BUILTINS   *
 **********************************/

CBOOL__PROTO(gc_usage) {
  flt64_t t;
  intmach_t u;
  tagged_t x;

  t = (flt64_t)stats.gc_click*1000/stats.userclockfreq;
  MakeLST(x,BoxFloat(t),atom_nil);
  u = stats.gc_acc;
  MakeLST(x,IntmachToTagged(u),x);
  u = stats.gc_count;
  MakeLST(x,IntmachToTagged(u),x);
  CBOOL__LASTUNIFY(x,X(0));
}

tagged_t gcmode_to_term(bool_t gcmode) {
  if (gcmode == TRUE) {
    return atom_on;
  } else {
    return atom_off;
  }
}

bool_t gcmode_from_term(tagged_t mode) {
  if (mode == atom_on) {
    return TRUE;
  } else {
    return FALSE;
  }
}

tagged_t gctrace_to_term(intmach_t gctrace) {
  if (gctrace == GCTRACE__OFF) {
    return atom_off;
  } else if (gctrace == GCTRACE__TERSE) {
    return atom_terse;
  } else {
    return atom_verbose;
  }
}

intmach_t gctrace_from_term(tagged_t trace) {
  if (trace == atom_off) {
    return GCTRACE__OFF;
  } else if (trace == atom_terse) {
    return GCTRACE__TERSE;
  } else {
    return GCTRACE__VERBOSE;
  }
}

CBOOL__PROTO(gc_mode) {
  CBOOL__UnifyCons(gcmode_to_term(current_gcmode),X(0));
  DEREF(X(1), X(1));
  current_gcmode = gcmode_from_term(X(1));
  CBOOL__PROCEED;
}

CBOOL__PROTO(gc_trace) {
  CBOOL__UnifyCons(gctrace_to_term(current_gctrace),X(0));
  DEREF(X(1), X(1));
  current_gctrace = gctrace_from_term(X(1));
  CBOOL__PROCEED;
}

CBOOL__PROTO(gc_margin) {
  CBOOL__UnifyCons(MakeSmall(current_gcmargin),X(0));
  DEREF(X(1), X(1));
  current_gcmargin = GetSmall(X(1));
  CBOOL__PROCEED;
}

try_node_t *address_nd_repeat;
try_node_t *address_nd_current_instance;
try_node_t *address_nd_current_atom;
try_node_t *address_nd_atom_concat;

/* Those have to do with garbage collecting of the abolished predicates.
   Should be made by only one worker?  Otherwise, access should be locked
   when doing this GC --- which means every predicate access should be
   locked! */

static intmach_t gcdef_count=0;             /* Shared, no locked */
static intmach_t gcdef_limit=0;             /* Shared, no locked */
typedef struct gcdef gcdef_t;
struct gcdef {                 /* Shared, no locked */
  intmach_t gctype;
  char *info;
};

static gcdef_t *gcdef_bin;

void leave_to_gc(intmach_t type, char *info) {
  if (gcdef_limit==0) {
    gcdef_bin = CHECKALLOC_ARRAY(gcdef_t, 2);
    gcdef_limit = 2;
  } else if (gcdef_count==gcdef_limit) {
    gcdef_bin = CHECKREALLOC0_ARRAY(gcdef_t, gcdef_bin, gcdef_limit, 2*gcdef_limit);
    gcdef_limit *= 2;
  }

  gcdef_bin[gcdef_count].gctype = type;
  gcdef_bin[gcdef_count].info = info;
  gcdef_count++;
}

void free_try(try_node_t *t) {
  try_node_t *t1, *t2;

  for (t1=t; !TRY_NODE_IS_NULL(t1); t1=t2) {
    t2=t1->next;
    CHECKDEALLOC0(try_node_t, t1);
  }
}

/* free a hashtab that contains try_node elements */
void free_hashtab_try(hashtab_t *sw) {
  hashtab_node_t *h1;
  intmach_t i;
  intmach_t size = HASHTAB_SIZE(sw);
  bool_t otherwise = FALSE;

  for (i=0; i<size; i++) {
    h1 = &sw->node[i];
    if (h1->key || !otherwise) {
      free_try(h1->value.try_chain);
    }
    if (!h1->key) {
      otherwise = TRUE;
    }
  }

  CHECKDEALLOC0_TAILED(hashtab_t, sw);
}

void free_incoreinfo(incore_info_t *p) {
  emul_info_t *stop;
  emul_info_t *cl, *cl1;

  stop = *p->clauses_tail;
  for (cl=p->clauses; cl!=stop; cl=cl1) {
    cl1 = cl->next;
    free_emulinfo(cl);
  }
  CHECKDEALLOC0(incore_info_t, p);
}

void free_info(intmach_t gctype, char *info);

void free_emulinfo(emul_info_t *cl) {
  CHECKDEALLOC0_TAILED(emul_info_t, cl);
}

void free_info(intmach_t gctype, char *info) {
  switch(gctype) {
  case ENTER_COMPACTCODE_INDEXED:
  case ENTER_PROFILEDCODE_INDEXED:
    {
      incore_info_t *i;
      i = (incore_info_t *)info;
      free_try(i->lstcase);
      free_hashtab_try(i->othercase);
      goto enter_nonindexed;
    }
  case ENTER_COMPACTCODE:
  case ENTER_PROFILEDCODE:
  enter_nonindexed:
    {
      incore_info_t *i;
      i = (incore_info_t *)info;
      free_try(i->varcase);
      free_incoreinfo(i);
    }
    break;
  case ENTER_INTERPRETED:
    {
      instance_t *n, *m;
      int_info_t *i;

      i = (int_info_t *)info;
      for (n = i->first; n; n=m) {
	m=n->forward;
	n->rank = ERRORTAG;
	CHECKDEALLOC0_TAILED(instance_t, n);
      }
      CHECKDEALLOC0_TAILED(hashtab_t, i->indexer);
      CHECKDEALLOC0(int_info_t, i);
      break;
    }
  default:
    break;
  }
}

void relocate_gcdef_clocks(instance_clock_t *clocks) {
  intmach_t i;

  for (i=0; i<gcdef_count; i++) {
    if (gcdef_bin[i].gctype==ENTER_INTERPRETED) {
      relocate_clocks(((int_info_t *)gcdef_bin[i].info)->first, clocks);
    }
  }
}

/***************************************************************************/

/* If we are not using threads, this simply points to a single WRB state;
   i.e., the list is actually a singleton. */
/* wrb_state_p wrb_state_list;  */

SLOCK goal_desc_list_l;
goal_descriptor_t *goal_desc_list = NULL;

SLOCK thread_to_free_l;
THREAD_T thread_to_free = (THREAD_T)NULL;

uintmach_t global_goal_number = 0;                 /* Last number taken */

/* The initial list has a single goal descriptor with no WAM and in
   IDLE state */

void init_goal_desc_list() {
  Init_slock(goal_desc_list_l);
  Init_slock(thread_to_free_l);
  
  goal_desc_list = CHECKALLOC(goal_descriptor_t);
  goal_desc_list->state = IDLE;
  goal_desc_list->worker_registers = NULL;
  Init_slock(goal_desc_list->goal_lock_l);
  goal_desc_list->forward = goal_desc_list->backward = goal_desc_list;
}


/* "goal" is to be the only working thread in the system.  The rest of the
   goals have no associated thread */

void reinit_list(goal_descriptor_t *myself) {
  goal_descriptor_t *goal_ref;
     
  Wait_Acquire_slock(goal_desc_list_l);
  goal_ref = goal_desc_list;
  do {
    if ((goal_ref != myself)) {
      unlink_wam(goal_ref);
      goal_ref->state = IDLE;
    }
    goal_ref = goal_ref->forward;
  } while (goal_ref != goal_desc_list);
  goal_desc_list = myself->forward;
  Release_slock(goal_desc_list_l);
}


/* Try to kill a goal (and release the wam it was attached to).
   Returns 0 if no error, -1 on error (maybe no such thread).  Actually,
   killing a thread should be done otherwise: the killed thread might be
   in an unstable state in which it should not be killed. */

/*
int kill_thread(this_goal)
     goal_descriptor_t *this_goal;
{
  return Thread_Cancel(this_goal->thread_handle);
}
*/


/* Cause kills to this thread to be immediately executed */

void allow_thread_cancellation() {
  Allow_Thread_Cancel;
}


/* Cause kills to this thread to be ignored (for symmetry with the above) */

void disallow_thread_cancellation() {
  Disallow_Thread_Cancel;
}


/* Should be called after the list is inited */
goal_descriptor_t *init_first_gd_entry() {
  goal_descriptor_t *first_gd;

  first_gd = gimme_a_new_gd();

  first_gd->thread_id = Thread_Id;
  first_gd->thread_handle = (THREAD_T)NULL; /* Special case? */
  first_gd->action = NO_ACTION;
  first_gd->goal = (tagged_t)NULL;
  
  return first_gd;
}

/* Returns a free goal descriptor, with a WAM attached to it.  If
   needed, create memory areas, initialize registers, etc. The worker
   is marked as WORKING in order to avoid other threads stealing it.
   The WAM areas are already initialized. */

goal_descriptor_t *gimme_a_new_gd() {
  goal_descriptor_t *gd_to_run;

  gd_to_run = look_for_a_free_goal_desc();
  if (gd_to_run != NULL) { 
    /* Make sure it has a WAM */
    if (!gd_to_run->worker_registers) {
      WITH_WORKER(free_wam(), {
	CVOID__CALL_N(associate_wam_goal, gd_to_run);
      });
    }
  } else {
    WITH_WORKER(free_wam(), {
      gd_to_run = CFUN__EVAL(attach_me_to_goal_desc_list);
    });
  }
  return gd_to_run;
}

/* We already have a WAM.  Create a goal descriptor in WORKING state,
   add it to the goal descriptor list, and mark it as ours.  */

CFUN__PROTO(attach_me_to_goal_desc_list, goal_descriptor_t *) {
  goal_descriptor_t *goal_desc_p;

  goal_desc_p = CHECKALLOC(goal_descriptor_t);
  goal_desc_p->state = WORKING;
  goal_desc_p->goal_number = ++global_goal_number;
  Init_slock(goal_desc_p->goal_lock_l);
  CVOID__CALL_N(associate_wam_goal, goal_desc_p);

  /* Add it at the end of the list, i.e., add it to the "backward"
     side of the list, where the non-free goal descriptors are. */
  Wait_Acquire_slock(goal_desc_list_l);
  goal_desc_p->forward = goal_desc_list;
  goal_desc_p->backward = goal_desc_list->backward;
  goal_desc_list->backward->forward = goal_desc_p;
  goal_desc_list->backward = goal_desc_p;
  Release_slock(goal_desc_list_l);
  CFUN__PROCEED(goal_desc_p);
}

/* cross-link a WAM and a goal */

CVOID__PROTO_N(associate_wam_goal, goal_descriptor_t *goal_desc) {
  goal_desc->worker_registers = w;
  w->misc->goal_desc_ptr = goal_desc;
}






/* The WAM used by goal is not to be used any more.  Remove the
   choicepoints and the possible dynamic concurrent choicepoints,
   unlink it fro the goal descriptor and return it to the free wam
   list. */

void unlink_wam(goal_descriptor_t *goal) {
  if (goal->worker_registers == NULL) return;
  WITH_WORKER(goal->worker_registers, {
    goal->worker_registers = NULL;
#if defined(USE_THREADS)		
    /* Clean the possible conc. chpt. */
    remove_link_chains(&TopConcChpt, InitialChoice);
#endif
    CVOID__CALL(release_wam);
  });
}

/* A goal descripter state is to be marked as free --- no thread is
   working on it.  It is not, however, deleted from the state list, or
   the WAM freed, for creating areas is a costly process.  However, we
   move it to the beginning of the list.  We should have exclusive
   access to it, therefore I do not protect the goal descriptor areas.
   The WAM is put back in the lis of available WAMs. */

void make_goal_desc_free(goal_descriptor_t *goal) {
  unlink_wam(goal);		/* Clean WAM, put it back to free list */
  Wait_Acquire_slock(goal_desc_list_l);
  goal->state = IDLE;
				/* Unlink from current place */
  goal->backward->forward = goal->forward;
  goal->forward->backward = goal->backward;
				/* Link at the beginning */
  goal->forward = goal_desc_list;
  goal->backward = goal_desc_list->backward;
  goal_desc_list->backward->forward = goal;
  goal_desc_list->backward = goal;
  goal_desc_list = goal;

  Release_slock(goal_desc_list_l);
}

/* release a goal descriptor */

void release_goal_desc(goal_descriptor_t *goal_desc) {
  if ((goal_desc->state != PENDING_SOLS) &&
      (goal_desc->state != FAILED)) {
    /* Trying to release a worker either working or without assigned
       work */
    return;
  }
  make_goal_desc_free(goal_desc);
}

/* What goal descriptor am I working for, if I do not know which is my
   WAM?  Its use is only justified when we are recovering from an
   interruption */

worker_t *get_my_worker() {
  THREAD_ID thr_id = Thread_Id;
  goal_descriptor_t *this_goal = goal_desc_list->backward;

  /* Freeze the status of the goal descriptor list */

  Wait_Acquire_slock(goal_desc_list_l);

  /* Go backwards: the goals at the beginning are free. */

  while( (this_goal != goal_desc_list) &&
         ((this_goal->state != WORKING ) ||
          !Thread_Equal(this_goal->thread_id, thr_id)))
    this_goal = this_goal->backward;
  Release_slock(goal_desc_list_l);
  
  if (Thread_Equal(this_goal->thread_id, thr_id) &&
      (this_goal->state == WORKING))
    return this_goal->worker_registers;
  else
    PANIC_FAULT("Could not find goal descriptor");    
}

/* Return a free goal descriptor from the ring.  Mark it as WORKING as
   soon as we find one free so that no other thread can steal it. If
   there is any free descriptor, it should appear at the beginning of
   the chain.
*/

goal_descriptor_t *look_for_a_free_goal_desc() {
  goal_descriptor_t *free_goal_desc;

  Wait_Acquire_slock(goal_desc_list_l);
  if (goal_desc_list->state == IDLE) {
    free_goal_desc = goal_desc_list;
    goal_desc_list = goal_desc_list->forward; 
    free_goal_desc->state = WORKING;
    free_goal_desc->goal_number = ++global_goal_number;
    /* Init_slock(free_goal_desc->goal_lock_l); */
  } else free_goal_desc = NULL;
  Release_slock(goal_desc_list_l);
  return free_goal_desc;
}

void enqueue_thread(THREAD_T thread) {
  Wait_Acquire_slock(thread_to_free_l);
  if (thread_to_free) 
    Thread_Join(thread_to_free);
  thread_to_free = thread;
  Release_slock(thread_to_free_l);
}

extern definition_t *address_true;

/* ------------------------------------------------------------------------ */ 

/*  
   Called from wam.c
   Collect all constraints that have been woken "recently" by
   scanning the newest trail segment.  Also, excise such entries
   belonging to the newest heap segment.  
   Each pending unification pushes 4 heap elems - cf enter_predicate: wam.c
*/

/* TODO: share code with collect_one_pending_unification */

CVOID__PROTO_N(collect_pending_unifications, intmach_t wake_count) {
  intmach_t sofar=0;
  tagged_t *tr = G->trail_top;
  tagged_t *h;
  tagged_t *tr0 = NULL;
  tagged_t *limit = w->choice->trail_top;
   
  h = G->heap_top;
  X(0) = atom_nil;
  while (sofar<wake_count && TrailYounger(tr,limit))  {
    tagged_t ref, value;
    
    TrailDec(tr);
    ref = *(tr);
    if (!TaggedIsCVA(ref)) continue;
    value = *TagpPtr(CVA,ref);
    if (value==ref) { 
      PANIC_FAULT("wake - unable to find all goals");  
    }
    
    sofar++; 
    *TaggedToPointer(ref) = ref;     		               /* untrail */
    
    HeapPush(h, ref); 
    HeapPush(h, value);  
    HeapPush(h, Tagp(LST,h-2));
    HeapPush(h, X(0));
    X(0) = Tagp(LST,h-2);
    
    if (!CondCVA(ref)) {
      tr0=tr;
      *tr=0;
    }
  }
  G->heap_top = h;
  SetWakeCount(0);
  
  if (sofar<wake_count) {
    PANIC_FAULT("wake - unable to find all goals");
  }
  
  /* now compress the trail */
  if (tr0) {
    CompressTrailNoGC(tr0);
  }
}                  

/* TODO: share code with collect_pending_unifications */

CVOID__PROTO(collect_one_pending_unification) {
  intmach_t sofar=0;
  tagged_t *tr = G->trail_top;
  tagged_t *tr0 = NULL;
  tagged_t *limit = w->choice->trail_top;
  
  while ( !sofar && TrailYounger(tr,limit)) {
    tagged_t ref, value;
    
    TrailDec(tr);
    ref = *(tr);
    if (!TaggedIsCVA(ref)) continue;
    value = *TagpPtr(CVA,ref);
    if (value==ref) { 
      PANIC_FAULT("wake - unable to find all goals");  
    }
    
    sofar++; 
    /* untrail */
    *TaggedToPointer(ref) = ref;     		               
    
#if 0 /* old attributes */
    X(0) = *TaggedToGoal(ref);
#else /* backporting new attributes */
    X(0) = ref;
#endif
    X(1) = value;
    
    if (!CondCVA(ref)) {
      tr0=tr;
      *tr=0;
    }
  }
  SetWakeCount(0);
  
  if (!sofar) {
    PANIC_FAULT("wake - unable to find all goals");
  }
  
  /* now compress the trail */
  if (tr0) {
    CompressTrailNoGC(tr0);
  }
}

/* Shared? Not easy: they have to do with the lifetime of dyn. predicates  */
instance_clock_t def_clock=0;
instance_clock_t use_clock=0;

typedef enum {X5, X2} WhichChain;

static bool_t wait_for_an_instance_pointer(instance_t **ins_pptr1,
					   instance_t **ins_pptr2,
					   int_info_t *root,
					   BlockingType block);

static instance_t *first_possible_instance(tagged_t head,
					   int_info_t *root,
					   instance_t **x2_n,
					   instance_t **x5_n);

static instance_handle_t *make_handle_to(instance_t *inst,
					 int_info_t *root,
					 tagged_t head,
					 WhichChain chain);

static void remove_handle(instance_handle_t *xi,
                          int_info_t *root,
                          WhichChain chain);

static void change_handle_to_instance(instance_handle_t *handle,
                                      instance_t *new_inst,
                                      int_info_t *root,
                                      WhichChain chain);

static void unlink_handle(instance_handle_t *xi, 
                   int_info_t *rt, 
                   WhichChain chain);

static void link_handle(instance_handle_t *handle,
                 instance_t *inst,
                 int_info_t *root,
                 WhichChain chain);

/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       $CURRENT_CLAUSES/2
   -----------------------------------------------------------------------*/

int_info_t *current_clauses_aux(tagged_t head);

/* This used to be nondeterministic. */
/* ASSERT: X1 is always unbound, unconditional. */
CBOOL__PROTO(current_clauses) {
  int_info_t *root;
  DEREF(X(0),X(0));
  root = current_clauses_aux(X(0));
  CBOOL__TEST(root != NULL); /* Fails if not interpreted */
  *TaggedToPointer(X(1)) = PointerToTerm(root);
  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------ */
/* current_instance */

/* ASSERT: X(0) is head, X(1) is body */

#define BLOCKIDX (1<<0)
#define EXECIDX  (1<<1)

#define SET_BLOCKING(arg) (arg) = ((arg) | BLOCKIDX)
#define SET_NONBLOCKING(arg) (arg) = ((arg) & ~BLOCKIDX)
#define IS_BLOCKING(arg) ((arg) & BLOCKIDX)
#define IS_NONBLOCKING(arg) !((arg) & BLOCKIDX)

#define SET_EXECUTING(arg) (arg) = ((arg) | EXECIDX)
#define SET_NONEXECUTING(arg) (arg) = ((arg) & ~EXECIDX)
#define EXECUTING(arg) ((arg) & EXECIDX)
#define NONEXECUTING(arg) !((arg) & EXECIDX)

/* MCL: "Blocking" specifies whether the predicate must block or not
   on concurrent predicates. */

static CINSNP__PROTO_N(current_instance_noconc, int_info_t *root);
static CINSNP__PROTO_N(current_instance_conc, int_info_t *root,
		       BlockingType block);

/* precondition: X(3) is set */
CINSNP__PROTO_N(current_instance, int_info_t *root, BlockingType block) {
  if (root->behavior_on_failure == DYNAMIC) {
    CINSNP__LASTCALL_N(current_instance_noconc, root);
  } else {
    CINSNP__LASTCALL_N(current_instance_conc, root, block);
  }
}

/* Take time into account, do not wait for predicates. */

#define ACTIVE_INSTANCE(I,TIME,CHAINP) \
  ((I)==NULL ? NULL : \
   ((TIME) >= (I)->birth && (TIME) < (I)->death) ? (I) : \
   CFUN__EVAL_N(active_instance,I,TIME,CHAINP))

#define CURRENT_INSTANCE(Head, Root, ActiveInstance, CODE_FAIL) { \
  __label__ var_case_switch; \
  __label__ xn_switch; \
  tagged_t head; \
  head=(Head); \
  DerefSw_HVAorCVAorSVA_Other(head, { goto var_case_switch; }, {}); \
  (Head) = head; \
  x2_next=NULL; \
  x5_next=NULL; \
  if (TaggedIsSTR(head)) { \
    DerefArg(head,head,1); \
    if (IsVar(head)) { \
    var_case_switch: \
      x5_chain = x2_chain = ActiveInstance((Root)->first,use_clock,TRUE); \
      if (x2_chain) { \
        x5_next = x2_next = ActiveInstance(x2_chain->forward,use_clock,TRUE); \
      } else { \
	CODE_FAIL; \
      } \
    } else if (TaggedIsLST(head)) { \
      x5_chain = ActiveInstance((Root)->lstcase,use_clock,FALSE); \
    xn_switch: \
      x2_chain = ActiveInstance((Root)->varcase,use_clock,FALSE); \
      if (x2_chain && x5_chain) { \
        if (x2_chain->rank < x5_chain->rank){ \
          x2_next = ActiveInstance(x2_chain->next_forward,use_clock,FALSE); \
          x5_next = x5_chain; \
          x5_chain = NULL; \
        } else { \
          x5_next = ActiveInstance(x5_chain->next_forward,use_clock,FALSE); \
          x2_next = x2_chain; \
          x2_chain = NULL; \
        } \
      } else if (x2_chain) { \
        x2_next = ActiveInstance(x2_chain->next_forward,use_clock,FALSE); \
      } else if (x5_chain) { \
        x5_next = ActiveInstance(x5_chain->next_forward,use_clock,FALSE); \
      } else { \
	CODE_FAIL; \
      } \
    } else { \
      hashtab_node_t *hnode; \
      if (TaggedIsSTR(head)) { \
	hnode = hashtab_get((Root)->indexer, \
			    (hashtab_key_t)TaggedToHeadfunctor(head)); \
      } else { \
	hnode = hashtab_get((Root)->indexer,(hashtab_key_t)head); \
      } \
      x5_chain = ActiveInstance(hnode->value.instp,use_clock,FALSE); \
      goto xn_switch; \
    } \
  } else { \
    goto var_case_switch; \
  } \
}

/* precondition: X(3) is set */
static CINSNP__PROTO_N(current_instance_noconc, int_info_t *root) {
  instance_t *x2_chain;
  instance_t *x5_chain;
  instance_t *x2_next;
  instance_t *x5_next;

  Wait_Acquire_Cond_lock(root->clause_insertion_cond);
  CURRENT_INSTANCE(X(0), root, ACTIVE_INSTANCE, { /* fail */
    Release_Cond_lock(root->clause_insertion_cond);
    CINSNP__FAIL;
  });

  if (x2_next || x5_next) {
    /* X(0) must be the head */
    /* X(1) must be the body */
    X(X2_CHN) = PointerOrNullToTerm(x2_next);
    /* X(3) - */
    X(ClockSlot) = MakeSmall(use_clock);
    X(X5_CHN) = PointerOrNullToTerm(x5_next);
    X(RootArg) = PointerOrNullToTerm(root);
    /* Clean unused registers (JF & MCL) */
    X(InvocationAttr) = MakeSmall(0); 
    X(PrevDynChpt) = TermNull;

    w->previous_choice = w->choice;

    CODE_CHOICE_NEW(w->choice, address_nd_current_instance);
  } else {
    /* Initialize dynpreserved (to ensure heapmargin correctness) */
    /* TODO: necessary? */
    /* X(0) must be the head */
    /* X(1) must be the body */
    X(X2_CHN) = TermNull;
    /* X(3) - */
    X(ClockSlot) = MakeSmall(use_clock);
    X(X5_CHN) = TermNull;
    X(RootArg) = PointerOrNullToTerm(root); /* necessary */
    X(InvocationAttr) = MakeSmall(0); 
    X(PrevDynChpt) = TermNull;
  }

  Release_Cond_lock(root->clause_insertion_cond);

  if (x2_chain != NULL) {
    w->ins = x2_chain;
    CINSNP__GOTO(w->ins->emulcode);
  } else if (x5_chain != NULL) {
    w->ins = x5_chain;
    CINSNP__GOTO(w->ins->emulcode);
  } else {
    CINSNP__FAIL; /* fail */ /* TODO: is this case possible? */
  }
}

/* First-solution special case of the above. */
/* ASSERT: X(0) is a small integer,
           X(1) is a dereferenced unconditional unbound */
/* Adapted to concurrent predicates (MCL) */
CBOOL__PROTO(first_instance) {
  int_info_t *root;
  instance_t *inst;

  DEREF(X(0), X(0));
  root = TaggedToRoot(X(0));
  if (root->behavior_on_failure == DYNAMIC) {
    inst = ACTIVE_INSTANCE(root->first,use_clock,TRUE);
  } else {
    Cond_Begin(root->clause_insertion_cond);
    inst = root->first;
    Broadcast_Cond(root->clause_insertion_cond);
  }

  CBOOL__TEST(inst != NULL); /* has solutions */
  *TaggedToPointer(X(1)) = PointerToTerm(inst);

  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------
	  NEXT_INSTANCE
   -----------------------------------------------------------------------*/

static CINSNP__PROTO(next_instance_conc);
static CINSNP__PROTO(next_instance_noconc);

/* TODO: trick to avoid incorrect nondet macros - do not emit a macro! */
CINSNP__PROTO(next_instance) {
  CODE_NEXT_INSTANCE();
}

CINSNP__PROTO(next_instance_noconc) {
  instance_t *x2_insp = TaggedToInstance(X(2));
  instance_t *x5_insp = TaggedToInstance(X(5));
  instance_clock_t clock = GetSmall(X(4));
  int_info_t *root = TaggedToRoot(X(6));

  Wait_Acquire_Cond_lock(root->clause_insertion_cond);

  if (x2_insp == x5_insp) {
    w->ins = x2_insp;
    x2_insp = x5_insp = ACTIVE_INSTANCE(x2_insp->forward,clock,TRUE);
  } else if (!x2_insp) {
  x5_alt:
    w->ins = x5_insp;
    x5_insp = ACTIVE_INSTANCE(x5_insp->next_forward,clock,FALSE);
  } else if (!x5_insp) {
  x2_alt:
    w->ins = x2_insp;
    x2_insp = ACTIVE_INSTANCE(x2_insp->next_forward,clock,FALSE);
  } else if (x2_insp->rank < x5_insp->rank) {
    goto x2_alt;
  } else {
    goto x5_alt;
  }

  Release_Cond_lock(root->clause_insertion_cond);

  if (!x2_insp && !x5_insp) {
    /* If there is *definitely* no next instance, remove choicepoint */
    SetDeep();
    SetChoice(w->previous_choice);
  } else {
    w->choice->x[X2_CHN] = X(X2_CHN) = PointerOrNullToTerm(x2_insp);
    w->choice->x[X5_CHN] = X(X5_CHN) = PointerOrNullToTerm(x5_insp);
  }
  if (w->ins == NULL) {
    TopConcChpt = TermToPointerOrNull(choice_t, X(PrevDynChpt));
    CINSNP__FAIL;
  } else {
    CINSNP__GOTO(w->ins->emulcode);
  }
}

#if defined(USE_THREADS)
CVOID__PROTO_N(close_predicate, int_info_t *root) {
  Cond_Begin(root->clause_insertion_cond);
  if (root->behavior_on_failure == CONC_OPEN) 
    root->behavior_on_failure = CONC_CLOSED;
  Broadcast_Cond(root->clause_insertion_cond);
}
CVOID__PROTO_N(open_predicate, int_info_t *root) {
  Cond_Begin(root->clause_insertion_cond);
  if (root->behavior_on_failure == CONC_CLOSED) 
    root->behavior_on_failure = CONC_OPEN;
  Broadcast_Cond(root->clause_insertion_cond);
}
#else
CVOID__PROTO_N(close_predicate, int_info_t *root) {
}
CVOID__PROTO_N(open_predicate, int_info_t *root) {
}
#endif

/* Similar to current_instance for concurrent predicates: wait if no clauses
   match and the predicate is not closed; also, handle the cases of
   predicate retraction, assertion, etc. by maintaining and moving around a
   list with the calls pending on every clause of a predicate.  When the end
   of the list is reached and the predicate is still "open" (i.e., more
   clauses might be added to it), the call does not fail: it is instead
   enqueued in a list reacheable from the root of the predicate. (MCL) 

   The case for non-blocking calls is interesting: there are three
   possibilities:

   a) There is no clause for the predicate
   b) There are clauses, but indexing dictates that none of them are 
      applicable for the current call.
   c) There are clauses, and at least one of them can match the current call.

   In cases (a) and (b) a non-blocking call does *not* need choicepoint.
   Case (c) needs a choicepoint, and the clause remains locked while it is
   being executed.  The lock is removed when execution finishes, or when the
   choicepoint is deallocated.

*/

/* precondition: X(3) is set */
static CINSNP__PROTO_N(current_instance_conc, int_info_t *root,
		       BlockingType block) {
  instance_t *x2_n = NULL, *x5_n = NULL;
  instance_t *current_one;
  bool_t try_instance;
  instance_handle_t *x2_next, *x5_next;

  DEBUG__TRACE(debug_concchoicepoints,
	       "** Entering current_instance_conc, choice = %lx, previous_choice = %lx, conc. = %lx\n",
	       (long)w->choice, (long)w->previous_choice, (long)TopConcChpt);

  do {
    try_instance =
     wait_for_an_instance_pointer(&(root->first), &(root->first), root, block);

 /* That was a non-blocking or concurrent predicate with no instance at all */
    if (!try_instance) {
      Wait_For_Cond_End(root->clause_insertion_cond);
      DEBUG__TRACE(debug_concchoicepoints,
		   "***(%ld)(%ld) Exiting current_instance_conc with failure, choice = %lx, previous_choice = %lx, conc. choice = %lx\n",
		   (long)Thread_Id, (long)GET_INC_COUNTER,
		   (long)w->choice, (long)w->previous_choice, (long)TopConcChpt);
      CINSNP__FAIL; /* fail */
    }

    /* If we are here, we have a lock for the predicate.  Get first possibly
       matching instance */

    current_one = first_possible_instance(X(0), root, &x2_n, &x5_n);
    if (current_one == NULL)  /* Let others enter and update */
        Wait_For_Cond_End(root->clause_insertion_cond);

  } while (!current_one && block == BLOCK);

  /* Here with (current_one || block == NON_BLOCK).  current_one == NULL
     implies that we do not have a lock --- but then block == NON_BLOCK, and
     therefore we can return with a NULL */
     
  if (!current_one) {
    DEBUG__TRACE(debug_concchoicepoints,
		 "***(%ld)(%ld) Exiting current_instance_conc with failure, choice = %lx, previous_choice = %lx, conc. choice = %lx\n",
		 (long)Thread_Id, (long)GET_INC_COUNTER,
		 (long)w->choice, (long)w->previous_choice, (long)TopConcChpt);
    CINSNP__FAIL; /* fail */
  }

#if defined(USE_THREADS)
#if defined(DEBUG_TRACE)
  /* TODO: if these tests are not trace messages, debug_conc and debug_concchoicepoints should always be true: IS THIS CODE A PRE/POSTCONDITION? */
  if (Cond_Lock_is_unset(root->clause_insertion_cond))
    DEBUG__TRACE(debug_concchoicepoints,
		 "***%ld(%ld) current_instance_conc: putting chpt without locks!\n",
		 (long)Thread_Id, (long)GET_INC_COUNTER);
  if (!root->first)
    DEBUG__TRACE(debug_conc,
		 "*** current_instance_conc: no first instance!\n");
#endif
#endif

 /* We do NOT release the clause lock here: we must retain it until we
    finish executing it.  It is released at Prolog level.  The same
    for next instance: when it provides a possible solution, it leaves
    a lock set, to be unset from Prolog.  If the Prolog unification
    fails, the lock is anyway unset from wam(), right before
    failure. */

#if defined(USE_THREADS)
  DEBUG__TRACE(debug_concchoicepoints,
	       "*** %ld(%ld) in c_i making chpt (now: choice = %lx)., inst. is %lx\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER, 
	       (long)w->choice, (long)current_one);
#endif

  x2_next = make_handle_to(x2_n, root, X(0), X2);
  x5_next = make_handle_to(x5_n, root, X(0), X5);

  X(X2_CHN) = PointerOrNullToTerm(x2_next);
  X(ClockSlot) = MakeSmall(use_clock);
  X(X5_CHN) = PointerOrNullToTerm(x5_next);

  /* pass root to RETRY_INSTANCE (MCL) */
  X(RootArg) = PointerOrNullToTerm(root);  
  if (block == BLOCK) {
    SET_BLOCKING(X(InvocationAttr));
  } else {
    SET_NONBLOCKING(X(InvocationAttr));
  }
  SET_EXECUTING(X(InvocationAttr));

  /* Save last dynamic top */
  X(PrevDynChpt) = PointerOrNullToTerm(TopConcChpt); 
  w->previous_choice = w->choice;

  CODE_CHOICE_NEW(w->choice, address_nd_current_instance);
  TopConcChpt = (choice_t *)w->choice;  /* Update dynamic top */

  DEBUG__TRACE(debug_concchoicepoints,
	       "***(%ld)(%ld) Exiting current_instance_conc, choice = %lx, previous_choice = %lx, conc. choice = %lx\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER,
	       (long)w->choice, (long)w->previous_choice, (long)TopConcChpt);
    
  w->ins = current_one;
  CINSNP__GOTO(w->ins->emulcode);
}

#if defined(USE_LOCKS)
 /* Releases the lock on a predicate; this is needed to ensure that a clause
    will not be changed while it is being executed. */

CVOID__PROTO_N(prolog_unlock_predicate, int_info_t *root) {
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) unlocking predicate, root is %p\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER, root);

#if defined(DEBUG_TRACE)
  if (root->behavior_on_failure == CONC_OPEN &&
      Cond_Lock_is_unset(root->clause_insertion_cond)) {
    /* TODO: remove debug_conc if this is a rtcheck and not a trace message */
    DEBUG__TRACE(debug_conc,
		 "WARNING: In unlock_predicate, root is %p, predicate is unlocked!!!!\n", 
		 root);
  }
#endif
  
  /* We have just finished executing an operation on a locked predicate;
     unlock the predicate and make sure the choicepoint is not marked as
     executing. */

  if (root->behavior_on_failure != DYNAMIC) {
    SET_NONEXECUTING((TopConcChpt->x[InvocationAttr])); 
    Wait_For_Cond_End(root->clause_insertion_cond);
  }
}

#else

/*
void init_dynamic_locks() {}
LOCK create_dynamic_lock(void) {return NULL;}
*/
CVOID__PROTO_N(prolog_unlock_predicate, int_info_t *root) {
  DEBUG__TRACE(debug_conc, "Using fake unlock_predicate!!!!\n");
}

#endif /* USE_LOCKS */ 


static bool_t wait_for_an_instance_pointer(instance_t **inst_pptr1,
					   instance_t **inst_pptr2,
					   int_info_t *root,
					   BlockingType block) {
  volatile instance_t *pptr1 = NULL, *pptr2 = NULL;

  /* We have to wait for a new clause only if we are blocking */

  while(TRUE) {
    /* Wait until a change is signaled, and test that the change affects us */

    if (block == BLOCK) {
      Wait_For_Cond_Begin( \
			   ((*inst_pptr1 == NULL) && \
			    (*inst_pptr2 == NULL) && \
			    root->behavior_on_failure == CONC_OPEN ), \
			   root->clause_insertion_cond \
			   );
    } else { /* In any case, leave the predicate locked */
      Cond_Begin(root->clause_insertion_cond);
    }
          
    /* Test again to find out which was the case */
      
    pptr1 = *inst_pptr1;
    pptr2 = *inst_pptr2;
    if (pptr1 || pptr2) 
      return TRUE;
    else if (block == NO_BLOCK || root->behavior_on_failure == CONC_CLOSED)
      return FALSE;
    else Wait_For_Cond_End(root->clause_insertion_cond); /*Let others update*/
  }
}

static instance_t *first_possible_instance(tagged_t x0,
					   int_info_t *root,
					   instance_t **x2_n, instance_t **x5_n) {
  instance_t *x2_chain;
  instance_t *x5_chain;
  instance_t *x2_next;
  instance_t *x5_next;

  /* TODO: good name? */
#define DUMMY_ACTIVE_INSTANCE(A,B,C) A

  CURRENT_INSTANCE(x0, root, DUMMY_ACTIVE_INSTANCE, { /* fail */
    return NULL;
  });

  *x2_n  = x2_next;
  *x5_n  = x5_next;

  return x2_chain ? x2_chain : x5_chain;
}

/* Current pointers to instances are x2_insp and x5_insp; look for a new
   pointer which presumably matches the query. */
#define LOCATE_NEXT_INSTANCE(X2_INSP, X5_INSP, IPP) { \
  if (!X2_INSP && !X5_INSP) { /* MCL */ \
    IPP = X2_INSP; \
  } else if (X2_INSP == X5_INSP) { /* normal = TRUE */ \
    IPP = X2_INSP; \
    X2_INSP = X5_INSP = X2_INSP->forward; \
  } else if (!X2_INSP) { /* normal = FALSE */ \
  x5_alt: \
    IPP = X5_INSP; \
    X5_INSP = X5_INSP->next_forward; \
  } else if (!x5_insp) { /* normal = FALSE */ \
  x2_alt: \
    IPP = X2_INSP; \
    X2_INSP = X2_INSP->next_forward; \
  } else if (X2_INSP->rank < X5_INSP->rank) { \
    goto x2_alt; \
  } else { \
    goto x5_alt; \
  } \
}

CINSNP__PROTO(next_instance_conc) {
  int_info_t *root = TaggedToRoot(X(RootArg));
  BlockingType block;      
  instance_handle_t *x2_ins_h, *x5_ins_h;
  bool_t next_instance_pointer;
  instance_t *x2_insp, *x5_insp;

  DEBUG__TRACE(debug_concchoicepoints,
	       "*** Entering next_instance_conc, choice = %lx, previous_choice = %lx, conc. choice = %lx\n",
	       (long)w->choice, (long)w->previous_choice, (long)TopConcChpt);

  /* = X(7) == atom_block ? BLOCK : NO_BLOCK;*/
  block = IS_BLOCKING(X(InvocationAttr)) ? BLOCK : NO_BLOCK; 

  /* When we baktrack after a call which did not finally succeed, the lock
     is still set. Unlock it before proceeding to the next clause. */

  if (EXECUTING(X(InvocationAttr))){
    DEBUG__TRACE(debug_concchoicepoints,
		 "*** in next_instance_conc changing to nonexecuting\n");
    SET_NONEXECUTING(X(InvocationAttr));
    Wait_For_Cond_End(root->clause_insertion_cond);
  }
  
  x2_ins_h = TaggedToInstHandle(X(X2_CHN));
  x5_ins_h = TaggedToInstHandle(X(X5_CHN));

/* x2_ins_h->inst_ptr and x5_ins_h->inst_ptr may be both NULL; that means no
   current instance is available.  Just wait for one.  If any of x2_insp or
   x5_insp are NULL pointer, they are automatically enqueued in root by
   change_handle_to_instance */

#if defined(USE_THREADS)
#if defined(DEBUG_TRACE)
  if (!root->first) {
    /* TODO: precondition or trace? */
    DEBUG__TRACE(debug_conc,
		 "*** %ld(%ld) in next_instance_conc without first instance.\n",
		 (long)Thread_Id, (long)GET_INC_COUNTER);
  }
#endif
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) in next_instance_conc with x2 = %lx, x5 = %lx, block = %lx\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER, 
	       (long)x2_ins_h->inst_ptr, (long)x5_ins_h->inst_ptr, (long)block);
#endif

  do {
    next_instance_pointer =
      wait_for_an_instance_pointer(&(x2_ins_h->inst_ptr), 
                                   &(x5_ins_h->inst_ptr),
                                   root, block);
    if (!next_instance_pointer) { /* Closed or non-waiting call */
      remove_handle(x2_ins_h, root, X2);
      remove_handle(x5_ins_h, root, X5);
      /* Time for new assertions */
      Wait_For_Cond_End(root->clause_insertion_cond);
      DEBUG__TRACE(debug_concchoicepoints,
		   "***(%ld)(%ld) Exiting current_instance_conc with failure, choice = %lx, previous_choice = %lx, conc. choice = %lx\n",
		   (long)Thread_Id, (long)GET_INC_COUNTER,
		   (long)w->choice, (long)w->previous_choice, (long)TopConcChpt);
      /* Remove choicepoint and fail */
      /* TODO: hmmm redundant? failure already manages choice points... (perhaps TopConcChpt is not... ) <- the code seems to be correct... */
      SetDeep();
      SetChoice(w->previous_choice);
      TopConcChpt = TermToPointerOrNull(choice_t, X(PrevDynChpt));
      CINSNP__FAIL;
    }

    /* Locate a satisfactory instance. */
    x2_insp = x2_ins_h->inst_ptr;
    x5_insp = x5_ins_h->inst_ptr;
    LOCATE_NEXT_INSTANCE(x2_insp, x5_insp, w->ins);

    /* Move handle forwards to re-start (if necesary) in a new clause */
    change_handle_to_instance(x2_ins_h, x2_insp, root, X2);
    change_handle_to_instance(x5_ins_h, x5_insp, root, X5);

#if defined(USE_THREADS)
#if defined(DEBUG_TRACE)
    if (!root->first) {
      /* TODO: precondition or trace? */
      DEBUG__TRACE(debug_conc,
		   "*** %ld(%ld) after jumping without first instance.\n",
		   (long)Thread_Id, (long)GET_INC_COUNTER);
    }
#endif
#endif

    if (!w->ins) /* Not instance -> release lock, continue in loop */
      Wait_For_Cond_End(root->clause_insertion_cond);
  } while (!w->ins);

#if defined(USE_THREADS)
#if defined(DEBUG_TRACE)
  if (!root->first) {
    /* TODO: precondition or trace? */
    DEBUG__TRACE(debug_conc,
		 "*** %ld(%ld) exiting n_i without first instance.\n",
		 (long)Thread_Id, (long)GET_INC_COUNTER);
  }
#endif  
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) exiting n_i with instance %lx.\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER, (long)w->ins);
#endif  

  /* Here with a possibly matching instance,
     a possibly empty next instance,
     and the lock on the instance. */

  w->choice->x[X2_CHN] = X(X2_CHN) = PointerOrNullToTerm(x2_ins_h);
  w->choice->x[X5_CHN] = X(X5_CHN) = PointerOrNullToTerm(x5_ins_h);
  SET_EXECUTING(X(InvocationAttr));
  DEBUG__TRACE(debug_concchoicepoints,
	       "***(%ld)(%ld) Exiting current_instance_conc, choice = %lx, previous_choice = %lx, conc. choice = %lx\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER,
	       (long)w->choice, (long)w->previous_choice, (long)TopConcChpt);

  /* w->ins is not NULL here */
  CINSNP__GOTO(w->ins->emulcode);
}

/****************************************************************************/

/* Add an invocation as pending from an instance; if there is anyone else
   pending on that instance, add ourselves to the list. */

instance_handle_t *make_handle_to(instance_t *inst,
				  int_info_t *root,
				  tagged_t head,
				  WhichChain chain) {
  instance_handle_t *this_handle;
  
  /* Create the handle */
  this_handle = CHECKALLOC(instance_handle_t);

  /* Allow re-indexation */
  this_handle->head = head;
  link_handle(this_handle, inst, root, chain);
#if defined(USE_THREADS)
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) made handle %lx to instance %lx\n", 
	       (long)Thread_Id, (long)GET_INC_COUNTER, (long)this_handle, (long)inst);
#endif

  return this_handle;
}

/* Remove handle from list.  xi might be pointed to directly from the root,
   or from an instance record: need to update the pointer itself. */

void remove_handle(instance_handle_t *xi,
		   int_info_t *root,
		   WhichChain chain) {
  unlink_handle(xi, root, chain);
  CHECKDEALLOC0(instance_handle_t, xi);

#if defined(USE_THREADS)
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) removed handle %lx (to instance %lx)\n", 
	       (long)Thread_Id, (long)GET_INC_COUNTER, (long)xi, (long)xi->inst_ptr);
#endif
}

/* Make a handle to point to a new instance. */

static void change_handle_to_instance(instance_handle_t *handle,
				      instance_t *new_inst,
				      int_info_t *root,
				      WhichChain chain) {
  if (handle->inst_ptr != new_inst) {      /* Do not move if not necessary */
    DEBUG__TRACE(debug_conc,
		 "*** %ld(%ld) changes handle %lx from instance %lx to %lx\n", 
		 (long)Thread_Id, (long)GET_INC_COUNTER, (long)handle,
		 (long)handle->inst_ptr, (long)new_inst);
    unlink_handle(handle, root, chain);
    link_handle(handle, new_inst, root, chain);
  }
}

static void link_handle(instance_handle_t *handle,
			instance_t *inst,
			int_info_t *root,
			WhichChain chain) {
#if defined(DEBUG_TRACE)
  if (Cond_Lock_is_unset(root->clause_insertion_cond)) {
    DEBUG__TRACE(debug_conc,
		 "*** Thread %ld(%ld) in link_handle() with lock unset!\n",
		 (long)Thread_Id, (long)GET_INC_COUNTER);
  }
#endif

  handle->inst_ptr = inst;             /* Instance we are looking at */
  handle->previous_handle = NULL;
  if (inst) {           /* Non-null instances go either to X2 or to X5... */
    if (chain == X2) {
      handle->next_handle = inst->pending_x2;
      inst->pending_x2 = handle;
    } else {
      handle->next_handle = inst->pending_x5;
      inst->pending_x5 = handle;
    }
  } else {                    /* handles to NULL instances go to the root */
    if (chain == X2) {
      handle->next_handle = root->x2_pending_on_instance;
      root->x2_pending_on_instance = handle;
    } else {
      handle->next_handle = root->x5_pending_on_instance;
      root->x5_pending_on_instance = handle;
    }
  }
  if (handle->next_handle)
    handle->next_handle->previous_handle = handle;
}

static void unlink_handle(instance_handle_t *xi,
			  int_info_t *root,
			  WhichChain chain) {
  instance_t *inst;

#if defined(DEBUG_TRACE)
  if (Cond_Lock_is_unset(root->clause_insertion_cond)) {
    DEBUG__TRACE(debug_conc,
		 "*** Thread_Id %ld(%ld) in unlink_handle() with lock unset!\n", 
		 (long)Thread_Id, (long)GET_INC_COUNTER);
  }
#endif

/* A handle is enqueued in X2 (unindexed links) or X5 (indexed links) iff it
   has a non-null instance pointer; otherwise, it must be enqueued in the
   root queue. */

  if ((inst = xi->inst_ptr)) {
    if (chain == X2 && inst->pending_x2 == xi)          /* First in queue */
      inst->pending_x2 = xi->next_handle;
    if (chain == X5 && inst->pending_x5 == xi)
      inst->pending_x5 = xi->next_handle;
  } else if (chain == X2) {             /* xi->inst_ptr is a NULL pointer */
    if (root->x2_pending_on_instance == xi)
      root->x2_pending_on_instance = xi->next_handle;
  } else {
    if (root->x5_pending_on_instance == xi)
      root->x5_pending_on_instance = xi->next_handle;
  }

  if (xi->next_handle)
    xi->next_handle->previous_handle = xi->previous_handle;
  if (xi->previous_handle)
    xi->previous_handle->next_handle = xi->next_handle;
}

/* Number of predicates asserted */
intmach_t num_of_predicates = 0;                                  /* Shared */

/* ------------------------------------------------------------------------- */

/* program_usage: [sizeof_used_space, 0] */
CBOOL__PROTO(program_usage) {
  tagged_t x;
  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,IntmachToTagged(mem_prog_count),x);
  CBOOL__LASTUNIFY(X(0),x);
}

/* internal_symbol_usage: [number_atoms_funcs_preds, number_pred_defs] */
CBOOL__PROTO(internal_symbol_usage) {
  tagged_t x;
  MakeLST(x,IntmachToTagged(num_of_predicates),atom_nil);
  MakeLST(x,IntmachToTagged(prolog_atoms->count),x);
  CBOOL__LASTUNIFY(X(0),x);
}

/* total_usage: [total_space, 0].  Changed to use total_mem_count (MCL) */
CBOOL__PROTO(total_usage) {
  tagged_t x;
  intmach_t n;

  /* n = (char *)sbrk(0) - mem_start; */
  n = total_mem_count;
  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,IntmachToTagged(n),x);
  CBOOL__LASTUNIFY(X(0),x);
}

statistics_t stats = {
  0, /*flt64_t ss_click*/
  0, /*intmach_t ss_global*/
  0, /*intmach_t ss_local*/
  0, /*intmach_t ss_control*/
  0, /*int64_t gc_click*/
  0, /*intmach_t gc_count*/
  0, /*intmach_t gc_acc*/
  0, /*intmach_t gc_longest_click*/
  0, /*int64_t startclick*/
  0, /*int64_t lastclick*/
  0, /*int64_t startwallclick*/
  0, /*int64_t lastwallclick*/
  0, /*int64_t startuserclick*/
  0, /*int64_t lastuserclick*/
  0, /*int64_t startsystemclick*/
  0, /*int64_t lastsystemclick*/

/* keep in mind that the next values can be redefined for a better
					 frequency */
  0, /*int64_t wallclockfreq*/
  0, /*int64_t userclockfreq*/
  0  /*int64_t systemclockfreq*/
};                                /* Shared */

/*-----------------------------------------------------------*/

CVOID__PROTO_N(trail_push_check, tagged_t x) {
  tagged_t *tr = G->trail_top;
  TrailPush(tr,x);
  G->trail_top = tr;
  TEST_CHOICE_OVERFLOW(w->choice, CHOICEPAD);
}

/*-----------------------------------------------------------*/

intmach_t copy_blob(tagged_t *src, tagged_t *dst) {
  functor_t f;
  intmach_t aligned_size;
  intmach_t i;

  /* get functor */
  f = *((functor_t *)src);
  src = HeapCharOffset(src, sizeof(functor_t));
  /* get size in bytes */
  aligned_size = BlobFunctorSizeAligned(f)+sizeof(functor_t);
  /* copy functor */
  *((functor_t *)dst) = f;
  dst = HeapCharOffset(dst, sizeof(functor_t));
  /* copy object in blob_unit_t chunks */
  for (i=sizeof(functor_t); i<aligned_size; i+=sizeof(blob_unit_t)) {
    blob_unit_t t;
    t = *((blob_unit_t *)src);
    src = HeapCharOffset(src, sizeof(blob_unit_t));
    *((blob_unit_t *)dst) = t;
    dst = HeapCharOffset(dst, sizeof(blob_unit_t));
  }
  return aligned_size;
}

/* compare the blobs pointed by a and b */
bool_t compare_blob(tagged_t *a, tagged_t *b) {
  intmach_t size;
  intmach_t i;
  functor_t fa;
  functor_t fb;
  fa = *((functor_t *)a);
  fb = *((functor_t *)b);
  if (fa != fb) return FALSE;
  size = BlobFunctorSize(fa)+sizeof(functor_t);
  a = HeapCharOffset(a, sizeof(functor_t));
  b = HeapCharOffset(b, sizeof(functor_t));
  for (i=sizeof(functor_t); i<size; i+=sizeof(blob_unit_t)) {
    if (*((blob_unit_t *)a) != *((blob_unit_t *)b)) return FALSE;
    a = HeapCharOffset(a, sizeof(blob_unit_t));
    b = HeapCharOffset(b, sizeof(blob_unit_t));
  }
  return TRUE;
}

#define ENSURE_LIVEINFO \
  RTCHECK({if (w->liveinfo == NULL) { PANIC_FAULT("null liveinfo"); }})

/* finish a bignum stored on top of the heap:
     - close the bignum and return a tagged word referencing it
     - or return a small integer tagged word (if it fits)
*/

CFUN__PROTO(bn_finish, tagged_t) {
  tagged_t *h;
  h = G->heap_top;
  /* todo[ts]: use blob functor size instead of BlobFunctorFix?? */
  if (*((functor_t *)h) == BlobFunctorFix32) {
    int32_t i;
    i = BignumGetInt32(HeapCharOffset(h, sizeof(functor_t)));
#if tagged__num_size < 32
    /* some int32_t are not small */
    if (!Int32IsSmall(i)) goto mirror_bignum;
#endif
    /* Return a small integer */
    CFUN__PROCEED(MakeSmall((intval_t)i));
#if tagged__num_size >= 32
  } else if (*((functor_t *)h) == BlobFunctorFix64) {
    int64_t i;
    i = BignumGetInt64(HeapCharOffset(h, sizeof(functor_t)));
    if (!Int64IsSmall(i)) goto mirror_bignum;
    /* Return a small integer */
    CFUN__PROCEED(MakeSmall(i));
#endif
  } else {
    goto mirror_bignum;
  }
 mirror_bignum:
  {
    /* Return a tagged bignum */
    intmach_t size;
    size = BlobFunctorSizeAligned(*((functor_t *)h))+sizeof(functor_t);
    *((functor_t *)HeapCharOffset(h, size)) = *((functor_t *)h);
    G->heap_top = HeapCharOffset(h, size+sizeof(functor_t));
    CFUN__PROCEED(Tagp(STR, h));
  }
}

CFUN__PROTO_N(bn_from_float_check, tagged_t, flt64_t f) {
  intmach_t req;

  ENSURE_LIVEINFO;
  req = bn_from_float(f, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn(LIVEINFO__HEAP(w->liveinfo)));
  if (req) {
    if (req == -1) {
      /* TODO: throw an exception or generate a big bignum? (see bn_from_float) */
      PANIC_FAULT("infinite bignum!");
    }
    CVOID__CALL_N(explicit_heap_overflow, (req+LIVEINFO__HEAP(w->liveinfo))*2, LIVEINFO__ARITY(w->liveinfo));
    if (bn_from_float(f, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn(LIVEINFO__HEAP(w->liveinfo)))) {
      PANIC_FAULT("miscalculated size of bignum");
    }
  }

  CFUN__LASTCALL(bn_finish);
}

/* declare a local bignum for ENSURE_BIGNUM */
#if defined(ALIGN_BLOB_64)
#define DECL_BIGNUM_FOR_INT32(XB) char XB[sizeof(functor_t) + sizeof(int32_t) + sizeof(int32_t)]
#else
#define DECL_BIGNUM_FOR_INT32(XB) char XB[sizeof(functor_t) + sizeof(int32_t)]
#endif
#define DECL_BIGNUM_FOR_INT64(XB) char XB[sizeof(functor_t) + sizeof(int64_t)]

#if tagged__num_size > 32
/* intval_t = int64_t */
#define DECL_BIGNUM_FOR_INTVAL(XB) DECL_BIGNUM_FOR_INT64(XB)
#else
/* intval_t = int32_t */
#define DECL_BIGNUM_FOR_INTVAL(XB) DECL_BIGNUM_FOR_INT32(XB)
#endif

/* pre: BN_X0 is STR(blob(bignum)) or NUM */
#define ENSURE_BIGNUM(BN_X, BN_XB, BN_XP) ({ \
  if (TaggedIsSTR((BN_X))) { \
    BN_XP = (bignum_t *)TagpPtr(STR,(BN_X)); \
  } else { /* TaggedIsSmall((BN_X)) */ \
    intval_t i; \
    char *ptr = &BN_XB[0]; \
    tagged_t *h = (tagged_t *)ptr; \
    i = GetSmall((BN_X)); \
    IntvalToBlobnm(h, i); \
    BN_XP = (bignum_t *)ptr; \
  } \
})

CFUN__PROTO_N(bn_call2, tagged_t, intmach_t (*f)(), tagged_t x0, tagged_t y0) {
  intmach_t req;
  DECL_BIGNUM_FOR_INTVAL(xb);
  DECL_BIGNUM_FOR_INTVAL(yb);
  bignum_t *x;
  bignum_t *y;

  /* TODO: a heap overflow here may have disastrous consequences!
       references to some terms are lost! */
  /* TODO: use a TaggedToFloat version that assumes that it is a float */
  if (IsFloat(x0)) x0 = CFUN__EVAL_N(bn_from_float_check, TaggedToFloat(x0));
  if (IsFloat(y0)) y0 = CFUN__EVAL_N(bn_from_float_check, TaggedToFloat(y0));

  ENSURE_BIGNUM(x0, xb, x);
  ENSURE_BIGNUM(y0, yb, y);

  ENSURE_LIVEINFO;
  req = (*f)(x, y, G->heap_top, Heap_Warn(LIVEINFO__HEAP(w->liveinfo)));
  if (req) {
    if (req == -1) {
      /* TODO: throw an exception or generate a big bignum? (see bn_from_float) */
      PANIC_FAULT("infinite bignum!");
    }
    /* TODO: this code is dirty... */
    intmach_t arity;
    arity = LIVEINFO__ARITY(w->liveinfo);
    X(arity) = x0;
    X(arity+1) = y0;
    CVOID__CALL_N(explicit_heap_overflow, (req+LIVEINFO__HEAP(w->liveinfo))*2, LIVEINFO__ARITY(w->liveinfo) + 2);
    x0 = X(arity);
    y0 = X(arity+1);
    ENSURE_BIGNUM(x0, xb, x);
    ENSURE_BIGNUM(y0, yb, y);
    if ((*f)(x, y, G->heap_top, Heap_Warn(LIVEINFO__HEAP(w->liveinfo)))) {
      PANIC_FAULT("miscalculated size of bignum");
    }
  }

  CFUN__LASTCALL(bn_finish);
}

CFUN__PROTO_N(bn_call1, tagged_t, intmach_t (*f)(), tagged_t x0) {
  intmach_t req;
  DECL_BIGNUM_FOR_INTVAL(xb);
  bignum_t *x;

  /* TODO: a heap overflow here may have disastrous consequences!
       references to some terms are lost! */
  /* TODO: use a TaggedToFloat version that assumes that it is a float */
  if (IsFloat(x0)) x0 = CFUN__EVAL_N(bn_from_float_check, TaggedToFloat(x0));

  ENSURE_BIGNUM(x0, xb, x);

  ENSURE_LIVEINFO;
  req = (*f)(x, G->heap_top, Heap_Warn(LIVEINFO__HEAP(w->liveinfo)));
  if (req) {
    if (req == -1) {
      /* TODO: throw an exception or generate a big bignum? (see bn_from_float) */
      PANIC_FAULT("infinite bignum!");
    }
    /* TODO: this code is dirty... */
    intmach_t arity;
    arity = LIVEINFO__ARITY(w->liveinfo);
    X(arity) = x0;
    CVOID__CALL_N(explicit_heap_overflow, (req+LIVEINFO__HEAP(w->liveinfo))*2, LIVEINFO__ARITY(w->liveinfo) + 1);
    x0 = X(arity);
    ENSURE_BIGNUM(x0, xb, x);
    if ((*f)(x, G->heap_top, Heap_Warn(LIVEINFO__HEAP(w->liveinfo)))) {
      PANIC_FAULT("miscalculated size of bignum");
    }
  }

  CFUN__LASTCALL(bn_finish);
}

#if defined(ALIGN_BLOB_64)
#define INT32_ALIGNED_BLOB_SIZE (2*sizeof(functor_t)+sizeof(int64_t))
#else
#define INT32_ALIGNED_BLOB_SIZE (2*sizeof(functor_t)+sizeof(int32_t))
#endif
#define INT64_ALIGNED_BLOB_SIZE (2*sizeof(functor_t)+sizeof(int64_t))

#define BLOB_CHECK_HEAP(AMOUNT) ({ \
  ENSURE_LIVEINFO; \
  TEST_HEAP_OVERFLOW(h, \
	             LIVEINFO__HEAP(w->liveinfo)+(AMOUNT), \
		     LIVEINFO__ARITY(w->liveinfo)); \
})

#if tagged__num_size < 32
/* this code is only necessary when some int32_t are not small
   integers */

CFUN__PROTO_N(int32_to_blob_check, tagged_t, int32_t i) {
  tagged_t *h;
  h = G->heap_top;
  BLOB_CHECK_HEAP(INT32_ALIGNED_BLOB_SIZE);
  HeapPush(h, BlobFunctorFix32);
  BignumPushInt32(h, i);
  HeapPush(h, BlobFunctorFix32);
  G->heap_top = h;
  CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT32_ALIGNED_BLOB_SIZE)));
}

CFUN__PROTO_N(int32_to_blob_nocheck, tagged_t, int32_t i) {
  tagged_t *h;
  h = G->heap_top;
  HeapPush(h, BlobFunctorFix32);
  BignumPushInt32(h, i);
  HeapPush(h, BlobFunctorFix32);
  G->heap_top = h;
  CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT32_ALIGNED_BLOB_SIZE)));
}
#endif

CFUN__PROTO_N(int64_to_blob_check, tagged_t, int64_t i) {
  if (Int64IsInt32(i)) {
    tagged_t *h;
    h = G->heap_top;
    BLOB_CHECK_HEAP(INT32_ALIGNED_BLOB_SIZE);
    HeapPush(h, BlobFunctorFix32);
    BignumPushInt32(h, (int32_t)i);
    HeapPush(h, BlobFunctorFix32);
    G->heap_top = h;
    CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT32_ALIGNED_BLOB_SIZE)));
  } else {
    tagged_t *h;
    h = G->heap_top;
    BLOB_CHECK_HEAP(INT64_ALIGNED_BLOB_SIZE);
    HeapPush(h, BlobFunctorFix64);
    BignumPushInt64(h, i);
    HeapPush(h, BlobFunctorFix64);
    G->heap_top = h;
    CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT64_ALIGNED_BLOB_SIZE)));
  }
}

CFUN__PROTO_N(int64_to_blob_nocheck, tagged_t, int64_t i) {
  if (Int64IsInt32(i)) {
    tagged_t *h;
    h = G->heap_top;
    HeapPush(h, BlobFunctorFix32);
    BignumPushInt32(h, (int32_t)i);
    HeapPush(h, BlobFunctorFix32);
    G->heap_top = h;
    CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT32_ALIGNED_BLOB_SIZE)));
  } else {
    tagged_t *h;
    h = G->heap_top;
    HeapPush(h, BlobFunctorFix64);
    BignumPushInt64(h, i);
    HeapPush(h, BlobFunctorFix64);
    G->heap_top = h;
    CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT64_ALIGNED_BLOB_SIZE)));
  }
}

/* pre: t is a STR(blob) */
/* todo[ts]: truncates bignums to int32_t, throw exception? */
int32_t blob_to_int32(tagged_t t) {
  SwBlob(t, functor, { /* STR(blob(float)) */
    flt64_t f;
    UnboxFlt64(HeapCharOffset(TagpPtr(STR, t), sizeof(functor_t)), f);
    return (int32_t)f;
  }, { /* STR(blob(bignum)) */
    return BignumGetInt32(HeapCharOffset(TagpPtr(STR, t), sizeof(functor_t)));
  });
}

/* pre: t is a STR(blob) */
/* todo[ts]: truncates bignums to int64_t, throw exception? */
int64_t blob_to_int64(tagged_t t) {
  SwBlob(t, functor, { /* STR(blob(float)) */
    flt64_t f;
    UnboxFlt64(HeapCharOffset(TagpPtr(STR, t), sizeof(functor_t)), f);
    return (int64_t)f;
  }, { /* STR(blob(bignum)) */
    if (functor == BlobFunctorFix32) {
      /* todo[ts]: bn_finish should make this case impossible! transform into a runtime check? */
      return (int64_t)BignumGetInt32(HeapCharOffset(TagpPtr(STR, t), sizeof(functor_t)));
    } else {
      return BignumGetInt64(HeapCharOffset(TagpPtr(STR, t), sizeof(functor_t)));
    }
  });
}

#define FLT64_ALIGNED_BLOB_SIZE (2*sizeof(functor_t)+sizeof(flt64_t))

CFUN__PROTO_N(flt64_to_blob_check, tagged_t, flt64_t i) {
  tagged_t *h;

  h = G->heap_top;
  ENSURE_LIVEINFO;
  TEST_HEAP_OVERFLOW(h,
		     LIVEINFO__HEAP(w->liveinfo)+FLT64_ALIGNED_BLOB_SIZE,
		     LIVEINFO__ARITY(w->liveinfo));
  HeapPush(h, BlobFunctorFlt64);
  HeapPushFlt64(h, i);
  HeapPush(h, BlobFunctorFlt64);
  G->heap_top = h;
  CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -FLT64_ALIGNED_BLOB_SIZE)));
}

CFUN__PROTO_N(flt64_to_blob_nocheck, tagged_t, flt64_t i) {
  tagged_t *h;

  h = G->heap_top;
  HeapPush(h, BlobFunctorFlt64);
  HeapPushFlt64(h, i);
  HeapPush(h, BlobFunctorFlt64);
  G->heap_top = h;
  CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -FLT64_ALIGNED_BLOB_SIZE)));
}

/* pre: t is a STR(blob) */
flt64_t blob_to_flt64(tagged_t t) {
  SwBlob(t, functor, { /* STR(blob(float)) */
    flt64_t f;
    UnboxFlt64(HeapCharOffset(TagpPtr(STR, t), sizeof(functor_t)), f);
    return f;
  }, { /* STR(blob(bignum)) */
    return bn_to_float((bignum_t *)TagpPtr(STR, t));
  });
}

/* create a new copy on the heap of the blob pointed by ptr */
CFUN__PROTO_N(make_blob, tagged_t, tagged_t *ptr) {
  intmach_t aligned_size;
  tagged_t *h;

  h = G->heap_top;
  aligned_size = copy_blob(ptr, h);
  /* todo[ts]: share code with bn_finish */
  *((functor_t *)HeapCharOffset(h, aligned_size)) = *((functor_t *)h);
  G->heap_top = HeapCharOffset(h, aligned_size+sizeof(functor_t));
  CFUN__PROCEED(Tagp(STR, h));
}

/*-------------------------------------------------------*/
/* Inserts the definition of a predicate */

#if defined(ABSMACH_OPT__functor_table)
definition_t *new_functor_ft(intmach_t atom, intmach_t arity)
#else
definition_t *new_functor(tagged_t functor)
#endif
{
  definition_t *func;
  func = CHECKALLOC(definition_t);
#if defined(ABSMACH_OPT__functor_table)
  func->atom = atom;
  func->arity = arity;
  func->string = ATMTAB_ATOM(atom)->name;
#else
  func->functor = functor;
#endif
#if defined(ABSMACH_OPT__profile_calls)
  func->number_of_calls = 0;
  func->time_spent = 0;
#endif
  func->properties.hardrtexp = 0;
  func->properties.spy = 0;
  func->properties.concurrent = 0;
  func->properties.dynamic = 0; 
  func->properties.multifile = 0;
  func->properties.nonvar = 0; 
  func->properties.var = 0;   
  func->code.raw = NULL;
  set_predtyp(func, ENTER_UNDEFINED);
  return func;
}

/*------------------------------------------------------------*/

void hashtab_expand(hashtab_t **psw, tagged_t otherwise) {
  hashtab_node_t *h1, *h2;
  intmach_t size;
  hashtab_t *oldsw;
  hashtab_t *newsw;
  intmach_t j;

  oldsw = *psw;
  size = HASHTAB_SIZE(oldsw);
  newsw = hashtab_new(size<<1, otherwise);

  for (j=size-1; j>=0; --j) {
    h1 = &(oldsw)->node[j];
    if (h1->key) {
      newsw->count++;
      h2 = hashtab_get(newsw,h1->key);
      h2->key = h1->key;
      h2->value.tagged = h1->value.tagged;
    }
  }

  (*psw) = newsw;

  CHECKDEALLOC0_TAILED(hashtab_t, oldsw);
  /* TODO: use a 'free' method? leave_to_gc(TABLE, (char *)oldsw); */
}

#if defined(ABSMACH_OPT__functor_table)
definition_t *insert_definition0_ft(intmach_t atom, intmach_t arity, bool_t insertp) 
#else
definition_t *insert_definition0(tagged_t functor, bool_t insertp) 
#endif
{
  hashtab_node_t *keyval;
  definition_t *value;
  hashtab_key_t key;

  /* Lock here -- we do not want two different workers to add predicates
     concurrently. */

#if !defined(ABSMACH_OPT__functor_table)
  Wait_Acquire_slock(prolog_predicates_l);
#endif
#if defined(ABSMACH_OPT__functor_table)
  key = (hashtab_key_t)FUNCTOR_KEY(atom, arity);
#else
  key = (hashtab_key_t)functor;
#endif
  keyval = hashtab_get(predicates_location, key);

  if (keyval->key) { /* Already existent */
    value = keyval->value.def;
  } else if (insertp) { /* New predicate */
#if defined(ABSMACH_OPT__functor_table)
    value = new_functor_ft(atom, arity);
#else
    value = new_functor(functor);
#endif
    keyval->key = key;
    keyval->value.def = value;
    predicates_location->count+=1;
    if (predicates_location->count * 2 > HASHTAB_SIZE(predicates_location)) {
      HASHTAB_EXPAND(&predicates_location);
    }
#if defined(ABSMACH_OPT__functor_table)
    if (arity == 0) {
      value->self0 = value;
    } else {
      value->self0 = insert_definition0_ft(atom, 0, TRUE);
    }
#endif
  } else {
    value = NULL;
  }
#if !defined(ABSMACH_OPT__functor_table)
  Release_slock(prolog_predicates_l);
#endif
  return value;
}

#if defined(ABSMACH_OPT__functor_table)
/* pre: X must be an atom */
tagged_t SetArity(tagged_t X, intmach_t A) { 
  if (A == 0) return X;
  return FuncFunctor(insert_definition0_ft(TaggedToFunc((X))->atom, A, TRUE));
}
tagged_t deffunctor(char *NAME, intmach_t ARITY) {
  return FuncFunctor(insert_definition0_ft(init_atom_check(NAME), ARITY, TRUE));
}
#else
definition_t *insert_definition(tagged_t functor, bool_t insertp) {
  return insert_definition0(functor,insertp);
}
#endif

/*------------------------------------------------------------*/

/* Create a most general term for a given functor or small int. */
CFUN__PROTO_N(make_structure, tagged_t, tagged_t functor) {
  intmach_t ar;
  tagged_t *h;

  ar = Arity(functor);
  if (ar==0 || !TaggedIsATM(functor)) {
    CFUN__PROCEED(functor);
  } else if (functor==functor_lst) {
    h = G->heap_top;
    ConstrHVA(h);
    ConstrHVA(h);
    G->heap_top = h;
    CFUN__PROCEED(Tagp(LST,HeapCharOffset(h,-2*sizeof(tagged_t))));
  } else {
    intmach_t i;
    h = G->heap_top;
    HeapPush(h,functor);
    i = ar;
    do {
      ConstrHVA(h);
    } while (--i);
    G->heap_top = h;
    CFUN__PROCEED(Tagp(STR,h-ar-1));
  }
}

/* Pre: term is STR(structure) or LST or ATM */
definition_t *find_definition(tagged_t term, tagged_t **argl, bool_t insertp) {
#if defined(ABSMACH_OPT__functor_table)
  Sw_STR_LST_ATM(term, { /* STR */
    tagged_t f = TaggedToHeadfunctor(term);
    *argl = TaggedToArg(term,1);
    return TaggedToFunc(f);
  }, { /* LST */ /* TODO: since all predicates are module qualified this case is strange! */
    *argl = TagpPtr(LST, term);
    return TaggedToFunc(functor_lst);
  }, { /* ATM */
    return TaggedToFunc(term);
  });
#else
  intmach_t arity;
  Sw_STR_LST_ATM(term, { /* STR */
    tagged_t f = TaggedToHeadfunctor(term);
    *argl = TaggedToArg(term,1);
    term = FUNCTOR_NAME(f);
    arity = Arity(f);
  }, { /* LST */ /* TODO: since all predicates are module qualified this case is strange! */
    *argl = TagpPtr(LST, term);
    term = atom_lst;
    arity = 2;
  }, { /* ATM */
    arity = 0;
  });
  return insert_definition(SetArity(term,arity),insertp);
#endif
}

/* Obtain the definition for a given predicate functor */
definition_t *resolve_definition(tagged_t complex) {
  tagged_t tagname,tagarity;
  if (!TaggedIsSTR(complex)) return NULL;
  if (TaggedToHeadfunctor(complex) != functor_slash) return NULL;
  DerefArg(tagname, complex, 1);
  DerefArg(tagarity, complex, 2);
  if (!TaggedIsSmall(tagarity)) return NULL;
  if (!TaggedIsATM(tagname)) return NULL;
  return LOOKUP_DEF(SetArity(tagname,GetSmall(tagarity)));
}

static void relocate_table_clocks(hashtab_t *sw, instance_clock_t *clocks) {
  hashtab_node_t *keyval;
  definition_t *d;
  intmach_t j = HASHTAB_SIZE(sw);
  
  for (--j; j>=0; --j) {
    keyval = &sw->node[j];
    if ((d = keyval->value.def) &&
        d->predtyp==ENTER_INTERPRETED)
      relocate_clocks(d->code.intinfo->first,clocks);	
  }
}

void relocate_clocks(instance_t *inst, instance_clock_t *clocks) {
  intmach_t i, j;
  instance_t *next;

  for (; inst; inst=next)
    {
      next = inst->forward;
      for (i=0; inst->birth>clocks[i]; i++)
	;
      inst->birth = i;
      if (inst->death!=0xffff)
	{
	  for (j=i; inst->death>clocks[j]; j++)
	    ;
	  inst->death = j;
	  if (i==j)
            expunge_instance(inst);
	}
    }
}

/* MCL: make sure we have a lock before expunging instances; this might 
   be done concurrently */

CBOOL__PROTO(prolog_purge) {
  instance_t *inst;
  intmach_t current_mem = total_mem_count;
  
  DEREF(X(0),X(0));
  inst = TaggedToInstance(X(0));

  Cond_Begin(inst->root->clause_insertion_cond);
  expunge_instance(inst);
  Broadcast_Cond(inst->root->clause_insertion_cond);

  INC_MEM_PROG((total_mem_count - current_mem));

  CBOOL__PROCEED;
}

/* Erase an instance. In the case of instances from concurrent predicates
   which are being pointed at, move the handle to the next available
   instance.  For now, fail if no matching instances are available.  When
   called for a concurrent predicate, it must be protected by a clause lock
   set at Prolog level.  Memory accounting: delay until we are done with
   concurrency-related pointer juggling. */

#define InstOrNull(Handle) Handle ? Handle->inst_ptr : NULL

CVOID__PROTO_N(prolog_erase_ptr, instance_t *node) {
  int_info_t *root;
  intmach_t current_mem;

#if defined(USE_THREADS)
  instance_handle_t *x2_ins_h, *x5_ins_h;
  instance_t *ipp, *x2_insp, *x5_insp;
#endif

  root = node->root;

#if defined(USE_THREADS)
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) entering prolog_erase()!\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER);
#if defined(DEBUG_TRACE)
  if (!root->first) {
    /* TODO: precondition or trace? */
    DEBUG__TRACE(debug_conc, 
		 "*** %ld(%ld) prolog_erase() without first instance!\n",
		 (long)Thread_Id, (long)GET_INC_COUNTER);
  }
#endif
#endif

#if defined(USE_THREADS)
 /* An instance is about to be deleted.  If the predicate is
    concurrent, and there are calls pointing at that instance, move
    the queue of pending calls to the new available instance.  In
    order to choose which clause is to be pointed at, any handle is
    equally valid; we use the first one.  This call must not block if
    no next instance exists: blocking is performed by
    '$current_instance'/1 . */
  if (root->behavior_on_failure != DYNAMIC) {
#if defined(USE_THREADS)
#if defined(DEBUG_TRACE)
    if (Cond_Lock_is_unset(root->clause_insertion_cond)) {
      DEBUG__TRACE(debug_conc,
		   "prolog_erase: entering for conc. pred. without lock!\n");
    }
#endif
#endif
    x2_ins_h = node->pending_x2;
    x5_ins_h = node->pending_x5;
    if (x2_ins_h || x5_ins_h) {
      x2_insp = InstOrNull(x2_ins_h);
      x5_insp = InstOrNull(x5_ins_h);
      LOCATE_NEXT_INSTANCE(x2_insp, x5_insp, ipp);

#if defined(USE_THREADS)
      DEBUG__TRACE(debug_conc,
		   "*** %ld(%ld) moving handles hanging from %lx\n",
		   (long)Thread_Id, (long)GET_INC_COUNTER, (long)node);
#endif

      if (ipp && (x2_insp || x5_insp)) {
        /* Make all the queues point to the next instance.  if x2_insp or
           x5_insp are null, make both to point to the same queue */
        if (x2_insp && x5_insp) {
          move_queue(&node->pending_x2, &x2_insp->pending_x2, x2_insp);
          move_queue(&node->pending_x5, &x5_insp->pending_x5, x5_insp);
        } else if (!x2_insp) {
          move_queue(&node->pending_x2, &x5_insp->pending_x5, x5_insp);
          move_queue(&node->pending_x5, &x5_insp->pending_x5, x5_insp);
        } else if (!x5_insp) {
          move_queue(&node->pending_x2, &x2_insp->pending_x2, x2_insp);
          move_queue(&node->pending_x5, &x2_insp->pending_x2, x2_insp);
        }
      } else {
        /* No next instance.  If the predicate is still concurrent,
           enqueue the list at the root and make it wait --- set the
           instance pointed to to NULL.  If the predicate is not
           concurrent, the call will faill because of the NULL
           pointer, and the handles will by deallocated by it.  */
        move_queue(&node->pending_x2, &root->x2_pending_on_instance, NULL);
        move_queue(&node->pending_x5, &root->x5_pending_on_instance, NULL);
      }
    }
  }
#endif

  current_mem = total_mem_count;
  node->death = use_clock = def_clock;
  if (root->behavior_on_failure != DYNAMIC ||                      /* MCL */
      node->birth == node->death)
    expunge_instance(node);
  else
    (void)CFUN__EVAL_N(active_instance,node,use_clock,TRUE);

  INC_MEM_PROG((total_mem_count - current_mem));

#if defined(USE_THREADS)
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) exiting prolog_erase()!\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER);
#endif
}

/*-----------------------------------------------------------------*/

/* Convert between internal and external instance ID's, i.e.
 * between integers and '$ref'(_,_).
 */
CBOOL__PROTO_N(instance_to_ref, instance_t *ptr, tagged_t t) {
  tagged_t *h;
  h = G->heap_top;
  HeapPush(h,functor_Dref);
  HeapPush(h,PointerToTerm(ptr));
  HeapPush(h,ptr->rank);
  G->heap_top = h;
  CBOOL__LASTUNIFY(Tagp(STR,HeapCharOffset(h,-3*sizeof(tagged_t))), t);
}
CFUN__PROTO_N(ref_to_instance, instance_t *, tagged_t x2) {
  tagged_t x1;
  instance_t *n;

  DerefSw_HVAorCVAorSVA_Other(x2,{
    CFUN__PROCEED(NULL);
  },{
    if (!TaggedIsSTR(x2)) CFUN__PROCEED(NULL);
    if (TaggedToHeadfunctor(x2) != functor_Dref) CFUN__PROCEED(NULL);

    DerefArg(x1,x2,1);
    DerefArg(x2,x2,2);
    if (!TaggedIsSmall(x1)) CFUN__PROCEED(NULL);
    n = TaggedToInstance(x1);
    if (!(n != NULL && n->rank == x2 && n->death == 0xffff)) CFUN__PROCEED(NULL);
    CFUN__PROCEED(n);
  });
}

#define DEBUG_TRACE_INSERT(C) \
  DEBUG__TRACE(debug_conc, \
	       "*** %ld(%ld) in insert%c (root = %lx, first = %lx, &first = %lx)\n", \
	       (long)Thread_Id, (long)GET_INC_COUNTER, (C), \
	       (unsigned long)root, \
	       (unsigned long)root->first, \
	       (unsigned long)&(root->first));

#define DEBUG_TRACE_LEAVING_INSERT(C) \
  DEBUG__TRACE(debug_conc, \
	       "*** %ld(%ld) leaving insert%c (root = %lx, first = %lx, &first = %lx)\n", \
	       (long)Thread_Id, (long)GET_INC_COUNTER, (C), \
	       (unsigned long)root, \
	       (unsigned long)root->first, \
	       (unsigned long)&(root->first));

/* If the predicate is concurrent, it is still open, it has no clauses
   (i.e., this first clause is also the last one) and there are invocations
   waiting for a clause, wake them up and make them point to the new clause.
   Unfortunately, this results in a lack of indexing. */

CBOOL__PROTO_N(inserta, int_info_t *root, instance_t *n) {
  instance_t **loc;
  intmach_t current_mem = total_mem_count;
#if defined(USE_THREADS)
  bool_t move_insts_to_new_clause = FALSE;
#endif

  Cond_Begin(root->clause_insertion_cond);

  DEBUG_TRACE_INSERT('a');

#if defined(USE_THREADS)
  if (root->behavior_on_failure == CONC_CLOSED) {
    Broadcast_Cond(root->clause_insertion_cond);
    USAGE_FAULT("$inserta in an already closed concurrent predicate");
  }
#endif

  /* (void)ACTIVE_INSTANCE(root->first,use_clock,TRUE); optional */
    
  if (!root->first) {
    n->rank = TaggedZero;
    n->forward = NULL;
    n->backward = n;
#if defined(USE_THREADS)
    if (root->behavior_on_failure == CONC_OPEN) {
      move_insts_to_new_clause = TRUE;    /* 'n' will be the new clause */
    }
#endif
  } else if (root->first->rank == TaggedLow) {
    PANIC_FAULT("database node full in assert or record");
  } else {
    n->rank = SmallSub(root->first->rank,1);
    n->forward = root->first;
    n->backward = root->first->backward;
    root->first->backward = n;
  }
  root->first = n;    
    
  n->root = root;
  n->birth = use_clock = def_clock;
  n->death = 0xffff;

#if defined(USE_THREADS)
  n->pending_x5 = n->pending_x2 = NULL;
#endif
    
  loc = (n->key==ERRORTAG ? &root->varcase :
	 n->key==functor_lst ? &root->lstcase :
	 &hashtab_lookup(&root->indexer,n->key)->value.instp);
    
  if (!(*loc)) {
    n->next_forward = NULL;
    n->next_backward = n;
  } else {
    n->next_forward = (*loc);
    n->next_backward = (*loc)->next_backward;
    (*loc)->next_backward = n;
  }
  (*loc) = n;
    
#if defined(USE_THREADS)
  if (move_insts_to_new_clause) {
    if (root->x2_pending_on_instance)
      move_queue(&root->x2_pending_on_instance, &n->pending_x2, n);
    if (root->x5_pending_on_instance)
      move_queue(&root->x5_pending_on_instance, &n->pending_x5, n);
  }
#endif

  DEBUG_TRACE_LEAVING_INSERT('a');

  Broadcast_Cond(root->clause_insertion_cond);
    
  INC_MEM_PROG((total_mem_count - current_mem));
  CBOOL__PROCEED;
}


/* ASSERT: X(0) is a small integer */
CBOOL__PROTO_N(insertz, int_info_t *root, instance_t *n) {
  instance_t **loc;
  intmach_t current_mem = total_mem_count;

  Cond_Begin(root->clause_insertion_cond);

  DEBUG_TRACE_INSERT('z');

#if defined(USE_THREADS)
  if (root->behavior_on_failure == CONC_CLOSED) {
    Broadcast_Cond(root->clause_insertion_cond);
    USAGE_FAULT("$insertz in an already closed concurrent predicate");
  }
#endif

  /* (void)ACTIVE_INSTANCE(root->first,use_clock,TRUE); optional */
    
  if (!root->first) {
    n->rank = TaggedZero;
    n->backward = n;
    root->first = n;
  } else if (root->first->backward->rank == TaggedIntMax) {
    PANIC_FAULT("database node full in assert or record");
  } else {
    n->rank = SmallAdd(root->first->backward->rank,1);
    n->backward = root->first->backward;
    root->first->backward->forward = n;
    root->first->backward = n;
  }

  n->root = root;
  n->birth = use_clock = def_clock;
  n->death = 0xffff;
  n->forward = NULL;
  n->next_forward = NULL;

#if defined(USE_THREADS)
  n->pending_x5 = n->pending_x2 = NULL;
#endif

  loc = (n->key==ERRORTAG ? &root->varcase :
	 n->key==functor_lst ? &root->lstcase :
	 &hashtab_lookup(&root->indexer,n->key)->value.instp);
    
  if (!(*loc)) {
    n->next_backward = n;
    (*loc) = n;
  } else {
    n->next_backward = (*loc)->next_backward;
    (*loc)->next_backward->next_forward = n;
    (*loc)->next_backward = n;
  }

#if defined(USE_THREADS)
#if defined(DEBUG_TRACE)
  if (root->behavior_on_failure != DYNAMIC) {
    DEBUG__TRACE(debug_conc,
		 "*** %ld(%ld) insertz'ed clause %lx\n",
		 (long)Thread_Id, (long)GET_INC_COUNTER, (long)n);
  }
#endif
#endif
    
#if defined(USE_THREADS)
  if (root->behavior_on_failure == CONC_OPEN){
    if (root->x2_pending_on_instance)
      move_queue(&root->x2_pending_on_instance, &n->pending_x2, n);
    if (root->x5_pending_on_instance)
      move_queue(&root->x5_pending_on_instance, &n->pending_x5, n);
  }
#endif

  DEBUG_TRACE_LEAVING_INSERT('z');

  Broadcast_Cond(root->clause_insertion_cond);

  INC_MEM_PROG((total_mem_count - current_mem));
  CBOOL__PROCEED;
}

/****************************************/

/* A LOGICAL VIEW OF DYNAMIC CODE UPDATES.
   Scheme adapted from
	T. Lindholm, R. A. O'Keefe, ``Efficient Implementation of a
	Defensible Semantics for Dynamic PROLOG Code'', Proc. 4th International
	Conference on Logic Programming, Melbourne, 1987.
   All modifications are my own inventions.
   Mats Carlsson.

   The idea is that an invocation of a dynamic predicate should not be
   affected by asserts/retracts until that invocation has finitely failed.
   This goes for record/recorded as well.

   Implementation: Each instance has an interval [birth,death) and there are
   two clocks DC and UC, initially zero.
   to add an instance: set its interval to [DC,0xffff), set UC=DC.
   to erase an instance: set its death to DC, set UC=DC,
                         if (birth==death) reclaim space.
   to use an instance: UC must be in its interval.
   to create a dynamic chpt: save UC in chpt, set DC=UC+1,
                             if (DC==0xffff) clock_overflow().

   Reclamation of space is "lazy": whenever we come across a doomed 
   instance, we try to delete a sequence of doomed instances.
   We have to inspect the chpt stack to find out whether it's safe
   to delete.

   N.B.  Instances with an empty interval can be deleted right away,
         provided that dynamic chpts take care to set the "next alt."
         at an instance relevant for the chpt.

   To reduce scanning costs, the chpts are marked as "static" up to
   the first dynamic chpt, starting from the root.
*/

/* Given an instance and a time T, skip over a sequence of instances
   not active at time T.  Return first active or NULL.
   While skipping, deallocate DEAD instances satisfying:
   either (1a) it is not downstream of any chpt for same root, or
          (1b) it was born after any such chpt, or
          (1c) it died before any such chpt.
	  */

/* if normal==TRUE follow first-chain else next_forward */
CFUN__PROTO_N(active_instance, instance_t *, instance_t *i, intmach_t itime, bool_t normal) {
  choice_t *b;
  instance_t *j;
  choice_t *b2;
  choice_t *latest_static = w->choice;
  instance_clock_t time = itime;
  instance_clock_t lotime = time;
  tagged_t lorank = TaggedIntMax;

  /* ensure that w->choice is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* We are still in "shallow mode" */
  /* Pre: !IsDeep() */
  CODE_MAYBE_NECK_TRY();

  if (!latest_static->next_alt) /* if called from wam() */
    latest_static = w->previous_choice;

  for (b=latest_static; !ChoiceptTestStatic(b); b=b2)  {
    b2=ChoiceCont(b);
    if (b->next_alt == address_nd_current_instance) {
      latest_static = b2;
      j = TaggedToInstance(b->x[X2_CHN]);
      if (j && (j->root==i->root)) {
        lotime = GetSmall(b->x[ClockSlot]);
        if (lorank>j->rank) lorank=j->rank;
      }
      j = TaggedToInstance(b->x[X5_CHN]);
      if (j && (j->root==i->root)) {
        lotime = GetSmall(b->x[ClockSlot]);
        if (lorank>j->rank) lorank=j->rank;
      }	  
    }
  }
  
  /* Mark all chpt before latest_static as static */

  for (b=latest_static;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b)) {
    ChoiceptMarkStatic(b);
  }
  
  if (normal) {
    /* Follow forward-chain ? */
    while (i &&
	   i->death != 0xffff &&
	   (lotime >= i->death ||
	    time < i->birth ||
	    (time >= i->death && lorank > i->rank)))  {
      j=i->forward;
      expunge_instance(i);
      i=j;
    }
    
    while (i && (time < i->birth || time >= i->death)) i=i->forward;
  } else {
    /* follow next_forward-chain ! */
    while (i &&
	   i->death != 0xffff &&
	   (lotime >= i->death ||
	    time < i->birth ||
	    (time >= i->death && lorank > i->rank))) {
      j=i->next_forward;
      expunge_instance(i);
      i=j;
    }
    
    while (i && (time < i->birth || time >= i->death)) i=i->next_forward;
  }
  CFUN__PROCEED(i);
}

/* Called from wam() when X(4) = use_clock = 0xfffe;
   All timestamps have to be compressed.
   Collect in T0..Tn distinct clock values existing in choicepoints,
   counting Tn=0xfffe.  Then compress all values in choicepoints and
   instances as:
   
   x in [0..T0] => 0
   x in (T0..T1] => 1
   ...

   Instances may get an empty lifetime; then they are expunged.

   Set use_clock = X(4) = n,
       def_clock = n+1
*/

CVOID__PROTO(clock_overflow) {
  instance_clock_t *clocks, *clockp;
  instance_clock_t t, current = 0xfffe;
  intmach_t count = 1;
  choice_t *b;

#if defined(USE_THREADS)
  DEBUG__TRACE(debug_conc, "*** in clock_overflow()\n");
#endif

  /* count # distinct clock values existing in choicepoints */
  for (b=w->previous_choice;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b)) {
    if (b->next_alt == address_nd_current_instance) {
      t = GetSmall(b->x[ClockSlot]);
      if (current!=t)
        current=t, count++;
    }
  }

  /* grab space for array of clock values */
  /* TODO: align TEST_HEAP_OVERFLOW automatically to word size */
  TEST_HEAP_OVERFLOW(G->heap_top, ((count*sizeof(instance_clock_t)+sizeof(tagged_t)-1)/sizeof(tagged_t))*sizeof(tagged_t), DynamicPreserved);
  clocks = (instance_clock_t *)G->heap_top;

  /* fill in distinct chpt clock values, relocating them as we go */
  clockp = clocks+count;
  *(--clockp) = 0xfffe;
  def_clock = count;
  use_clock = count-1;
  X(4) = MakeSmall(count-1);
  
  for (b=w->previous_choice;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b)) {
    if (b->next_alt == address_nd_current_instance) {
      t = GetSmall(b->x[ClockSlot]);
      if ((*clockp)!=t) *(--clockp)=t;
      b->x[ClockSlot] = MakeSmall(clockp-clocks);
    }
  }
  /* relocate all instance clocks */
  relocate_table_clocks(predicates_location,clocks);
  relocate_gcdef_clocks(clocks);
}

/* Remove the linked chains which point to the calls to concurrent
   predicates which were suspended.  Start at topdynamic (topmost dynamic
   choicepoint) and go down the choicepoint stack until the next dynamic
   choicepoint to be considered is older than chpttoclear.  Then, return the
   value of that dynamic choicepoint in the variable topdynamic (it is the
   topmost dynamic choicepoint after the call!). */

void remove_link_chains(choice_t **topdynamic, choice_t *chpttoclear) {
  choice_t *movingtop = *topdynamic;
#if defined(USE_THREADS)
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) removing from %lx until %lx\n", 
	       (long)Thread_Id, (long)GET_INC_COUNTER, 
	       (unsigned long)*topdynamic,
	       (unsigned long)chpttoclear);
#endif
  
  while (ChoiceYounger(movingtop, chpttoclear)) {
#if defined(USE_THREADS)
    DEBUG__TRACE(debug_conc,
		 "*** %ld(%ld) removing handle at (dynamic) choice %lx\n", 
		 (long)Thread_Id, (long)GET_INC_COUNTER, 
		 (unsigned long)movingtop);
#endif

    Cond_Begin(TaggedToRoot(movingtop->x[RootArg])->clause_insertion_cond);

    RTCHECK({
      if (TaggedToInstHandle(movingtop->x[X2_CHN]) == NULL)
	TRACE_PRINTF("*** %ld(%ld) remove_link_chains: X2 handle is NULL!!\n",
		(long)Thread_Id, (long)GET_INC_COUNTER);
    });
    RTCHECK({
      if (TaggedToInstHandle(movingtop->x[X5_CHN]) == NULL)
	TRACE_PRINTF("*** %ld(%ld) remove_link_chains: X5 handle is NULL!!\n",              (long)Thread_Id, (long)GET_INC_COUNTER);
    });

    remove_handle(TaggedToInstHandle(movingtop->x[X2_CHN]), 
                  TaggedToRoot(movingtop->x[RootArg]),
                  X2);
    remove_handle(TaggedToInstHandle(movingtop->x[X5_CHN]), 
                  TaggedToRoot(movingtop->x[RootArg]),
                  X5);

    Broadcast_Cond(TaggedToRoot(movingtop->x[RootArg])->clause_insertion_cond);

    movingtop=TermToPointerOrNull(choice_t, movingtop->x[PrevDynChpt]);
  }
#if defined(USE_THREADS)
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) remove_link_chains: done at %lx\n", 
	       (long)Thread_Id, (long)GET_INC_COUNTER, 
	       (unsigned long)movingtop);
#endif
  *topdynamic = movingtop;
}

CVOID__PROTO_N(make_undefined, definition_t *f) {
  /*Wait_Acquire_slock(prolog_predicates_l);*/
  if (f->predtyp==ENTER_INTERPRETED) {
    /* erase as much as possible */
    instance_t *i, *j;

    for (i = f->code.intinfo->first; i; i=j) {
      j = i->forward;
      if (i->death==0xffff) {
        i->death = use_clock = def_clock;
        if (i->birth==i->death) { 
	  /* make_undefined() is called from abolish() and
	     define_predicate(), which in turn are called directly
	     from Prolog and do not put any lock.  When reloading
	     Prolog code, the existent clauses (including those of
	     concurrent predicates) are erased, so we better put a
	     lock on those predicates.  MCL.
	  */
          Cond_Begin(f->code.intinfo->clause_insertion_cond);
          expunge_instance(i);
          Broadcast_Cond(f->code.intinfo->clause_insertion_cond);
        }
      }
    }

    Cond_Begin(f->code.intinfo->clause_insertion_cond);
    (void)ACTIVE_INSTANCE(f->code.intinfo->first,use_clock,TRUE);
    Broadcast_Cond(f->code.intinfo->clause_insertion_cond);
  }

  leave_to_gc(f->predtyp, (char *)f->code.intinfo);

  f->properties.hardrtexp = 0;
  f->properties.spy = 0;
  f->properties.concurrent = 0;
  f->properties.dynamic = 0; 
  f->properties.multifile = 0;
  f->properties.nonvar = 0; 
  f->properties.var = 0;   
  set_predtyp(f,ENTER_UNDEFINED);

  /*Release_slock(prolog_predicates_l);*/
}

/* Really get rid of abolished predicate. */
CBOOL__PROTO(empty_gcdef_bin) {
  gcdef_t *g;
  intmach_t current_mem = total_mem_count;

  while (gcdef_count>0)  {
    g = &gcdef_bin[--gcdef_count];
    free_info(g->gctype, g->info);
  }
  INC_MEM_PROG((total_mem_count - current_mem));

  CBOOL__PROCEED;
}

/* JFMC: abolish/1 predicate calls abolish C function. */
CBOOL__PROTO(prolog_abolish) {
  tagged_t *junk;
  definition_t *f;

  DEREF(X(0),X(0));
  f = find_definition(X(0),&junk,FALSE);
  CBOOL__LASTCALL_N(abolish, f);
}

/* Make a predicate undefined.  Also, forget spypoints etc. */
CBOOL__PROTO_N(abolish, definition_t *f) {
  intmach_t current_mem = total_mem_count;
  /* MCL: abolish/1 must succeed even in the case of undefined predicates */
  if (f == NULL) CBOOL__PROCEED;
  CBOOL__TEST(f->predtyp <= ENTER_INTERPRETED);
  if (f->predtyp != ENTER_UNDEFINED) {
    f->properties.spy = 0;
    f->properties.hardrtexp = 0;
    CVOID__CALL_N(make_undefined, f);
    num_of_predicates--;
  }
  INC_MEM_PROG((total_mem_count - current_mem));
  CBOOL__PROCEED;
}

CBOOL__PROTO(erase_clause) {
  intmach_t current_mem = total_mem_count;

  DEREF(X(0),X(0));
  free_emulinfo(TaggedToEmul(X(0)));
  INC_MEM_PROG((total_mem_count - current_mem));

  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------------- */
/* Define an interpreted predicate.  It is open iff it is concurrent. */

void predicate_def__interpreted(definition_t *f, bool_t concurrent) {
  int_info_t *d;
  d = CHECKALLOC(int_info_t);

  /*f->code.intinfo->clause_insertion_cond = create_dynamic_lock();*/
  Init_Cond(d->clause_insertion_cond);

  /*  MCL added on 26 Nov 98 */
  d->x2_pending_on_instance = NULL;
  d->x5_pending_on_instance = NULL;

  d->first = NULL;
  d->varcase = NULL;
  d->lstcase = NULL;
  d->indexer = HASHTAB_NEW(2);

  f->code.intinfo = d;

  f->properties.dynamic = 1;
  f->properties.concurrent = (concurrent != 0);
  /* set the runtime behavior */
  f->code.intinfo->behavior_on_failure =
#if defined(USE_THREADS)
    concurrent ? CONC_OPEN : DYNAMIC;
#else
    DYNAMIC;
#endif

  set_predtyp(f, ENTER_INTERPRETED);
}

void predicate_def__incore(definition_t *f) {
  incore_info_t *d;
  d = CHECKALLOC(incore_info_t);
  d->clauses = NULL;
  d->clauses_tail = &d->clauses;
  d->varcase = fail_alt;
  d->lstcase = NULL; /* Used by native preds to hold nc_info */
  d->othercase = NULL; /* Used by native preds to hold index_clause */
  f->code.incoreinfo = d;
  f->properties.nonvar = 0;
  f->properties.var = 0;
  set_predtyp(f, ENTER_COMPACTCODE);
}

/* --------------------------------------------------------------------------- */ 

CVOID__PROTO_N(choice_overflow, intmach_t pad, bool_t remove_trail_uncond);
CVOID__PROTO_N(number_to_string, tagged_t term, intmach_t base);

CVOID__PROTO_N(push_choicept, try_node_t *alt);
CVOID__PROTO(pop_choicept);

/* Copy a term in a remote worker to the local worker.  Returns the local
   term pointer.  It has (nontermination) problems when copying structures
   with self references. */

CBOOL__PROTO(prolog_copy_term);

CFUN__PROTO_N(cross_copy_term, tagged_t, tagged_t remote_term) {
  bool_t ok MAYBE_UNUSED;
  X(0) = remote_term;
  LoadHVA(X(1), G->heap_top);
  ok = CFUN__EVAL(prolog_copy_term);
  RTCHECK({
    /* TODO: raise an exception! */
    if (!ok) fprintf(stderr, "Could not copy term in cross_copy_term!!!!\n");
  });
  CFUN__PROCEED(X(1));
}

/* ---------------------------------------------------------------- */
/*         BUILTIN C PREDICATES                                     */
/* ---------------------------------------------------------------- */

CBOOL__PROTO(prompt) {
  CBOOL__UnifyCons(current_prompt,X(0));
  DEREF(current_prompt,X(1)); 
  CBOOL__PROCEED;
}

CBOOL__PROTO(unknown) {
  CBOOL__UnifyCons(current_unknown,X(0));
  DEREF(current_unknown,X(1)); 
  CBOOL__PROCEED;
}

/* $setarg(+I, +Term, +Newarg, +Mode):
 * Replace (destructively) I:th arg of Term by Newarg.
 * Mode=on -> dereference, undo on backtracking;
 * Mode=off -> we're undoing now, don't dereference;
 * Mode=true -> dereference, don't undo later.
 *
 * Put in at the express request of Seif Haridi.
 */
CBOOL__PROTO(setarg) {
  tagged_t t1, t2, *ptr;
  tagged_t oldarg, number, complex, newarg, *x;
  
  number = X(0);
  complex = X(1);
  newarg = X(2);
  DEREF(X(3),X(3));
  
  if (X(3) != atom_off) {
    DerefSw_HVAorCVAorSVA_Other(number,{goto barf1;},{});
    DerefSw_HVAorCVAorSVA_Other(complex,{goto barf2;},{});
    DerefSw_HVAorCVAorSVA_Other(newarg,{goto unsafe_value;},{});
  } else {
  unsafe_value:
    if (TaggedIsSVA(newarg)){
      ptr = G->heap_top;
      LoadHVA(t1,ptr);
      G->heap_top = ptr;
      BindSVA(newarg,t1);
      newarg = t1;
    }
  }

  SwOnTagS(complex, f, { /* LST */
    if (number==MakeSmall(1)) {
      ptr = TaggedToCar(complex);
    } else if (number==MakeSmall(2)) {
      ptr = TaggedToCdr(complex);
    } else {
      goto barf1;
    }
  }, { /* STR(blob) */
    goto barf1;
  }, { /* STR(struct) */
    intmach_t i;
    i = GetSmall(number);
    if (i<=0 || i>Arity(f)) goto barf1;
    ptr = TaggedToArg(complex,i);
  }, { /* Other */
    goto barf2;
  });
  
  oldarg = *ptr;
  DEBUG__TRACE(debug_setarg,
	       "{setarg: %spatching %p with 0x%lx (old was 0x%lx)}\n",
	       X(3) == atom_on ? "" : "un",
	       ptr,
	       (unsigned long)newarg,
	       (unsigned long)oldarg);
  *ptr = newarg;

  if ((X(3)==atom_on) && CondHVA(Tagp(HVA, ptr))) {
    /* undo setarg upon backtracking */
    tagged_t *limit = w->choice->trail_top;
    
    /* check first if location already trailed is same segment */
    /* TODO: this makes suite/wave upd arrays (without cut in
       initialize_leaves) very slow! */
    t1 = Tagp(HVA, ptr);

    for (x=G->trail_top; TrailYounger(x,limit);) {
      TrailDec(x);
      t2 = *(x);
      if (t1 == t2) {
	DEBUG__TRACE(debug_setarg, "{setarg: not trailed (1)}\n");
        CBOOL__PROCEED;
      } else if (TaggedIsSTR(t2) &&
		 TaggedToHeadfunctor(t2)==functor_Dsetarg) {
	/* TODO: this extra test makes setarg of conditional vars 7
	   times slower */
	tagged_t mutated;
	intmach_t i;
	tagged_t *ptr2;
	mutated = *TaggedToArg(t2,2);
	i = GetSmall(*TaggedToArg(t2,1));
	ComplexToArg(ptr2, mutated, i);
	if (ptr2 == ptr) {
	  DEBUG__TRACE(debug_setarg, "{setarg: not trailed (2)}\n");
	  CBOOL__PROCEED;
	}
      }
    }
    
    DEBUG__TRACE(debug_setarg, "{setarg: trailing}\n");
    
    ptr = G->heap_top;
    t2 = Tagp(STR,ptr);
    HeapPush(ptr,functor_Dsetarg);
    HeapPush(ptr,number);
    HeapPush(ptr,complex);
    HeapPush(ptr,oldarg);
    HeapPush(ptr,atom_off);
    TrailPush(G->trail_top,t2);
    G->heap_top = ptr;

    /* TODO: the following line is wrong... but check if it can be
       fixed (it would help to find the 'ptr' in gc code without
       inspecting the Dsetarg functor */
    /* ** trail smashed location for segmented GC ** */
    /*    TrailPush(G->trail_top,t1); */

    TEST_CHOICE_OVERFLOW(w->choice, CHOICEPAD);
  }
  
  CBOOL__PROCEED;
  
 barf1:
  MINOR_FAULT("setarg/3: incorrect 1st argument");
  
 barf2:
  MINOR_FAULT("setarg/3: incorrect 2nd argument");
}

CBOOL__PROTO(undo) {
  tagged_t goal;
  goal = X(0);
  DerefSw_HVAorCVAorSVA_Other(goal,{
    MINOR_FAULT("$undo/1: invalid argument");
  },{
    /* TODO: accepting atoms require modifying the GC code that treats
       the trail */
    if (!TaggedIsSTR(goal)) { MINOR_FAULT("$undo/1: invalid argument"); }
    TrailPush(G->trail_top,goal);
    TEST_CHOICE_OVERFLOW(w->choice, CHOICEPAD);
    CBOOL__PROCEED;
  });
}

CBOOL__PROTO(prolog_global_vars_set_root) {
  /* Warning: program must not backtrack after executing this code */
  DEREF(GLOBAL_VARS_ROOT,X(0));
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_global_vars_get_root) {
  CBOOL__LASTUNIFY(GLOBAL_VARS_ROOT,X(0));
}

/* x1 = constraints on x0, or '[]' if there are none */
CBOOL__PROTO(frozen) {
  DEREF(X(0),X(0));
  Sw_CVA_HVAorSVA_Other(X(0), { /* CVA */
    CBOOL__LASTUNIFY(Tagp(LST,TaggedToGoal(X(0))),X(1));
  }, { /* HVA SVA */
    CBOOL__UnifyCons(atom_nil,X(1));
    CBOOL__PROCEED;
  }, { /* Other */
    CBOOL__FAIL;
  });
}

CBOOL__PROTO(defrost) {
  tagged_t t;
  tagged_t *h;

  h = G->heap_top;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (X(1)==atom_nil) {
    LoadHVA(t,h);
  } else {
    LoadCVA(t,h);
    HeapPush(h,*TaggedToCar(X(1)));
    HeapPush(h,*TaggedToCdr(X(1)));
  }
  BindCVANoWake(X(0),t);
  G->heap_top = h;
  CBOOL__PROCEED;
}

CBOOL__PROTO(ferror_flag) {
  CBOOL__UnifyCons(current_ferror_flag,X(0));
  DEREF(current_ferror_flag,X(1)); 
  CBOOL__PROCEED;
}

CBOOL__PROTO(quiet_flag) {
  CBOOL__UnifyCons(current_quiet_flag,X(0));
  DEREF(current_quiet_flag,X(1)); 
  CBOOL__PROCEED;
}

bool_t prolog_init_radix();

CBOOL__PROTO(prolog_radix) {
  CBOOL__UnifyCons(current_radix,X(0));
  DEREF(current_radix,X(1));
  prolog_init_radix();
  CBOOL__PROCEED;
}

CBOOL__PROTO(constraint_list) {
  intmach_t pad;
  
  tagged_t *h;
  tagged_t l, v, clist;
  
  DEREF(X(0),X(0));
  pad = HeapCharAvailable(G->heap_top);
  while ((CFUN__EVAL_N(find_constraints, TaggedToPointer(X(0))) * 2*sizeof(tagged_t))+CONTPAD > pad) {
    l = *G->trail_top;
    while (l!=atom_nil) {
      v = l;
      l = *TagpPtr(CVA, v);
      *TagpPtr(CVA, v) = v;
    }
    pad *= 2;
    CVOID__CALL_N(explicit_heap_overflow,2*pad,2);
  }
  h = G->heap_top;
  l = *G->trail_top;
  clist = atom_nil;
  while (l!=atom_nil) {
    v = l;
    l = *TagpPtr(CVA, v);
    *TagpPtr(CVA, v) = v;
    HeapPush(h,v);
    HeapPush(h,clist);
    clist = Tagp(LST,HeapCharOffset(h,-2*sizeof(tagged_t)));
  }
  G->heap_top = h;
  CBOOL__LASTUNIFY(clist,X(1));
}

CFUN__PROTO_N(find_constraints, intmach_t, tagged_t *limit) {
  choice_t *purecp; /* oldest CVA-free cp */
  choice_t *cp;
  intmach_t found = 0;
  
  cp = purecp = ChoiceNext0(w->choice,0);
  cp->next_alt = fail_alt;
  cp->flags = 0;
  cp->trail_top = G->trail_top;
  cp->heap_top = G->heap_top;
  *G->trail_top = atom_nil;
  while (limit<cp->heap_top) {
    choice_t *prevcp;

    prevcp = ChoiceCont(cp);
      
    if (1 /* !ChoiceptTestNoCVA(cp)*/) {
      tagged_t *h = prevcp->heap_top; 

      if (h<limit) h = limit;
      while (h<cp->heap_top) {
	tagged_t v = *h++;
	      
	if (BlobHF(v)) {
	  h = HeapCharOffset(h, BlobFunctorSizeAligned(v)+sizeof(functor_t));
	} else {
	  if (TaggedIsATM(v)) {
	    h += Arity(v);
	  } else if (v == Tagp(CVA, h - 1)) {
	    h[-1] = *G->trail_top;
	    *G->trail_top = v;
	    found++;
	    h += 2;
	    purecp = prevcp;
	  }
	}
      }
      /* Christian Holzbaur pointed out that this is unsafe, e.g.
	 | ?- dif(X,1), (true; fail), (X=2; frozen(X,Fr)).
	 if (purecp!=prevcp && limit<=prevcp->heap_top)
	 ChoiceptMarkNoCVA(cp); */
    }
      
    cp = prevcp;
  }
  
  CFUN__PROCEED(found);
}

/* support for circularity checks:
   $eq(X,Y) :- X==Y, occupies_same_location(X,Y).
 */
CBOOL__PROTO(prolog_eq) {
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  CBOOL__LASTTEST(X(0)==X(1));
}

int_info_t *current_clauses_aux(tagged_t head) {
  if (!IsVar(head)) {
    tagged_t *junk;
    definition_t *d;

    d = find_definition(head, &junk, FALSE);
    if ((d!=NULL) && (d->predtyp==ENTER_INTERPRETED)) {
      return d->code.intinfo;
    }
  }
  return NULL; 
}

/***************************************************************************/
/* Dynamic prolog database update */
/* (C coded) --jfran */

#define GET_ROOT \
  DEREF(X(2), X(2)); \
  int_info_t *root; \
  root = TaggedToRoot(X(2));

/* Fails if not interpreted */
#define HEAD_ROOT \
  DEREF(X(0), X(0)); \
  int_info_t *root; \
  root = current_clauses_aux(X(0)); \
  CBOOL__TEST(root != NULL);

/* Fails if not interpreted */
#define HEAD_ROOT_INSNP \
  DEREF(X(0), X(0)); \
  int_info_t *root; \
  root = current_clauses_aux(X(0)); \
  if (root == NULL) CINSNP__FAIL;

CFUN__PROTO(compile_term_aux, instance_t *);
#define COMPILE_TERM \
  instance_t *ptr; \
  ptr = CFUN__EVAL(compile_term_aux);

// '$asserta_root'/3
CBOOL__PROTO(prolog_asserta_root) {
  GET_ROOT;
  COMPILE_TERM;
  CBOOL__CALL_N(inserta, root, ptr);
  CBOOL__PROCEED;
}
// '$asserta'/2
CBOOL__PROTO(prolog_asserta) {
  HEAD_ROOT;
  COMPILE_TERM;
  CBOOL__CALL_N(inserta, root, ptr);
  CBOOL__PROCEED;
}
// '$asserta_ref'/3
CBOOL__PROTO(prolog_asserta_ref) {
  HEAD_ROOT;
  COMPILE_TERM;
  CBOOL__CALL_N(inserta, root, ptr);
  CBOOL__CALL_N(instance_to_ref, ptr, X(2));
  CBOOL__PROCEED;
}
// '$assertz_root'/3
CBOOL__PROTO(prolog_assertz_root) {
  GET_ROOT;
  COMPILE_TERM;
  CBOOL__CALL_N(insertz, root, ptr);
  CBOOL__PROCEED;
}
// '$assertz'/2
CBOOL__PROTO(prolog_assertz) {
  HEAD_ROOT;
  COMPILE_TERM;
  CBOOL__CALL_N(insertz, root, ptr);
  CBOOL__PROCEED;
}
// '$assertz_ref'/3
CBOOL__PROTO(prolog_assertz_ref) {
  HEAD_ROOT;
  COMPILE_TERM;
  CBOOL__CALL_N(insertz, root, ptr);
  CBOOL__CALL_N(instance_to_ref, ptr, X(2));
  CBOOL__PROCEED;
}

// '$erase'/2
CINSNP__PROTO(prolog_erase) {
  HEAD_ROOT_INSNP;
  X(3) = MakeSmall(0); /* erase */
  CINSNP__LASTCALL_N(current_instance, root, BLOCK);
}
// '$erase_nb_root'/3
CINSNP__PROTO(prolog_erase_nb_root) {
  GET_ROOT;
  X(3) = MakeSmall(0); /* erase */
  CINSNP__LASTCALL_N(current_instance, root, NO_BLOCK);
}
// '$erase_nb'/2
CINSNP__PROTO(prolog_erase_nb) {
  HEAD_ROOT_INSNP;
  X(3) = MakeSmall(0); /* erase */
  CINSNP__LASTCALL_N(current_instance, root, NO_BLOCK);
}
// '$erase_ref'/1
CBOOL__PROTO(prolog_erase_ref) {
  tagged_t Ref;
  instance_t *ptr;
  DEREF(Ref, X(0));
  ptr = CFUN__EVAL_N(ref_to_instance, Ref);
  CBOOL__TEST(ptr != NULL); 
  CVOID__CALL_N(prolog_erase_ptr, ptr);
  CBOOL__PROCEED;
}
// '$current'/2
CINSNP__PROTO(prolog_current) {
  HEAD_ROOT_INSNP;
  X(3) = MakeSmall(2); /* nothing */
  CINSNP__LASTCALL_N(current_instance, root, BLOCK);
}
// '$current_nb_root'/3
CINSNP__PROTO(prolog_current_nb_root) {
  GET_ROOT;
  X(3) = MakeSmall(2); /* nothing */
  CINSNP__LASTCALL_N(current_instance, root, NO_BLOCK);
}
// '$current_nb'/2
CINSNP__PROTO(prolog_current_nb) {
  HEAD_ROOT_INSNP;
  X(3) = MakeSmall(2); /* nothing */
  CINSNP__LASTCALL_N(current_instance, root, NO_BLOCK);
}
// '$current_nb_ref'/3
CINSNP__PROTO(prolog_current_nb_ref) {
  tagged_t Ref;
  DEREF(Ref, X(2));
  if (!IsVar(Ref)) {
    instance_t *ptr;
    ptr = CFUN__EVAL_N(ref_to_instance, Ref);
    if (ptr == NULL) CINSNP__FAIL; 
    /* Initialize dynpreserved (to ensure heapmargin correctness) */
    /* TODO: necessary? */
    /* X(0) must be the head */
    /* X(1) must be the body */
    X(X2_CHN) = TermNull;
    X(3) = MakeSmall(3); /* nothing, no unlock */
    X(ClockSlot) = MakeSmall(0);
    X(X5_CHN) = TermNull; 
    X(RootArg) = TermNull; /* TODO: hmmm unknown, so no unlock_predicate can be done... is this correct? */
    X(InvocationAttr) = MakeSmall(0);
    X(PrevDynChpt) = TermNull;
    w->ins = ptr;
    CINSNP__GOTO(w->ins->emulcode);
  } else {
    HEAD_ROOT_INSNP;
    X(3) = Ref; /* (variable) get ref */
    CINSNP__LASTCALL_N(current_instance, root, NO_BLOCK);
  }
}
// '$open_pred'/1
CBOOL__PROTO(prolog_open_pred) {
  HEAD_ROOT;
  CVOID__CALL_N(open_predicate, root);
  CBOOL__PROCEED;
}
// '$close_pred'/1
CBOOL__PROTO(prolog_close_pred) {
  HEAD_ROOT;
  CVOID__CALL_N(close_predicate, root);
  CBOOL__PROCEED;
}

/***************************************************************************/

#if defined(ABSMACH_OPT__atomgc)
CBOOL__PROTO(prolog_erase_atom) {
  intmach_t index;

  DEREF(X(0), X(0));
  index = AtomIndex(X(0));

  DEBUG__TRACE(debug_atomgc, "erasing atom %s at %ld\n", atmtab[index]->value.atomp->name, (long)index);

/* atmtab[i] point to parts of other data structure, so we fix the values
   there and then set a null pointer in atmtab[] */

/* 1 cannot be the key of any entry (see init_atom_check()), and is used to
   mark a deleted entry (see atom_gethash()) */

  atmtab[index]->key = 1;
  atmtab[index]->value.atomp = NULL;
  atmtab[index] = NULL;
  prolog_atoms->count--;
  CBOOL__PROCEED;
}
#endif

extern char *ciao_versiontag;

CBOOL__PROTO(prolog_version) {
  CBOOL__LASTUNIFY(GET_ATOM(ciao_versiontag), X(0));
}

CBOOL__PROTO(prolog_print_emulator_version) {
  print_string(Output_Stream_Ptr, emulator_version);
  CBOOL__PROCEED;
}

/* Return the arguments with which the current prolog was invoked */

char **prolog_argv;
int prolog_argc; 

CBOOL__PROTO(prolog_unix_argv) {
  tagged_t list = atom_nil;
  char **p1 = prolog_argv;
  intmach_t i;
  
  for (i=prolog_argc; i>1;) {
    MakeLST(list,GET_ATOM(p1[--i]),list);
  }
  CBOOL__LASTUNIFY(list,X(0));
}

/* TODO: move to other module */
/*
list_to_path([M|Ms], SepB, Sep, Path0, Path) :-
	compose_paths(Path0, M, SepB, Path1),
	list_to_path_2(Ms, Sep, Path1, Path).

list_to_path_2([], _, Path, Path) :- !.
list_to_path_2([M|Ms], Sep, Path0, Path) :-
	compose_paths(Path0, M, Sep, Path1),
	list_to_path_2(Ms, Sep, Path1, Path).
	
compose_paths(Path0, Rel, Sep, Path) :-
	atom_concat(Path0, Sep, Path1),
	atom_concat(Path1, Rel, Path).
*/

/* list_to_path(Ms, Sepb, Sep, Path0, Path) concats Path0 and atoms in
   Ms, using character Sepb as the separator between Path0 and the
   first element of Ms and character Sep as the separator between
   elements of Ms (separators with 0 value are ignored) */

CBOOL__PROTO(prolog_list_to_path) {
  /* Concatts */
  tagged_t ms;
  intmach_t sepb;
  intmach_t sep;
  tagged_t path0;
  tagged_t t;
  tagged_t t1;
  intmach_t new_atom_length;
  char *s, *s1;

  /* Deref and test some types */
  DEREF(X(0),X(0));
  ms = X(0);
  DEREF(X(1),X(1));
  CBOOL__TEST(TaggedIsSmall(X(1)));
  sepb = GetSmall(X(1));
  DEREF(X(2),X(2));
  CBOOL__TEST(TaggedIsSmall(X(2)));
  sep = GetSmall(X(2));
  DEREF(X(3),X(3));
  CBOOL__TEST(TaggedIsATM(X(3)));
  path0 = X(3);

  /* Test Ms types and get full atom length */
  new_atom_length = GetAtomLen(path0);
  if (sepb != 0) new_atom_length++;
  t = ms;
  CBOOL__TEST(TaggedIsLST(t));
  DerefCar(t1, t);
  DerefCdr(t, t);
  CBOOL__TEST(TaggedIsATM(t1));
  new_atom_length += GetAtomLen(t1);
  while (t != atom_nil) {
    if (sep != 0) new_atom_length++;
    CBOOL__TEST(TaggedIsLST(t));
    DerefCar(t1, t);
    DerefCdr(t, t);
    CBOOL__TEST(TaggedIsATM(t1));
    new_atom_length += GetAtomLen(t1);
  }
  new_atom_length++; /* for trailing 0 */

  /* Allocate space and build atom */
  GET_ATOM_BUFFER(s, new_atom_length);
  /* Concat path0 */
  s1 = GetString(path0);
  while (*s1) *s++ = *s1++;
  if (sepb != 0) *s++ = sepb; /* add sepb separator */
  /* Concat atom list */
  t = ms;
  DerefCar(t1, t);
  DerefCdr(t, t);
  /* Concat atom */
  s1 = GetString(t1);
  while (*s1) *s++ = *s1++;
  while (t != atom_nil) {
    if (sep != 0) *s++ = sep; /* add sep separator */
    DerefCar(t1, t);
    DerefCdr(t, t);
    /* Concat atom */
    s1 = GetString(t1);
    while (*s1) *s++ = *s1++;
  }
  *s = '\0'; /* add trailing 0 */
  
  CBOOL__LASTUNIFY(GET_ATOM(Atom_Buffer),X(4));
}

