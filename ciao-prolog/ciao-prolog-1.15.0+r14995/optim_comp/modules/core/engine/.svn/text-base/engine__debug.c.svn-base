#include <engine/basiccontrol.native.h>

#include <string.h>

/* TODO: COMPLETE THE DOCUMENTATION OF THIS FILE
   -- jfran

   When DEBUG compilation flag is enabled, you can activate the 
   memory checks at some program points using INSCOUNT_FROM,
   INSCOUNT_TO and INSCOUNT_STEP environment variables.
*/

#if defined(DEBUG_TRACE)
intmach_t debug_c = 0;
bool_t debug_predtrace = FALSE; /* trace predicate calls -- Shared */
bool_t debug_gc = FALSE; /* debug garbage collection -- Shared */
bool_t debug_threads = FALSE; /* debug thread creation -- Shared */
bool_t debug_choicepoints = FALSE; /* debug choicepoints state -- Shared */
bool_t debug_concchoicepoints = FALSE; /* debug conc. chpt. state -- Shared */
bool_t debug_mem = FALSE; /* debug memory manegement -- Shared */
bool_t debug_conc = FALSE; /* debug concurrency -- Shared */
bool_t debug_setarg = FALSE; /* debug setarg -- Shared */
bool_t debug_atomgc = FALSE; /* debug atomgc -- Shared */

/* TODO: think in other way to do this... */
bool_t debug_trace__get_opt(const char *arg) {
  if (strcmp(arg, "-tp") == 0)        /* Trace predicates */
    debug_predtrace = TRUE;
  else if (strcmp(arg, "-dcp") == 0)  /*debug regular choicepoints*/
    debug_choicepoints = TRUE;
  else if (strcmp(arg, "-dconccp") == 0) /*conc. choicepoints*/
    debug_concchoicepoints = TRUE;
  else if (strcmp(arg, "-dt") == 0)           /* debug threads */
    debug_threads = TRUE;
  else if (strcmp(arg, "-dgc") == 0)      /* debug garb. coll. */
    debug_gc = TRUE;
  else if (strcmp(arg, "-dmem") == 0)       /* debug mem. man. */
    debug_mem = TRUE;
  else if (strcmp(arg, "-dconc") == 0)    /* debug concurrency */
    debug_conc = TRUE;
  else if (strcmp(arg, "-dsetarg") == 0)    /* debug setarg */
    debug_setarg = TRUE;
  else if (strcmp(arg, "-datomgc") == 0)    /* debug atomgc */
    debug_atomgc = TRUE;
  else if (strcmp(arg, "-d") == 0) /* debug dynlink */
    debug_c = 1;
  else
    return FALSE;
  return TRUE;
}
#endif

#if defined(USE_DEBUG_INSCOUNT)
intmach_t around(intmach_t x, intmach_t base, intmach_t radius) {
  if (((base - radius) <= x) && (x <= (base + radius))) return 1;
  return 0;
}

intmach_t debug_inscount_from = 0;
intmach_t debug_inscount_to = 0;
intmach_t debug_inscount_step = 0;

intmach_t debug_inscount = 0;

bool_t dump_cond() {
  bool_t c;
  c = FALSE;
  if (debug_inscount <= debug_inscount_to &&
      debug_inscount >= debug_inscount_from &&
      ((debug_inscount - debug_inscount_from) % debug_inscount_step) == 0) {
    c |= TRUE;
  }
  return c;
}

void init_debug_inscount() {
  debug_inscount_from = resources__get_variable("INSCOUNT_FROM", -1);
  debug_inscount_to = resources__get_variable("INSCOUNT_TO", -1);
  debug_inscount_step = resources__get_variable("INSCOUNT_STEP", 0);
}
#endif

#if defined(USE_RTCHECKS)

#define BADWORD_GCTAGS 1
#define BADWORD_OUTHEAP 2
#define BADWORD_OUTSTACK 4
#define BADWORD_ANY (BADWORD_OUTSTACK|BADWORD_OUTHEAP|BADWORD_GCTAGS)

#define BADWORD_SENSIBLE(MASK, WHAT) (((MASK)&(WHAT)) != 0)

CFUN__PROTO_N(badword, char *, tagged_t t, char *local_top, intmach_t mask) {
  char *reason;
  tagged_t *ptr;
  ptr = TaggedToPointer(t);
#if !defined(EXTGC)
  if (BADWORD_SENSIBLE(mask, BADWORD_GCTAGS) && (t & GC_ANYMASK)) {
    reason = "gctags";
  } else
#endif
    if (BADWORD_SENSIBLE(mask, BADWORD_OUTHEAP) && (OnHeap(ptr) && (TaggedIsNUMorATM(t) || ptr > G->heap_top))) {
      reason = "outheap";
    } else if (BADWORD_SENSIBLE(mask, BADWORD_OUTSTACK) && (OnStack(ptr) && (TaggedIsNUMorATM(t) || ptr > (tagged_t *)local_top))) {
      reason = "outstack";
    } else {
      reason = NULL;
    }
  CFUN__PROCEED(reason);
}

void dump_tagged(tagged_t t) {
#if defined(EXTGC)
  TRACE_PRINTF("tag%d(0x%x)", TagOf(t), (unsigned int)TaggedToPointer(t));
#else
  TRACE_PRINTF("tag%d(0x%x)[%d]", TagOf(t), (unsigned int)TaggedToPointer(t), (int)(t & GC_ANYMASK));
#endif
}

intmach_t dump_maxwarn = 10;
intmach_t dump_warncount = 10;

CFUN__PROTO_N(proofread_warn, intmach_t, char *text, tagged_t *p, char *local_top, intmach_t mask, bool_t quiet) {
  tagged_t t, t2;
  char *reason;
  reason = CFUN__EVAL_N(badword, *p, local_top, mask);
  if (reason == NULL) CFUN__PROCEED(0);
  t = *p;
  
  if (!quiet) {
    if (dump_warncount < dump_maxwarn) {
      TRACE_PRINTF("* %s (%s): *(%p) = ", text, reason, p);
      dump_tagged(t);
      if (!TaggedIsNUMorATM(t)) {
	TRACE_PRINTF(" deref1:");
	t2 = *TaggedToPointer(t);
	dump_tagged(t2);
      }
      TRACE_PRINTF("\n");
      dump_warncount++;
    } else if (dump_warncount == dump_maxwarn) {
      TRACE_PRINTF("{proofread: too much warnings}\n");
      dump_warncount++;
    }
  }
  CFUN__PROCEED(1);
}

CFUN__PROTO_N(TESTINT, intmach_t, tagged_t *p, intmach_t mask, bool_t quiet) {
  intmach_t bad = 0;
  tagged_t t1;
  t1 = *p;
  if (IsHeapPtr(t1)) {
    bad |= CFUN__EVAL_N(proofread_warn, "h", p, (char *)G->local_top, mask, quiet);
  } else if (TaggedIsSVA(t1)) {
    bad |= CFUN__EVAL_N(proofread_warn, "s", p, (char *)G->local_top, mask, quiet);
  }
  CFUN__PROCEED(bad);
}

CFUN__PROTO_N(worker_integrity, intmach_t, intmach_t arity, bool_t quiet) {
  frame_t *a;
  intmach_t bad = 0;
  tagged_t t1;
  tagged_t *pt1;
  choice_t *n, *n2;
  choice_t *aux_choice;
  frame_t *frame;
  char *old_local_top = 0;
  intmach_t i;
  extern bcp_t contcode;
  extern try_node_t *fail_alt;
  
  old_local_top = (char *)G->local_top;

  /* ensure that w->choice is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* DO NOT modify Deep state -- we are still in "shallow mode" */
  /* Pre: !IsDeep() */
  CODE_MAYBE_NECK_TRY();
  
  /* Make a fake frame to store X regs */
  CODE_ALLOC(a);
  a->x[0] = TaggedZero;
  for (i=0; i<arity; i++)
    a->x[i+1] = X(i);
  CODE_CFRAME(a, CONTCODE(arity+1));

  if (!quiet) {
    TRACE_PRINTF("[time = %d] {proofread: checking heap[start:%p, top:%p, end:%p, size:%d, maxsize:%ld]}\n", debug_inscount, w->heap_start, G->heap_top, w->heap_end, w->heap_end - w->heap_start, SMALLPTR_MASK);
  }
  dump_warncount = 0;
	  
  /* Test heap */
  pt1 = Heap_Start;
  while (HeapYounger(G->heap_top,pt1)) {
    t1 = *pt1;
    pt1++;
    /* TODO: check structure!! it would be a good idea to do this
       check only for functor heads */
    if (BlobHF(t1)) {
      pt1 = HeapCharOffset(pt1, BlobFunctorSizeAligned(t1)+sizeof(functor_t));
    } else {
      bad |= CFUN__EVAL_N(TESTINT, pt1-1, BADWORD_ANY, quiet);
    }
  }

  /* Test the trail stack */
  if (!quiet) {
    TRACE_PRINTF("[time = %d] {proofread: checking trail[start:%p, top:%p]}\n", debug_inscount, w->trail_start, G->trail_top);
  }
  dump_warncount = 0;

  pt1 = Trail_Start;
  while (TrailYounger(G->trail_top,pt1)) {
    t1 = *pt1;
    /* TODO: move this definition to a macro? */
    if (!IsHeapPtr(t1) && !TaggedIsSVA(t1)) {
      if (!quiet) {
	TRACE_PRINTF("[time = %d] {proofread: non heap term nor stack var in trail: %p (0x%x)}\n", debug_inscount, pt1, (unsigned int)(t1));
      }
      bad |= TRUE;
    }
    /* during this test OUTSTACK in trail is not bad */
    bad |= CFUN__EVAL_N(TESTINT, pt1, BADWORD_OUTHEAP|BADWORD_GCTAGS, quiet);
    pt1 += TrailDir;
  }

  /* Test the root of logical global variables */
  bad |= CFUN__EVAL_N(TESTINT, &GLOBAL_VARS_ROOT, BADWORD_OUTHEAP|BADWORD_GCTAGS, quiet);

  /* Test the choice stack */
  if (!quiet) {
    TRACE_PRINTF("[time = %d] {proofread: checking choice[choice:%p, start:%p]}\n", debug_inscount, w->choice, w->choice_start);
    TRACE_PRINTF("[time = %d] {proofread: checking stack[start:%p, top:%p, end:%p, size:%d, maxsize:%ld]}\n", debug_inscount, w->stack_start, G->local_top, w->stack_end, w->stack_end - w->stack_start, SMALLPTR_MASK);
  }
  dump_warncount = 0;
  /* AA, HH and TR are free pointers;  BB is last used word. */
  aux_choice = ChoiceNext0(w->choice,0);
  aux_choice->next_alt = fail_alt;
  aux_choice->frame = G->frame;
  aux_choice->next_insn = G->next_insn;
  aux_choice->heap_top = G->heap_top;
  aux_choice->local_top = G->local_top; // JFKKNEW
  /* pointers in choice&env stks */
  for (n=aux_choice; n!=InitialChoice; n=n2) {
    if (n->next_alt == NULL) {
      /* TODO: hmmm... */
      if (!quiet) {
	TRACE_PRINTF("[time = %d] {proofread: error: next_alt = NULL, exiting integrity check}\n", debug_inscount);
      }
      bad |= 1;
      goto end;
    }
    ChoiceForeachX(n, i, {
      bad |= CFUN__EVAL_N(TESTINT, &n->x[i], BADWORD_ANY, quiet);
    });

    /* TODO: write some test to check if next_insn is correct
    if ((unsigned)n->next_insn > (unsigned)0xc0000000) {
      if (!quiet) {
	TRACE_PRINTF("{strange next_insn %p... exiting integrity check}\n", n->next_insn);
      }
      bad |= 1;
      goto end;
    }
    */
    i = FrameSize(n->next_insn);
  
    frame = n->frame;
    if (frame == NULL) {
      if (!quiet) {
	TRACE_PRINTF("[time = %d] {proofread: error: frame == NULL, exiting integrity check}\n", debug_inscount);
      }
      bad |= 1;
      goto end;
    }
    /* TODO: use the same macros to traverse the frame than in engine__gc */
    n2=ChoiceCont(n);
    while (frame >= n2->local_top) {
      pt1 = (tagged_t *)StackCharOffset(frame,i);
      while (pt1!=frame->x) {
	t1 = *(--pt1);
	bad |= CFUN__EVAL_N(TESTINT, pt1, BADWORD_ANY, quiet);
      }
      /* TODO: write some test to check if next_insn is correct
      if ((unsigned)frame->next_insn > (unsigned)0xc0000000) {
	if (!quiet) {
	  TRACE_PRINTF("{strange next_insn %p... exiting integrity check}\n", frame->next_insn);
	}
	bad |= 1;
	goto end;
      }
      */
      if (frame == NULL) {
	if (!quiet) {
	  TRACE_PRINTF("[time = %d] {proofread: error: frame == NULL, exiting integrity check}\n", debug_inscount);
	}
	bad |= 1;
	goto end;
      }
      if (frame->next_insn == NULL) {
	if (!quiet) {
	  TRACE_PRINTF("[time = %d] {proofread: error: frame->next_insn == NULL... exiting integrity check; frame=%p}\n", debug_inscount, frame);
	}
	bad |= 1;
	goto end;
      }
      i = FrameSize(frame->next_insn);
      frame = frame->frame;
    } 
  }

 end:
  /* Destroy fake frame */

  /* TODO: this is not necessary since X regs has not been
     modified... right? */ 
  for (i=0; i<arity; i++)
    X(i) = a->x[i+1];

  DEALLOCATE(a);
  G->local_top = (frame_t *)old_local_top;

  /* Return result of integrity check */
  CFUN__PROCEED(bad);
}

#include <string.h>

char *proofread__from = NULL;
void proofread__showfrom() {
  if (!proofread__from) return;
  TRACE_PRINTF("[time = %d] {proofread: from '%s'}\n", debug_inscount, proofread__from);
  proofread__from = NULL;
}

CBOOL__PROTO_N(proofread, char *text, intmach_t arity, bool_t force) {
  intmach_t trail_free;

  INSCOUNT_NEXT();
  proofread__from = text;

  /* TODO: recover this integrity test? (and make it general) */
  /* fast integrity test */
  //  if ((unsigned)G->heap_top >= (unsigned)0x4fffffff) {
  //    TRACE_PRINTF("{no more addressable mem}\n");
  //    abort();
  //  }

  /* TODO: write a macro to test any stack */
  trail_free = (char *)w->choice - (char *)G->trail_top;
  if (trail_free < 0) {
    proofread__showfrom();
    TRACE_PRINTF("[time = %d] {proofread: error: trail/choice space is negative!!: choice: %p trail_top: %p (available: 0x%x)}\n", debug_inscount, w->choice, G->trail_top, trail_free);
  }
#if defined(DEBUG__TRACE__LOW_STACKS)
  else if (trail_free < CHOICEPAD) {
    proofread__showfrom();
    TRACE_PRINTF("[time = %d] {proofread: warning: trail/choice space is near end: choice: %p trail_top: %p (available: 0x%x)}\n", debug_inscount, w->choice, G->trail_top, trail_free);
  }
#endif

  if (!force) {
    if (!dump_cond()) CBOOL__FAIL;
  }

  /* slow and expensive integrity test */
  if (CFUN__EVAL_N(worker_integrity, arity, TRUE)) {
    /* the quiet call found errors, do it again verbosely */
    proofread__showfrom();
    CBOOL__LASTTEST(CFUN__EVAL_N(worker_integrity, arity, FALSE) ? TRUE : FALSE);
  }
  CBOOL__FAIL;
}

CVOID__PROTO_N(display_term, tagged_t term, stream_node_t *stream, bool_t quoted);

/* TODO: print arguments up to a determined depth */
CVOID__PROTO_N(dump_call, char *s, definition_t *func) {
  short i;
  tagged_t t;

  RTCHECK(CBOOL__SUCCEED_N(proofread, "call level", FuncArity(func), FALSE));

  if (!dump_cond()) return;

  TRACE_PRINTF("[time = %d] ", debug_inscount);
  TRACE_PRINTF("%s: ", s);

  TRACE_PRINTF(GetString(FuncName(func)));
  if (FuncArity(func) > 0) {
    putc('(', TraceFile);
    for (i = 0; i < FuncArity(func); i++) {
      if (i > 0) putc(',', TraceFile);
      DEREF(t,X(i));
      CVOID__CALL_N(display_term,t,stream_trace, TRUE);
    }
    putc(')', TraceFile);
  }
  putc('\n', TraceFile);
}
#endif
