/* Stacks expansion and garbage collection */

#include <engine/basiccontrol.native.h>

#if defined(EXTGC)
/* TODO: very inefficient! */
/* note: choice and trail pointers belong to the same stack */
#if defined(PROF_AG)
int ptr2gc_count[100];
int ptr2gc_seen[100];
int ptr2gc_sect[100];
int ptr2gc_max = 0;
CFUN__PROTO_N(ptr_to_gcptr, extgc_t *, tagged_t *ptr, intmach_t line, intmach_t n, intmach_t sect) {
  ptr2gc_max = ptr2gc_max > n+1 ? ptr2gc_max : n+1;
  ptr2gc_sect[n] = sect;
  ptr2gc_count[n]++;
  if (ptr >= Heap_Start && ptr < Heap_End) {
    ptr2gc_seen[n] |= 1<<0;
    return ptr_to_gcptr_sect_heap(ptr);
  } else if (ptr >= Stack_Start && ptr < Stack_End) {
    ptr2gc_seen[n] |= 1<<1;
    return ptr_to_gcptr_sect_frame(ptr);
  } else if (ptr >= Trail_Start && ptr < Trail_End) {
    ptr2gc_seen[n] |= 1<<2;
    return ptr_to_gcptr_sect_trail(ptr);
  } else {
    fprintf(stderr, "ptr_to_gcptr: unknown pointer %p at line %d (id %d)\n", ptr, line, n);
    abort();
  }
}
#else
CFUN__PROTO_N(ptr_to_gcptr, extgc_t *, tagged_t *ptr) {
  if (ptr >= Heap_Start && ptr < Heap_End) {
    return ptr_to_gcptr_sect_heap(ptr);
  } else if (ptr >= Stack_Start && ptr < Stack_End) {
    return ptr_to_gcptr_sect_frame(ptr);
  } else if (ptr >= Trail_Start && ptr < Trail_End) {
    return ptr_to_gcptr_sect_trail(ptr);
  } else {
    fprintf(stderr, "ptr_to_gcptr: unknown pointer %p\n", ptr);
    abort();
  }
}
#endif
#endif

/* TODO: move USE_* options to engine__definitions header */

/* TODO: seems to work very well, but see if it can be handled without that tricky */
#define USE_REGGLOB 1 /* register global vars using trail */
#define USE_GC_SETARG 1 /* fix setarg when segmented GC is enabled. todo: make setarg optional */

#define USE_SEGMENTED_GC 1
#define USE_EARLY_RESET 1

#if defined(USE_GC_SETARG)
#define SwOnTrTag(Reg, CODE_Var, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVAorSVA_STR(Reg, { \
    CODE_Var; \
  }, { \
    if (TaggedToHeadfunctor(Reg)==functor_Dsetarg) { \
      CODE_SetArg; \
    } else { \
      CODE_UndoGoal; \
    } \
  }); \
}
#else
#define SwOnTrTag(Reg, CODE_Var, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVAorSVA_STR(Reg, { \
    CODE_Var; \
  }, { \
    CODE_UndoGoal; \
  }); \
}
#endif

#if defined(USE_GC_SETARG)
#define SwOnTrTagT(Reg, CODE_HVAorCVA, CODE_SVA, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVA_SVA_STR(Reg, { \
    CODE_HVAorCVA; \
  }, { \
    CODE_SVA; \
  }, { \
    if (TaggedToHeadfunctor(Reg)==functor_Dsetarg) { \
      CODE_SetArg; \
    } else { \
      CODE_UndoGoal; \
    } \
  }); \
}
#else
#define SwOnTrTagT(Reg, CODE_HVAorCVA, CODE_SVA, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVA_SVA_STR(Reg, { \
    CODE_HVAorCVA; \
  }, { \
    CODE_SVA; \
  }, { \
    CODE_UndoGoal; \
  }); \
}
#endif

#if defined(USE_GC_SETARG)
#define SwOnTrTagTU(Reg, CODE_HVAorCVA, CODE_SVA, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVA_SVA_STR(Reg, { \
    CODE_HVAorCVA; \
  }, { \
    CODE_SVA; \
  }, { \
    if (GC_UNMARKED(TaggedToHeadfunctor(Reg))==functor_Dsetarg) { \
      CODE_SetArg; \
    } else { \
      CODE_UndoGoal; \
    } \
  }); \
}
#else
#define SwOnTrTagTU(Reg, CODE_HVAorCVA, CODE_SVA, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVA_SVA_STR(Reg, { \
    CODE_HVAorCVA; \
  }, { \
    CODE_SVA; \
  }, { \
    CODE_UndoGoal; \
  }); \
}
#endif

#if defined(USE_GC_SETARG)
#define MutatedPtr(Reg, Ptr) { \
  tagged_t m__mutated; \
  intval_t m__i; \
  m__mutated = *TaggedToArg((Reg),2); \
  m__i = GetSmall(*TaggedToArg((Reg),1)); \
  ComplexToArg(Ptr, m__mutated, m__i); \
}
#define MutatedPtrU(Reg, Ptr) { \
  tagged_t m__mutated; \
  intval_t m__i; \
  m__mutated = GC_UNMARKED(*TaggedToArg((Reg),2)); \
  m__i = GetSmall(GC_UNMARKED(*TaggedToArg((Reg),1))); \
  ComplexToArg(Ptr, m__mutated, m__i); \
}
#define OldvarPtr(Reg, Ptr) { Ptr = TaggedToArg((Reg),3); }
#endif


CVOID__PROTO(gc__heap_collect);
CVOID__PROTO(trail__remove_uncond);

#if defined(USE_SEGMENTED_GC)
#define GC_HEAP_IN_SEGMENT(P) OffHeaptop((P),Gc_Heap_Start)
#define GC_STACK_IN_SEGMENT(P) OffStacktop((P),Gc_Stack_Start)
#else
#define GC_HEAP_IN_SEGMENT(P) TRUE
#define GC_STACK_IN_SEGMENT(P) TRUE
#endif

#if defined(USE_GCSTATS)
extern int64_t (*userclick)(void);
#endif

/***************************************************************************/
/* Do a backward and forward pass over the choice stack */
/* TODO: Gc_Aux_Choice is the newest choice and Gc_Choice_Start the oldest */
#define CHOICE_PASS(CP, PREVCP, ARITY, BACKWARD, FORWARD) { \
  try_node_t *ALT; \
  (CP) = Gc_Aux_Choice; \
  (PREVCP) = w->choice; \
  (ALT) = fail_alt; \
  (ARITY) = (ALT)->arity; \
  while (ChoiceYounger((CP),Gc_Choice_Start)) { \
    BACKWARD; \
    CHOICE_PASS__ReverseChoice((CP),(PREVCP),(ALT)); \
    (ARITY) = (ALT)->arity; \
  } \
  while (ChoiceYounger(Gc_Aux_Choice,(CP))) { \
    CHOICE_PASS__UndoChoice((CP),(PREVCP),(ALT)); \
    (ARITY) = (ALT)->arity; \
    FORWARD; \
  } \
}

#define CHOICE_PASS__ReverseChoice(CP,PREVCP,ALT) { \
  try_node_t *m_alt; \
  m_alt = (ALT); \
  (CP) = (PREVCP); \
  (ALT) = (CP)->next_alt; \
  (PREVCP) = ChoiceCont((CP)); \
  (CP)->next_alt = m_alt; \
}

#define CHOICE_PASS__UndoChoice(CP,PREVCP,ALT) { \
  try_node_t *m_alt; \
  m_alt = (ALT); \
  (PREVCP) = (CP); \
  (ALT) = (PREVCP)->next_alt; \
  (CP) = ChoiceNext0((PREVCP), (ALT)->arity); \
  (PREVCP)->next_alt = m_alt; \
}

/***************************************************************************/
/* Do DO for each frame variable (*FRAME_X) of each frame between the
   initial value of FRAME and END_FRAME. */
#define ForEachFrameX(FRAME, FRAME_SIZE, END_FRAME, FRAME_X, DO) { \
  while (OffStacktop((FRAME), (END_FRAME))) { \
    TG_av(FRAME_X) = (tagged_t *)StackCharOffset((FRAME), (FRAME_SIZE)); \
    TGavag__resolve_ag(1, sect_frame, FRAME_X); \
    while (TG_av(FRAME_X) != (FRAME)->x) { \
      TGavag__inc(FRAME_X, -StackDir); \
      DO; \
    } \
    (FRAME_SIZE) = FrameSize((FRAME)->next_insn); \
    (FRAME) = (FRAME)->frame; \
  } \
}

/* Do DO for each variable CHOICE->x[I] in the choice point */
#define ForEachChoiceX_avv(CHOICE, PTR, DO) ({ \
  intmach_t i; \
  TGavv__decl(PTR); \
  i = ChoiceArity((CHOICE)); \
  TGav__set(PTR, &(CHOICE)->x[i]); \
  for (;;) { \
    if (i <= 0) break; \
    i--; \
    TGav__inc(PTR, -1); \
    DO; \
  } \
})
#define ForEachChoiceX_avagvg(CHOICE, PTR, DO) ({ \
  intmach_t i; \
  TGavagvg__decl(PTR); \
  i = ChoiceArity((CHOICE)); \
  TGavag__set__resolve_ag(2, sect_choice, PTR, &(CHOICE)->x[i]); \
  for (;;) { \
    if (i <= 0) break; \
    i--; \
    TGavag__inc(PTR, -1); \
    DO; \
  } \
})
#define ForEachChoiceX_avagv(CHOICE, PTR, DO) ({ \
  intmach_t i; \
  TGavagv__decl(PTR); \
  i = ChoiceArity((CHOICE)); \
  TGavag__set__resolve_ag(3, sect_choice, PTR, &(CHOICE)->x[i]); \
  for (;;) { \
    if (i <= 0) break; \
    i--; \
    TGavag__inc(PTR, -1); \
    DO; \
  } \
})

/***************************************************************************/

/* TODO: is this test correct? */
#if defined(USE_RTCHECKS)
inline static bool_t rtcheck__is_M(tagged_t *t0) {
  TGavagg__decl(t);
  TGavag__set__resolve_ag(4, sect_any, t, t0);
  TG_load_g(t);
  return TG_IsM(t);
}
/* must be inside the heap */
#define ASSERT__INTORC0(X, EV) { \
  if (!OnHeap(X)) { \
    TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: %p out of heap cannot be relocated into %p}\n", (long)debug_inscount, (long)__LINE__, (X), (EV)); \
  } \
}
/* must be inside the heap and not marked */
#define ASSERT__INTORC(X, EV) ({ \
  ASSERT__INTORC0(TG_av(X), TG_av(EV)); \
  if (!rtcheck__is_M(TG_av(X))) { \
    TG_load_v(X); \
    TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: should be marked 0x%lx (at %p)}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_v(X)), TG_av(X)); \
  } \
  if (rtcheck__is_M(TG_av(EV))) { \
    TG_load_v(EV); \
    TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: cannot relocate into marked 0x%lx (at %p)}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_v(EV)), TG_av(EV)); \
  } \
})
#define ASSERT__VALID_TAGGED(X) ({ \
  if (IsHeapPtr((X)) && !OnHeap(TaggedToPointer((X)))) { \
    TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: out of heap cell 0x%lx wrote}\n", (long)debug_inscount, (long)__LINE__, (long)(X)); \
  } \
})
#define ASSERT__NO_MARK(X) ({ \
  if (rtcheck__is_M(TG_av(X))) { \
    TG_load_v(X); \
    TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: cell 0x%lx at %p is marked}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_v(X)), TG_av(X)); \
  } \
})
#else
#define ASSERT__INTORC0(X, EV)
#define ASSERT__INTORC(X, EV)
#define ASSERT__VALID_TAGGED(X)
#define ASSERT__NO_MARK(X)
#endif

static CVOID__PROTO(calculate_segment_choice);

/***************************************************************************/
/* Service routine for HEAPMARGIN* instructions.
 * pad - required amount of heap space.
 * arity - number of live X regs at this point.
 */
CVOID__PROTO_N(explicit_heap_overflow, intmach_t pad, intmach_t arity) {
  intmach_t i;
  frame_t *a;

  DEBUG__TRACE(debug_gc,
	       "Thread %ld calling explicit_heap_overflow\n", (long)Thread_Id);

  /* ensure that w->choice is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* We are still in "shallow mode" */
  /* Pre: !IsDeep() */
  CODE_MAYBE_NECK_TRY();
  
  /* ensure that X regs are seen by heap_overflow(): make a frame */
  CODE_ALLOC(a);
  a->x[0] = TaggedZero;
  for (i=0; i<arity; i++)
    a->x[i+1] = X(i);
  CODE_CFRAME(a, CONTCODE(arity+1));

  CVOID__CALL_N(heap_overflow,pad);
  for (i=0; i<arity; i++)
    X(i) = a->x[i+1];
  SetLocalTop(a);
  DEALLOCATE(a);
}

/***************************************************************************/

/* Set w->segment_choice to most recent choicept which is marked as pure. */
static CVOID__PROTO(calculate_segment_choice) {
#if defined(USE_SEGMENTED_GC)
  choice_t *n;

  w->segment_choice = NULL;
  for (n=w->choice;
       w->segment_choice==NULL;
       n=ChoiceCont(n)) {
    if (ChoiceptTestPure(n))
      w->segment_choice = n;
  }
#else
  w->segment_choice=InitialChoice;
#endif
}

/***************************************************************************/

/* delete 0's from the trail of several choice points */
CVOID__PROTO_N(trail__compress, bool_t from_gc) {
  TGavag__decl(curr);
  TGavag__decl(dest);
  choice_t *cp;
  choice_t *prevcp;
  intmach_t arity MAYBE_UNUSED;

  TGavag__set__resolve_ag(5, sect_trail, dest, Gc_Choice_Start->trail_top);
  TGavag__copy(curr, dest);
  CHOICE_PASS(cp, prevcp, arity, {
  }, {
    COMPRESS_TRAIL(cp, curr, dest);
    cp->flags = 0;
    if (from_gc) ChoiceptMarkPure(cp);
  });

#if defined(EXTGC)
  /* clean remaining GC marks (GC marks of freed memory must also be 0
     with EXTGC) */
  CLEAN_AREA(Trail_Start, (char *)Trail_End-(char *)Trail_Start, TG_av(dest), G->trail_top);
#endif

  G->trail_top = TG_av(dest);
}

/***************************************************************************/
/* Here when w->choice and G->trail_top are within CHOICEPAD from each other. */
CVOID__PROTO_N(choice_overflow, intmach_t pad, bool_t remove_trail_uncond) {
#if defined(USE_GCSTATS)	  
  int64_t click0;
#endif
  tagged_t *choice_top;
  intmach_t shallow_try;
  
  RTCHECK(CBOOL__SUCCEED_N(proofread, "Before choice_overflow", 0, TRUE));
  DEBUG__TRACE(debug_gc,
	       "Thread %ld calling choice overflow\n", (long)Thread_Id);

#if defined(USE_GCSTATS)	  
  click0 = userclick();
#endif

  shallow_try = 0;
  if (!IsDeep()) {
    if (IsShallowTry()) { /* ensure A', P' exist */
      shallow_try = 1;
      w->choice->next_alt = G->next_alt;
      w->choice->local_top = G->local_top;
    }
  }

  if (remove_trail_uncond) {
    /* note: trail__remove_uncond not executed in compile_term */
    CVOID__CALL(calculate_segment_choice);
    CVOID__CALL(trail__remove_uncond);
    CVOID__CALL_N(trail__compress,FALSE);
  }

  /* ASSUMED: --CHOICE, TRAIL++ */

  choice_top = (tagged_t *)(((char *)w->choice)+w->value_trail);
  if (ChoiceCharAvailable(choice_top) < pad) {
    choice_t *b;
    tagged_t *newtr;
    intmach_t mincount, newcount, oldcount, trail_reloc_factor, choice_reloc_factor;
    
    {
      mincount = pad - ChoiceCharDifference(choice_top,G->trail_top);
      oldcount = ChoiceCharDifference(Choice_Start,Choice_End);
      newcount = oldcount + (oldcount<mincount ? mincount : oldcount);
      newtr = REALLOC_AREA(Trail_Start, oldcount, newcount);
      DEBUG__TRACE(debug_gc,
		   "Thread %ld is reallocing TRAIL from %lx to %lx\n", 
		   (long)Thread_Id, (long)Trail_Start, (long)newtr);
    }
    trail_reloc_factor = (char *)newtr - (char *)Trail_Start;
    choice_reloc_factor = (trail_reloc_factor + (newcount-oldcount));
    {
      tagged_t *tr;
      tagged_t *trb;
      
      tr = GetRelocatedPointer(tagged_t, Choice_Start, trail_reloc_factor);
      trb = GetRelocatedPointer(tagged_t, choice_top, trail_reloc_factor);
      Trail_Start = Choice_End = newtr;                /* new low bound */
      Choice_Start = Trail_End = (tagged_t *)TrailCharOffset(newtr, newcount);      /* new high bound */
      /* Do not take out (tagged_t) casting, or the engine will break!! */

      {
        uintmach_t *x;
        uintmach_t *y;
	/* Copy the new choicepoint stack */
	/* TODO: move this copy loop to absmach_postdef */
        x = (uintmach_t *)Choice_Start;
	y = (uintmach_t *)tr;
        while ((uintmach_t *)trb < y) {
	  --y;
          --x;
	  *x = *y;
	}
        w->choice = b = (choice_t *)(((char *)x)-w->value_trail);
      }

#if defined(USE_THREADS)
      {
        choice_t *concchpt0;
        choice_t *concchpt;
        /* Relocate the chain of concurrent dynamic choicepoints. */
	/* The initial TopConcChpt was set to be the initial choice. MCL. */
        RelocatePointer(choice_t, TopConcChpt, choice_reloc_factor);
	concchpt = TopConcChpt;

        while(concchpt != InitialChoice) {
	  DEBUG__TRACE(debug_concchoicepoints || debug_gc,
		       "*** %ld(%ld) Changing dynamic chpt@%lx\n",
		       (long)Thread_Id, (long)GET_INC_COUNTER, 
		       (long)concchpt);
          concchpt0 = TermToPointerOrNull(choice_t, concchpt->x[PrevDynChpt]);
	  /* TODO: assert in_choice_stack(concchpt0) */
          RelocatePointer(choice_t, concchpt0, choice_reloc_factor);
          concchpt->x[PrevDynChpt] = PointerOrNullToTerm(concchpt0);
          concchpt = concchpt0;
        }
      }
#endif
    }
    RelocatePointer(choice_t, w->previous_choice, choice_reloc_factor);
    RelocatePointer(tagged_t, G->trail_top, trail_reloc_factor);

    /* Relocate trail_top of each choice */
    while (OffChoicetop(b,Choice_Start)){
      RelocatePointer(tagged_t, b->trail_top, trail_reloc_factor);
      b = ChoiceCont(b);
    }
  }
  
  if (shallow_try) { /* ShallowTry was on */
    SetShallowTry();
  }

#if defined(USE_GCSTATS)	  
  stats.ss_control++;
  click0 = userclick()-click0;
  stats.startclick += click0;
  stats.lastclick += click0;
  stats.ss_click += click0;
#endif

  RTCHECK(CBOOL__SUCCEED_N(proofread, "After choice_overflow", 0, TRUE));
}

/***************************************************************************/
/* pre: stack top and end are within STACKPAD bytes from each other */
CVOID__PROTO(stack_overflow) {
  intmach_t count, reloc_factor;
  tagged_t *newh;
  choice_t *n, *n2;
#if defined(USE_GCSTATS)	  
  flt64_t click0 = userclick();
#endif
  
  RTCHECK(CBOOL__SUCCEED_N(proofread, "Before stack_overflow", 0, TRUE));
  DEBUG__TRACE(debug_gc,
	       "Thread %ld calling stack overflow\n", (long)Thread_Id);

  UpdateLocalTop(w->choice,G->frame);
  
  count = StackCharSize;
  newh = REALLOC_AREA(Stack_Start, count, 2*count);
  count = 2*StackCharSize;
  DEBUG__TRACE(debug_gc,
	       "Thread %ld is reallocing STACK from %lx to %lx\n", 
	       (long)Thread_Id, (long)Stack_Start, (long)newh);

  reloc_factor = (char *)newh - (char *)Stack_Start;

  /* HH, AA and TR are free pointers;  BB is last used word. */

  if (reloc_factor!=0) {
    choice_t *aux_choice;
    frame_t *frame;
    intmach_t i;
    
    aux_choice = ChoiceNext0(w->choice,0);
    aux_choice->next_alt = fail_alt;
    aux_choice->frame = GetRelocatedPointer(frame_t, G->frame, reloc_factor);
    aux_choice->next_insn = G->next_insn;
    aux_choice->local_top = GetRelocatedPointer(frame_t, G->local_top, reloc_factor);
    
    /* relocate pointers in trail */
    {
      TGavv__decl(pt1);
      TGav__set(pt1, Trail_Start);
      while (TrailYounger(G->trail_top,TG_av(pt1))) {
	TG_load_v(pt1);
	if (TaggedIsSVA(TG_v(pt1))) {
	  RelocateTagged(pt1, reloc_factor);
	}
	TGav__inc(pt1, TrailDir);
      }
    }
    
    /* relocate pointers in choice&env stks */
    for (n=aux_choice; n!=InitialChoice; n=n2){
      ForEachChoiceX_avv(n, ptr, {
	TG_load_v(ptr);
	if (TaggedIsSVA(TG_v(ptr))) {
	  RelocateTagged(ptr, reloc_factor);
	}
      });
      
      i = FrameSize(n->next_insn);
      frame = n->frame;
      n2=ChoiceCont(n);
      RelocatePointer(frame_t, n2->local_top, reloc_factor);
      RelocatePointer(frame_t, n2->frame, reloc_factor);
      while (frame >= n2->local_top) {
	{
	  TGavv__decl(pt1);
	  TGav__set(pt1, (tagged_t *)StackCharOffset(frame,i));
	  while (TG_av(pt1) != frame->x) {
	    TGav__inc(pt1, -1);
	    TG_load_v(pt1);
	    if (TaggedIsSVA(TG_v(pt1))) {
	      RelocateTagged(pt1, reloc_factor);
	    }
	  }
	}
        if (frame->frame) {
          RelocatePointer(frame_t, frame->frame, reloc_factor);
	  i = FrameSize(frame->next_insn);
	  frame = frame->frame;
        } else {
          frame = NULL;
	}
      } 
    }
    
    G->frame = aux_choice->frame;
    G->local_top = aux_choice->local_top;
    SetChoice(w->choice);
  }
  
  Stack_Start = newh; /* new low bound */
  Stack_End = (tagged_t *)StackCharOffset(newh, count); /* new high bound */
#if defined(USE_GCSTATS)	  
  stats.ss_local++;
  click0 = userclick()-click0;
  stats.startclick += click0;
  stats.lastclick += click0;
  stats.ss_click += click0;
#endif

  RTCHECK(CBOOL__SUCCEED_N(proofread, "After stack_overflow", 0, TRUE));
}

/***************************************************************************/

bool_t gcexplicit = FALSE;       /* Shared, no locked --- global flag */
/* TODO: really global flags?? */
bool_t current_gcmode;
intmach_t current_gctrace;
intmach_t current_gcmargin;

/* pre: heap top and end are within CALLPAD bytes from each other */
CVOID__PROTO_N(heap_overflow, intmach_t pad) { 
  tagged_t *oldh = G->heap_top;
  tagged_t *newh = G->heap_top;
  tagged_t *lowboundh;
  bool_t cint_event;
  bool_t event;
  bool_t gc = gcexplicit;
  /*extern intmach_t gc_total_grey;*//*Now in a register*/

  event = TestEvent();
  cint_event = TestCIntEvent();
  
  RTCHECK(CBOOL__SUCCEED_N(proofread, "Before heap_overflow", 0, TRUE));
  DEBUG__TRACE(debug_gc,
	       "Thread %ld calling heap_overflow\n", (long)Thread_Id);

  gcexplicit = FALSE;
  CVOID__CALL(calculate_segment_choice);
  if (gc ||
      (current_gcmode == TRUE &&
      HeapCharDifference(Heap_Start,oldh) >= GCMARGIN_CHARS)) {
    CVOID__CALL(gc__heap_collect);
    newh = G->heap_top;
    lowboundh = HeapCharOffset(newh, -Gc_Total_Grey);
    if (!gc &&
        (HeapCharDifference(newh,oldh) < GCMARGIN_CHARS ||
         HeapCharAvailable(newh) < pad) &&
        !(HeapCharDifference(lowboundh,oldh) < GCMARGIN_CHARS ||
          HeapCharAvailable(lowboundh) < pad)) {
      /* garbage collect the entire heap */
      w->segment_choice = InitialChoice;
      CVOID__CALL(gc__heap_collect);
      newh = G->heap_top;
    }
  }
  if ((!gc &&
       HeapCharDifference(newh,oldh) < GCMARGIN_CHARS) ||
      HeapCharAvailable(newh) < pad) {
    /* increase heapsize */
    intmach_t mincount, newcount, oldcount, reloc_factor;
    choice_t *n, *n2;
    tagged_t *newh;
#if defined(USE_GCSTATS)	  
    flt64_t click0;
#endif
    intmach_t wake_count;

#if defined(USE_GCSTATS)	  
    click0 = userclick();
#endif
    wake_count = WakeCount();
    
    UpdateLocalTop(w->choice,G->frame);
    
    mincount = pad - HeapCharAvailable(G->heap_top);
    oldcount = HeapCharSize;
    newcount = oldcount + (oldcount<mincount ? mincount : oldcount);
    
    newh = REALLOC_AREA(Heap_Start, oldcount, newcount);
    DEBUG__TRACE(debug_gc,
		 "Thread %ld is reallocing HEAP from %lx to %lx\n", 
		 (long)Thread_Id, (long)Heap_Start, (long)newh);

    reloc_factor = (char *)newh - (char *)Heap_Start;
      
    /* AA, HH and TR are free pointers;  BB is last used word. */
      
    if (reloc_factor!=0) {
      choice_t *aux_choice;
      frame_t *frame;
      intmach_t i;
	  
      aux_choice = ChoiceNext0(w->choice,0);
      aux_choice->next_alt = fail_alt;
      aux_choice->frame = G->frame;
      aux_choice->next_insn = G->next_insn;
      aux_choice->heap_top = G->heap_top;
      aux_choice->local_top = G->local_top; /* segfault patch -- jf */
	  
      /* relocate pointers in global stk */
      {
	TGavv__decl(pt1);
	TGav__set(pt1, newh);
	RelocatePointer(tagged_t, G->heap_top, reloc_factor);
	while (HeapYounger(G->heap_top,TG_av(pt1))) {
	  TG_load_v(pt1);
	  if (BlobHF(TG_v(pt1))) {
	    TGav__charinc(pt1, BlobFunctorSizeAligned(TG_v(pt1))+2*sizeof(functor_t));
	  } else {
	    if (IsHeapPtr(TG_v(pt1))) {
	      RelocateTagged(pt1, reloc_factor);
	    }
	    /* TODO: check that the written tagged is ok? */
	    TGav__inc(pt1, 1);
	  }
	}
      }

      /* relocate pointers in global vars root */
      {
	TGavv__decl(pt1);
	TGav__set(pt1, &GLOBAL_VARS_ROOT);
	TG_load_v(pt1);
	if (IsHeapPtr(TG_v(pt1))) {
	  RelocateTagged(pt1, reloc_factor);
	}
      }

      /* relocate pointers in trail stk */
      {
	TGavv__decl(pt1);
	TGav__set(pt1, Trail_Start);
	while (TrailYounger(G->trail_top,TG_av(pt1))) {
	  TG_load_v(pt1);
	  if (IsHeapPtr(TG_v(pt1))) {
	    RelocateTagged(pt1, reloc_factor);
	  }
	  TGav__inc(pt1, TrailDir);
	}
      }

      /* relocate pointers in choice&env stks */
      for (n=aux_choice; n!=InitialChoice; n=n2) {
	ForEachChoiceX_avv(n, ptr, {
	  TG_load_v(ptr);
	  if (IsHeapPtr(TG_v(ptr))) {
	    RelocateTagged(ptr, reloc_factor);
	  }
	});
	      
	i = FrameSize(n->next_insn);
	frame = n->frame;
	n2=ChoiceCont(n);

	while (frame >= n2->local_top) {
	  {
	    TGavv__decl(pt1);
	    TGav__set(pt1, (tagged_t *)StackCharOffset(frame,i));
	    while (TG_av(pt1)!=frame->x) {
	      TGav__inc(pt1, -1);
	      TG_load_v(pt1);
	      if (IsHeapPtr(TG_v(pt1))) {
		RelocateTagged(pt1, reloc_factor);
	      }
	    }
	  }
	  i = FrameSize(frame->next_insn);
	  frame = frame->frame;
	} 
	RelocatePointer(tagged_t, n->heap_top, reloc_factor);
      }
      RelocatePointer(tagged_t, n->heap_top, reloc_factor);
	  
      SetChoice(w->choice); /* update cond registers */
    }
      
    Heap_Start = newh; /* new low bound */
    Heap_End = HeapCharOffset(newh, newcount); /* new high bound */

    UnsetEvent();
    UnsetCIntEvent();
    if (event) {
      SetWakeCount(wake_count);
    }
    if (cint_event) {
      SetCIntEvent();
    }

#if defined(USE_GCSTATS)	  
    stats.ss_global++;
    click0 = userclick()-click0;
    stats.startclick += click0;
    stats.lastclick += click0;
    stats.ss_click += click0;
#endif
  }
  RTCHECK(CBOOL__SUCCEED_N(proofread, "After heap_overflow", 0, TRUE));
}

/***************************************************************************/
/* Collect all constraints that have been woken "recently" by
   scanning the newest trail segment.  Also, excise such entries
   belonging to the newest heap segment. */
CVOID__PROTO_N(collect_goals_from_trail, intmach_t wake_count) {
  intmach_t sofar=0;
  tagged_t *tr = G->trail_top;
  tagged_t *h = G->heap_top;
  tagged_t *tr0 = NULL;
  tagged_t *limit = w->choice->trail_top;
  
  while (sofar<wake_count && TrailYounger(tr,limit)) {
    tagged_t ref, value;

    TrailDec(tr);
    ref = *tr;
    if (!TaggedIsCVA(ref)) continue;
    value = *TagpPtr(CVA,ref);
    if (value==ref) {
      PANIC_FAULT("wake - unable to find all goals");
    }
    if (sofar++ > 1) {
      HeapPush(h,X(0));
      HeapPush(h,X(1));
      X(1) = Tagp(LST,HeapCharOffset(h,-2*sizeof(tagged_t)));
    } else if (sofar > 1) {
      X(1) = X(0);
    }

    X(0) = Tagp(LST,TaggedToGoal(ref));
    if (!CondCVA(ref)) {
      tr0=tr;
      *tr=0;
    }
  }
  G->heap_top = h;
  SetWakeCount(0);

  if (sofar<wake_count) {
    PANIC_FAULT("wake - unable to find all goals");
  } else if (sofar==1) {
    X(1) = *TaggedToCdr(X(0));
    X(0) = *TaggedToCar(X(0));
  }

  /* now compress the trail */
  if (tr0) {
    CompressTrailNoGC(tr0);
  }
}

/* sweep trail segment to get rid of unconditional entries */
/* (cut transforms some conditional trails in unconditional ones, this
   pass removes the unneeded unconditional trails) */
CVOID__PROTO(trail__remove_uncond) {
  tagged_t *tr;
  choice_t *orig_b;
  choice_t *b;
  intmach_t wake_count;

  RTCHECK(CBOOL__SUCCEED_N(proofread, "Before trail__remove_uncond", 0, TRUE));

  orig_b = w->choice;

  b = w->choice;

  /* TODO: move to a macro and call before this function */
  Gc_Aux_Choice = ChoiceNext0(b,0);
  Gc_Aux_Choice->flags = 0;
  Gc_Aux_Choice->next_alt = fail_alt;
  Gc_Aux_Choice->trail_top = G->trail_top;
  Gc_Choice_Start = w->segment_choice;  

  wake_count = WakeCount();

  /* Go from new to old. */
  tr = G->trail_top;
  b = Gc_Aux_Choice;
  while (!OffChoicetop(Gc_Choice_Start,b)) {
    tagged_t *x;

    SetChoice(b); /* set cond registers */
    x=b->trail_top;
    while (TrailYounger(tr,x)) {
      tagged_t t1;

      TrailDec(tr);
      t1 = *tr;
      Sw_HVA_CVA_SVA_Other(t1, { /* HVA */
	if (!CondHVA(t1)) *tr = 0;
      }, { /* CVA */
	if (wake_count>0) {
	  wake_count--; /* do not remove any wake goal */
	} else {
	  if (!CondCVA(t1)) *tr = 0;
	}
      }, { /* SVA */
	if (!CondSVA(t1)) *tr = 0;
      }, { /* nonvar */
        /* kill unconditional 'undo setarg' */
        if (TaggedIsSTR(t1)) {
	  if (TaggedToHeadfunctor(t1)==functor_Dsetarg) {
	    tagged_t mutated;
	    mutated = *TaggedToArg(t1,2);
	    if (!CondHVA(Tagp(HVA, TaggedToPointer(mutated)))) {
	      *tr = 0;
	    }
	  }
	}
      });
    }
    b = ChoiceCont(b);
  }
  
  /* restore choice and shadow registers */
  SetChoice(orig_b);

  RTCHECK(CBOOL__SUCCEED_N(proofread, "After trail__remove_uncond", 0, TRUE));
}

static CVOID__PROTO(shunt_variables);
static CVOID__PROTO(mark_trail_cva);
static CVOID__PROTO_N(mark_frames, frame_t *frame, intmach_t frame_size);
static CVOID__PROTO(mark_choicepoints);
static CVOID__PROTO_N(mark_root, tagged_t *start);
static CVOID__PROTO_N(sweep_frames, frame_t *frame, intmach_t frame_size);
static CVOID__PROTO(sweep_choicepoints);
static CVOID__PROTO(compress_heap);

#define gc_TrailStart		(w->segment_choice->trail_top)
#define gc_HeapStart		(w->segment_choice->heap_top)
#define gc_StackStart		(w->segment_choice->local_top)
#define gc_ChoiceStart		(w->segment_choice)

/**********************************
 *  GARBAGE COLLECTION ROUTINES   *
 **********************************/

/* Based on the algorithms described in:

   "Garbage Collection for Prolog Based on WAM",
   by K. Appleby, M. Carlsson, S. Haridi, and D. Sahlin,
   Communications of the ACM 31:6, pp. 719-741,

   somewhat complicated by support for freeze & wait decls
   (constrained variables), and undo/1 (goals on the trail).
*/

/***************************************************************************/
/*** The Shunting Phase ***/

/* invariant: in shunting phase only variables are marked */

#define shunt__ensure_unmarked(X) GC_UNMARKED_M(X)

/* marks in trail entries for variable shunting (using M marks) */
#define shunt__ignoreTrailEntry(X) TG_SetM(X)
#define shunt__cleanTrailEntry(X) TG_UnsetM(X)
/* marks in variables for variables whose value was given in the
   future */
/* TODO: is correct its use for trailed cva? */
#define shunt__setTrailed(X) TG_SetM(X)
#define shunt__cleanTrailed(X) TG_UnsetM(X)
#define shunt__setAll_setTrailed(T, X) TG_SetAll_SetM(T, X)

/* marks in trail entries for variable shunting (using M marks) */
#define shunt__ignoredTrailEntry(X) TG_IsM(X)
/* marks in variables for variables whose value was given in the
   future */
/* TODO: is correct its use for trailed cva? */
#define shunt__isTrailed(X) TG_IsM(X)

/* Copy a tagged (without marks) to ptr and clean the trailed mark */
/* pre: shunt__ensure_unmarked(v) */
/* post: TG_v(ptr) == v && !shunt__isTrailed(ptr) */
#define shunt__copyNoTrailed_cleanTrailed(v, ptr) TG_MoveUNMARKED_M_UnsetM(v, ptr)

/* dereference *DEST until the cell pointed by DEST is not a variable
   or is marked */
/* postcondition: shunt__isTrailed(DEST) */
/* TODO: this postcondition is not right since DEST is a pointer... 
   postcondition: *TaggedToPointer(DEST) is marked or !IsVar(DEST) */
#define gc_shuntVariable(DEST) { \
  tagged_t shunt__x; \
  ASSERT__NO_MARK(DEST); \
  shunt__x = TG_v(DEST); \
  while (1) { \
    if (!IsVar(shunt__x)) break; \
    TGavagvg__decl(shunt__xp); \
    TGavag__set__resolve_ag(6, sect_any, shunt__xp, TaggedToPointer(shunt__x)); \
    TG_load_g(shunt__xp); \
    if (shunt__isTrailed(shunt__xp)) break; \
    TG_load_v0(shunt__xp); \
    if (TG_v(shunt__xp) == shunt__x) break; \
    shunt__x = TG_v(shunt__xp); \
  } \
  TG_Put(shunt__x, DEST); \
} 

static CVOID__PROTO(shunt_variables) {
  TGavagvg__decl(pt);
  choice_t *cp;
  choice_t *prevcp;
  intmach_t arity;
  tagged_t *limit;

  TGavag__set__resolve_ag(7, sect_trail, pt, G->trail_top);
  CHOICE_PASS(cp, prevcp, arity, {
    /* backward pass */
    /* all variables in the trail has a value given in the future */
    /* (leave unmarked only the more recent trail entry for each variable) */
    limit = prevcp->trail_top;
    while (TrailYounger(TG_av(pt),limit)) {
      TGavag__inc(pt, -TrailDir);
      TG_load_v(pt);
      /* TODO: precondition !shunt__ignoredTrailEntry(pt); ? */
      if (TG_v(pt) == 0) goto ignore_trail_entry;
      SwOnTrTag(TG_v(pt), { /* var */
	TGavagg__decl(ptr);
	TGavag__set__resolve_ag(8, sect_any, ptr, TaggedToPointer(TG_v(pt)));
	TG_load_g(ptr);
	if (shunt__isTrailed(ptr)) {
	  RTCHECK(TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: variable shunting detected that a variable at %p was trailed twice}\n", (long)debug_inscount, (long)__LINE__, TG_av(ptr)));
	  goto ignore_trail_entry;
	} else {
	  shunt__setTrailed(ptr);
	  /* TODO: trust !shunt__ignoredTrailEntry(pt); */
	}
      }, { /* Dsetarg */
	TGavag__decl(ptr);
	MutatedPtr(TG_v(pt), TG_av(ptr));
	TGavag__resolve_ag(9, sect_heap, ptr);
	shunt__setTrailed(ptr);
	OldvarPtr(TG_v(pt), TG_av(ptr));
	TGavag__resolve_ag(10, sect_heap, ptr);
	shunt__setTrailed(ptr); /* avoid shunting of the oldvar */
      }, { /* undo goal */
      });
      continue;
    ignore_trail_entry:
      shunt__ignoreTrailEntry(pt);
    }
  }, {
    /* forward pass */
    /* variables trailed in this choicepoint do not have a value given
       in the future */
    limit = cp->trail_top;
    TGavag__set__resolve_ag(11, sect_trail, pt, prevcp->trail_top);
    while (TrailYounger(limit,TG_av(pt))) {
      TG_load_g(pt);
      if (!shunt__ignoredTrailEntry(pt)) {
	TG_load_v0(pt);
	SwOnTrTag(TG_v(pt), { /* var */
	  TGavag__decl(ptr);
	  TGavag__set__resolve_ag(12, sect_any, ptr, TaggedToPointer(TG_v(pt)));
	  shunt__cleanTrailed(ptr);
	}, { /* Dsetarg */
	  tagged_t *mptr;
	  TGavag__decl(ptr);
	  MutatedPtr(TG_v(pt), mptr);
	  TGavag__set__resolve_ag(13, sect_heap, ptr, mptr);
	  shunt__cleanTrailed(ptr);
	}, { /* undo goal */
	  TGavag__decl(ptr);
	  TGavag__set__resolve_ag(14, sect_heap, ptr, TaggedToPointer(TG_v(pt)));
	  shunt__cleanTrailed(ptr);
	});
      }
      TGavag__inc(pt, 1);
    }
    /* shunt variables trailed in this choicepoint (may point out of
       the choice heap segment) */
    TGavag__set__resolve_ag(15, sect_trail, pt, prevcp->trail_top);
    while (TrailYounger(limit,TG_av(pt))) {
      TG_load_g(pt);
      if (!shunt__ignoredTrailEntry(pt)) {
	TG_load_v0(pt);
	SwOnTrTag(TG_v(pt), { /* var */
	  /* TODO: trust *TaggedToPointer(v) is not marked (we have
	     unmarked it in the previous loop) */
	  /* shunt it: the var may be out of the choice heap segment */
	  TGavv__decl(ptr);
	  TGav__set(ptr, TaggedToPointer(TG_v(pt)));
	  TG_load_v(ptr);
	  gc_shuntVariable(ptr);
	}, { /* Dsetarg */
	  tagged_t *mptr;
	  TGavv__decl(ptr);
	  MutatedPtr(TG_v(pt), mptr);
	  TGav__set(ptr, mptr);
	  TG_load_v(ptr);
	  gc_shuntVariable(ptr);
	}, { /* undo goal */
	  TGavv__decl(ptr);
	  TGav__set(ptr, TaggedToPointer(TG_v(pt)));
	  TG_load_v(ptr);
	  gc_shuntVariable(ptr);
	});
      } else {
	shunt__cleanTrailEntry(pt); /* ignore mark bits of this entry */
      }
      TGavag__inc(pt, 1);
    }
    TGavag__set__resolve_ag(16, sect_heap, pt, prevcp->heap_top);
    while (HeapYounger(cp->heap_top,TG_av(pt))) {
      TG_load_v(pt);
      if (BlobHF(TG_v(pt))) {
	TGavag__charinc(pt, BlobFunctorSizeAligned(TG_v(pt))+2*sizeof(functor_t));
      } else {
	TG_load_g0(pt);
	if (!shunt__isTrailed(pt)) {
	  if (TG_v(pt) == Tagp(CVA, TG_av(pt))) { /* v is an unbound CVA */
	    /* TODO: document this... */
	    TGavag__inc(pt, 2);
	    TG_load_v(pt);
	    tagged_t cva_susp = TG_v(pt);
	    if (cva_susp == MakeSmall(0)) {
	      /* do nothing, see bu2_attach_attribute_weak */
	      //fprintf(stderr, "weak %p\n", TG_av(pt));
	    } else {
	      /* go back and link the CVA so that it is not lost */
	      //fprintf(stderr, "strong %p\n", TG_av(pt));
	      TGavag__inc(pt, -2);
	      TG_load_v(pt);
	      tagged_t v = TG_v(pt);
	      shunt__setAll_setTrailed(Cvas_Found, pt);
	      Cvas_Found = v;
	      TGavag__inc(pt, 2);
	    }
#if 0
	    tagged_t v = TG_v(pt);
	    shunt__setAll_setTrailed(Cvas_Found, pt);
	    Cvas_Found = v;
	    TGavag__inc(pt, 2);
#endif
	  } else {
	    gc_shuntVariable(pt);
	  }
	}
	TGavag__inc(pt, 1);
      }
    }
    /* unset marks to avoid shunting of setarg oldvar: has to be done
       after the heap is shunt because the setarg entries are inside
       the heap */
    /* TODO: move those entries to the trail? it will need taking them
       into account in all the trail passes (forward is easy, but
       backward is not) */
#if defined(USE_GC_SETARG)
    TGavag__set__resolve_ag(18, sect_trail, pt, prevcp->trail_top);
    while (TrailYounger(limit,TG_av(pt))) {
      TG_load_g(pt);
      if (!shunt__ignoredTrailEntry(pt)) {
	TG_load_v0(pt);
	SwOnTrTag(TG_v(pt), { /* var */
	}, { /* Dsetarg */
	  TGavag__decl(ptr);
	  OldvarPtr(TG_v(pt), TG_av(ptr));
	  TGavag__resolve_ag(19, sect_heap, ptr);
	  shunt__cleanTrailed(ptr);
	}, { /* undo goal */
	});
      }
      TGavag__inc(pt, 1);
    }
#endif
    /* shunt frame vars */
    {
      intmach_t frame_size;
      frame_t *frame;
      frame_size = FrameSize(cp->next_insn);
      frame = cp->frame;
      ForEachFrameX(frame, frame_size, prevcp->local_top, pt, {
	TG_load_g(pt);
	if (!shunt__isTrailed(pt)) {
	  TG_load_v0(pt);
	  gc_shuntVariable(pt);
	}
      });
    }

    /* shunt choice vars */
    TGavag__set__resolve_ag(20, sect_choice, pt, cp->x + arity);
    while (TG_av(pt) != cp->x) {
      TGavag__inc(pt, -1);
      /* TODO: trust ASSERT__NO_MARK(pt); */
      TG_load_v(pt);
      gc_shuntVariable(pt);
    }
  });

#if defined(USE_RTCHECKS)
  /* postcondition: no variable in frame is marked at end of shunting */
  /* TODO: CVAs may be marked... I don't know, see Cvas_Found */
  CHOICE_PASS(cp, prevcp, arity, {
  }, {
    /* forward pass */
    intmach_t frame_size;
    frame_t *frame;
    limit = cp->trail_top;
    TGavag__set__resolve_ag(21, sect_trail, pt, prevcp->trail_top);
    while (TrailYounger(limit, TG_av(pt))) {
      ASSERT__NO_MARK(pt);
      TGavag__inc(pt, 1);
    }
    TGavag__set__resolve_ag(22, sect_heap, pt, prevcp->heap_top);
    while (HeapYounger(cp->heap_top, TG_av(pt))) {
      TG_load_v(pt);
      if (BlobHF(TG_v(pt))) {
	TGavag__charinc(pt, BlobFunctorSizeAligned(TG_v(pt))+2*sizeof(functor_t));
      } else {
	ASSERT__NO_MARK(pt);
	TGavag__inc(pt, 1);
      }
    }
    frame_size = FrameSize(cp->next_insn);
    frame = cp->frame;
    ForEachFrameX(frame, frame_size, prevcp->local_top, pt, {
      ASSERT__NO_MARK(pt);
    });
    TGavag__set__resolve_ag(23, sect_choice, pt, cp->x + arity);
    while (TG_av(pt) != cp->x) {
      TGavag__inc(pt, -1);
      ASSERT__NO_MARK(pt);
    }
  });
#endif
}

/***************************************************************************/
/**** The Marking Phase ****/

#define TG_Reverse(curr,next) { \
  tagged_t *temp; \
  temp = TaggedToPointer(TG_v(next)); \
  TG_PutPtr(TG_av(curr),next); \
  TGavag__copy(curr, next); \
  TGav__set(next, temp); \
}

#define TG_Undo(curr,next) { \
  tagged_t *temp; \
  temp = TaggedToPointer(TG_v(curr)); \
  TG_PutPtr(TG_av(next),curr); \
  TGav__set(next, TG_av(curr)); \
  TGavag__set__resolve_ag(24, sect_any, curr, temp); \
}

#define TG_Advance(curr,next) { \
  tagged_t *temp; \
  temp = TaggedToPointer(TG_v(curr)); \
  TG_PutPtr(TG_av(next),curr); \
  TGavag__inc(curr, -1); \
  TG_load_v(curr); \
  TGav__set(next, TaggedToPointer(TG_v(curr))); \
  TG_PutPtr(temp,curr); \
}

#define MarkRoot(S) CVOID__CALL_N(mark_root, (S))

/* Mark a root (was markVariable)
   Cyclic structs require this! (todo: what?)
   pre: start always points outside GC segment
   pre: !TG_IsM(start)
   pre: IsHeapPtr(TG_v(start))
*/
static CVOID__PROTO_N(mark_root, tagged_t *start0) {
  intmach_t found;
  TGav__decl(start);
  TGavagvg__decl(current);
  TGavagvg__decl(next);

  TGav__set(start, start0);
  TGavag__set__resolve_ag(25, sect_any, current, TG_av(start));

  RTCHECK({
    if (OnHeap(TG_av(start)) && GC_HEAP_IN_SEGMENT(TG_av(start))) {
      TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: marking a tagged inside the heap segment at %p}\n", (long)debug_inscount, (long)__LINE__, TG_av(start));
    }
  });
  RTCHECK({
    if (rtcheck__is_M(TG_av(start))) {
      TG_load_v(start);
      TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: marking already marked var 0x%lx at %p}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_v(start)), TG_av(start));
    }
  });
  RTCHECK({
    TG_load_v(start);
    if (!IsHeapPtr(TG_v(start))) {
      TRACE_PRINTF("[time = %ld] {assert[engine__gc:%ld]: marking a non heap term 0x%lx at %p}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_v(start)), TG_av(start));
    }
  });

  found = 0;
  
  TG_load_v(current);
  TGav__set(next, TaggedToPointer(TG_v(current)));
  TG_SetR(current);
  goto first_forward;
 forward:
  {}
  TG_load_g(current);
  if (TG_IsM(current)) goto backward;
  found += sizeof(tagged_t);
 first_forward:
  TG_SetM(current);
  if (GC_HEAP_IN_SEGMENT(TG_av(next))) {
    TG_load_v(current);
    switch(TagOf(TG_v(current))) {
    case SVA: /* No pointers from heap to stack */
      PANIC_FAULT("GC: stack variable in heap");
    case CVA: /* new 3-field CVA */
      {
	/* N.B. there can be LST pointers to the second cell as well */
	TGavagg__decl(next2);
	TGavag__resolve_ag(27, sect_heap, next);
	TGavag__add(next, 2, next2);
	TG_load_g(next2);
	if (!TG_IsROrM(next2)) {
	  /* no marking in progress as CVA nor as LST */
	  /* treat as 3-cell tuple */
	  TGavag__inc(next, 1);
	  TG_SetR(next);
	  TGavag__inc(next, 1);
	  TG_SetR(next);
	  TG_load_v(next);
	  TG_Reverse(current,next);
	  goto forward;
	} else {
	  /* otherwise, just treat the value cell */
	  goto treat_value_cell;
	}
      }
    case HVA:
      TGavag__resolve_ag(28, sect_heap, next);
    treat_value_cell:
      TG_load_g(next);
      if (TG_IsROrM(next)) {
	goto backward;
      } else {
	TG_load_v0(next);
	TG_Reverse(current,next);
	/* note: next may be something that is not a valid pointer
	   (i.e. part of a non-pointer tagged) */
	goto forward;
      }
    case LST:
      {
	/* TODO: equivalence with next case? */
	TGavagg__decl(next1);
	TGavag__resolve_ag(29, sect_heap, next);
	TGavag__add(next, 1, next1);
	TG_load_g(next1);
	if (TG_IsR(next1)) goto backward;
	TG_load_g(next);
	if ((TG_IsM(next) && TG_IsM(next1))) {
	  goto backward;
	} else {
	  TGavag__inc(next, 1);
	  TG_SetR(next);
	  TG_load_v(next);
	  TG_Reverse(current,next);
	  goto forward;
	}
      }
    case STR: {
      TGavag__resolve_ag(30, sect_heap, next);
      TG_load_g(next);
      if (TG_IsM(next)) goto backward;
      TG_load_v0(next);
      if (BlobHF(TG_v(next))) {
	/* box */
	intmach_t size = BlobFunctorSizeAligned(TG_v(next))+2*sizeof(functor_t);
	TG_SetM(next);
	found += size;
	goto backward;
      } else {
	TGavagg__decl(next1);
	TGavag__add(next, 1, next1);
	TG_load_g(next1);
	if (!TG_IsR(next1)) {
	  intmach_t n;
	  for (n = Arity(TG_v(next)); n>0; --n) {
	    TGavag__inc(next, 1);
	    TG_SetR(next);
	  }
	  TG_load_v(next);
	  TG_Reverse(current,next);
	  goto forward;
	} else {
	  goto backward;
	}
      } }
    default: /* all other treated as constants */
      goto backward;
    }
  } else {
    goto backward;
  }
 backward:
  for (;;) {
    TG_load_g(current);
    if (TG_IsR(current)) break;
    /* internal cell */
    /* note: next may be something that is not a valid pointer
       (i.e. part of a non-pointer tagged) */
    TG_load_v(current);
    TG_Undo(current,next);
  }
  /* head of chain */
  TG_UnsetR(current);
  if (TG_av(current) != TG_av(start)) {
    /* note: next may be something that is not a valid pointer
       (i.e. part of a non-pointer tagged) */
    TG_load_v(current);
    TG_Advance(current,next);
    goto forward;
  } else {
    Total_Found += found;
    return;
  }
}

/* mark all unbound/newly bound constraint variables */
static CVOID__PROTO(mark_trail_cva) {
  TGavv__decl(tr);
  intmach_t wake_count;

  TGav__set(tr, G->trail_top);
  wake_count = WakeCount();

  /* mark unbound CVAs */
  while (Cvas_Found != atom_nil) {
    tagged_t v;
    v = Cvas_Found;
    //    fprintf(stderr, "marking unbound CVA %x\n", v);
    TG_Put(v, tr);
    /* TODO: trust IsHeapPtr(*TG_av(tr)) */
    TGavagv__decl(ptr);
    TGavag__set__resolve_ag(31, sect_heap, ptr, TagpPtr(CVA, v));
    TG_load_v(ptr);
    Cvas_Found = shunt__ensure_unmarked(TG_v(ptr));
    shunt__copyNoTrailed_cleanTrailed(v, ptr);
    MarkRoot(TG_av(tr));
  }

  /* mark newly bound CVAs */
  /* TODO: put asserts to ensure that we do not run off of trail */
  while (wake_count>0) {
    TGav__inc(tr, -TrailDir);
    TG_load_v(tr);
    if (TaggedIsCVA(TG_v(tr))) {
      wake_count--;
      MarkRoot(TG_av(tr));
    }
  }
}

/* A frame slot is marked iff it is in the chain of environments for
   one of the frozen execution states, regardless of contents. */
static CVOID__PROTO_N(mark_frames, frame_t *frame, intmach_t frame_size) {
  TGavagvg__decl(ev);
  ForEachFrameX(frame, frame_size, Gc_Stack_Start, ev, {
    TG_load_g(ev);
    if (TG_IsM(ev)) return; /* finish, rest of frames are marked */
    TG_load_v0(ev);
    if (IsHeapPtr(TG_v(ev))) {
      MarkRoot(TG_av(ev));
    } else {
      TG_SetM(ev); /* mark everything to remember that this frame is done */
    }
  });
}

/* Mark choicepoints, corresponding chains of frames and trail */
static CVOID__PROTO(mark_choicepoints) {
  choice_t *cp;
  TGavagvg__decl(tr);
  tagged_t *limit;

#if defined(USE_SEGMENTED_GC)    
  TGavag__set__resolve_ag(32, sect_trail, tr, G->trail_top);
  limit = Gc_Choice_Start->trail_top;
  while (TrailYounger(TG_av(tr),limit)) {
    TGavagvg__decl(p);
    TGavag__inc(tr, -TrailDir);
    TG_load_v(tr);
    /* TODO: trust IsHeapPtr(*TG_av(tr)) <- sure?? */
    // todo: 0 here is ERRORTAG?
    if (TG_v(tr)==0) continue;
    TG_load_g0(tr);
    if (TG_IsM(tr)) continue;

    /* TODO: why?? Must be done before any early reset is done.  why?? */
    SwOnTrTagT(TG_v(tr), { /* HVA CVA */
      TGavag__set__resolve_ag(33, sect_heap, p, TaggedToPointer(TG_v(tr)));
      if (!GC_HEAP_IN_SEGMENT(TG_av(p))) goto mark_not_in_segment;
    }, { /* SVA */
      TGavag__set__resolve_ag(34, sect_frame, p, TaggedToPointer(TG_v(tr)));
      if (!GC_STACK_IN_SEGMENT(TG_av(p))) goto mark_not_in_segment;
    }, { /* Dsetarg */
      TGavagvg__decl(ptr);
      MutatedPtr(TG_v(tr), TG_av(ptr));
      if (!GC_HEAP_IN_SEGMENT(TG_av(ptr))) {
	TG_load_v(ptr);
	if (IsHeapPtr(TG_v(ptr))) {
	  TGavag__resolve_ag(35, sect_heap, ptr);
	  TG_load_g(ptr);
	  if (!TG_IsM(ptr)) {
	    Gcgrey -= Total_Found;
	    MarkRoot(TG_av(ptr));
	    Gcgrey += Total_Found;
	  }
	}
      }
    }, { /* undo goal */
    });
    continue;

  mark_not_in_segment:
    /* v is a heap or stack variable which points to a tagged not
       in the segment */
    TG_SetM(tr); /* so won't look at it again */
    TG_load_v(p);
    if (IsHeapPtr(TG_v(p))) {
      TG_load_g0(p);
      if (!TG_IsM(p)) {
        /* mark (*p), because it is a heap term, it is not marked,
	   and it may point to something in the segment */
        Gcgrey-= Total_Found;
        MarkRoot(TG_av(p));
        Gcgrey+= Total_Found;
      }
    }
  }
#endif

  /* Go from new to old. */  
  TGavag__set__resolve_ag(36, sect_trail, tr, G->trail_top);
  cp = Gc_Aux_Choice;
  while (ChoiceYounger(cp, Gc_Choice_Start)) {
    CVOID__CALL_N(mark_frames, cp->frame, FrameSize(cp->next_insn));
    ForEachChoiceX_avagvg(cp, ptr, {
      TG_load_g(ptr);
      if (!TG_IsM(ptr)) { /* TODO: why?? why not a precond? */
	TG_load_v0(ptr);
	if (IsHeapPtr(TG_v(ptr))) {
	  MarkRoot(TG_av(ptr));
	}
      }
    });
    cp = ChoiceCont(cp);

    /* Consider values of trailed variables which are not in the
       segment: they might point to heap terms in the segment. We
       don't know if these values are globally alive or not, but we
       must be conservative since we are only collecting a segment of
       heap */
    limit = cp->trail_top;
    while (TrailYounger(TG_av(tr),limit)) {
      TGavag__inc(tr, -TrailDir);
      /* TODO: trust IsHeapPtr(*TG_av(tr)) <- sure?? */
      TG_load_v(tr);
      if (TG_v(tr)==0) continue;
      TG_load_g0(tr);
      if (TG_IsM(tr)) continue;

      /* mark goals and unbound constrained variables;
	 reset unmarked bound variables */

      /* TODO: early reset of setarg? */
      
      /* TODO: rest of trail entries (HVA or SVA) remains unmarked:
	 what happens with them? -- see shunt_variable */

      /* TODO: document: without early reset the bn.pl benchmark does
	 not work */
      
      /* Early reset: "a trailed heap or local stack entry which is not
	 reachable for the forward continuation of the active computation
	 (but might be for its alternative branches, i.e. on backtracking)
	 can be set to unbound during garbage collection and the trail
	 entry itself can be discarded as well" (see Heap Garbage
	 Collection in XSB: Practice and Experience). */
      if (!IsVar(TG_v(tr))) { /* undo goal or Dsetarg */
	MarkRoot(TG_av(tr));
      } else {
#if defined(USE_EARLY_RESET)
	/* IsVar(TG_v(tr)) */
	if (TaggedIsCVA(TG_v(tr))) {
	  TGavagg__decl(ptr);
	  TGavag__set__resolve_ag(37, sect_heap, ptr, TagpPtr(CVA, TG_v(tr)));
	  TG_load_g(ptr);
	  if (!TG_IsM(ptr)) {
	    /* TODO: which one of these is correct? */
#if 1
	    TG_Put(atom_nil, ptr);
	    MarkRoot(TG_av(tr));
#else
	    TG_Put(TG_v(tr), ptr);
	    MarkRoot(TG_av(tr));
	    TG_Put(0, tr);
#endif
	  }
	} else {
	  /* precondition: no future marking will reach *TaggedToPointer(v) */
	  TGavagg__decl(ptr);
	  TGavag__set__resolve_ag(38, sect_any, ptr, TaggedToPointer(TG_v(tr)));
	  TG_load_g(ptr);
	  if (!TG_IsM(ptr)) {
	    /* early untrail (it is not used in this choice) */
	    TG_Put(TG_v(tr), ptr);
	    /* disable trail entry */
	    TG_Put(0, tr);
	  }
	}
#else /* !defined(USE_EARLY_RESET) */
	if (TaggedIsCVA(TG_v(tr))) {
	  MarkRoot(TG_av(tr));
	}
#endif
      }
    }
  }
}

/***************************************************************************/
/**** The Compaction Phase ****/

#define intoRelocationChain(j,curr) ({ \
  TG_load_v(curr); \
  TG_MoveValue_MoveR(j,curr); \
  TG_PutPtr_SetR(TG_av(curr),j); \
})

#define HeapTermIntoRelocChain(ht__ev) ({ \
  TG_UnsetM(ht__ev); \
  TG_load_v(ht__ev); \
  if (IsHeapPtr(TG_v(ht__ev))) { \
    TGavagv__decl(ht__p); \
    TGavag__set__resolve_ag(39, sect_heap, ht__p, TaggedToPointer(TG_v(ht__ev))); \
    if (GC_HEAP_IN_SEGMENT(TG_av(ht__p))) { \
      ASSERT__INTORC(ht__p,ht__ev); \
      TG_load_v(ht__p); \
      intoRelocationChain(ht__p,ht__ev); \
    } \
  } \
})

#define updateRelocationChain(A,B) ({ \
  __label__ finish; \
  TGavagvg__decl(j); \
 \
  /* R-bit is set in TG_v(A) */ \
  TGavag__set__resolve_ag(40, sect_any, j, TaggedToPointer(TG_v(A))); \
  for(;;) { \
    TG_load_g(j); \
    if (TG_IsR(j)) { \
      tagged_t *j2; \
      TG_load_v0(j); \
      j2 = TaggedToPointer(TG_v(j)); \
      TG_PutPtr_UnsetR(TG_av(B),j); \
      ASSERT__VALID_TAGGED(*TG_av(j)); \
      TGavag__set__resolve_ag(41, sect_any, j, j2); \
    } else { \
      goto finish; \
    } \
  } \
  finish: \
    { \
      tagged_t c1; \
      TG_load_v0(j); \
      c1 = TG_v(j); \
      TG_PutPtr(TG_av(B),j); \
      ASSERT__VALID_TAGGED(*TG_av(j)); \
      TG_load_v(A); \
      TG_MoveValue_UnsetR(c1,A); \
      ASSERT__VALID_TAGGED(*TG_av(A)); \
    } \
})

/* sweep frame chain */
static CVOID__PROTO_N(sweep_frames, frame_t *frame, intmach_t frame_size) {
  TGavagvg__decl(ev);
  ForEachFrameX(frame, frame_size, Gc_Stack_Start, ev, {
    TG_load_g(ev);
    if (!TG_IsM(ev)) return; /* finish, rest of frames are swept */
    HeapTermIntoRelocChain(ev);
  });
}

/* Sweep choicepoints, corresponding chains of frames and trail */
static CVOID__PROTO(sweep_choicepoints) {
  choice_t *cp;
  TGavagv__decl(tr);
  tagged_t *tr_start;
  tagged_t v;

  TGavag__set__resolve_ag(42, sect_trail, tr, G->trail_top);
  tr_start = Gc_Choice_Start->trail_top;
  while (TrailYounger(TG_av(tr), tr_start)) {
    TGavag__inc(tr, -TrailDir);
    TG_load_v(tr);
    if (TG_v(tr) == 0) continue;

    v = GC_UNMARKED(TG_v(tr));
    SwOnTrTagTU(v, { /* HVA CVA */
#if defined(USE_SEGMENTED_GC)      
      TGavagv__decl(p);
      TGavag__set__resolve_ag(43, sect_heap, p, TaggedToPointer(v));
      if (!GC_HEAP_IN_SEGMENT(TG_av(p))) {
	/* (See the equivalent code in mark_trail) */
	HeapTermIntoRelocChain(p);
	goto sw_next;
      } else {
	goto sw_default;
      }
#else
      goto sw_default;
#endif
    }, { /* SVA */
#if defined(USE_SEGMENTED_GC)      
      TGavagv__decl(p);
      TGavag__set__resolve_ag(44, sect_frame, p, TaggedToPointer(v));
      if (!GC_STACK_IN_SEGMENT(TG_av(p))) {
	/* (See the equivalent code in mark_trail) */
	HeapTermIntoRelocChain(p);
	goto sw_next;
      } else {
	goto sw_default;
      }
#else
      goto sw_default;
#endif
    }, { /* Dsetarg */
      /* (See the equivalent code in mark_trail) */
      TGavagvg__decl(ptr);
      MutatedPtrU(v, TG_av(ptr));
      if (!GC_HEAP_IN_SEGMENT(TG_av(ptr))) {
	/* the mutated arg was treated in mark_trail so we need to put
	   it in the relocation chain */
	TGavag__resolve_ag(45, sect_heap, ptr);
	TG_load_g(ptr);
	if (TG_IsM(ptr)) { /* avoid puting into relocation chain twice */
	  HeapTermIntoRelocChain(ptr);
	}
      }
      /* TODO: this is needed because of marking in mark_choicepoints */
      goto sw_default;
    }, { /* undo goal */
      goto sw_default;
    });
  sw_default:
    HeapTermIntoRelocChain(tr);
    continue;
  sw_next:
    TG_UnsetM(tr);
    continue;
  }

  cp = Gc_Aux_Choice;
  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    CVOID__CALL_N(sweep_frames, cp->frame, FrameSize(cp->next_insn));
    ForEachChoiceX_avagv(cp, ptr, {
      /* TODO: some x are not marked... right? (see line 972) */
      HeapTermIntoRelocChain(ptr);
    });
    cp = ChoiceCont(cp);
  }
}

static CVOID__PROTO(compress_heap) {
  choice_t *cp;
  TGavagvg__decl(curr);
  TGavagv__decl(dest);
  intmach_t garbage_bytes;
  intmach_t extra;

  cp = Gc_Aux_Choice;
  TGavag__set__resolve_ag(46, sect_heap, curr, G->heap_top);
  TGavag__set__resolve_ag(47, sect_heap, dest, HeapCharOffset(Gc_Heap_Start,Total_Found));
  garbage_bytes = 0;
  /* the upward phase */
  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    cp->heap_top = TG_av(dest);
    cp = ChoiceCont(cp);
	
    while (HeapYounger(TG_av(curr),cp->heap_top)) {
      TGavag__inc(curr, -1);
      TG_load_v(curr);
      TG_load_g0(curr);
#if defined(ABSMACH_OPT__qtag)
      if (BlobHF(TG_v(curr))) { /* a box tail */
#else
      if ((!TG_IsR(curr)) && BlobHF(TG_v(curr))) { /* a box tail */
#endif
	extra = BlobFunctorSizeAligned(TG_v(curr))+sizeof(functor_t);
	TGavag__charinc(curr, -extra); /* skip to box header */
	TG_load_v(curr);
	TG_load_g0(curr);
	if (TG_IsM(curr)) {
	  TGavag__charinc(dest, -extra);
	} else {
	  garbage_bytes += extra;
	}
      } else {
	extra = 0;
      }
      if (TG_IsM(curr)) {
	if (garbage_bytes >= 2*sizeof(functor_t)) {
	  /* box the garbage as a bignum (whose bits must be ignored) */
	  /* note: garbage starts at extra+sizeof(tagged_t) (or
	     extra+sizeof(functor_t)) */
	  /* todo[ts]: garbage_bytes may be greater than the maximum
	     length of a bignum!! (when USE_ATMQMASK is on) */
	  tagged_t cv2;
	  cv2 = BlobFunctorBignum((garbage_bytes-2*sizeof(functor_t))/sizeof(bignum_t));
	  TGavag__decl(ptr);
	  TGavag__copy(ptr, curr);
	  TGavag__charinc(ptr, extra+sizeof(tagged_t));
	  TG_MoveUNMARKED_M_UnsetM(cv2, ptr);
	  garbage_bytes = 0;
	} else if (garbage_bytes) {
	  /* do not box the garbage (i.e. it is shorter than the
	     smallest bignum) */
	  garbage_bytes = 0;
	}
	TGavag__inc(dest, -1);
	if (TG_IsR(curr)) {
	  updateRelocationChain(curr,dest);
	  TG_load_v(curr);
	}
	if (IsHeapPtr(TG_v(curr))) {
	  TGavagv__decl(p);
	  TGavag__set__resolve_ag(48, sect_heap, p, TaggedToPointer(TG_v(curr)));
	  if (HeapYounger(TG_av(curr),TG_av(p)) && GC_HEAP_IN_SEGMENT(TG_av(p))) {
	    ASSERT__INTORC0(TG_av(p),TG_av(curr));
            TG_load_v(p);
	    intoRelocationChain(p,curr);
	  } else if (TG_av(p)==TG_av(curr)) {
	    /* a cell pointing to itself */
	    TG_PutPtr(TG_av(dest),curr);
	  }
	}
      } else {
	garbage_bytes += sizeof(tagged_t);
      }
    }
  }

  /* The downward phase */
  /* TG_av(curr) and dest both point to the beginning of the heap */
  TGavag__charinc(curr, garbage_bytes);
  while (HeapYounger(G->heap_top,TG_av(curr))) {
    TG_load_g(curr);
    if (TG_IsM(curr)) {
      TG_load_v0(curr);
      if (TG_IsR(curr)) {
	updateRelocationChain(curr,dest);
	TG_load_v(curr);
      }
      tagged_t cv;
      cv = GC_UNMARKED_M(TG_v(curr));  /* M and R-bit off */
      {
	if (BlobHF(cv)) { /* move a box */
	  intmach_t i;
#if defined(EXTGC)
	  tagged_t *dest0 = TG_av(dest);
#endif	  
	  TG_MoveUNMARKED_M_UnsetM(cv, curr);
	  for (i = BlobFunctorSizeAligned(cv)+sizeof(functor_t); i > 0; i -= sizeof(blob_unit_t)) {
	    blob_unit_t t;
	    t = *((blob_unit_t *)TG_av(curr));
	    TG_av(curr) = HeapCharOffset(TG_av(curr), sizeof(blob_unit_t));
	    *((blob_unit_t *)TG_av(dest)) = t;
	    TG_av(dest) = HeapCharOffset(TG_av(dest), sizeof(blob_unit_t));
	  }
#if defined(EXTGC)
	  CLEAN_AREA(Heap_Start, (char *)Heap_End-(char *)Heap_Start, dest0, TG_av(dest));
#endif	  
	  TGavag__resolve_ag(49, sect_heap, curr);
	  TGavag__resolve_ag(50, sect_heap, dest);
	  TG_MoveUNMARKED_M_UnsetM(cv, dest);
	} else if (IsHeapPtr(cv)) {
	  TGavagv__decl(p);
	  TGavag__set__resolve_ag(51, sect_heap, p, TaggedToPointer(cv));
	  if (HeapYounger(TG_av(p),TG_av(curr))) {
	    /* move the current cell and insert into the reloc.chain */
	    TG_MoveUNMARKED_M_UnsetM(cv, dest);
	    ASSERT__INTORC(p,dest);
            TG_load_v(p);
	    intoRelocationChain(p,dest);
	  } else { /* just move the current cell */
	    TG_MoveUNMARKED_M_UnsetM(cv, dest);
	  }
	} else { /* just move the current cell */
	  TG_MoveUNMARKED_M_UnsetM(cv, dest);
	}
      }
      TGavag__inc(dest, 1);
      TGavag__inc(curr, 1);
    } else {
      TG_load_v0(curr);
      /* Pre: BlobHF(cv) */
      if (BlobHF(TG_v(curr))) {
	/* skip a box, of at least 2*sizeof(functor_t) size (garbage
	   has been boxed in the upward phase) */
	TGavag__charinc(curr, BlobFunctorSizeAligned(TG_v(curr))+2*sizeof(functor_t));
      } else {
	/* skip a single tagged */
	TGavag__charinc(curr, sizeof(tagged_t));
      }
    }
  }
#if defined(USE_RTCHECKS)
  /* clean freed section to catch bugs */
  TGavag__copy(curr, dest);
  while (TG_av(curr) < G->heap_top) {
    TG_MoveUNMARKED_M_UnsetM(atom_nil, curr);
    TGavag__inc(curr, 1);
  }
#endif

#if defined(EXTGC)
  /* clean remaining GC marks (GC marks of freed memory must also be 0
     with EXTGC) */
  CLEAN_AREA(Heap_Start, (char *)Heap_End-(char *)Heap_Start, TG_av(dest), G->heap_top);
#endif

  G->heap_top = TG_av(dest);
}

/***************************************************************************/
/**** The main garbage collection routine *****/

/* The X REGISTERS have been saved already in an frame */
/* note: calculate_segment_choice has to be called before */
CVOID__PROTO(gc__heap_collect) {
#if defined(USE_GCSTATS)
  intmach_t hz;
  intmach_t gc_reclaimed;
  flt64_t t1,t2;
  flt64_t mark_time, compress_time, gc_time;
#endif
  frame_t *newa;

  DEBUG__TRACE(debug_gc,
	       "Thread %ld enters gc__heap_collect\n", (long)Thread_Id);

  GetFrameTop(newa,w->choice,G->frame);

#if defined(USE_GCSTATS)
  hz = HeapCharUsed(G->heap_top); /* current heap size */
  switch (current_gctrace) {
  case GCTRACE__OFF:
    break;
  case GCTRACE__TERSE:
    print_string(stream_trace, "{GC}\n");
    break;
  case GCTRACE__VERBOSE:
    TRACE_PRINTF("\n{GC}  GC start: Heap (%lx[%lx] - %lx) size = %ld bytes\n",
		 (long)Heap_Start,
		 (long)gc_HeapStart,
		 (long)G->heap_top,
		 (long)hz);
    TRACE_PRINTF("{GC}        Stack (%lx[%lx] - %lx)\n",
		 (long)Stack_Start,
		 (long)gc_StackStart,
		 (long)newa);
    TRACE_PRINTF("{GC}     Choice (%lx[%lx] - %lx)\n",
		 (long)Choice_Start,
		 (long)gc_ChoiceStart,
		 (long)w->choice);
    TRACE_PRINTF("{GC}           Trail (%lx[%lx] - %lx)\n",
		 (long)Trail_Start,
		 (long)gc_TrailStart,
		 (long)G->trail_top);
    break;
  }
  t1 = userclick();
#endif  

#if defined(USE_REGGLOB)
  /* push special REGISTERS on the trail stack */
  TrailPush(G->trail_top,GLOBAL_VARS_ROOT);
#endif
  Total_Found = 0;
  Gcgrey = 0;
  if (w->segment_choice==InitialChoice) Gc_Total_Grey = 0;
  CVOID__CALL(trail__remove_uncond); /* sets Gc_Aux_Choice, gc_Choice_Start */
  Gc_Aux_Choice->local_top = newa;
  Gc_Aux_Choice->heap_top = G->heap_top;
  Gc_Aux_Choice->frame = G->frame;
  Gc_Aux_Choice->next_insn = G->next_insn;
  Gc_Heap_Start = gc_HeapStart;
  Gc_Stack_Start = gc_StackStart;

  Cvas_Found = atom_nil;

  if (WakeCount()) {
    /* TODO: why? */
    if (current_gctrace == GCTRACE__VERBOSE) {
      TRACE_PRINTF("{GC}  Shunting disabled due to pending unifications\n");
    }
  } else {
    CVOID__CALL(shunt_variables);
  }

  CVOID__CALL(mark_trail_cva);
  CVOID__CALL(mark_choicepoints);
  CVOID__CALL_N(trail__compress,TRUE); /* remove holes put by trail__remove_uncond and early reset in mark_choicepoints */

  Gc_Total_Grey += Gcgrey;
#if defined(USE_GCSTATS)	  
  t2 = userclick();
  mark_time = t2 - t1;
  if( current_gctrace == GCTRACE__VERBOSE ) {
    TRACE_PRINTF("{GC}  mark: %ld bytes marked in %.3f sec\n",
		 (long int)Total_Found, mark_time);
    TRACE_PRINTF("{GC}        no more than %ld garbage bytes left\n",
		 (long int)Gcgrey);
  }
#endif

  CVOID__CALL(sweep_choicepoints);
  CVOID__CALL(compress_heap);

#if defined(USE_REGGLOB)
  /* pop special regs from the trail stack */
  TrailDec(G->trail_top);
  GLOBAL_VARS_ROOT = *(G->trail_top);
#endif
  SetChoice(w->choice);	/* shadow regs may have changed */
#if defined(USE_GCSTATS)
  /* statistics */
  compress_time = userclick() - t2;
  gc_time = mark_time + compress_time;
  stats.gc_click += gc_time;
  stats.startclick += gc_time;
  stats.lastclick += gc_time;
  if (stats.gc_longest_click < gc_time) {
    stats.gc_longest_click = gc_time;
  }
  stats.gc_count++;
  gc_reclaimed = hz-HeapCharUsed(G->heap_top);
  stats.gc_acc += gc_reclaimed;
  if (current_gctrace==GCTRACE__VERBOSE) {
    TRACE_PRINTF("{GC}  Compress: %ld bytes reclaimed in %.3f sec\n",
		 (long)gc_reclaimed,
		 compress_time);
    TRACE_PRINTF("{GC}           Heap size = %ld bytes\n",
		 (long)HeapCharUsed(G->heap_top));
    TRACE_PRINTF("{GC}           Free heap = %ld bytes\n",
		 (long)HeapCharAvailable(G->heap_top));
    TRACE_PRINTF("{GC}  %ld bytes reclaimed in %ld gc's\n",
		 (long int)stats.gc_acc,
		 (long int)stats.gc_count);
    TRACE_PRINTF("{GC}  GC time = %.6f  Total = %.6f\n\n",
		 ((flt64_t)gc_time)/stats.userclockfreq,((flt64_t)stats.gc_click)/stats.userclockfreq);
  }
#endif
}

void init_gc() {
  current_gcmode = TRUE;
  current_gctrace = GCTRACE__OFF;
  current_gcmargin = 500; /* Quintus has 1024 */
}
