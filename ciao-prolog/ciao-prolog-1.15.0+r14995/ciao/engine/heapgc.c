/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* #defines MUST precede #includes here. */
#define SEGMENTED_GC 1
#define EARLY_RESET 1

#if defined(DEBUG)
#include "threads.h"
/*#include <time.h>*/
#endif

#include "datadefs.h"
#include "gcdatadefs.h"
#include "gcsupport.h"
#include "support.h"


/* declarations for global functions accessed here */

#include "heapgc_defs.h"
#include "stacks_defs.h"
#include "start_defs.h"
#include "timing_defs.h"

/* local declarations */

static void shuntVariables(Argdecl);
static void markTrail(Argdecl);
static void markFrames(Argdecl, frame_t *frame, bcp_t l);
static void markChoicepoints(Argdecl);
static void markVariable(Argdecl, tagged_t *start);
static void updateRelocationChain(tagged_t *curr, tagged_t *dest);
static void sweepTrail(Argdecl);
static void sweepFrames(Argdecl, frame_t *frame, bcp_t l);
static void sweepChoicepoints(Argdecl);
static void compressHeap(Argdecl);


/**********************************
 *  GARBAGE COLLECTION BUILTINS   *
 **********************************/

bool_t gc_usage(Arg)
     Argdecl;
{
  ENG_FLT t;
  tagged_t x;

  t= (ENG_FLT)ciao_statistics.gc_tick*1000/GET_CLOCKFREQ(ciao_statistics);
  MakeLST(x,MakeFloat(Arg,t),atom_nil);
  t= ciao_statistics.gc_acc*sizeof(tagged_t);
  MakeLST(x,MakeInteger(Arg,t),x);
  t= ciao_statistics.gc_count;
  MakeLST(x,MakeInteger(Arg,t),x);
  return cunify(Arg,x,X(0));
}

bool_t gc_mode(Arg)
     Argdecl;
{
    Unify_constant(current_gcmode,X(0));
    DEREF(current_gcmode,X(1));
    return TRUE;
}

bool_t gc_trace(Arg)
     Argdecl;
{
  /*tagged_t new; */ /* unused */

    Unify_constant(current_gctrace,X(0));
    DEREF(current_gctrace,X(1));
    return TRUE;
}

bool_t gc_margin(Arg)
     Argdecl;
{
  /*tagged_t new; */ /*unused*/

    Unify_constant(current_gcmargin,X(0));
    DEREF(current_gcmargin,X(1));
    return TRUE;
}


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


/* gc global variables */

/*  These were shared, and a lot of havoc when concurrent GC was taking place.

long gc_total_grey=0;
node_t *gc_aux_node;
node_t *gc_choice_start;
tagged_t *gc_trail_start;

static tagged_t *gc_heap_start;
static frame_t *gc_stack_start;
static long gcgrey;
static long total_found;
static tagged_t cvas_found;

*/




/*** The Shunting Phase ***/

#define gc_shuntVariable(shunt_dest) \
{ \
  CIAO_REGISTER tagged_t shunt_src; \
  while (IsVar(shunt_dest) && \
         !gc_IsMarked(shunt_src=CTagToPointer(shunt_dest)) && \
	 shunt_src!=shunt_dest) \
    shunt_dest = shunt_src; \
}

static void shuntVariables(Arg)
     Argdecl;
{
    CIAO_REGISTER tagged_t *pt = w->trail_top;
    CIAO_REGISTER node_t *cp = Gc_Aux_Node;
    CIAO_REGISTER node_t *prevcp = w->node;
    CIAO_REGISTER try_node_t *alt = fail_alt;
    int i;
    tagged_t *limit;
    frame_t *frame;

    while (ChoiceYounger(cp,Gc_Choice_Start)) {
      limit = TagToPointer(prevcp->trail_top);
      while (TrailYounger(pt,limit)) {
        tagged_t v = TrailPop(pt);

        if (v!=0 && IsVar(v) && !gc_IsMarked(CTagToPointer(v)))
          gc_MarkM(CTagToPointer(v));
        else
          gc_MarkM(pt[0]);
      }
      gc_ReverseChoice(cp,prevcp,alt);
    }

    while (ChoiceYounger(Gc_Aux_Node,cp)) {
      gc_UndoChoice(cp,prevcp,alt);
      limit = TagToPointer(cp->trail_top);
      pt = TagToPointer(prevcp->trail_top);
      while (TrailYounger(limit,pt)) {
        tagged_t v = *pt++;

        if (!gc_IsMarked(v))
          gc_UnmarkM(CTagToPointer(v));
      }
      pt = TagToPointer(prevcp->trail_top);
      while (TrailYounger(limit,pt)) {
        tagged_t v = *pt++;

        if (gc_IsMarked(v))
          gc_UnmarkM(pt[-1]);
        else
          gc_shuntVariable(CTagToPointer(v));
      }
      pt = NodeGlobalTop(prevcp);
      while (HeapYounger(NodeGlobalTop(cp),pt)) {
        tagged_t v = *pt++;

        if (v&QMask) pt += LargeArity(v);
        else if (!gc_IsMarked(v)) {
          if (v==Tag(CVA,pt-1))
            gc_MarkM(Cvas_Found),
              pt[-1] = Cvas_Found,
              Cvas_Found = v,
              pt += 2;
          else {
            gc_shuntVariable(pt[-1]);
          }
        }
      }
      i = FrameSize(cp->next_insn);
      frame = cp->frame;
      while (OffStacktop(frame,NodeLocalTop(prevcp))) {
        pt = (tagged_t *)StackCharOffset(frame,i);
        while (pt!=frame->term)
          if (!gc_IsMarked(*(--pt)))
            gc_shuntVariable(*pt);
        i = FrameSize(frame->next_insn);
        frame = frame->frame;
      }
	
      pt = cp->term+OffsetToArity(alt->node_offset);
      while (pt!=cp->term) {
        --pt;
        gc_shuntVariable(*pt);
      }
    }
}


/**** The Marking Phase ****/

/* First mark all unbound/newly bound constraint variables,
   all old heap reference, all old stack reference.
   Must be done before any early reset is done.
*/
static void markTrail(Arg)
     Argdecl;
{
  tagged_t *tr = w->trail_top;
  tagged_t v;
  int wake_count = WakeCount;

  while (Cvas_Found!=atom_nil)	/* mark unbound CVAs */
    *tr = v = Cvas_Found,
    Cvas_Found = CTagToCVA(v),
    gc_UnmarkM(Cvas_Found),
    CTagToCVA(v) = v,
    markVariable(Arg, tr);

  /* find_constraints(Arg,Gc_Choice_Start);
     markVariable(tr);
     {
       tagged_t l = *tr;
       tagged_t v;

       while (TagIsCVA(l))
	 v = l,
	 l = CTagToPointer(v),
	 CTagToPointer(v) = v;
     }
     */
				/* mark newly bound CVAs */
  while (wake_count>0){
    CIAO_REGISTER tagged_t v= TrailPop(tr);

    if (TagIsCVA(v))
      --wake_count,
	markVariable(Arg, tr);
  }

#if defined(SEGMENTED_GC)
    /* First mark all trailed old variables */
    tr = w->trail_top;
    while (TrailYounger(tr,Gc_Trail_Start)) {
      CIAO_REGISTER tagged_t v = TrailPop(tr);
      tagged_t *p = TagToPointer(v);

      if (v!=0 && !gc_IsMarked(v) &&
          ((IsHeapVar(v) && !OffHeaptop(p,Gc_Heap_Start)) ||
           (IsStackVar(v) && !OffStacktop(p,Gc_Stack_Start)))) {
        gc_MarkM(*tr);	                     /* so won't look at it again */
        if (IsHeapTerm(*p) && !gc_IsMarked(*p) &&
            OffHeaptop(TagToPointer(*p),Gc_Heap_Start)) {
          Gcgrey-= Total_Found;
          markVariable(Arg, p);
          Gcgrey+= Total_Found;
        }
      }
    }
#endif
}

/* A frame slot is marked iff it is in the chain of environments for
   one of the frozen execution states, regardless of contents. */
static void markFrames(Arg, frame,l)
     Argdecl;
     frame_t *frame;
     bcp_t l;
/* Mark frame chain */
{
    CIAO_REGISTER tagged_t *ev;

    while (OffStacktop(frame,Gc_Stack_Start))
      {
	ev= (tagged_t *)StackCharOffset(frame,FrameSize(l));
	while (ev!=frame->term)
	  {
	    CIAO_REGISTER tagged_t v;
	
	    StackDecr(ev);
	    if (gc_IsMarked(v= *ev)) return;
	    if (IsHeapTerm(v))
	      markVariable(Arg, ev);
	    else
	      gc_MarkM(*ev);
	  }
      l= frame->next_insn;
      frame= frame->frame;
    }
}

/* A choicepoint slot is marked iff it contains a heap reference. */
/* A trail slot is marked iff it contains
   an unbound constrained variable reference or a goal.
*/
static void markChoicepoints(Arg)
     Argdecl;
/* Mark choicepoints and corresponding chains of frames */
/* and mark remaining trail entries */
{
  CIAO_REGISTER node_t *cp = Gc_Aux_Node;
  tagged_t *tr = w->trail_top;
  tagged_t *limit;

  while (ChoiceYounger(cp,Gc_Choice_Start))
    {
      CIAO_REGISTER int n = cp->next_alt->node_offset;
      CIAO_REGISTER int i = OffsetToArity(n);

      markFrames(Arg, cp->frame, cp->next_insn);
      while ((i--)>0)
	{
	  if (IsHeapTerm(cp->term[i]))
	    markVariable(Arg, &cp->term[i]);
	}
      cp = ChoiceCharOffset(cp, -n);

      /* mark goals and unbound constrained variables;
	 reset unmarked bound variables
	 between cp->trail_top and tr */

      limit = TagToPointer(cp->trail_top);
      while (TrailYounger(tr,limit))
	{
	  CIAO_REGISTER tagged_t v = TrailPop(tr);
	
	  if (v==(tagged_t)NULL || gc_IsMarked(v))
	    ;
	  else if (!IsVar(v))
	    markVariable(Arg, tr);
#ifdef EARLY_RESET
	  else if (TagIsCVA(v))
	    {
	      if (!gc_IsMarked(CTagToCVA(v)))
		CTagToCVA(v)= v, markVariable(Arg, tr), *tr= 0;
	    }
	  else
	    {
	      if (!gc_IsMarked(CTagToPointer(v)))
		CTagToPointer(v)= v, *tr= 0;
	    }
#else
	  else if (TagIsCVA(v))
	    markVariable(Arg, tr);
#endif
	}
    }
}

/* delete 0's from the trail */
void compressTrail(Arg,from_gc)
     Argdecl;
     bool_t from_gc;
{
    CIAO_REGISTER tagged_t cv, *curr, *dest;
    tagged_t *limit;
    CIAO_REGISTER node_t *cp = Gc_Aux_Node;
    CIAO_REGISTER node_t *prevcp = w->node;
    CIAO_REGISTER try_node_t *alt = fail_alt;

    while (ChoiceYounger(cp,Gc_Choice_Start)) {
      gc_ReverseChoice(cp,prevcp,alt);
    }
    curr = dest = Gc_Trail_Start;
    while (ChoiceYounger(Gc_Aux_Node,cp))
      {
	gc_UndoChoice(cp,prevcp,alt);
	limit = TagToPointer(cp->trail_top);
	while (TrailYounger(limit,curr))
	  {
	    if ((cv = TrailNext(curr)))
	      TrailPush(dest,cv);
	  }
	cp->trail_top -= limit-dest;
	if (from_gc) ChoiceptMarkPure(cp);
      }

    w->trail_top = dest;
}


static void markVariable(Arg, start)
     Argdecl;
     tagged_t *start;
/* Mark a variable.
   start always points outside the heap.  Cyclic structs require this!
   (!gc_IsMarked(*start)) is true.
*/
{
  CIAO_REGISTER tagged_t *current, *next;

  current= start;
  next= TagToPointer(*current);
  gc_MarkF(*current);
  goto first_forward;
 forward:
  if (gc_IsMarked(*current)) goto backward;
  Total_Found++;
 first_forward:
  gc_MarkM(*current);
#if defined(SEGMENTED_GC)
  if (OffHeaptop(next,Gc_Heap_Start))
#endif
    switch(TagOf(*current))
      {
      case SVA: /* No pointers from heap to stack */
	SERIOUS_FAULT("GC: stack variable in heap");
      case CVA: /* new 3-field CVA */
                /* N.B. there can be LST pointers to the second cell as well */
	if (!gc_IsForM(*(next+2)))
	  {			/* no marking in progress as CVA nor as LST */
				/* treat as 3-cell tuple */
	    gc_MarkF(PreHeapRead(next));
	    gc_MarkF(PreHeapRead(next));
	    gc_Reverse(current,next);
	    goto forward;
          }			/* otherwise, just treat the value cell */
      case HVA:
	if (gc_IsForM(*next)) goto backward;
	gc_Reverse(current,next);
	goto forward;
      case LST:
	if (gc_IsFirst(*(next+1)) ||
	    (gc_IsMarked(*next) &&
	    gc_IsMarked(*(next+1))))
          goto backward;
	gc_MarkF(PreHeapRead(next));
	gc_Reverse(current,next);
	goto forward;
      case STR:
	if (gc_IsMarked(*next))
	  ;
	else if (*next&QMask)	/* box */
	  {
	    int ar = LargeArity(*next);
	
	    gc_MarkM(*next);
	    Total_Found += ar+1;
	  }
	else if (!gc_IsFirst(*(next+1)))
	  {
	    CIAO_REGISTER int n;
	
	    for (n = Arity(*next); n>0; --n)
	      gc_MarkF(PreHeapRead(next));
	    gc_Reverse(current,next);
	    goto forward;
	  }
      default: /* all other treated as constants */
	; /* goto backward */
      }
 backward:
  while (!gc_IsFirst(*current))
    {
      /* internal cell */
      gc_Undo(current,next);
    }
  /* head of chain */
  gc_UnmarkF(*current);
  if (current!=start)
    {
      gc_Advance(current,next);
      goto forward;
    }
}


/**** The Compaction Phase ****/

#define intoRelocationChain(j,curr) \
{   *(curr)= gc_PutValueFirst(*(j),*(curr)); \
    *(j)= gc_PutValueFirst((tagged_t)curr|GC_FIRSTMASK,*(j)); }

#if defined(DEBUG)
/*static int upcount = 0;*/
#endif

static void updateRelocationChain(curr,dest)
tagged_t *curr,*dest;
{
    CIAO_REGISTER tagged_t *j;
    CIAO_REGISTER tagged_t j1,c1;


#if defined(DEBUG)
    /* Make it go slower to trace it with TOP tool */
    /* struct timespec delay = {0, 0} ;*/
    /* upcount++; */
    /* printf("%d\n", upcount); */
    /* nanosleep(&delay, NULL); */
#endif


    /* F-bit is set in *curr */
    c1= *curr;
    do {
	j= TagToPointer(c1);
	j1= *j;
	c1= gc_PutValueFirst(j1,c1);
	*(j)= gc_PutValueFirst((tagged_t)dest,j1);
	        /* dest is a pointer, i.e its F-bit is FALSE */
    }
    while (gc_IsFirst(c1));
    *curr= c1;
}



static void sweepTrail(Arg)
     Argdecl;
{
  CIAO_REGISTER tagged_t *tr;
  CIAO_REGISTER tagged_t v, *p;

  tr= w->trail_top;
  while (TrailYounger(tr,Gc_Trail_Start))
    {
      v= TrailPop(tr); /* tr now points to the popped element */
      if (v==0) continue;
      gc_UnmarkM(*tr);
      p= TagToPointer(v);
#if defined(SEGMENTED_GC)
      if ((IsHeapVar(v) && !OffHeaptop(p,Gc_Heap_Start)) ||
	  (IsStackVar(v) && !OffStacktop(p,Gc_Stack_Start)))
	{
	  CIAO_REGISTER tagged_t *p1= TagToPointer(*p);
	
	  if (IsHeapTerm(*p) &&
	      gc_IsMarked(*p) &&
	      OffHeaptop(p1,Gc_Heap_Start))
	    {
	      gc_UnmarkM(*p);
	      intoRelocationChain(p1,p);
	    }
	}
      else if (IsHeapTerm(v) && OffHeaptop(p,Gc_Heap_Start))
	intoRelocationChain(p,tr);
#else
      if (IsHeapTerm(v))
	intoRelocationChain(p,tr);
#endif
    }
}

static void sweepFrames(Arg, frame,l)
     Argdecl;
     frame_t *frame;
     bcp_t l;
/* sweep frame chain */
{
    CIAO_REGISTER tagged_t *ev;

    while (OffStacktop(frame,Gc_Stack_Start))
      {
	ev= (tagged_t *)StackCharOffset(frame,FrameSize(l));
	while (ev!=frame->term)
	  {
	    CIAO_REGISTER tagged_t v, *p;
	
            StackDecr(ev);
	    if( !gc_IsMarked(v= *ev) ) return;
	    gc_UnmarkM(*ev);
	    p= TagToPointer(v);
	    if( IsHeapTerm(v)
#if defined(SEGMENTED_GC)
	       && OffHeaptop(p,Gc_Heap_Start)
#endif
	       )
	      intoRelocationChain(p,ev);
	  }
	l= frame->next_insn;
	frame= frame->frame;
      }
}

static void sweepChoicepoints(Arg)
     Argdecl;
/* sweep choicepoints and corresponding chains of frames */
{
  CIAO_REGISTER node_t *cp = Gc_Aux_Node;

    while (ChoiceYounger(cp,Gc_Choice_Start))
      {
	CIAO_REGISTER int n = cp->next_alt->node_offset;
	CIAO_REGISTER int i = OffsetToArity(n);

	sweepFrames(Arg, cp->frame, cp->next_insn);
	while ((i--)>0)
	  {
	    CIAO_REGISTER tagged_t v= cp->term[i];
	    CIAO_REGISTER tagged_t *p= TagToPointer(v);
	
	    gc_UnmarkM(cp->term[i]);
	    if (IsHeapTerm(v)
#if defined(SEGMENTED_GC)
		&& OffHeaptop(p,Gc_Heap_Start)
#endif
		)
	      intoRelocationChain(p, &cp->term[i]);
	  }
	cp = ChoiceCharOffset(cp, -n);
      }
}


static void compressHeap(Arg)
     Argdecl;
{
    CIAO_REGISTER tagged_t cv;
    CIAO_REGISTER node_t *cp = Gc_Aux_Node;
    CIAO_REGISTER tagged_t *curr= w->global_top;
    CIAO_REGISTER tagged_t *dest= HeapOffset(Gc_Heap_Start,Total_Found);
    int garbage_words = 0;
    int extra;

    /* the upward phase */
    while (ChoiceYounger(cp,Gc_Choice_Start)) {
	cp->global_top = dest;
	cp=ChoiceCharOffset(cp,-cp->next_alt->node_offset);
	
	while (HeapYounger(curr,NodeGlobalTop(cp))) {
	    cv= HeapPop(curr);
	    if (cv&QMask) {	/* skip to box header */
    		extra = LargeArity(cv);
		
		curr -= extra;
		cv = *curr;
		if (gc_IsMarked(cv))
		  dest -= extra;
		else
		  garbage_words += extra;
	      } else
	      extra = 0;
	    if (gc_IsMarked(cv)) {
		if (garbage_words)
		  curr[extra+1] = MakeFunctorFix + ((garbage_words-3)<<2),
		  garbage_words = 0;
		HeapDecr(dest);
		if (gc_IsFirst(cv)) {
		    updateRelocationChain(curr,dest);
		    cv= *curr;
		  }
		if (IsHeapTerm(cv)) {
		    CIAO_REGISTER tagged_t *p= TagToPointer(cv);
		
		    if (HeapYounger(curr,p)
#if defined(SEGMENTED_GC)
			&& OffHeaptop(p,Gc_Heap_Start)
#endif
			) {
			intoRelocationChain(p,curr);
		      }
		    else if (p==curr)        /* a cell pointing to itself */
		      *curr= gc_PutValue((tagged_t)dest,cv);
		  }
	      } else
	      garbage_words++;
	  }
      }

    /* The downward phase */
    /* curr and dest both point to the beginning of the heap */
    curr += garbage_words;
    while (HeapYounger(w->global_top,curr)) {
	cv= *curr;
	if (gc_IsMarked(cv)) {
	    if (gc_IsFirst(cv)) {
		updateRelocationChain(curr,dest);
		cv= *curr;
	      }
	    gc_UnmarkM(cv);  /* M and F-flags off */
	    {
	      CIAO_REGISTER tagged_t *p= TagToPointer(cv);
	
	      if (IsHeapTerm(cv) && HeapYounger(p,curr)) {		
		  /* move the current cell and insert into the reloc.chain */
		  *dest= cv;
		  intoRelocationChain(p,dest);
		}
	      else if (cv&QMask) { /* move a box */
		  *curr = cv;
		  for (extra = LargeArity(cv); extra>0; extra--)
		    *dest++ = *curr++;
		  *dest = cv;
		} else		/* just move the current cell */
		*dest= cv;
	    }
	    (void)HeapNext(dest);
	  }
	else			/* skip a box---all garbage is boxed */
	  curr += LargeArity(cv);
	(void)HeapNext(curr);
      }
    w->global_top = dest;
}


/**** The main garbage collection routine *****/

void GarbageCollect(Arg)
     Argdecl;
/* The X CIAO_REGISTERS have been saved already in an frame */
{
  long hz, sz, cz, tz;
    ENG_FLT t1,t2;
    frame_t *newa;

#if defined(DEBUG)
    if (debug_gc)
      printf("Thread %d enters GarbageCollect\n", (int)Thread_Id);
#endif

    ComputeA(newa, w->node);
    hz = HeapDifference(Heap_Start,w->global_top); /* current heap size */
    sz = StackDifference(Stack_Start,w->global_top); /* current stack size */
    cz = ChoiceDifference(Choice_Start,w->global_top); /*  choicep size */
    tz = TrailDifference(Trail_Start,w->global_top); /* current trail size */
    if (current_gctrace != atom_off) {
      if (current_gctrace == atom_terse) 
        print_string(Error_Stream_Ptr, "{GC}");
      else {

        ENG_TTYPRINTF0("\n{GC}  Heap GC started\n");
        ENG_TTYPRINTF3("Heap:   from %x to %x (total size = %d)\n",
                       (unsigned int)Heap_Start, 
                       (unsigned int)Heap_End,
                       (unsigned int)HeapDifference(Heap_Start, Heap_End));
        ENG_TTYPRINTF3("        top at %x (used = %d, free = %d)\n",
                       (unsigned int)w->global_top,  
                       (unsigned int)HeapDifference(Heap_Start, w->global_top),
                       (unsigned int)HeapDifference(w->global_top, Heap_End));
        ENG_TTYPRINTF1("        GC start at %x\n", 
                       (unsigned int)gc_HeapStart);

        ENG_TTYPRINTF3("Stack:  from %x to %x (total size = %d)\n",
                       (unsigned int)Stack_Start, 
                       (unsigned int)Stack_End,
                       (unsigned int)StackDifference(Stack_Start, Stack_End));
        ENG_TTYPRINTF3("        top at %x (used = %d, free = %d)\n",
                       (unsigned int)w->local_top, 
                       (unsigned int)StackDifference(Stack_Start,w->local_top),
                       (unsigned int)StackDifference(w->local_top, Stack_End));
        ENG_TTYPRINTF1("        GC start at %x\n", 
                       (unsigned int)gc_StackStart);

        ENG_TTYPRINTF3("Choice/Trail: from %x to %x (total size = %d)\n",
                       (unsigned int)Choice_Start, 
                       (unsigned int)Choice_End,
                      (unsigned int)ChoiceDifference(Choice_Start,Choice_End));
        ENG_TTYPRINTF2("        Ch. top at %x (used = %d)\n", 
                       (unsigned int)w->node, 
                       (unsigned int)ChoiceDifference(Choice_Start, w->node));
        ENG_TTYPRINTF2("        Tr. top at %x (used = %d)\n", 
                       (unsigned int)w->trail_top, 
                      (unsigned int)TrailDifference(Trail_Start,w->trail_top));
        ENG_TTYPRINTF1("        Ch./Tr. free %d\n",
                       (unsigned int)ChoiceDifference(w->node, w->trail_top));
      }
    }

    t1 = BASE_RUNTICK;

#if defined(USE_GLOBAL_VARS)
    TrailPush(w->trail_top,GLOBAL_VARS_ROOT);
#endif
    
    /* push special CIAO_REGISTERS on the trail stack */
    TrailPush(w->trail_top,Current_Debugger_State);

    Total_Found= 0;
    Gcgrey= 0;
    if (w->segment_node == InitialNode) 
      Gc_Total_Grey = 0;
    trail_gc(Arg); /* sets Gc_Aux_Node, gc_Choice_Start, Gc_Trail_Start */
    Gc_Aux_Node->local_top = newa;
    Gc_Aux_Node->global_top = w->global_top;
    Gc_Aux_Node->frame = w->frame;
    Gc_Aux_Node->next_insn = w->next_insn;
    Gc_Heap_Start = gc_HeapStart;
    Gc_Stack_Start = gc_StackStart;

    Cvas_Found = atom_nil;

  if ( WakeCount ) {
    if (current_gctrace == atom_verbose) {
      ENG_TTYPRINTF0("{GC}  Shunting disabled due to pending unifications\n");
    }
  }
  else shuntVariables(Arg);

    markTrail(Arg);
    markChoicepoints(Arg);
    compressTrail(Arg,TRUE);

    Gc_Total_Grey += Gcgrey;
    t1 = (t2= BASE_RUNTICK)-t1;
    if (current_gctrace == atom_verbose) {
	ENG_TTYPRINTF2("        mark: %ld cells marked in %.3f sec\n",
		      Total_Found,t1);
#if defined(SEGMENTED_GC)
	ENG_TTYPRINTF1("        no more than %ld garbage cells left\n",
		      Gcgrey);
#endif
      }

    sweepTrail(Arg);
    sweepChoicepoints(Arg);
    compressHeap(Arg);
				/* pop special regs from the trail stack */
    Current_Debugger_State = TrailPop(w->trail_top);
#if defined(USE_GLOBAL_VARS)
    GLOBAL_VARS_ROOT = TrailPop(w->trail_top);
#endif
    
    SetShadowregs(w->node);	/* shadow regs may have changed */
				/* statistics */
    t2= BASE_RUNTICK-t2;
    ciao_statistics.gc_tick   += t1+t2;
    ciao_statistics.starttick += t1+t2;
    ciao_statistics.lasttick  += t1+t2;
    ciao_statistics.gc_count++;
    ciao_statistics.gc_acc+= hz-HeapDifference(Heap_Start,w->global_top);
    if( current_gctrace==atom_verbose ) {
	ENG_TTYPRINTF2("        Heap: %d cells reclaimed in %.3f sec\n",
		      (int)(hz-HeapDifference(Heap_Start,w->global_top)),
                      t2);
        ENG_TTYPRINTF3("Heap:   from %x to %x (total size = %d)\n",
                       (unsigned int)Heap_Start, 
                       (unsigned int)Heap_End,
                       (unsigned int)HeapDifference(Heap_Start, Heap_End));
        ENG_TTYPRINTF3("        top at %x (used = %d, free = %d)\n",
                       (unsigned int)w->global_top,  
                       (unsigned int)HeapDifference(Heap_Start, w->global_top),
                       (unsigned int)HeapDifference(w->global_top, Heap_End));
        ENG_TTYPRINTF1("        GC start at %x\n", 
                       (unsigned int)gc_HeapStart);

	ENG_TTYPRINTF2("        Total: %ld cells reclaimed in %ld gc's\n",
		      ciao_statistics.gc_acc,ciao_statistics.gc_count);
	ENG_TTYPRINTF2("        GC time = %.6f  Total= %.6f\n\n",
		      ((ENG_FLT)(t1+t2))/GET_CLOCKFREQ(ciao_statistics),
                      ((ENG_FLT)ciao_statistics.gc_tick)/
                                GET_CLOCKFREQ(ciao_statistics));
      }
}
