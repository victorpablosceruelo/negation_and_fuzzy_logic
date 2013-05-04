#include <engine/basiccontrol.native.h>

extern bool_t gcexplicit;
CBOOL__PROTO(gc_start) {
  gcexplicit = TRUE;
  CVOID__CALL_N(heap_overflow,CALLPAD*2);
  CBOOL__PROCEED;
}

/* 
   Support for generating new atoms with "funny names", always different.
   Make sure that the generation works OK with concurrency.  */

/* This seems to be the right size: one character less, and time (at large)
   doubles; one character more, and comparison in the symbol table takes
   longer. */
#define NEW_ATOM_LEN 13
#define NUM_OF_CHARS 62
static char allowed_char_table[NUM_OF_CHARS + 1] =
"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
static char new_atom_str[] = "!!!!!!!!!!!!!";
#define FIRST_CHAR 0
#define LAST_CHAR  (NUM_OF_CHARS-1)

static uintmach_t x = 13*17;

CBOOL__PROTO(prolog_new_atom) {
  ERR__FUNCTOR("prolog_sys:new_atom", 1);
  intmach_t i;
  intmach_t previous_atoms_count;
  tagged_t new_atom;

  DEREF(X(0), X(0));
  if (!IsVar(X(0)))
    ERROR_IN_ARG(X(0), 1, VARIABLE);

  Wait_Acquire_slock(atom_id_l);

  previous_atoms_count = prolog_atoms->count;
  do {
    for (i = 0; i < NEW_ATOM_LEN; i++) {
      x = (((new_atom_str[i] + x - FIRST_CHAR) * 13) + 300031);
      new_atom_str[i] = allowed_char_table[(x % NUM_OF_CHARS) + FIRST_CHAR];
      x = x / NUM_OF_CHARS;
    }
    new_atom = GET_ATOM(new_atom_str);
    /* Make sure no smart guy already inserted the atom we have in mind */
  } while(prolog_atoms->count == previous_atoms_count);

  Release_slock(atom_id_l);
  CBOOL__LASTUNIFY(X(0), new_atom);
}

/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       CURRENT_ATOM/1
   -----------------------------------------------------------------------*/

extern try_node_t *address_nd_current_atom;

CBOOL__PROTO(current_atom) {
#if defined(ABSMACH_OPT__functor_table)
  /* TODO: why??? fix! */
  fprintf(stderr, "{bug: nd_current_atom is not yet supported in ABSMACH_OPT__functor_table mode}\n");
  CBOOL__FAIL;
#else
  DEREF(X(0),X(0));
  if (TaggedIsATM(X(0)))
    CBOOL__PROCEED;
  if (!IsVar(X(0)))
    MINOR_FAULT("current_atom/1: incorrect 1st arg");
#if defined(ABSMACH_OPT__atomgc)
  { 
    /* There is an (improbable) case: the 0-th table entry is empty.
       Take it into account. */
    intmach_t size = HASHTAB_SIZE(prolog_atoms) >> 1;
    intmach_t i = 0;
    while (i < size && (atmtab[i] == NULL))
      i++;
    X(1) = MakeSmall(i);  /* Yes, I am not considering an empty symbol table */
  }
#else
  X(1) = TaggedZero;
#endif
  CVOID__CALL_N(push_choicept,address_nd_current_atom);
  CBOOL__LASTCALL(nd_current_atom);
#endif
}

CBOOL__PROTO(nd_current_atom) {
#if defined(ABSMACH_OPT__functor_table)
  fprintf(stderr, "{bug: nd_current_atom is not yet supported in ABSMACH_OPT__functor_table mode}\n");
  CBOOL__FAIL;
#else
  intmach_t i = GetSmall(X(1));

#if defined(ABSMACH_OPT__atomgc)
 /* 

    Atom GC may leave holes in the atom table.  Therefore: 
    
    1- The "following" valid entry is not necessarily the next one; 
    we may have to skip a number of empty atom entries.

    2- Stopping when the number of indices read reaches the current number of
    atoms is not right.  We have to use instead the size of the table. 

  */

  intmach_t size = HASHTAB_SIZE(prolog_atoms) >> 1;

  /* Invariant: at entry, the current i points to a nonempty atom */
  CBOOL__UnifyCons(Tagn(ATM,i),X(0));
  /* Go forward until the next non-empty atom; final stop when the end of
     the table has been reached.  */
  i++;
  while(i < size && (atmtab[i] == NULL)) i++;
  
  if (i < size) {
    /* We got the next index */
    w->choice->x[1] = MakeSmall(i);
  } else {
    CVOID__CALL(pop_choicept);
  }
#else
  w->choice->x[1] = SmallAdd(w->choice->x[1], 1);
  CBOOL__UnifyCons(Tagn(ATM,i),X(0));
    
  if (i+1 == prolog_atoms->count) {
    CVOID__CALL(pop_choicept);
  }
#endif
  CBOOL__PROCEED;
#endif
}

/* ------------------------------------------------------------------ */

extern int64_t (*userclick)(void);
extern int64_t (*systemclick)(void);
extern int64_t (*wallclick)(void);

#if defined(ABSMACH_OPT__mem_profile)
extern intmach_t hwm_global;
extern intmach_t hwm_local;
extern intmach_t hwm_trail;
extern intmach_t hwm_choice;
#endif

CBOOL__PROTO(statistics) {
#if defined(PROFILE_STATS)
  intmach_t used, free, total;
  frame_t *newa;

  TRACE_PRINTF("general opts: %s\n", GENERAL_OPTS);
  TRACE_PRINTF("tagscheme opts: %s\n", TAGSCHEME_OPTS);
  TRACE_PRINTF("tagged size: %ld bits\n", (long)(sizeof(tagged_t)*8));
  TRACE_PRINTF("pointer size: %ld bits\n", (long)tagged__ptr_size);
  TRACE_PRINTF("small number size: %ld bits\n", (long)tagged__num_size);
  TRACE_PRINTF("bignum unit size: %ld bits\n", (long)(sizeof(bignum_t)*8));
  TRACE_PRINTF("tagged tag mask: 0x%lx\n", (unsigned long)TAGMASK);
  TRACE_PRINTF("tagged pointer mask: 0x%lx\n", (unsigned long)PTRMASK);
#if defined(EXTGC)
  TRACE_PRINTF("extgc gc mask: 0x%lx\n", (unsigned long)EXTGC_ANYMASK);
#else
  TRACE_PRINTF("tagged gc mask: 0x%lx\n", (unsigned long)GC_ANYMASK);
#endif

#if defined(USE_OWN_MALLOC)
#if defined(USE_MMAP) && OWNMALLOC_MmapAllowed
  TRACE_PRINTF("ownmalloc mmap size: 0x%lx\n", (unsigned long)OWNMALLOC_MmapSize);
#else
  TRACE_PRINTF("ownmalloc block size: 0x%lx\n", (unsigned long)OWNMALLOC_BLOCKSIZE);
#endif
#else
  TRACE_PRINTF("ownmalloc disabled\n");
#endif
  TRACE_PRINTF("smallptr base: 0x%lx\n", (unsigned long)SMALLPTR_BASE);
  TRACE_PRINTF("smallptr mask: 0x%lx\n", (unsigned long)SMALLPTR_MASK);
  TRACE_PRINTF("smallptr upper free bits: %ld\n", (long)SMALLPTR_UPPERBITS);
  TRACE_PRINTF("smallptr lower free bits: %ld\n", (long)SMALLPTR_LOWERBITS);

  /*
  TRACE_PRINTF("memory (total)    %10ld bytes\n",
               (intmach_t)((char *)sbrk(0)-mem_start));
  */
  TRACE_PRINTF("total memory allocated: %ld bytes\n", (long)total_mem_count);

  TRACE_PRINTF("program space (including reserved for atoms): %ld bytes\n", 
	       (long)mem_prog_count);
  TRACE_PRINTF("number of atoms and functor/predicate names: %ld\n", 
	       (long)prolog_atoms->count);
  TRACE_PRINTF("number of predicate definitions: %ld\n", 
	       (long)num_of_predicates);

  used = HeapCharDifference(Heap_Start,G->heap_top);
  free = HeapCharAvailable(G->heap_top);
  TRACE_PRINTF("global stack allocated: %ld bytes\n",
	       (long)(used+free));
  TRACE_PRINTF("global stack used (including dead data): %ld bytes\n",
	       (long)used);
  TRACE_PRINTF("global stack free: %ld bytes\n",
	       (long)free);
#if defined(ABSMACH_OPT__mem_profile)
  TRACE_PRINTF("global stack high water mark: %ld bytes\n",
	       (long)hwm_global);
#endif

  GetFrameTop(newa,w->choice,G->frame);
  used = StackCharUsed(newa);
  free = StackCharAvailable(newa);
  TRACE_PRINTF("local stack allocated: %ld bytes\n",
	       (long)(used+free));
  TRACE_PRINTF("local stack used: %ld bytes\n",
	       (long)used);
  TRACE_PRINTF("local stack free: %ld bytes\n",
	       (long)free);
#if defined(ABSMACH_OPT__mem_profile)
  TRACE_PRINTF("local stack high water mark: %ld bytes\n",
	       (long)hwm_local);
#endif

  total = TrailCharDifference(Trail_Start, Trail_End);
  TRACE_PRINTF("control+trail stack allocated: %ld bytes\n",
	       (long)total);
  used = TrailCharDifference(Trail_Start,G->trail_top);
  TRACE_PRINTF("control+trail stack used for trail (including dead data): %ld bytes\n",
	       (long)used);
  used = ChoiceCharDifference(Choice_Start,w->choice);
  TRACE_PRINTF("control+trail stack used for choice: %ld bytes\n",
	       (long)used);

  /*
  used = TrailCharDifference(Trail_Start,G->trail_top);
  free = TrailCharDifference(G->trail_top,w->choice)/2;
  TRACE_PRINTF("trail stack total: %ld bytes\n",
	       used+free);
  TRACE_PRINTF("trail stack used: %ld bytes\n",
	       used);
  TRACE_PRINTF("trail stack free: %ld bytes\n",
	       free);
  used = ChoiceCharDifference(Choice_Start,w->choice);
  free = ChoiceCharDifference(w->choice,G->trail_top)/2;
  TRACE_PRINTF("control stack total: %ld bytes\n",
	       used+free);
  TRACE_PRINTF("control stack used: %ld bytes\n",
	       used);
  TRACE_PRINTF("control stack free: %ld bytes\n",
	       free);
  */
#if defined(ABSMACH_OPT__mem_profile)
  TRACE_PRINTF("trail stack high water mark: %ld bytes\n",
	       (long)hwm_trail);
#endif
#if defined(ABSMACH_OPT__mem_profile)
  TRACE_PRINTF("control stack high water mark: %ld bytes\n",
	       (long)hwm_choice);
#endif

  TRACE_PRINTF("space overflows: %ld global, %ld local, and %ld control in %.6f sec.\n",
	       (long)stats.ss_global,
	       (long)stats.ss_local,
	       (long)stats.ss_control,
	       ((flt64_t)stats.ss_click)/stats.userclockfreq);
  TRACE_PRINTF("garbage collections: %ld passes which collected %ld bytes in %.6f sec.\n",
	       (long)stats.gc_count,
	       (long)stats.gc_acc,
	       ((flt64_t)stats.gc_click)/stats.userclockfreq);
  TRACE_PRINTF("longest garbage collection: %.6f sec.\n",
	       ((flt64_t)stats.gc_longest_click)/stats.userclockfreq);

  TRACE_PRINTF("total execution time: %.6f run, %.6f user, %.6f system, %.6lf wall (seconds)\n",
	       (flt64_t)(userclick()-stats.startclick)/stats.userclockfreq,
	       (flt64_t)(userclick()-stats.startuserclick)/stats.userclockfreq,
	       (flt64_t)(systemclick()-stats.startsystemclick)/stats.systemclockfreq,
	       (flt64_t)(wallclick()-stats.startwallclick)/stats.wallclockfreq);
#if 0
  /* TODO: this information is redundant */
  TRACE_PRINTF("total execution time in clock clicks: %ld run, %ld user, %ld system, %ld wall\n",
	       (long long)(userclick()-stats.startclick),
	       (long long)(userclick()-stats.startuserclick),
	       (long long)(systemclick()-stats.startsystemclick),
	       (long long)(wallclick()-stats.startwallclick));
  /* TODO: this information has nothing to do with statistics but with
     CPU specs */
  TRACE_PRINTF("clock frequency: %ld run-user %ld Hz system %ld Hz wall\n",
	       (long long)stats.userclockfreq,
	       (long long)stats.systemclockfreq,
	       (long long)stats.wallclockfreq);
#endif
#endif
  CBOOL__PROCEED;
}

