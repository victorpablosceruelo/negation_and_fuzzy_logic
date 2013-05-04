#include <engine/basiccontrol.native.h>

/* copy_term(?Old,?New):
 * Algorithm:
 * If Old is a plain variable, just return.
 * Otherwise allocate a frame containing Old and New.
 * The frame slot for Old will be progressively replaced by a copy of Old.
 * Thus all relevant parts of the old and new structures are reachable from
 * the frame slot, should GC occur.
 * While copying, all old variables encountered are bound to their copies.
 * This requires a choicepoint.
 * Finally untrail but trail any new CVA:s, deallocate frame & choicept,
 * and unify copy with New.
 */

/* TODO: does it work with cyclic terms? */

#define TopOfOldHeap TagpPtr(HVA, w->global_uncond)
#define GCTEST(Pad) { \
  if (HeapCharAvailable(G->heap_top) < (Pad)) \
    CVOID__CALL_N(heap_overflow,(Pad)); \
  if (ChoiceCharDifference(w->choice,G->trail_top) < (Pad)) \
    CVOID__CALL_N(choice_overflow,(Pad),TRUE); \
}

#define TEMPLATE_COPY_TERM(Name_copy_term, Name_copy_it, COPY_CVA) \
static CVOID__PROTO_N(Name_copy_it, tagged_t *loc); \
 \
CBOOL__PROTO(Name_copy_term) { \
  tagged_t t1, *pt1, *pt2; \
 \
  t1 = X(0); \
  DerefSw_HVA_CVA_SVA_Other(t1,{ \
    CBOOL__PROCEED; \
  }, { \
  }, { \
    CBOOL__PROCEED; \
  }, { \
  }); \
 \
  X(0) = t1; \
  CVOID__CALL_N(push_choicept,fail_alt); /* try, arity=0 */ \
  /* allocate, size=2 */ \
  { /* save all the X registers in a frame */ \
    frame_t *frame; \
    intmach_t i; \
    intmach_t arity; \
     \
    arity = 2; \
    CODE_ALLOC(frame); \
    CODE_CFRAME(frame, CONTCODE(arity)); \
    for(i=0; i<arity; i++) frame->x[i] = X(i); \
  } \
 \
  CVOID__CALL_N(Name_copy_it,&G->frame->x[0]); /* do the copying */ \
 \
  pt1 = pt2 = w->choice->trail_top; /* untrail */ \
  while (TrailYounger(G->trail_top,pt2)) { \
    /* old var */ \
    t1 = *pt2; \
    pt2 += TrailDir;	 \
    *TaggedToPointer(t1) = t1; \
  } \
  G->trail_top = pt1; \
 \
  { /* restore X registers from the frame */ \
    intmach_t arity; \
    frame_t *frame; \
    intmach_t i; \
    frame = G->frame;  \
    arity = FrameSizeToCount(FrameSize(G->next_insn));  \
    for(i=0; i<arity; i++) X(i) = frame->x[i]; \
    SetLocalTop(frame); \
    DEALLOCATE(frame); \
  } \
  /* trust */ \
  CVOID__CALL(pop_choicept);		 \
  CBOOL__LASTUNIFY(X(0),X(1)); \
} \
 \
static CVOID__PROTO_N(Name_copy_it, tagged_t *loc) { \
  tagged_t t1, t2, *pt1, *pt2; \
  intmach_t i; \
  intmach_t term_so_far; /* size of new heap before copying subterms */ \
 \
 start: \
  t1 = *loc; \
  HeapDerefSw_HVA_CVA_NUMorATM_LST_STR(t1,{ /* HVA */ \
    if (CondHVA(t1)) { \
      *loc = Tagp(HVA, loc); \
      t2 = Tagp(HVA, loc); \
      BindHVA(t1,t2); \
      return; \
    } else { \
      goto just_t1; \
    } \
  },{ /* CVA */ \
    COPY_CVA; \
  },{ /* NUM ATM */ \
    goto just_t1; \
  },{ /* LST */ \
    pt1 = TagpPtr(LST, t1); \
    pt2 = G->heap_top; \
    *loc = Tagp(LST,pt2); \
    goto copy_2_cells; \
  }, { /* STR */ \
    SwStruct(hf, t1, { /* STR(blob) */ \
      goto just_t1; \
    },{ /* STR(struct) */ \
      pt1 = TaggedToArg(t1,1); \
      pt2 = G->heap_top; \
      *loc = Tagp(STR,pt2); \
      HeapPush(pt2,hf); \
      for (i=Arity(hf); i>0; --i) { \
	t1 = *pt1; \
	pt1++; \
	HeapPush(pt2,t1); \
      } \
      G->heap_top = pt2; \
      term_so_far = HeapCharDifference(TopOfOldHeap,pt2); \
      GCTEST(CHOICEPAD); \
      for (i=Arity(hf); i>1; --i) { \
	CVOID__CALL_N(Name_copy_it,HeapCharOffset(TopOfOldHeap,term_so_far-i*sizeof(tagged_t))); \
      } \
      goto again; \
    }); \
  }); \
  return; \
 just_t1: \
  *loc = t1; \
  return; \
 copy_2_cells: \
  t1 = *pt1; \
  pt1++; \
  HeapPush(pt2,t1); \
  t1 = *pt1; \
  pt1++; \
  HeapPush(pt2,t1); \
  G->heap_top = pt2; \
  term_so_far = HeapCharDifference(TopOfOldHeap,pt2); \
  GCTEST(CHOICEPAD); \
  CVOID__CALL_N(Name_copy_it,HeapCharOffset(TopOfOldHeap,term_so_far-2*sizeof(tagged_t))); \
  goto again; \
 again: \
  GCTEST(CHOICEPAD); \
  loc = HeapCharOffset(TopOfOldHeap,term_so_far-1*sizeof(tagged_t)); \
  goto start; \
}

TEMPLATE_COPY_TERM(prolog_copy_term, copy_it, ({
    if (CondCVA(t1)) {
      pt1 = TaggedToGoal(t1);
      pt2 = G->heap_top;
      LoadCVA(t2,pt2);
      BindCVANoWake(t1,t2);
      *loc = t2;
      goto copy_2_cells;
    } else {
      goto just_t1;
    }
    }));

/* Added a version of copy_term/2 that shares attributed variables --Jose F. Morales */
TEMPLATE_COPY_TERM(prolog_copy_term_shattr, copy_it_shattr, ({
      goto just_t1;
    }));

CFUN__PROTO_N(fu2_arg, tagged_t, tagged_t number, tagged_t term) {
  tagged_t t0;
  
  DerefSw_HVAorCVAorSVA_Other(number,{
    goto barf1;
  },{
    DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(term, { /* HVA CVA SVA */
      goto barf2;
    }, { /* NUM ATM */
      goto barf2;
    }, { /* LST */ 
      if (number==MakeSmall(1)) {
	t0 = *TaggedToCar(term);
	CFUN__PROCEED(t0);
      } else if (number==MakeSmall(2)) {
	t0 = *TaggedToCdr(term);
	CFUN__PROCEED(t0);
      } else {
	goto barf1;
      }
    }, { /* STR */
      SwStruct(f, term, { /* STR(blob) */
	goto barf1;
      }, { /* STR(struct) */
	/* TODO: do we check that 'number' is a small integer? */
	intmach_t i = GetSmall(number);
	if (i<=0 || i>Arity(f)) goto barf1;
	t0 = *TaggedToArg(term,i);
	CFUN__PROCEED(t0);
      });
    });
  });

 barf1:
  MINOR_FAULT("arg/3: incorrect 1st argument");

 barf2:
  MINOR_FAULT("arg/3: incorrect 2nd argument");
}

/*---------------------------------------------------------------*/

CBOOL__PROTO_N(bu3_functor, tagged_t term, tagged_t name, tagged_t arity) {
  tagged_t tagarity;

  DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(term, { /* HVA CVA SVA */
    DerefVar(name);
    DerefVar(arity);
    if (IsAtomic(name) && (arity==MakeSmall(0))) {
      CBOOL__LASTUNIFY(name,term);
    } else if (TaggedIsATM(name) &&
	       (arity>MakeSmall(0)) && (arity<MakeSmall(ARITYLIMIT))) {
      CBOOL__LASTUNIFY(CFUN__EVAL_N(make_structure, SetArity(name,GetSmall(arity))), term);
    } else {
      CBOOL__FAIL;
    }
  }, { /* NUM ATM */
    tagarity = MakeSmall(0);
    goto unif;
  }, { /* LST */
    term = atom_lst;
    tagarity = MakeSmall(2);
    goto unif;
  }, { /* STR */
    SwStruct(f, term, { /* STR(blob) */
      tagarity = MakeSmall(0);
      goto unif;
    }, { /* STR(struct) */
      term = FUNCTOR_NAME(f);
      tagarity = MakeSmall(Arity(f));
      goto unif;
    });
  });

 unif:
  CBOOL__UnifyCons(tagarity,arity);
  CBOOL__LASTUNIFY(term,name);
}

/*---------------------------------------------------------------*/

CBOOL__PROTO_N(bu2_univ, tagged_t term, tagged_t list) {
  tagged_t car, cdr, *argp, *argq;
  intmach_t arity;
  tagged_t f;

  DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(term, { /* HVA CVA SVA */
    goto construct;
  }, { /* NUM ATM */
    goto nil_list;
  }, { /* LST */
    f = functor_lst;
    argp = TaggedToCar(term);
    argq = HeapCharOffset(argp,2*sizeof(tagged_t));
    goto build_list;
  }, { /* STR */
    SwStruct(hf, term, { /* STR(blob) */
      goto nil_list;
    }, { /* STR(struct) */
      f = hf;
      argp = TaggedToArg(term,1);
      argq = HeapCharOffset(argp,Arity(f)*sizeof(tagged_t));
      goto build_list;
    });
  });

 nil_list:
  MakeLST(cdr,term,atom_nil);
  CBOOL__LASTUNIFY(cdr,list);
  
 build_list:
  cdr = atom_nil;
  while (HeapYounger(argq,argp)) {
    argq--;
    car = *argq;
    MakeLST(cdr,car,cdr);
  }
  MakeLST(cdr,FUNCTOR_NAME(f),cdr);
  CBOOL__LASTUNIFY(cdr,list);

 construct:
  cdr = list;
  DerefSw_HVAorCVAorSVA_Other(cdr,{
    goto bomb;
  },{
    if (TaggedIsLST(cdr)) {
      DerefCar(f,cdr);
      DerefCdr(cdr,cdr);
      if (IsAtomic(f) && (cdr==atom_nil))
	CBOOL__LASTUNIFY(f,term);
      if (IsVar(f)) goto bomb;
      if (!TaggedIsATM(f))
	MINOR_FAULT("=../2: incorrect 2nd argument");
  
      arity = 0;
      argp = G->heap_top;
      HeapPush(G->heap_top,f);
      while (TaggedIsLST(cdr) && arity<ARITYLIMIT) {
	DerefCar(car,cdr);
	DerefCdr(cdr,cdr);
	HeapPush(G->heap_top,car);
	arity++;
      }
      if (IsVar(cdr)) goto bomb;
      if (cdr!=atom_nil || arity==ARITYLIMIT)
	MINOR_FAULT("=../2: incorrect 2nd argument");
  
      f = SetArity(f,arity);
      if (f==functor_lst) { /* rewrite as a list; note: cannot handle LST cast directly because arity is not known until all the input is processed */
	G->heap_top = argp;
	argq = HeapCharOffset(G->heap_top,1*sizeof(tagged_t));
	car = *argq;
	argq++;
	cdr = *argq;
	argq++;
	HeapPush(G->heap_top,car);
	HeapPush(G->heap_top,cdr);
	CBOOL__LASTUNIFY(Tagp(LST,argp),term);
      } else {
	*argp = f;
	CBOOL__LASTUNIFY(Tagp(STR,argp),term);
      }
    } else { /* cdr is not LST */
      MINOR_FAULT("=../2: incorrect 2nd argument");
    }
  });

 bomb:
  USAGE_FAULT("=../2: illegal arguments");
}

