#include <engine/basiccontrol.native.h>

/* instance */
/* TODO: optimize, port to improlog, make it work with cyclic terms */

static CBOOL__PROTO_N(cinstance_args_aux, intmach_t arity, tagged_t *pt1, tagged_t *pt2, tagged_t *x1, tagged_t *x2, intmach_t *n);
static CBOOL__PROTO_N(cinstance_aux, tagged_t x1, tagged_t x2, intmach_t *n);

CBOOL__PROTO_N(cinstance_args, intmach_t arity, tagged_t *pt1, tagged_t *pt2, intmach_t *n) {
  tagged_t x1;
  tagged_t x2;
  CBOOL__CALL_N(cinstance_args_aux, arity, pt1, pt2, &x1, &x2, n);
  CBOOL__LASTCALL_N(cinstance_aux, x1, x2, n);
}

#define ENSURE_CHOICE(Pad) { \
  if (ChoiceCharDifference(w->choice,G->trail_top) < (Pad)) \
    CVOID__CALL_N(choice_overflow,(Pad),TRUE); \
}

static CBOOL__PROTO_N(cinstance_args_aux, intmach_t arity, tagged_t *pt1, tagged_t *pt2, tagged_t *x1, tagged_t *x2, intmach_t *n) {
  tagged_t t1 = ~0;
  tagged_t t2 = ~0;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  /* really: < 2*arity */
  ENSURE_CHOICE(2*CHOICEPAD);

  for (; arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    if (arity>1) {
      CBOOL__CALL_N(cinstance_aux, t1, t2, n);
    }
    pt1 = HeapCharOffset(pt1, sizeof(tagged_t));
    pt2 = HeapCharOffset(pt2, sizeof(tagged_t));
  }

  *x1 = t1;
  *x2 = t2;

  ENSURE_CHOICE(CHOICEPAD);
  CBOOL__PROCEED;
}

CBOOL__PROTO(cinstance) {
  tagged_t t1;
  // tagged_t t2;
  tagged_t *pt1;
  tagged_t *pt2;
  bool_t result;
  intmach_t n;

  // t1 = X(0);
  // t2 = X(1);
  n = 0;

  /* create a choice point to undo temporal bindings */
  CVOID__CALL_N(push_choicept, fail_alt);

  /* check if X(0) is an instance of X(1) */
  result = CBOOL__SUCCEED_N(cinstance_aux, X(0), X(1), &n);

  /* untrail temporal bindings */
  /* TODO: merge with untrail in copy_it */
  pt1 = pt2 = w->choice->trail_top; /* untrail */
  while (TrailYounger(G->trail_top,pt2)) {
    /* old var */
    t1 = *pt2;
    pt2 += TrailDir;	
    *TaggedToPointer(t1) = t1;
  }
  G->trail_top = pt1;

  /* remove the choice point */
  CVOID__CALL(pop_choicept);
  
  CBOOL__LASTTEST(result);
}

static CBOOL__PROTO_N(cinstance_aux, tagged_t x1, tagged_t x2, intmach_t *n) {
  tagged_t u, v, t1, nt;

 in:
  u=x1;
  v=x2;

  nt = MakeSmall(*n);

  DerefSw_HVA_CVA_SVA_Other(u, {
    DerefSw_HVA_CVA_SVA_Other(v, {
      /* HVA x HVA */ BindHVA(v, nt); if (u != v) { BindHVA(u, nt); } goto var_win;
    }, {
      /* HVA x CVA */ goto lose; /* CVAs are not supported */
    }, {
      /* HVA x SVA */ BindSVA(v, nt); if (u != v) { BindHVA(u, nt); } goto var_win;
    }, {
      /* HVA x NVA */ goto lose;
    });
  }, {
    DerefSw_HVA_CVA_SVA_Other(v, {
      /* CVA x HVA */ goto lose; /* CVAs are not supported */
    }, {
      /* CVA x CVA */ goto lose; /* CVAs are not supported */
    }, {
      /* CVA x SVA */ goto lose; /* CVAs are not supported */
    }, {
      /* CVA x NVA */ goto lose; /* CVAs are not supported */
    });
  }, {
    DerefSw_HVA_CVA_SVA_Other(v, {
      /* SVA x HVA */ BindHVA(v, nt); if (u != v) { BindSVA(u, nt); } goto var_win;
    }, {
      /* SVA x CVA */ goto lose; /* CVAs are not supported */
    }, {
      /* SVA x SVA */ BindSVA(v, nt); if (u != v) { BindSVA(u, nt); } goto var_win;
    }, {
      /* SVA x NVA */ goto lose;
    });
  }, {
    DerefSw_HVA_CVA_SVA_Other(v, {
      /* NVA x HVA */ BindHVA(v,u); goto win;
    }, {
      /* NVA x CVA */ goto lose; /* CVAs are not supported */
    }, {
      /* NVA x SVA */ BindSVA(v,u); goto win;
    }, { /* NVA x NVA */
      if (u == v) {
	/* same NVA */
	goto win;
      } if (!TaggedSameTag(u, v)) {
	  /* fail */
	  goto lose;
      } else {
	Sw_NUMorATM_LST_STR(u, { /* NUM ATM */
	  /* fail */
	  goto lose;
	}, {
	  /* LST x LST */
	  CBOOL__CALL_N(cinstance_args_aux, 2, TaggedToCar(u), TaggedToCar(v), &x1, &x2, n);
	  goto in;
	}, {
	  /* STR x STR */
	  t1 = TaggedToHeadfunctor(v);
	  if (TaggedToHeadfunctor(u) != t1) {
	    /* fail */
	    goto lose;
	  } else {
	    if (FunctorIsBlob(t1)) {
	      /* STRBlob x STRBlob */
	      CBOOL__TEST(compare_blob(TagpPtr(STR, u), TagpPtr(STR, v)));
	      goto win;
	    } else {
	      /* STRStruct x STRStruct */
	      CBOOL__CALL_N(cinstance_args_aux, Arity(t1), TaggedToArg(u,1), TaggedToArg(v,1), &x1, &x2, n);
	      goto in;
	    }
	  }
	});
      }
    });
  });

 var_win:
  (*n)++;
  goto win;

  /* two non variables */
 win:
  CBOOL__PROCEED;
 lose:
  CBOOL__FAIL;
}

/* --------------------------------------------------------------------------- */
