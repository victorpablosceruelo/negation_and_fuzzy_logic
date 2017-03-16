#include <engine/basiccontrol.native.h>

CFUN__PROTO_N(compare__1, intmach_t, tagged_t x1, tagged_t x2);

CBOOL__PROTO_N(bu2_lexeq, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0==x1 || CFUN__EVAL_N(compare__1,x0,x1)==0);
}

CBOOL__PROTO_N(bu2_lexne, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0!=x1 && CFUN__EVAL_N(compare__1,x0,x1)!=0);
}

CBOOL__PROTO_N(bu2_lexlt, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0!=x1 && CFUN__EVAL_N(compare__1,x0,x1)<0);
}

CBOOL__PROTO_N(bu2_lexle, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0==x1 || CFUN__EVAL_N(compare__1,x0,x1)<=0);
}

CBOOL__PROTO_N(bu2_lexgt, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0!=x1 && CFUN__EVAL_N(compare__1,x0,x1)>0);
}

CBOOL__PROTO_N(bu2_lexge, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0==x1 || CFUN__EVAL_N(compare__1,x0,x1)>=0);
}

CFUN__PROTO_N(fu2_compare, tagged_t, tagged_t x1, tagged_t x2) {
  intmach_t i;

  if (x1==x2) {
    CFUN__PROCEED(atom_equal);
  } else {
    i = CFUN__EVAL_N(compare__1,x1,x2);
    if (i < 0)
      CFUN__PROCEED(atom_lessthan);
    else if (i > 0)
      CFUN__PROCEED(atom_greaterthan);
    else
      CFUN__PROCEED(atom_equal);
  }
}

/* local declarations */

static CFUN__PROTO_N(compare__2, intmach_t, tagged_t x1, tagged_t x2);
static CFUN__PROTO_N(compare__args, intmach_t, intmach_t arity, tagged_t *pt1, tagged_t *pt2, tagged_t *x1, tagged_t *x2);

/* help function for builtin compare/3
 * returns -1 if u @< v
 * returns  0 if u == v
 * returns +1 if u @> v
 */
CFUN__PROTO_N(compare__1, intmach_t, tagged_t x1, tagged_t x2) {
  intmach_t result;
  result = CFUN__EVAL_N(compare__2,x1,x2);
  VALUETRAIL__UNDO();
  CFUN__PROCEED(result);
}

#define GetRank(u, urank) \
  SwTagC(u, f, { /* NUM */ \
    urank = 2; \
  },{ /* ATM */ \
    urank = 3; \
  },{ /* LST */ \
    urank = 4; \
  },{ /* STR(blob(float)) */ \
    urank = 1; \
  },{ /* STR(blob(bignum)) */ \
    urank = 2; \
  },{ /* STR(struct) */ \
    urank = 4; \
  })

static CFUN__PROTO_N(compare__2, intmach_t, tagged_t x1, tagged_t x2) {
  tagged_t u, v;
  tagged_t *pt1, *pt2;
  intmach_t i, j, urank, vrank;	/* FLO=1, INT=2, ATM=3, COMPLEX=4 */

 in:
  u=x1;
  v=x2;
  DerefSw_HVAorCVAorSVA_Other(u,{ goto var_x; },{});
  DerefSw_HVAorCVAorSVA_Other(v,{ CFUN__PROCEED(1); },{});
  if (u==v) return 0;
  if (TaggedIsSmall(u) && TaggedIsSmall(v)) goto var_var;

  GetRank(u, urank);
  GetRank(v, vrank);

  if (urank<vrank) CFUN__PROCEED(-1);
  if (urank>vrank) CFUN__PROCEED(1);
  /* same rank */
  switch (urank) {
  case 1: /* FLO, FLO */
    {
      flt64_t f1 = TaggedToFloat(u);
      flt64_t f2 = TaggedToFloat(v);

      /* TODO: document */
      if (f1<f2) CFUN__PROCEED(-1);
      if (f1>f2) CFUN__PROCEED(1);
      {
	intmach_t j;
	union {
	  flt64_t f;
	  int32_t p[2];
	} u1, u2;
	u1.f = f1;
	u2.f = f2;

	u1.f = -u1.f;
	u2.f = -u2.f;
	if (u1.p[0] == u2.p[0]) j = 1; else j = 0;

	CFUN__PROCEED((u1.p[j] < u2.p[j] ? -1 : u1.p[j] > u2.p[j] ? 1 : 0));
      }
    }
  case 2: /* INT, INT */
    {
      if (TaggedIsSmall(u)&&TaggedIsSmall(v)) {
	CFUN__PROCEED((u<v ? -1 : u>v));
      } else if (TaggedIsSmall(u)) {
	CFUN__PROCEED((bn_positive(TaggedToBignum(v)) ? -1 : 1));
      } else if (TaggedIsSmall(v)) {
	CFUN__PROCEED((bn_positive(TaggedToBignum(u)) ? 1 : -1));
      } else {
	CFUN__PROCEED(bn_compare(TaggedToBignum(u),TaggedToBignum(v)));
      }
    }
  case 3: /* ATM, ATM */
    goto compare_uv;
  case 4: /* COMPLEX, COMPLEX */
    DecompComplex(u, pt1, i);
    DecompComplex(v, pt2, j);
    if (u==v) {
      intmach_t result = CFUN__EVAL_N(compare__args,i,pt1,pt2,&x1,&x2);
      if (result) CFUN__PROCEED(result);
      goto in;
    } else if (i!=j) {
      CFUN__PROCEED((i<j ? -1 : 1));
    } else {
      goto compare_uv;
    }
  }

 compare_uv:
  {
    /* compare u and v atoms (or functor) text */
    unsigned char *up = (unsigned char *)GetString(u);
    unsigned char *vp = (unsigned char *)GetString(v);

    while ((u = *up++) && (v = *vp++)) {
      if (u!=v) CFUN__PROCEED((u<v ? -1 : 1));
    }
    CFUN__PROCEED((u ? 1 : v ? -1 : 0));
  }

 var_x:
  DerefSw_HVAorCVAorSVA_Other(v, {
    goto var_var;
  },{
    CFUN__PROCEED(-1);
  });
 var_var:
  CFUN__PROCEED((u<v ? -1 : u>v ? 1 : 0));
}

static CFUN__PROTO_N(compare__args, intmach_t, intmach_t arity, tagged_t *pt1, tagged_t *pt2, tagged_t *x1, tagged_t *x2) {
  intmach_t result;
  tagged_t t1 = ~0, t2 = ~0; /* Avoid compiler complaints */
  
  /* Adapted from terminating unification of complex structures:
     See cunify_args(). */

  VALUETRAIL__TEST_OVERFLOW(CHOICEPAD);
  for (result=0; !result && arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    /* TODO: share with code from unify_args_loop in engine/absmach_def.pl */
    if (t1 != t2) {
      HeapDerefSw_HVAorCVA_Other(t1, {
	goto noforward;
      }, {});
      HeapDerefSw_HVAorCVA_Other(t2, {
	goto noforward;
      }, {});
      if (t1 == t2) {
	goto noforward;
      } else {
	Sw_LSTorSTR_Other(t1, {
	  Sw_LSTorSTR_Other(t2, {
	    /* replace smaller value by larger value,
	       using choice stack as value trail */
	    if (t1>t2) {
	      VALUETRAIL__SET(pt1, t2);
	    } else {
	      VALUETRAIL__SET(pt2, t1);
	    }
	  }, {
	    goto noforward;
	  });
	}, {
	  goto noforward;
	});
      }
    noforward:
      if (arity>1 && t1!=t2) result = CFUN__EVAL_N(compare__2,t1,t2);
    }
    pt1++;
    pt2++;
  }
  
  if (!result) {
    *x1 = t1;
    *x2 = t2;
  }

  /* TODO: remove.. it seems to be unnecessary */
  // VALUETRAIL__TEST_OVERFLOW(CHOICEPAD); /* TODO: was equiv to CHOICEPAD/2... why???? */
  
  CFUN__PROCEED(result);
}




