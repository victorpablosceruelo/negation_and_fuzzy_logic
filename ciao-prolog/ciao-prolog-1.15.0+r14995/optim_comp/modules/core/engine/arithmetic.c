#include <engine/basiccontrol.native.h>

#include <math.h>

#if defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(Win32) || defined(IRIX)
# define aint(f) (f>=0.0 ? floor(f) : ceil(f))
#endif

/* Switch on arithmetic values */
/* TODO: dangerous! NEVAL may do heap overflows and lose pointers in t
   u ... <- must fix this (i.e. M = A*A*A*A*A*A*A*A*A*A*A*A*A*A*A*A*A,
   N is M)... */
/* a single value */
#define EvalSwitch1(T, SMALL, NONSMALL) { \
  DerefSw_HVAorCVAorSVA_Other((T), { \
    ERROR_IN_ARG((T), 1, EVALUABLE); \
  }, { \
    if (TaggedIsSmall((T))) { SMALL; } \
    NEVAL((T), ERROR_IN_ARG((T), 1, EVALUABLE)); \
    if (TaggedIsSmall((T))) { SMALL; } \
    NONSMALL; \
  }); \
}
/* a pair of values (call SMALL if both are small) */
#define EvalSwitch2(T, U, SMALL, NONSMALL) { \
  DerefSw_HVAorCVAorSVA_Other((T), { \
    ERROR_IN_ARG((T), 1, EVALUABLE); \
  }, { \
    DerefSw_HVAorCVAorSVA_Other((U), { \
      ERROR_IN_ARG((U), 2, EVALUABLE); \
    }, { \
      if (TaggedIsSmall((T)) && TaggedIsSmall((U))) { SMALL; } \
      NEVAL((T), ERROR_IN_ARG((T), 1, EVALUABLE)); \
      NEVAL((U), ERROR_IN_ARG((U), 2, EVALUABLE)); \
      if (TaggedIsSmall((T)) && TaggedIsSmall((U))) { SMALL; } \
      NONSMALL; \
    }); \
  }); \
}
/* deref and eval the argument number I */
#define DerefEval(T, I) { \
  DerefSw_HVAorCVAorSVA_Other((T), { \
    ERROR_IN_ARG((T), (I), EVALUABLE); \
  }, { \
    NEVAL((T), ERROR_IN_ARG((T), (I), EVALUABLE)); \
  }); \
}

static CFUN__PROTO_N(lsh_internal, tagged_t, tagged_t t, intmach_t dist);
static CFUN__PROTO_N(rsh_internal, tagged_t, tagged_t t, intmach_t dist);

CFUN__PROTO_N(evaluate, tagged_t, tagged_t v) {
  ERR__FUNCTOR("arithmetic:is", 2);

 restart:
  SwEval(v, t_head_functor, { /* NUM */
    CFUN__PROCEED(v);
  }, { /* LST */ 
    tagged_t t;
    DerefCdr(t,v);
    if (t != atom_nil) goto err;
    DerefCar(v,v);
    goto restart;
  }, { /* STR(blob) */
    CFUN__PROCEED(v);
  }, { /* STR(struct) */
    void *Proc;
    Proc = hashtab_get(switch_on_function,t_head_functor)->value.proc;
    if (Proc==NULL) goto err;
    switch (Arity(t_head_functor)) {
    case 1:
      {
	tagged_t t;
	t = *TaggedToArg(v,1);
	CFUN__LASTCALL_N(((ctagged1_t)(Proc)),t);
      }
    case 2:
      {
	tagged_t t;
	tagged_t u;
	t = *TaggedToArg(v,1);
	u = *TaggedToArg(v,2);
	CFUN__LASTCALL_N(((ctagged2_t)(Proc)),t,u);
      }
    }
    goto err;
  }, { /* other */
    goto err;
  });
 err:
  ERROR_IN_ARG(v, 1, EVALUABLE);
}

CBOOL__PROTO_N(bu2_numeq, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:(=:=)", 2);
  tagged_t t,u;
  
  t=x0;
  u=x1;
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)==TaggedToFloat(u));
  } else if (TaggedIsSmall(t) || TaggedIsSmall(u)) {
    CBOOL__FAIL;
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))==0);
  }

 small:
  CBOOL__LASTTEST(t==u);
}

CBOOL__PROTO_N(bu2_numne, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:(=\\=)", 2);
  tagged_t t,u;
  
  t=x0;
  u=x1;
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)!=TaggedToFloat(u));
  } else if (TaggedIsSmall(t) || TaggedIsSmall(u)) {
    CBOOL__PROCEED;
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))!=0);
  }
  
 small:
  CBOOL__LASTTEST(t!=u);
}

CBOOL__PROTO_N(bu2_numlt, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:(<)", 2);
  tagged_t t,u;
  
  t=x0;
  u=x1;
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)<TaggedToFloat(u));
  } else if (TaggedIsSmall(t)) {
    CBOOL__LASTTEST(bn_positive(TaggedToBignum(u)));
  } else if (TaggedIsSmall(u)) {
    CBOOL__LASTTEST(!bn_positive(TaggedToBignum(t)));
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))<0);
  }

 small:
  CBOOL__LASTTEST(t<u);
}

CBOOL__PROTO_N(bu2_numle, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:(=<)", 2);
  tagged_t t,u;
  
  t=x0;
  u=x1;
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)<=TaggedToFloat(u));
  } else if (TaggedIsSmall(t)) {
    CBOOL__LASTTEST(bn_positive(TaggedToBignum(u)));
  } else if (TaggedIsSmall(u)) {
    CBOOL__LASTTEST(!bn_positive(TaggedToBignum(t)));
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))<=0);
  }

 small:
  CBOOL__LASTTEST(t<=u);
}

CBOOL__PROTO_N(bu2_numgt, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:(>)", 2);
  tagged_t t,u;
  
  t=x0;
  u=x1;
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)>TaggedToFloat(u));
  } else if (TaggedIsSmall(t)) {
    CBOOL__LASTTEST(!bn_positive(TaggedToBignum(u)));
  } else if (TaggedIsSmall(u)) {
    CBOOL__LASTTEST(bn_positive(TaggedToBignum(t)));
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))>0);
  }

 small:
  CBOOL__LASTTEST(t>u);
}

CBOOL__PROTO_N(bu2_numge, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:(>=)", 2);
  tagged_t t,u;
  
  t=x0;
  u=x1;
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)>=TaggedToFloat(u));
  } else if (TaggedIsSmall(t)) {
    CBOOL__LASTTEST(!bn_positive(TaggedToBignum(u)));
  } else if (TaggedIsSmall(u)) {
    CBOOL__LASTTEST(bn_positive(TaggedToBignum(t)));
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))>=0);
  }
  
 small:
  CBOOL__LASTTEST(t>=u);
}

CFUN__PROTO_N(fu1_minus, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$-'", 2);
  tagged_t t;

  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    CFUN__PROCEED(BoxFloatCheck(-TaggedToFloat(t)));
  } else {
    CFUN__LASTCALL_N(bn_call1,bn_minus,t);
  }

 small:
  if (t==TaggedLow) {
    CFUN__PROCEED(IntmaxPlus1ToTagged());
  } else {
    CFUN__PROCEED(TaggedZero-(t-TaggedZero));
  }
}

CFUN__PROTO_N(fu1_plus, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$+'", 2);
  tagged_t t;

  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t)));
  } else {
    CFUN__LASTCALL_N(bn_call1,bn_plus,t);
  }

 small:
  CFUN__PROCEED(t);
}

CFUN__PROTO_N(fu1_integer, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$integer'", 2);
  tagged_t t;

  /* TODO: if p!=NULL we require a copy... since the source number
     lives in the bytecode... what p?? */
  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    /* TODO: use a TaggedToFloat version that assumes that it is a float */
    CFUN__LASTCALL_N(bn_from_float_check, TaggedToFloat(t));
  } else {
    CFUN__LASTCALL_N(bn_call1,bn_plus,t);
  }

 small:
  CFUN__PROCEED(t);
}

CFUN__PROTO_N(fu1_float, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$float'", 2);
  tagged_t t;
  t=X0; 
  DerefEval(t, 1);
  CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t)));
}

CFUN__PROTO_N(fu1_add1, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$++'", 2);
  tagged_t t;

  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) + 1.0));
  } else {
    CFUN__LASTCALL_N(bn_call1,bn_incr,t);
  }

 small:
  if (t==TaggedIntMax) {
    CFUN__PROCEED(IntmaxPlus1ToTagged());
  } else {
    CFUN__PROCEED(SmallAdd(t, 1));
  }
}

CFUN__PROTO_N(fu1_sub1, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$--'", 2);
  tagged_t t;

  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) - 1.0));
  } else {
    CFUN__LASTCALL_N(bn_call1,bn_decr,t);
  }

 small:
  if (t==TaggedLow) {
    CFUN__PROCEED(IntminMinus1ToTagged());
  } else {
    CFUN__PROCEED(SmallSub(t, 1));
  }
}

/* binary functions */

CFUN__PROTO_N(fu2_plus, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$+'", 3);
  tagged_t t,u;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) + TaggedToFloat(u)));
  } else {
    CFUN__LASTCALL_N(bn_call2,bn_add,t,u);
  }

 small:
#if tagged__num_size == 32
  /* int64_t required to detect overflows */
  CFUN__PROCEED(Int64ToTaggedCheck((int64_t)GetSmall(t) + (int64_t)GetSmall(u)));
#else
#if defined(VAL_OVERFLOWS_INTO_TAG_OR_QTAG)
  /* val bits overflow into the tag bits */
  {
    tagged_t t1;
    t1 = t+(u-TaggedZero);
    if (TaggedIsSmall(t1)) { /* with qtag, ensure that QTAGMASK is 0 */
      CFUN__PROCEED(t1);
    } else {
      goto nonsmall;
    }
  }
#else
  CFUN__PROCEED(IntvalToTaggedCheck(GetSmall(t) + GetSmall(u)));
#endif
#endif
}

CFUN__PROTO_N(fu2_minus, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$-'", 3);
  tagged_t t,u;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) - TaggedToFloat(u)));
  } else {
    CFUN__LASTCALL_N(bn_call2,bn_subtract,t,u);
  }

 small:
#if tagged__num_size == 32
  /* int64_t required to detect overflows */
  CFUN__PROCEED(Int64ToTaggedCheck((int64_t)GetSmall(t) - (int64_t)GetSmall(u)));
#else
#if defined(VAL_OVERFLOWS_INTO_TAG_OR_QTAG)
  /* val bits overflow into the tag bits */
  {
    tagged_t t1;
    t1 = t-(u-TaggedZero);
    if (TaggedIsSmall(t1)) { /* with qtag, ensure that QTAGMASK is 0 */
      CFUN__PROCEED(t1);
    } else {
      goto nonsmall;
    }
  }
#else
  CFUN__PROCEED(IntvalToTaggedCheck(GetSmall(t) - GetSmall(u)));
#endif
#endif
}

CFUN__PROTO_N(fu2_times, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$*'", 3);
  tagged_t t,u;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) * TaggedToFloat(u)));
  } else {
    goto big;
  }

 small:
  {
#if defined(VAL_OVERFLOWS_INTO_TAG_OR_QTAG)
    /* val bits overflow into the tag bits */
    intval_t st;
    intval_t su;
    intval_t stu;
    tagged_t tu;

    st = GetSmall(t);
    su = (intval_t)(u-TaggedZero);
    stu = st*su;
    tu = ((tagged_t)stu)+TaggedZero;
      
    if (su==0 || (stu/su==st && TaggedIsSmall(tu))) {
      CFUN__PROCEED(tu);
    } else {
      goto big;
    }
#else
    intval_t st;
    intval_t su;
    intval_t stu;

    st = GetSmall(t);
    su = GetSmall(u);
    stu = st*su;
    
    if (su==0 || (stu/su==st)) { /* no overflow */
      CFUN__PROCEED(IntvalToTaggedCheck(stu));
    } else {
      goto big;
    }
#endif
  }
 big:
  CFUN__LASTCALL_N(bn_call2,bn_multiply,t,u);
}

CFUN__PROTO_N(fu2_fdivide, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$/'", 3);
  tagged_t t,u;
  t=X0; 
  u=X1;
  DerefEval(t, 1);
  DerefEval(u, 2);
  CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t)/TaggedToFloat(u)));
}

CFUN__PROTO_N(fu2_idivide, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$//'", 3);
  tagged_t t,u;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  /*bn_quotient_wanted = TRUE;*/
  CFUN__LASTCALL_N(bn_call2,bn_quotient_remainder_quot_wanted,t,u);
  
 small:
  CFUN__PROCEED(IntvalToTaggedCheck((intval_t)(t-TaggedZero)/(intval_t)(u-TaggedZero)));
}

CFUN__PROTO_N(fu2_rem, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$rem'", 3);
  tagged_t t,u;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  /*bn_quotient_wanted = FALSE;*/
  CFUN__LASTCALL_N(bn_call2,bn_quotient_remainder_quot_not_wanted,t,u);
  
 small:
  CFUN__PROCEED((intval_t)(t-TaggedZero)%(intval_t)(u-TaggedZero)+TaggedZero);
}

CFUN__PROTO_N(fu1_sign, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$sign'", 2);
  tagged_t t;
  flt64_t f;
  
  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    f = TaggedToFloat(t);
    CFUN__PROCEED(((f == 0.0) ? t :
		   (f < 0.0) ? BoxFloatCheck(-1.0) :
		   BoxFloatCheck(1.0)));
  } else {
    CFUN__PROCEED(((!bn_positive(TaggedToBignum(t))) ?
		   SmallAdd(TaggedZero,-1) :
		   SmallAdd(TaggedZero,1)));
  }

 small:
  CFUN__PROCEED((t==TaggedZero) ? TaggedZero :
		(t < TaggedZero) ? SmallAdd(TaggedZero,-1) :
		SmallAdd(TaggedZero,1));
}

CFUN__PROTO_N(fu2_mod, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$mod'", 3);
  tagged_t t,u;
  intval_t rem, denom;
  tagged_t T_rem;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  /*bn_quotient_wanted = FALSE;*/
  T_rem = CFUN__EVAL_N(bn_call2,bn_quotient_remainder_quot_not_wanted,t,u);
  CFUN__PROCEED(T_rem != TaggedZero &&
		CFUN__EVAL_N(fu1_sign,u) != CFUN__EVAL_N(fu1_sign,T_rem)
		? CFUN__EVAL_N(bn_call2,bn_add,T_rem,u) : T_rem);

 small:
  denom = (intval_t)(u-TaggedZero);
  rem = (intval_t)(t-TaggedZero)%denom;
  CFUN__PROCEED(((denom > 0 && rem < 0) || (denom < 0 && rem > 0) ?
		rem+denom : rem) + TaggedZero);
}

CFUN__PROTO_N(fu1_abs, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$abs'", 2);
  tagged_t t;
  flt64_t f;
  
  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    f = TaggedToFloat(t);
    CFUN__PROCEED(f < 0.0 ? BoxFloatCheck(-f) : t);
  } else {
    CFUN__PROCEED(!bn_positive(TaggedToBignum(t)) ? CFUN__EVAL_N(bn_call1,bn_minus,t) : t);
  }

 small:
  if (t==TaggedLow) {
    CFUN__PROCEED(IntmaxPlus1ToTagged());
  } else if (t < TaggedZero) {
    CFUN__PROCEED(TaggedZero-(t-TaggedZero));
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO_N(fu1_not, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$\\\\'", 2);
  tagged_t t;
  
  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  CFUN__LASTCALL_N(bn_call1,bn_not,t);
  
 small:
  CFUN__PROCEED(t^(TaggedIntMax-TaggedLow));
}

CFUN__PROTO_N(fu2_xor, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$#'", 3);
  tagged_t t,u;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  CFUN__LASTCALL_N(bn_call2,bn_xor,t,u);

 small:
  CFUN__PROCEED(t^u^TaggedZero);
}

CFUN__PROTO_N(fu2_and, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$/\\\\'", 3);
  tagged_t t,u;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  CFUN__LASTCALL_N(bn_call2,bn_and,t,u);

 small:
  CFUN__PROCEED(((t^ZMask)&(u^ZMask))^ZMask);
}

CFUN__PROTO_N(fu2_or, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$\\\\/'", 3);
  tagged_t t,u;

  t=X0; 
  u=X1; 
  EvalSwitch2(t, u, {goto small;}, {goto nonsmall;});

 nonsmall:
  CFUN__LASTCALL_N(bn_call2,bn_or,t,u);

 small:
  CFUN__PROCEED(((t^ZMask)|(u^ZMask))^ZMask);
}

static CFUN__PROTO_N(lsh_internal, tagged_t, tagged_t t, intmach_t dist) {
  if (TaggedIsSmall(t)) {
#if defined(ABSMACH_OPT__qtag)
    /* note: this code have strong dependencies to the traditional tag
       scheme in Ciao */
    tagged_t u;
    switch (dist) {
    case 0:
      CFUN__PROCEED(t);
    case 1:
      u = (t<<1) + 0x78000000;
      break;
    case 2:
      u = (t<<2) + 0x68000000;
      break;
    case 3:
      u = (t<<3) + 0x48000000;
      break;
    case 4:
      u = (t<<4) + 0x08000000;
      break;
    default:
      u = 0;
    }
    if (TaggedIsSmall(u)) {
      CFUN__PROCEED(u);
    } else {
      goto shift_bn;
    }
#else
    intval_t value = GetSmall(t);

    if (dist<tagged__num_size &&
	((value>=0 && value < (((uintval_t)1)<<(tagged__num_size-1-dist))) ||
	 (value<0 && value >= (((intval_t)-1)<<(tagged__num_size-1-dist))))) {
      /* ok, no overflow (for a intval_t type) after shift */
      CFUN__PROCEED(IntvalToTaggedCheck(value<<dist));
    } else {
      goto shift_bn;
    }
#endif
  } else {
    goto shift_bn;
  }
 shift_bn:
  CFUN__LASTCALL_N(bn_call2, bn_lshift, t, IntmachToTagged(dist));
}

static CFUN__PROTO_N(rsh_internal, tagged_t, tagged_t t, intmach_t dist) {
  if (TaggedIsSmall(t)) {
    if (dist>=tagged__num_size) {
      CFUN__PROCEED(MakeSmall((t>=TaggedZero)-1));
    } else {
#if defined(ABSMACH_OPT__qtag)
      CFUN__PROCEED(((intval_t)((t>>dist)-(TaggedZero>>dist)) & -4) + TaggedZero);
#else
      CFUN__PROCEED(MakeSmall(GetSmall(t)>>dist));
#endif
    }
  } else {
    CFUN__LASTCALL_N(bn_call2, bn_rshift, t, IntmachToTagged(dist));
  }
}

CFUN__PROTO_N(fu2_lsh, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$<<'", 3);
  tagged_t t,u;
  intmach_t dist;
  t=X0; 
  u=X1;
  DerefEval(t, 1);
  DerefEval(u, 2);
  dist = TaggedToIntmach(u);
  CFUN__PROCEED(dist<0 ?
		CFUN__EVAL_N(rsh_internal,t,-dist) :
		CFUN__EVAL_N(lsh_internal,t,dist));
}

CFUN__PROTO_N(fu2_rsh, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$>>'", 3);
  tagged_t t,u;
  intmach_t dist;
  t=X0; 
  u=X1; 
  DerefEval(t, 1);
  DerefEval(u, 2);
  dist = TaggedToIntmach(u);
  CFUN__PROCEED(dist<0 ?
		CFUN__EVAL_N(lsh_internal,t,-dist) :
		CFUN__EVAL_N(rsh_internal,t,dist));
}

/* Pre: U is NUM or STR(blob(float)) or STR(blob(bignum)) */
/* execute FloatCode if it is a float number, else negate the number
   (and execute BignumHook if it is a bignum) */
#define NegateInteger(u, BignumHook, FloatCode) { \
  if (TaggedIsSmall(u)) { /* NUM */  \
    if (u<=TaggedZero) { /* u is negative */ \
      if (u==TaggedLow) { \
	/* the positive of the minimum negative number is not \
	   representable as a small integer, need to create a bignum \
	*/ \
	u = IntmaxPlus1ToTagged(); \
	BignumHook; \
      } else { \
	/* negate */ \
	u = TaggedZero-(u-TaggedZero); \
      } \
    } \
  } else if (IsFloat(u)) { /* STR(blob(float)) */ \
    FloatCode; \
  } else { /* STR(blob(bignum)) */ \
    if (!bn_positive(TaggedToBignum(u))) { \
      /* negate */ \
      u = CFUN__EVAL_N(bn_call1,bn_minus,u); \
    } \
    type += 2; /* u is a STR(blob(bignum)) */ \
  } \
}

/* GCD for rat arithm., ch feb 92 */
CFUN__PROTO_N(fu2_gcd, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$gcd'", 3);
  tagged_t u,v;
  intval_t type;
  
  u=X0; 
  v=X1; 
  DerefEval(u, 1);
  DerefEval(v, 2);

  type = 0;
  NegateInteger(u, {
    type += 2; /* remember that u is a bignum */
  }, {
    ERROR_IN_ARG(u, 1, INTEGER); /* cannot treat float numbers */
  });
  NegateInteger(v, {
    type += 1; /* remember that v is a bignum */
  }, {
    ERROR_IN_ARG(v, 2, INTEGER); /* cannot treat float numbers */
  });

  if (u==TaggedZero) CFUN__PROCEED(v);
  if (v==TaggedZero) CFUN__PROCEED(u);
  /*bn_quotient_wanted = FALSE;*/

 again:
  switch (type) { /* u x v */
  case 0: /* small x small */
  small_x_small:
    {
      uintval_t x;
      uintval_t y;
      x = GetSmall(u);
      y = GetSmall(v);
    small_x_small_2:
      x = x % y; if (x==0) CFUN__PROCEED(MakeSmall(y));
      y = y % x; if (y==0) CFUN__PROCEED(MakeSmall(x));
      goto small_x_small_2;
    }

  case 1: /* small x big */
    v = CFUN__EVAL_N(bn_call2,bn_quotient_remainder_quot_not_wanted,v,u);
    if (v==TaggedZero) CFUN__PROCEED(u);
    goto small_x_small;

  case 2: /* big x small */
    u = CFUN__EVAL_N(bn_call2,bn_quotient_remainder_quot_not_wanted,u,v);
    if (u==TaggedZero) CFUN__PROCEED(v);
    goto small_x_small;

  default:
    /* case 3: */ /* big x big */
    u = CFUN__EVAL_N(bn_call2,bn_quotient_remainder_quot_not_wanted,u,v); 
    if (u==TaggedZero) CFUN__PROCEED(v);
    if (TaggedIsSmall(u)) type -= 2; /* now u is small */
    v = CFUN__EVAL_N(bn_call2,bn_quotient_remainder_quot_not_wanted,v,u); 
    if (v==TaggedZero) CFUN__PROCEED(u);
    if (TaggedIsSmall(v)) type -= 1; /* now v is small */
    goto again;
  }
} 

CFUN__PROTO_N(fu1_intpart, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$float_integer_part'", 2);
  tagged_t t;
  flt64_t f;
  t=X0; 
  DerefEval(t, 1);
  f = TaggedToFloat(t);
  CFUN__PROCEED(BoxFloatCheck(aint(f)));
}

CFUN__PROTO_N(fu1_fractpart, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$float_fractional_part'", 2);
  tagged_t t;
  flt64_t f;
  t=X0; 
  DerefEval(t, 1);
  f = TaggedToFloat(t);
  CFUN__PROCEED(BoxFloatCheck(f-aint(f)));
}

CFUN__PROTO_N(fu1_floor, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$floor'", 2);
  tagged_t t;

  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    /* TODO: use a TaggedToFloat version that assumes that it is a float */
    CFUN__LASTCALL_N(bn_from_float_check, floor(TaggedToFloat(t)));
  } else {
    CFUN__LASTCALL_N(bn_call1,bn_plus,t);
  }

 small:
  CFUN__PROCEED(t);
}

CFUN__PROTO_N(fu1_round, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$round'", 2);
  tagged_t t;

  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    /* TODO: use a TaggedToFloat version that assumes that it is a float */
    CFUN__LASTCALL_N(bn_from_float_check, floor(TaggedToFloat(t)+0.5));
  } else {
    CFUN__LASTCALL_N(bn_call1,bn_plus,t);
  }

 small:
  CFUN__PROCEED(t);
}

CFUN__PROTO_N(fu1_ceil, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$ceiling'", 2);
  tagged_t t;

  t=X0; 
  EvalSwitch1(t, {goto small;}, {goto nonsmall;});

 nonsmall:
  if (IsFloat(t)) {
    /* TODO: use a TaggedToFloat version that assumes that it is a float */
    CFUN__LASTCALL_N(bn_from_float_check, ceil(TaggedToFloat(t)));
  } else {
    CFUN__LASTCALL_N(bn_call1,bn_plus,t);
  }

 small:
  CFUN__PROCEED(t);
}

CFUN__PROTO_N(fu2_pow, tagged_t, tagged_t X0, tagged_t X1) {
  ERR__FUNCTOR("arithmetic:'$**'", 3);
  tagged_t t,u;
  t=X0; 
  u=X1; 
  DerefEval(t, 1);
  DerefEval(u, 2);
  CFUN__PROCEED(BoxFloatCheck(pow(TaggedToFloat(t),TaggedToFloat(u))));
}

CFUN__PROTO_N(fu1_exp, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$exp'", 2);
  tagged_t t;
  t=X0; 
  DerefEval(t, 1);
  CFUN__PROCEED(BoxFloatCheck(exp(TaggedToFloat(t))));
}

CFUN__PROTO_N(fu1_log, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$log'", 2);
  tagged_t t;
  t=X0; 
  DerefEval(t, 1);
  CFUN__PROCEED(BoxFloatCheck(log(TaggedToFloat(t))));
}

CFUN__PROTO_N(fu1_sqrt, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$sqrt'", 2);
  tagged_t t;
  t=X0; 
  DerefEval(t, 1);
  CFUN__PROCEED(BoxFloatCheck(sqrt(TaggedToFloat(t))));
}

CFUN__PROTO_N(fu1_sin, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$sin'", 2);
  tagged_t t;
  t=X0; 
  DerefEval(t, 1);
  CFUN__PROCEED(BoxFloatCheck(sin(TaggedToFloat(t))));
}

CFUN__PROTO_N(fu1_cos, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$cos'", 2);
  tagged_t t;
  t=X0; 
  DerefEval(t, 1);
  CFUN__PROCEED(BoxFloatCheck(cos(TaggedToFloat(t))));
}

CFUN__PROTO_N(fu1_atan, tagged_t, tagged_t X0) {
  ERR__FUNCTOR("arithmetic:'$atan'", 2);
  tagged_t t;
  t=X0; 
  DerefEval(t, 1);
  CFUN__PROCEED(BoxFloatCheck(atan(TaggedToFloat(t))));
}
