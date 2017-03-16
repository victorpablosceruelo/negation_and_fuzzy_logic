#include <engine/basiccontrol.native.h>

CBOOL__PROTO_N(bu1_atom, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0, { CBOOL__FAIL; }, {
    CBOOL__LASTTEST(IsNonvarAtom(x0));
  });
}

CBOOL__PROTO_N(bu1_atomic, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0, { CBOOL__FAIL; }, {
    CBOOL__LASTTEST(IsNonvarAtomic(x0));
  });
}

CBOOL__PROTO_N(bu1_float, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0, { CBOOL__FAIL; }, {
    CBOOL__LASTTEST(IsFloat(x0));
  });
}

CBOOL__PROTO_N(bu1_integer, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0, { CBOOL__FAIL; }, {
    CBOOL__LASTTEST(IsInteger(x0));
  });
}

CBOOL__PROTO_N(bu1_number, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0, { CBOOL__FAIL; }, {
    CBOOL__LASTTEST(IsNumber(x0));
  });
}

CBOOL__PROTO_N(bu1_var, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0, {
    CBOOL__PROCEED;
  }, {
    CBOOL__FAIL;
  });
}

CBOOL__PROTO_N(bu1_nonvar, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0, {
    CBOOL__FAIL;
  }, {
    CBOOL__PROCEED;
  });
}

CFUN__PROTO_N(fu1_type, tagged_t, tagged_t t0) {
  DerefSw_any(t0, f, {
    CFUN__PROCEED(atm_var);
  }, {
    CFUN__PROCEED(atm_var);
  }, {
    CFUN__PROCEED(atm_attv);
  }, {
    CFUN__PROCEED(atm_int);
  }, {
    CFUN__PROCEED(atm_atm);
  }, {
    CFUN__PROCEED(atm_lst);
  }, {
    CFUN__PROCEED(atm_float);
  }, {
    CFUN__PROCEED(atm_int);
  }, {
    CFUN__PROCEED(atm_str);
  });
  CFUN__PROCEED(ERRORTAG); /* avoid warnings */
} 

/* ground */
/* TODO: optimize, port to improlog, make it work with cyclic terms */
static CBOOL__PROTO_N(cground_args_aux, int arity, tagged_t *pt1, tagged_t *x1);
static CBOOL__PROTO_N(cground_aux, tagged_t x1);

static CBOOL__PROTO_N(cground_args_aux, int arity, tagged_t *pt1, tagged_t *x1) {
  tagged_t t1 = ~0;
  for (; arity>0; --arity) {
    t1 = *pt1;
    if (arity > 1) {
      CBOOL__CALL_N(cground_aux, t1);
    }
    pt1 = HeapCharOffset(pt1, sizeof(tagged_t));
  }
  *x1 = t1;
  CBOOL__PROCEED;
}

CBOOL__PROTO(cground) {
  CBOOL__LASTCALL_N(cground_aux, X(0));
}

static CBOOL__PROTO_N(cground_aux, tagged_t x1) {
  tagged_t u;

 in:
  u=x1;
  DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(u, { /* HVA CVA SVA */
    /* note: CVAs are not supported */
    goto lose;
  }, { /* NUM ATM */
    goto win;
  }, { /* LST */
    CBOOL__CALL_N(cground_args_aux, 2, TaggedToCar(u), &x1);
    goto in;
  }, { /* STR */
    SwStruct(f, u, { /* STR(blob) */
      goto win;
    }, { /* STR(struct) */
      CBOOL__CALL_N(cground_args_aux, Arity(f), TaggedToArg(u,1), &x1);
      goto in;
    });
  });
 lose:
  CBOOL__FAIL;
 win:
  CBOOL__PROCEED;
}

