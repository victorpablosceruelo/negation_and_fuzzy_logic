#include <engine/basiccontrol.native.h>

#include <string.h>

/* TODO: a kludge... define in absmach_def */
/* Offset of operand 1 of the exec_cinsnp instruction */
#if defined(OPCODE__exec_cinsnp)
#define DEFOP_exec_cinsnp OPCODE__exec_cinsnp
#define SIZEDEFOP_exec_cinsnp (FTYPE_size(f_o))
#elif defined(OPCODE__pad2__exec_cinsnp)
#define DEFOP_exec_cinsnp OPCODE__pad2__exec_cinsnp
#define SIZEDEFOP_exec_cinsnp (FTYPE_size(f_o)+2)
#elif defined(OPCODE__pad4__exec_cinsnp)
#define DEFOP_exec_cinsnp OPCODE__pad4__exec_cinsnp
#define SIZEDEFOP_exec_cinsnp (FTYPE_size(f_o)+4)
#elif defined(OPCODE__pad6__exec_cinsnp)
#define DEFOP_exec_cinsnp OPCODE__pad6__exec_cinsnp
#define SIZEDEFOP_exec_cinsnp (FTYPE_size(f_o)+6)
#endif
/* Offset of operand 1 of the un_xvar instruction */
#if defined(OPCODE__un_xvar) && !defined(OPCODE__pad2__un_xvar) && !defined(OPCODE__pad4__un_xvar) && !defined(OPCODE__pad6__un_xvar)
#define SIZEDEFOP_un_xvar (FTYPE_size(f_o))
#elif !defined(OPCODE__un_xvar) && defined(OPCODE__pad2__un_xvar) && !defined(OPCODE__pad4__un_xvar) && !defined(OPCODE__pad6__un_xvar)
#define SIZEDEFOP_un_xvar (FTYPE_size(f_o)+2)
#elif !defined(OPCODE__un_xvar) && !defined(OPCODE__pad2__un_xvar) && defined(OPCODE__pad4__un_xvar) && !defined(OPCODE__pad6__un_xvar)
#define SIZEDEFOP_un_xvar (FTYPE_size(f_o)+4)
#elif !defined(OPCODE__un_xvar) && !defined(OPCODE__pad2__un_xvar) && !defined(OPCODE__pad4__un_xvar) && defined(OPCODE__pad6__un_xvar)
#define SIZEDEFOP_un_xvar (FTYPE_size(f_o)+6)
#else
#error "more than one padded version for un_xvar"
#endif

#define Poff(X) BCoff(P,(X))
#define POp(T,X) BCOp(P,T,(X))

#if defined(ABSMACH_OPT__iblt)
CBOOL__PROTO_N(bu1_var, tagged_t x0);
CBOOL__PROTO_N(bu1_nonvar, tagged_t x0);
CBOOL__PROTO_N(bu2_numgt, tagged_t x0, tagged_t x1);
CBOOL__PROTO_N(bu2_numlt, tagged_t x0, tagged_t x1);
CBOOL__PROTO_N(bu2_numne, tagged_t x0, tagged_t x1);
CFUN__PROTO_N(fu1_plus, tagged_t, tagged_t X0);
CFUN__PROTO_N(fu1_minus, tagged_t, tagged_t X0);
CFUN__PROTO_N(fu1_add1, tagged_t, tagged_t X0);
CFUN__PROTO_N(fu1_sub1, tagged_t, tagged_t X0);
CFUN__PROTO_N(fu2_plus, tagged_t, tagged_t X0, tagged_t X1);
CFUN__PROTO_N(fu2_minus, tagged_t, tagged_t X0, tagged_t X1);
CFUN__PROTO_N(fu2_arg, tagged_t, tagged_t number, tagged_t complex);
CBOOL__PROTO_N(bu3_functor, tagged_t term, tagged_t name, tagged_t arity);
#endif

/***************************************************************************/ 
/* Dynamic compiler */
/* TODO: generate automatically? */

#define ALIGN_OF(P,ModPad) (((intp_t)(P))&(ModPad-1))

#define EMITtok(T, X) { \
  BCOp(P, FTYPE_ctype(T), 0) = (X); \
  P = BCoff(P, FTYPE_size(T)); \
}

#define Xop(X) ((X)*sizeof(tagged_t)+sizeof(worker_t))
#define Xinv(X) (((X)-sizeof(worker_t))/sizeof(tagged_t))

#define EMIT_opcode_padx4(Insn,P0,P2) { if (ALIGN_OF(P,4) == 0) { EMIT_opcode_pad##P0(Insn); } else { EMIT_opcode_pad##P2(Insn); } }
#define EMIT_opcode_padx8(Insn,P0,P2,P4,P6) { switch(ALIGN_OF(P,8)) { case 0: EMIT_opcode_pad##P0(Insn); break; case 2: EMIT_opcode_pad##P2(Insn); break; case 4: EMIT_opcode_pad##P4(Insn); break; default: EMIT_opcode_pad##P6(Insn); } }

/* _A0: special case for P alignment 0 */
#define EMIT_opcode_pad04(Insn) EMIT_opcode_padx4(Insn,0,2)
#define EMIT_opcode_pad04_A0(Insn) EMIT_opcode_pad0(Insn)
#define EMIT_opcode_pad24(Insn) EMIT_opcode_padx4(Insn,2,0)
#define EMIT_opcode_pad24_A0(Insn) EMIT_opcode_pad2(Insn)
#define EMIT_opcode_pad08(Insn) EMIT_opcode_padx8(Insn,0,6,4,2)
#define EMIT_opcode_pad08_A0(Insn) EMIT_opcode_pad0(Insn)
#define EMIT_opcode_pad28(Insn) EMIT_opcode_padx8(Insn,2,0,6,4)
#define EMIT_opcode_pad28_A0(Insn) EMIT_opcode_pad2(Insn)
#define EMIT_opcode_pad48(Insn) EMIT_opcode_padx8(Insn,4,2,0,6)
#define EMIT_opcode_pad48_A0(Insn) EMIT_opcode_pad4(Insn)
#define EMIT_opcode_pad68(Insn) EMIT_opcode_padx8(Insn,6,4,2,0)
#define EMIT_opcode_pad68_A0(Insn) EMIT_opcode_pad6(Insn)

#define EMIT_opcode_pad0(Insn) { \
  EMIT_o(OPCODEenc(Insn)); \
}
#define EMIT_opcode_pad2(Insn) { \
  EMIT_o(OPCODEenc(pad2__##Insn)); \
  EMIT_Q(0); \
}
#define EMIT_opcode_pad4(Insn) { \
  EMIT_o(OPCODEenc(pad4__##Insn)); \
  EMIT_Q(0); \
  EMIT_Q(0); \
}
#define EMIT_opcode_pad6(Insn) { \
  EMIT_o(OPCODEenc(pad6__##Insn)); \
  EMIT_Q(0); \
  EMIT_Q(0); \
  EMIT_Q(0); \
}

#define EMIT_Q(X) EMITtok(f_Q, (X))
#define EMIT_o(X) EMITtok(f_o, (X))
#define EMIT_e(X) EMITtok(f_e, (X))
#define EMIT_x(X) EMITtok(f_x, Xop((X)))
#define EMIT_t(X) EMITtok(f_t, (X))
#define EMIT_l(X) EMITtok(f_l, (X))
#define EMIT_E(X) EMITtok(f_E, (X))
#define EMIT_Cc(X) EMITtok(f_Cc, (X))
#define EMIT_Ci(X) EMITtok(f_Ci, (X))
#define EMIT_i(X) EMITtok(f_i, (X))
#define EMIT_Blob(BLOB) { \
  P = BCoff(P, copy_blob(TagpPtr(STR,(BLOB)), (tagged_t *)P)); \
}

#define EMIT_opcode(X) EMIT_o(OPCODEenc(X))

/***************************************************************************/ 
/* Ensure that we have enough X registers */

#define ENSURE_XREGS(AMOUNT, ERROR) { \
  if ((AMOUNT) >= w->reg_bank_size) { \
    ERROR; \
  } \
}
  
/***************************************************************************/ 
/* Compiler term */

/* A buffer for generated bytecode */
/* TODO: increase size automatically */
#define PBUFF_used ((char *)P - (char *)w->misc->pbuff)
#define PBUFF_pad 1024
#define PBUFF_ensure(AMOUNT) { \
  while (PBUFF_used + (AMOUNT) >= w->misc->pbuff_length) { \
    char *old = w->misc->pbuff; \
    w->misc->pbuff = CHECKREALLOC0_ARRAY(char, (char *)w->misc->pbuff, w->misc->pbuff_length, w->misc->pbuff_length*2); \
    w->misc->pbuff_length *= 2; \
    P = BCoff(P, (char *)w->misc->pbuff - old); /* relocate P */ \
  } \
}

/* Begin generation of bytecode */
#define PBUFF_begin { P = w->misc->pbuff; }

/* Finish generation of bytecode */
#define PBUFF_finish(Where) { \
  Where = ALLOC_BYTECODE(PBUFF_used); \
  memcpy(Where, w->misc->pbuff, PBUFF_used); \
}
#define ALLOC_BYTECODE(SIZE) CHECKALLOC_ARRAY(char, (SIZE))

#if defined(USE_CTERM)

/* Stack for compile term */

#define CTermBegin s2->trail_origo = G->trail_top-DynamicPreserved
#define CTermTop s2->trail_origo+DynamicPreserved
#define CTermSize G->trail_top-s2->trail_origo

//CTERMSTK
#define CTermPush(X) { \
  if (ChoiceCharAvailable(w->choice) < CHOICEPAD) { \
    CVOID__CALL_N(ctermstk__overflow, s2); \
  } \
  TrailPush(G->trail_top, (X)); \
}
#define CTermUntrail(TR,Ref) { \
  TrailDec(TR); \
  Ref = *(TR); \
  if (IsVar(Ref)) { \
    *TaggedToPointer(Ref) = Ref; \
  } \
}

/* Meaning of NESTEDMARK__* marks

   The meaning of the mark, during c_term_mark:
       SINGLEVAR: the variable is a singleton
       USEDVAR: the variable appears at least twice in the term
       USEDVARCVA: the variable is a CVA

   Before c_term, all unifications with SINGLEVAR TagNested are
   undone. The meaning of the mark, during c_term:
       USEDVAR: the X register will be unified with new variable
       USEDVARCVA: like USEDVAR but the CVA value is pushed
                   in the trail to be compiled later
       VAL: the X register contains a variable
*/

/* From a nested var to the original var (stored in the trail) */
#define NestedSrc(X) (*((tagged_t *)s2->trail_origo+NestedValue((X))))
/* A new, unique special atom (related to a index in the trail stack) */
#define NestedNew TagNested(((tagged_t *)G->trail_top) - (tagged_t *)s2->trail_origo)

typedef struct _cterm_state_t cterm_state_t;
struct _cterm_state_t {
  /* pass1 */
  intmach_t hsize;
  intmach_t maxtemps;
  /* pass2 */
  intmach_t x_variables;
  bcp_t p;
  /* common */
  tagged_t *trail_origo;
};

/* expand the ctermstk */
static CVOID__PROTO_N(ctermstk__overflow, cterm_state_t *s2) {
  tagged_t *tr;
  intmach_t reloc;

  /* expand the choice stack */
  /* note: choice overflow does preserve gcmarks */
  tr = G->trail_top;
  CVOID__CALL_N(choice_overflow,CHOICEPAD,FALSE);
  /* obtain the reloc factor */
  reloc = (char *)G->trail_top - (char *)tr;
  /* reloc trail_origo */
  s2->trail_origo = (tagged_t *)((char *)s2->trail_origo + reloc);
}

static CVOID__PROTO_N(c_term_mark, tagged_t t, intmach_t temps, cterm_state_t *s2) {
  tagged_t t1;

 start:
  SwitchOnHeapVarB(t, t_head_functor, { /* HVA */
    //CTERMSTK
    *TaggedToPointer(t) = NestedNew;
    CTermPush(t);
    return;
  }, { /* CVA */
    //CTERMSTK
    *TaggedToPointer(t) = NestedSetMark__USEDVARCVA(NestedNew);
    CTermPush(t);
    s2->hsize += 2 * sizeof(tagged_t);
    if (s2->maxtemps < temps) s2->maxtemps = temps;
    {
      tagged_t *goal = TaggedToGoal(t);
      t1 = *goal;
      CVOID__CALL_N(c_term_mark, t1, temps+1, s2);
      t = *(goal+1);
    }
    goto start; /* continue with last argument */
  }, { /* NUM */
    return;
  }, { /* ATM */
    if (IsATM_TagNested(t)) {
      if (NestedGetMark(t) == NESTEDMARK__SINGLEVAR) {
	/* nested var */
	(*TaggedToPointer(NestedSrc(t))) = NestedSetMark__USEDVAR((*TaggedToPointer(NestedSrc(t))));
      }
      return;
    } else { /* normal atom */
      return;
    }
  }, { /* LST */
    s2->hsize += 2 * sizeof(tagged_t);
    if (s2->maxtemps < temps) s2->maxtemps = temps;
    t1 = *TaggedToCar(t);
    CVOID__CALL_N(c_term_mark, t1, temps+1, s2);
    t = *TaggedToCdr(t);
    goto start; /* continue with last argument */
  }, { /* STR(blob) */
    intmach_t size;
    size = BlobFunctorSizeAligned(t_head_functor);
    s2->hsize += size+2*sizeof(functor_t);
    return;
  }, { /* STR(struct) */
    intmach_t i;
    intmach_t arity;
    arity = Arity(t_head_functor);
    s2->hsize += (1+arity)*sizeof(tagged_t);
    if (s2->maxtemps < temps) s2->maxtemps = temps;
    for (i=1; i<arity; i++) {
      t1 = *TaggedToArg(t,i);
      CVOID__CALL_N(c_term_mark, t1, temps+arity-i, s2);
    }
    t = *TaggedToArg(t,arity);
    goto start; /* continue with last argument */
  });
}

static CBOOL__PROTO_N(c_term_args,
		      bcp_t P,
		      tagged_t *s,
		      intmach_t ar,
		      intmach_t FreeReg,
		      cterm_state_t *s2);

#define EMIT_ux_nil(Xreg) ({			\
      EMIT_op(ux_nil);				\
      EMIT_x((Xreg));				\
    })
#define EMIT_ux_cons(Xreg, T) ({		\
      EMIT_op(ux_cons);				\
      EMIT_x((Xreg));				\
      EMIT_t((T));				\
    })
#define EMIT_ux_lst(Xreg, T) ({			\
      EMIT_op(ux_lst);				\
      EMIT_x((Xreg));				\
    })
#define EMIT_ux_str(Xreg, HeadFunctor) ({	\
      EMIT_op(ux_str);				\
      EMIT_x((Xreg));				\
      EMIT_t((HeadFunctor));			\
    })
#define EMIT_ux_blob(Xreg, Blob) ({		\
      EMIT_op(ux_blob);				\
      EMIT_x(Xreg);				\
      EMIT_Blob(Blob);				\
    })
#define EMIT_un_xvar(T) ({			\
      EMIT_op(un_xvar);				\
      EMIT_x((T));				\
    })
#define EMIT_un_xval(T) ({			\
      EMIT_op(un_xval);				\
      EMIT_x((T));				\
    })
#define EMIT_un_nil() ({			\
      EMIT_op(un_nil);				\
    })
#define EMIT_un_lst() ({			\
      EMIT_op(un_lst);				\
    })
#define EMIT_un_str(HeadFunctor) ({		\
      EMIT_op(un_str);				\
      EMIT_t((HeadFunctor));			\
    })
#define EMIT_un_blob(Blob) ({			\
      EMIT_op(un_blob);				\
      EMIT_Blob(Blob);				\
    })
#if 0
  EMIT_op(kontinue);

  EMIT_op(un_cons);
  EMIT_t(t);

  EMIT_op(un_voidr1);

  EMIT_op(un_void);

  EMIT_op(un_xvar);
  EMIT_x(0);

  EMIT_op(ux_constraint);
  EMIT_x(Xreg);

  EMIT_op(heapmargin_call);
  EMIT_l(s2->hsize);
  EMIT_i(DynamicPreserved);

  EMIT_op(dynamic_neck__proceed);

  EMIT_op(call);
  EMIT_E(address_restart);
  EMIT_e(sizeof(frame_t)); /* initial FrameSize */

  EMIT_op(exit_toplevel);

  EMIT_op(retry_cbool__proceed);
  EMIT_Cc(proc);

  EMIT_op(exec_cinsnp);
  EMIT_Ci(proc);

  EMIT_op(failins);

  EMIT_op(proceed);

  EMIT_op(lastcall);
  EMIT_E(address_fail);
#endif

static CBOOL__PROTO_N(c_term,
		      tagged_t t,
		      intmach_t Xreg, intmach_t FreeReg,
		      cterm_state_t *s2) {
  intmach_t ar;
  bcp_t P;
  tagged_t *s;

  P = s2->p;
  
  PBUFF_ensure(PBUFF_pad);

  /* precondition: t cannot be a variable */
  /* precondition: t cannot be a nested */
  /* precondition: t is already dereferenced */
  SwNonvar(t, t_head_functor, { /* NUM */
    goto cons;
  }, {
    /* ATM */
    if (t == atom_nil) {
      EMIT_ux_nil(Xreg);
      s2->p = P;
      CBOOL__PROCEED;
    } else {
    cons:
      /* any cons */
      EMIT_ux_cons(Xreg, t);
      s2->p = P;
      CBOOL__PROCEED;
    }
  }, { /* LST */
    EMIT_ux_lst(Xreg, t);
    s = TagpPtr(LST, t);
    ar = 2;
    CBOOL__LASTCALL_N(c_term_args, P, s, ar, FreeReg, s2);
  }, { /* STR(blob) */
    PBUFF_ensure(PBUFF_pad + BlobFunctorSizeAligned(t_head_functor)+sizeof(functor_t));
    EMIT_ux_blob(Xreg, t);
    s2->p = P;
    CBOOL__PROCEED;
  }, { /* STR */
    EMIT_ux_str(Xreg, t_head_functor);
    s = TaggedToArg(t,1);
    ar = Arity(t_head_functor);
    CBOOL__LASTCALL_N(c_term_args, P, s, ar, FreeReg, s2);
  });
  /* note: this point should not be reachable */
  CBOOL__FAIL;
}

static CBOOL__PROTO_N(c_term_args,
		      bcp_t P,
		      tagged_t *s,
		      intmach_t ar,
		      intmach_t FreeReg,
		      cterm_state_t *s2) {
  tagged_t t;
  intmach_t i, Treg;

  /* Emit tail-recursive UNIFY sequence for all arguments */

  intmach_t saved_nestedstack_count = w->misc->nestedstack_count;

  Treg = 0;

#define NoNested Treg == 0

 again:
  for (i = 1; i <= ar; i++) {
    t = *s;
    s++;
    HeapDerefSw_HVAorCVA_Other(t, {
      goto void_var;
    }, {
      PBUFF_ensure(PBUFF_pad);
      SwNonvar(t, t_head_functor, { /* NUM */
	goto constant;
      }, { /* ATM (or nested) */
	if (IsATM_TagNested(t)) {
	  switch (NestedGetMark(t)) {
	  case NESTEDMARK__USEDVARCVA:
	    EMIT_un_xvar(NestedValue(t));
	    /* enqueue constraint */
	    CTermPush(t);
	    /* next occurrence of the same variable will be a nested value */
	    (*TaggedToPointer(NestedSrc(t))) = NestedSetMark__VAL(*TaggedToPointer(NestedSrc(t)));
	    break;
	    /* case NESTEDMARK__SINGLEVAR: error! single vars are removed before c_term is executed */
	  case NESTEDMARK__USEDVAR:
	    EMIT_un_xvar(NestedValue(t));
	    /* next occurrence of the same variable will be a nested value */
	    (*TaggedToPointer(NestedSrc(t))) = NestedSetMark__VAL(*TaggedToPointer(NestedSrc(t)));
	    break;
	  default: /*case NESTEDMARK__VAL:*/
	    EMIT_un_xval(NestedValue(t));
	    break;
	  }
	  goto next_arg;
	} else if (t == atom_nil) {
	  EMIT_un_nil();
	  goto next_arg;
	} else {
	  goto constant;
	}
      }, { /* LST */
	if ((i==ar) && NoNested) { /* tail-recursive */
	  EMIT_un_lst();
	  s = TagpPtr(LST, t);
	  ar=2;
	  goto again;
	} else {
	  goto unify_nested;
	}
      }, { /* STR(blob) */
	if ((i==ar) && NoNested) { /* tail-recursive (blob) */
	  PBUFF_ensure(PBUFF_pad + BlobFunctorSizeAligned(t_head_functor)+sizeof(functor_t));
	  EMIT_un_blob(t);
	  goto next_arg;
	} else {
	  goto unify_nested;
	}
      }, { /* STR */
	if ((i==ar) && NoNested) { /* tail-recursive */
	  EMIT_un_str(t_head_functor);
	  s = TaggedToArg(t,1);
	  ar=Arity(t_head_functor);
	  goto again;
	} else {
	  goto unify_nested;
	}
      });
      /* TODO: it should not reach this point */
      goto next_arg;
    constant:
      /* any constant */
      EMIT_op(un_cons);
      EMIT_t(t);
      goto next_arg;
    void_var:
      /* free variables (not unified with a TagNested are singleton) */
#if defined(ABSMACH_OPT__varops)
      EMIT_op(un_voidr1);
#else
      EMIT_op(un_void);
#endif
      goto next_arg;
    unify_nested:
      {}
      intmach_t insoff;
      insoff = PBUFF_used;
      EMIT_un_xvar(0); /* relocate later */
      Treg++; 
      w->misc->nestedstack[w->misc->nestedstack_count].p_off = insoff + SIZEDEFOP_un_xvar;
      w->misc->nestedstack[w->misc->nestedstack_count].s = s-1;
      w->misc->nestedstack_count++;
      if (w->misc->nestedstack_count >= w->misc->nestedstack_length) {
	PANIC_FAULT("nested stack overflow");
      }
      goto next_arg;
    next_arg:
      continue;
    });
  }
  s2->p = P;

  /* Emit code for nested args */
  CBOOL__TEST(FreeReg >= s2->x_variables);
  if (NoNested) {
    /* TODO: is it necessary to reset nestedstack_count here? or exit
       just with CBOOL__PROCEED? */
    goto end;
  }

  intmach_t curr_nestedstack_count = w->misc->nestedstack_count;
  intmach_t nestedstack_i = saved_nestedstack_count;
  intmach_t used_regs = Treg;
  /* initial X register for nested terms */
  intmach_t xr = 1 + FreeReg - used_regs;
  while (nestedstack_i < curr_nestedstack_count) {
    s = w->misc->nestedstack[nestedstack_i].s;
    P = BCoff(w->misc->pbuff, w->misc->nestedstack[nestedstack_i].p_off);
    POp(FTYPE_ctype(f_x), 0) = Xop(xr);
    DEREF(t,*s);
    CBOOL__CALL_N(c_term,t,xr,xr,s2);
    nestedstack_i++;
    xr++;
  }
 end:
  w->misc->nestedstack_count = saved_nestedstack_count;
  CBOOL__PROCEED;
}

static CBOOL__PROTO_N(c_term_cva,
		      tagged_t *s,
		      intmach_t Xreg, intmach_t FreeReg,
		      cterm_state_t *s2) {
  bcp_t P;

  P = s2->p;
  PBUFF_ensure(PBUFF_pad);
  EMIT_op(ux_constraint);
  EMIT_x(Xreg);
  CBOOL__LASTCALL_N(c_term_args, P, s, 2, FreeReg, s2);
}

/* Note on memory consumption: compile_term_aux may increase the size of the
   stacks and the size of the X register bank, so the simple approach "see
   how much the total memory has increased" does not work.  Instead, we try
   to find out exactly where we allocate and deallocate memory for program
   storage.  c_term_mark does not allocate program memory (instead, it may
   call choice_overflow, which expands stacks). */

#if defined(ABSMACH_OPT__regmod)
extern tagged_t regmod__mark;
#endif

CFUN__PROTO(compile_term_aux, instance_t *) {
  tagged_t head;
  tagged_t body;
  intmach_t lsize;
  tagged_t t0;
  tagged_t *pt1;
  tagged_t *pt2;
  instance_t *object;

  cterm_state_t ss2;
  cterm_state_t *s2 = &ss2;
  bcp_t P;

  s2->hsize=CONTPAD;
  s2->maxtemps=0;
  //CTERMSTK
  CTermBegin;

  w->misc->nestedstack_count = 0;

  head = X(0);
  body = X(1);

  HeapDerefSw_HVAorCVA_Other(head, {
  }, {
    CVOID__CALL_N(c_term_mark, head, 0, s2);
  });

  HeapDerefSw_HVAorCVA_Other(body, {
  }, {
    CVOID__CALL_N(c_term_mark, body, 0, s2);
  });

  //CTERMSTK
  /* tidy out void vars */
  /* (remove from the trail SINGLEVAR TagNested) */
  /* note: trail does not contain TagNested yet (for constraints),
     but references to variables */
  pt1 = pt2 = CTermTop;
  while (TrailYounger(G->trail_top, pt1)) {
    t0 = *pt1;
    if (NestedGetMark((*TaggedToPointer(t0))) != NESTEDMARK__SINGLEVAR) {
      /* NESTEDMARK__USEDVARCVA */
      /* NESTEDMARK__USEDVAR */
      /* update nested index */
      (*TaggedToPointer(t0)) = NestedAdd((*TaggedToPointer(t0)), -((tagged_t *)pt1-(tagged_t *)pt2));
      TrailPush(pt2,t0);
    } else {
      /* NESTEDMARK__SINGLEVAR */
      /* untrail (remove variable) */
      (*TaggedToPointer(t0)) = t0;
    }
    pt1 += TrailDir;
  }
  G->trail_top = pt2;
  s2->x_variables = CTermSize;

  /* ensure enough X registers */
  /* TODO: why 2*maxtemps? */
  ENSURE_XREGS(s2->x_variables + 2*s2->maxtemps, {goto too_large_nox;});

  PBUFF_begin;

  PBUFF_ensure(PBUFF_pad);

  if (s2->hsize>=CALLPAD) {
    EMIT_op(heapmargin_call);
    EMIT_l(s2->hsize);
    EMIT_i(DynamicPreserved);
  }

  s2->p = P;
  if (!IsVar(head)) {
    if (!CBOOL__SUCCEED_N(c_term,head,0,w->reg_bank_size-1,s2)) goto too_large;
  }
  if (!IsVar(body)) {
    if (!CBOOL__SUCCEED_N(c_term,body,1,w->reg_bank_size-1,s2)) goto too_large;
  }
  //CTERMSTK
  /* note: compile constraints pointed by Nested in the trail */
  while (IsTagNested(TrailGetTop(G->trail_top))) {
    if (!TrailYounger(G->trail_top,Trail_Start)) break;
    TrailDec(G->trail_top);
    t0 = *(G->trail_top);
    /* note: NestedSrc(t0) is a CVA */
    if (!CBOOL__SUCCEED_N(c_term_cva,
			  TaggedToGoal(NestedSrc(t0)),
			  NestedValue(t0),
			  w->reg_bank_size-1,
			  s2)) goto too_large;
  }
  P = s2->p;

  EMIT_op(dynamic_neck__proceed);

  intmach_t newsize;
  newsize = PBUFF_used;
  lsize=TAILED_SIZE_TO_BYTES(instance_t, newsize);
  SET_CHECKALLOC_TAILED(object, instance_t, newsize);
  INC_MEM_PROG(lsize);
  object->pending_x2 = NULL;
  object->pending_x5 = NULL;
#if defined(ABSMACH_OPT__regmod)
  object->mark = regmod__mark;
#endif
  memcpy(object->emulcode, w->misc->pbuff, newsize);

  //CTERMSTK
  pt2 = G->trail_top;
  G->trail_top = CTermTop;
  while (TrailYounger(pt2,G->trail_top)) {
    CTermUntrail(pt2,t0);
  }

  if (TaggedIsSTR(head))  {
    DerefArg(t0,head,1);
    if (TaggedIsSTR(t0))
      object->key = TaggedToHeadfunctor(t0);
    else if (TaggedIsLST(t0))
      object->key = functor_lst;
    else if (!IsVar(t0))
      object->key = t0;
    else
      object->key = ERRORTAG;
  } else
    object->key = ERRORTAG;

  CFUN__PROCEED(object);

 too_large:
  SERIOUS_FAULT("term too large in assert or record");

 too_large_nox:
  SERIOUS_FAULT("term too large in assert or record (increase XREGBANKSIZE value)");
}

#endif /* USE_CTERM */

/***************************************************************************/ 

extern tagged_t atom_true;

/* Support for if/3 */
CBOOL__PROTO_N(bu1_if, tagged_t x0) {
  DEREF(x0,x0);
  *TaggedToCar(x0) = atom_true;
  CBOOL__PROCEED;
}

CBOOL__PROTO(metachoice) {
  CBOOL__UnifyCons(ChoiceToTagged(w->choice),X(0));
  CBOOL__PROCEED;
}

CBOOL__PROTO(metacut) {
  DEREF(X(0),X(0));
  CODE_CUT(ChoiceFromTagged(X(0)));
  CBOOL__PROCEED;
}

extern try_node_t *address_nd_repeat;

CBOOL__PROTO(prolog_repeat) {
  CVOID__CALL_N(push_choicept,address_nd_repeat);
  CBOOL__PROCEED;
}

CBOOL__PROTO(nd_repeat) {
  CBOOL__PROCEED;
}

/* TODO: do not share, use one per worker */
JMP_BUF abort_env; /* Shared */

CBOOL__PROTO(flush_output);

#include <stdio.h>
/* TODO: check that the test frame overflow in complete_frame behaves
   correctly when SETUP_PENDING_CALL is called inside an 'event' */
#define SETUP_PENDING_CALL(Func) { \
  intmach_t i; \
  intmach_t arity; \
  frame_t *frame; \
  CODE_ALLOC(frame); \
  frame->x[0] = PointerToTerm(Func); \
  arity = FuncArity(Func); \
  for (i=0; i<arity; i++) frame->x[i+1] = X(i); \
  CODE_CFRAME(frame, CONTCODE(arity+1)); \
}

CFUN__PROTO_N(call_firstgoal, intmach_t, tagged_t goal, goal_descriptor_t *goal_desc) {
  intmach_t i;
  w->choice->x[0] = X(0) = goal;
  G->next_insn = bootcode;

 enter_wam:
  i = SETJMP(abort_env);
  if (i == 0) { /* Just made setjmp */
    G->x[0] = w->choice->x[0];
    in_abort_context = TRUE;
    wam(goal_desc);
    if (w->misc->exit_code == WAM_ABORT) goto abort_wam;
    goto end_wam;
  } else if (i == -1) {
    goto sigint_wam;
  } else {
    goto abort_wam;
  }
 sigint_wam:
  /* SIGINT during I/O */
  {
    definition_t *func = int_address;
    int_address = NULL;
    SETUP_PENDING_CALL(func);
  }
  goto enter_wam;
 abort_wam:
  /* Prepare to call the abort predicate */
  (void)CBOOL__SUCCEED(flush_output);
#if defined(USE_THREADS)
  (void)CBOOL__SUCCEED(prolog_eng_killothers);
#endif
  in_abort_context = FALSE;
  CVOID__CALL(reinitialize_stacks);
  in_abort_context = TRUE;
  CVOID__CALL(local_init_each_time);
  glb_init_each_time();
  /* TODO: do not rewrite bootcode! */
  {
    bcp_t P;
    P = bootcode;
    EMIT_op(call);
    EMIT_E(address_restart);
    EMIT_e(sizeof(frame_t)); /* initial FrameSize */
  }
  goto enter_wam;
 end_wam:
  (void)CBOOL__SUCCEED(flush_output);
  CFUN__PROCEED(w->misc->exit_code);
}

/* Here with wam and goal */

CVOID__PROTO_N(startgoal__1, goal_descriptor_t *goal_desc) {
  G->next_insn = startgoalcode;
  w->choice->x[0] = X(0);    /* Arg. of a call/1 */
  SetDeep(); 

#if defined(USE_THREADS)
  DEBUG__TRACE(debug_threads,
	       "%ld (%ld) Goal %lx entering wam()\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER, (long)goal_desc);
#endif

  wam(goal_desc);
}

CFUN__PROTO_N(get_thread_result, intmach_t,
	   intmach_t doing_backtracking, goal_descriptor_t *goal_desc);

THREAD_RES_T startgoal(THREAD_ARG wo) {
  goal_descriptor_t *goal_desc = (goal_descriptor_t *)wo;
  intmach_t result_state;

  WITH_WORKER(goal_desc->worker_registers, {
    CVOID__CALL_N(startgoal__1, goal_desc);
    result_state = CFUN__EVAL_N(get_thread_result, 0, goal_desc);
  });
  
  return (THREAD_RES_T)(intp_t)(result_state == PENDING_SOLS);
}

/* If we hit the initial ghost choicepoint, then it means that no
   solution was returned by this call.  If we call the
   make_backtracking() primitive, then KEEP_STACKS is true. */

THREAD_RES_T make_backtracking(THREAD_ARG wo) {
  goal_descriptor_t *goal_desc = (goal_descriptor_t *)wo;
  intmach_t result_state;

  wam(goal_desc);
  WITH_WORKER(goal_desc->worker_registers, {
    result_state = CFUN__EVAL_N(get_thread_result, 1, goal_desc);
  });
  return (THREAD_RES_T)(intp_t)(result_state == PENDING_SOLS);
}

CFUN__PROTO_N(get_thread_result, intmach_t,
	      intmach_t doing_backtracking, goal_descriptor_t *goal_desc) {
  intmach_t result_state;
  intmach_t wam_result;
  wam_result = w->misc->exit_code;
  if (wam_result == WAM_ABORT) {
    /* TODO: return an error code or throw an exception, this is a
       CFUN and MAJOR_FAULT cannot be used here */
    MAJOR_FAULT("Wam aborted!");
  }

#if defined(USE_THREADS)
  DEBUG__TRACE(debug_threads,
	       "%ld (%ld) Goal %lx exited wam() %ld\n", 
	       (long)Thread_Id, (long)GET_INC_COUNTER, (long)goal_desc, (long)doing_backtracking);
#endif

  (void)CBOOL__SUCCEED(flush_output);

  /* eng_wait() may change NEEDS_FREEING and consults the state of the
     thread; therefore we lock until it is settled down */

  Wait_Acquire_slock(goal_desc->goal_lock_l);
  if (G->next_alt == termcode) {
    /* We can make the WAM available right now */
    unlink_wam(goal_desc);
    goal_desc->state = FAILED;
  } else {
    goal_desc->state = PENDING_SOLS;
  }

  /* In some cases (i.e., Win32) the resources used up by the thread
     are not automatically freed upon thread termination.  If needed,
     the thread handle is enqued. In an (I hope) future implementation
     the thread will go to sleep instead of dying. */

  if ((goal_desc->action & NEEDS_FREEING) ||
      (doing_backtracking && wam_result == WAM_INTERRUPTED)) { /* Implies thread created */
#if defined(USE_THREADS)
    DEBUG__TRACE(debug_threads,
		 "Goal %lx enqueuing itself\n", (long)goal_desc);
#endif
    /* Free, enqueue myself */
    enqueue_thread(goal_desc->thread_handle);
  } else {
    /* Free whoever was there, enqueue no one */
    enqueue_thread((THREAD_T)NULL);
  }

  /* Save the state for the exit result (if we release the goal, its
     state may change before we return from the function). */
  result_state = goal_desc->state;

  if (!doing_backtracking) {
    /* Goals failed when executed by the local thread, and goals for
       which no Id was requested, release the memory areas
       automatically */
    if ((wam_result == WAM_INTERRUPTED) ||
	!(goal_desc->action & KEEP_STACKS) ||
	((goal_desc->state == FAILED) && !(goal_desc->action & CREATE_THREAD)))
      make_goal_desc_free(goal_desc);
  }
  /*
    else if ((goal_desc->state == FAILED) && !(goal_desc->action &
    CREATE_THREAD)) make_goal_desc_free(goal_desc);
  */

  Release_slock(goal_desc->goal_lock_l);

#if defined(USE_THREADS)
  DEBUG__TRACE(debug_threads || debug_conc,
	       "*** %ld(%ld) Goal %lx is EXITING %ld\n", 
	       (long)Thread_Id, (long)GET_INC_COUNTER, (long)goal_desc,
	       (long)doing_backtracking);
#endif
  CFUN__PROCEED(result_state);
}

/***************************************************************************/ 

bcp_t call_code;
bcp_t default_code;
bcp_t null_code;
try_node_t nullgoal_alt;
try_node_t defaultgoal_alt;
try_node_t startgoal_alt;

CVOID__PROTO(ciao_initcode) {
  bcp_t P;

  /* some C->Prolog interface initializations */
  PBUFF_begin;
  EMIT_op(call);
  EMIT_E(address_call);
  EMIT_e(sizeof(frame_t)); /* initial FrameSize */
  EMIT_op(exit_toplevel);
  PBUFF_finish(call_code);

  PBUFF_begin;
  EMIT_op(exit_toplevel);
  PBUFF_finish(null_code);

  PBUFF_begin;
  EMIT_op(exit_toplevel);
  PBUFF_finish(default_code);
  
  startgoal_alt.altop = OPCODEenc(restore_all_no_alt);
  startgoal_alt.arity = 1;
  startgoal_alt.clause = NULL;
  startgoal_alt.code = (char *)call_code;
  startgoal_alt.next = NULL;
#if defined(ABSMACH_OPT__incoreopt)
  startgoal_alt.previous = NULL;
#endif

  defaultgoal_alt.altop = OPCODEenc(restore_all_next_alt);
  defaultgoal_alt.arity = 0;
  defaultgoal_alt.clause = NULL;
  defaultgoal_alt.code = (char *)default_code;
  defaultgoal_alt.next = &nullgoal_alt; 
#if defined(ABSMACH_OPT__incoreopt)
  defaultgoal_alt.previous = NULL; 
#endif

  nullgoal_alt.altop = OPCODEenc(restore_all_next_alt);
  nullgoal_alt.arity = 0;
  nullgoal_alt.clause = NULL;
  nullgoal_alt.code = (char *)null_code;
  nullgoal_alt.next = &nullgoal_alt; /* loop forever */
#if defined(ABSMACH_OPT__incoreopt)
  nullgoal_alt.previous = 0;
#endif
}

/***************************************************************************/ 

bcp_t startgoalcode; /* WAM code to start a goal -- Shared */
bcp_t bootcode; /* WAM bootstrap to run bootgoal -- Shared */
bcp_t contcode; /* continuations of FrameSize N after exceptions -- Shared */
bcp_t failcode; /* continuation of FrameSize 0 that fails -- Shared */
bcp_t exitcode; /* continuation of FrameSize 0 that exits wam -- Shared */

try_node_t *termcode; /* "clause" of arity 1 that exits wam -- Shared */
try_node_t *fail_alt; /* null alternative, arity=0 -- Shared */

/* pre: typeof(T) == try_node_t */
/* TODO: find better solution? (avoid patching some predefined nodes) */
/* TODO: use BCOp and use correct ftype */
#define SET_NONDET(T) { \
  if ((T)->clause != NULL) (T)->code = (char *)BCoff(((T)->clause->emulcode), FTYPE_size(f_i)); \
}

void incore_insert(try_node_t **t0,
		   intmach_t effar,
		   emul_info_t *ref) {
  try_node_t *t;

  /* Init the try_node to insert. */
  t = CHECKALLOC(try_node_t);
  t->arity = effar;

  t->clause = ref;
  TRY_NODE_SET_DET(t, ref);

  if (TRY_NODE_IS_NULL(*t0)) {
    *t0 = t;
  } else {
    try_node_t *tt;
#if defined(ABSMACH_OPT__incoreopt)
    /* try_node will be a cyclic list... we can access the 'last' node from the 'first' */
    tt = *t0;
    tt = tt->previous;
    SET_NONDET(tt);
#else
    tt = *t0;
    if (tt->next==NULL) {
      SET_NONDET(tt);
    }
    while (!TRY_NODE_IS_NULL(tt->next)) {
      tt = tt->next;
      if (tt->next==NULL) {
        SET_NONDET(tt);
	break;
      }
    }
#endif
    tt->altop = OPCODEenc(restore_all_next_alt);
    tt->next = t;
#if defined(ABSMACH_OPT__incoreopt)
    t->previous = tt;
#endif
  }
#if defined(ABSMACH_OPT__incoreopt)
  (*t0)->previous = t;
#endif
}

/* Copy a try_node structure (recursively) */

try_node_t *incore_copy(try_node_t *from) {
  try_node_t *copy;
  try_node_t *t;
  try_node_t *t0;

  if (TRY_NODE_IS_NULL(from)) {
    copy = fail_alt;
  } else {
    copy = t = CHECKALLOC(try_node_t);
    t->altop = from->altop;
    t->arity = from->arity;
    t->clause = from->clause;
    t->code = from->code;
    from=from->next;
    while (!TRY_NODE_IS_NULL(from)) {
      t0 = t;
      t0->next = t = CHECKALLOC(try_node_t);
      t->altop = from->altop;
      t->arity = from->arity;
      t->clause = from->clause;
      t->code = from->code;
#if defined(ABSMACH_OPT__incoreopt)
      t->previous = t0;
#endif
      from = from->next; 
    }
    t->next = NULL;
#if defined(ABSMACH_OPT__incoreopt)
    copy->previous = t;
#endif
  }
  return copy;
}

/* TODO: what does this comment mean??: I still could not make the
   "pointer to the last try_node" work with the has table try nodes; I
   am passing a NULL pointer which is checked by incore_insert() and
   not used.  */

void incore_puthash(hashtab_t **psw,
		    intmach_t effar,
		    emul_info_t *current,
		    hashtab_key_t k) {
  intmach_t i;
  hashtab_node_t *h1;
  try_node_t *otherwise = NULL;
  intmach_t size = HASHTAB_SIZE(*psw);

  if (k==ERRORTAG) { /* add an alt. to default and to every key */
    for (i=0; i<size; i++) {
      h1 = &(*psw)->node[i];
      if (h1->key) {
        incore_insert(&h1->value.try_chain,effar,current);
      } else {
	/* empty entries share the same try_chain */
	if (!otherwise) {
	  incore_insert(&h1->value.try_chain,effar,current);
	  otherwise = h1->value.try_chain;
	} else {
	  h1->value.try_chain = otherwise;
	}
      }
    }
  } else {
    h1 = hashtab_get(*psw,k);
    if (!h1->key) {
      h1->key = k;
      otherwise=h1->value.try_chain;
      h1->value.try_chain = incore_copy(otherwise);
      incore_insert(&h1->value.try_chain,effar,current);
      if (((*psw)->count+=1)<<1 > size) {
        HASHTAB_EXPAND_DEF(psw, otherwise);
      }
    } else {
      incore_insert(&h1->value.try_chain,effar,current);
    }
  }
}

CFUN__PROTO_N(def_retry_cbool, try_node_t *, cbool_t proc, intmach_t arity) {
  try_node_t *item;

  item = CHECKALLOC(try_node_t);
  item->altop = OPCODEenc(restore_all_next_alt);
  item->arity = arity;
  item->clause = NULL;
  item->next = item;
#if defined(ABSMACH_OPT__incoreopt)
  item->previous = NULL;
#endif

  {
    bcp_t P;
    PBUFF_begin;
    EMIT_op(retry_cbool__proceed);
    EMIT_Cc(proc);
    PBUFF_finish(item->code);
  }
  CFUN__PROCEED(item);
}

CFUN__PROTO_N(def_retry_cinsnp, try_node_t *, cinsnp_t proc, intmach_t arity) {
  try_node_t *item;

  item = CHECKALLOC(try_node_t);
  item->altop = OPCODEenc(exec_cinsnp_alt);
  item->arity = arity;
  item->clause = NULL;
  item->next = item;
#if defined(ABSMACH_OPT__incoreopt)
  item->previous = NULL;
#endif

  item->code = (char *)proc;
  return item;
}

CFUN__PROTO_N(def_exec_cinsnp, bcp_t, cinsnp_t proc) {
  bcp_t code;

  {
    bcp_t P;
    PBUFF_begin;
    EMIT_op(exec_cinsnp);
    EMIT_Ci(proc);
    PBUFF_finish(code);
  }
  CFUN__PROCEED(code);
}

#define CINSNP__LOOP(PTR) CFUN__EVAL_N(cinsnp_loop, PTR)
static inline CFUN__PROTO_N(cinsnp_loop, bcp_t, bcp_t newp) {
  while (BCOp(newp,FTYPE_ctype(f_o),0) == opIdEnc(DEFOP_exec_cinsnp)) {
    newp = CINSNP__INSNCONT((*(cinsnp0_t)BCOp(newp,char *,SIZEDEFOP_exec_cinsnp)));
  }
  return newp;
}

CFUN__PROTO_N(def_success_cinsnp, bcp_t, cinsnp_t proc, intmach_t frame_live_size) {
  bcp_t code;
  intmach_t code_off;

  {
    bcp_t P;
    PBUFF_begin;
    /* TODO: trick to align the exec_cinsnp instruction to 8 byte */
    P = BCoff(P, 8-FTYPE_size(f_e));
    /* frame size */
    EMIT_e(FrameSize0(frame_live_size));
    /* here starts the bytecode */
    code_off = PBUFF_used;
    EMIT_op(exec_cinsnp);
    EMIT_Ci(proc);
    PBUFF_finish(code);
  }
  CFUN__PROCEED(BCoff(code, code_off));
}

/* Find a null alt. of 'arity', or push a new one. */
static CFUN__PROTO(get_null_alt, try_node_t *) {
  try_node_t *a;
  bcp_t insnfail;

  a = CHECKALLOC(try_node_t);

  {
    bcp_t P;
    PBUFF_begin;
    EMIT_op(failins);
    PBUFF_finish(insnfail);
  }

  a->altop = OPCODEenc(restore_all_no_alt);
  a->arity = 0;
  a->clause = NULL;
  a->code = (char *)insnfail;
  a->next = NULL; /* no more alternatives */
#if defined(ABSMACH_OPT__incoreopt)
  a->previous = NULL;
#endif

  CFUN__PROCEED(a);
}

/***************************************************************************/ 
/* Frame/Choicept manipulations */

/* Pre: Deep */
CVOID__PROTO(pop_choicept) {
  choice_t *b = w->choice;
  b = ChoiceCont(b);
  SetChoice(b);
}

CVOID__PROTO_N(push_choicept, try_node_t *alt) {
  choice_t *b;
  CODE_CHOICE_NEW(b, alt);
  CODE_NECK_TRY(b);
  SetDeep();
}

CBOOL__PROTO(nd_repeat);
CBOOL__PROTO(nd_current_atom);
CBOOL__PROTO(nd_current_module);
CBOOL__PROTO(nd_module_uses);
CBOOL__PROTO(nd_atom_concat);

extern try_node_t *address_nd_repeat;
extern try_node_t *address_nd_current_atom;
extern try_node_t *address_nd_current_predicate;
extern try_node_t *address_nd_current_module;
extern try_node_t *address_nd_module_uses;
extern try_node_t *address_nd_predicate_property;
extern try_node_t *address_nd_atom_concat;

extern try_node_t *address_nd_current_instance;
bcp_t proceed_code;
liveinfo_t prolog_format_print_integer__liveinfo;

CVOID__PROTO(init_some_bytecode) {
  intmach_t i;
  
  address_nd_repeat = CFUN__EVAL_N(def_retry_cbool,nd_repeat,0);
  address_nd_current_atom = CFUN__EVAL_N(def_retry_cbool,nd_current_atom,2);
  address_nd_current_predicate = CFUN__EVAL_N(def_retry_cbool,nd_current_predicate,4);
  address_nd_current_module = CFUN__EVAL_N(def_retry_cbool,nd_current_module,2);
  address_nd_module_uses = CFUN__EVAL_N(def_retry_cbool,nd_module_uses,3);
  address_nd_predicate_property = CFUN__EVAL_N(def_retry_cbool,nd_predicate_property,5);
  address_nd_atom_concat = CFUN__EVAL_N(def_retry_cbool,nd_atom_concat,4);

  {
    intmach_t exitcode_off;
    bcp_t P;
    PBUFF_begin;
    EMIT_op(call);
    EMIT_E(address_call);
    EMIT_e(sizeof(frame_t)); /* initial FrameSize */
    exitcode_off = PBUFF_used;
    EMIT_op(exit_toplevel);
    PBUFF_finish(bootcode);
    exitcode = BCoff(bootcode, exitcode_off);
  }

  termcode = CFUN__EVAL_N(def_retry_cbool,NULL,1); /* size of initial cpt. */
  {
    bcp_t P;
    P = (bcp_t)termcode->code;
    EMIT_op(exit_toplevel);
  }

  {
    bcp_t P;
    PBUFF_begin;
    EMIT_op(proceed);
    PBUFF_finish(proceed_code);
  }

  {
    bcp_t P;
    PBUFF_begin;
    EMIT_op(call);
    EMIT_E(address_call);
    EMIT_e(sizeof(frame_t)); /* initial FrameSize */
    EMIT_op(exit_toplevel);
    PBUFF_finish(startgoalcode);
  }

  fail_alt = CFUN__EVAL(get_null_alt);
  {
    intmach_t failcode_off;
    bcp_t P;
    PBUFF_begin;
    EMIT_e(sizeof(frame_t)); /* FrameSize */
    /* here starts the instruction */
    failcode_off = PBUFF_used;
    EMIT_op(lastcall);
    EMIT_E(address_fail);
    PBUFF_finish(failcode);
    failcode = BCoff(failcode, failcode_off);
  }

  {
    /* CONTCODE(i+1)
       is a good continuation when interrupting 'foo/i'. */
    bcp_t P;
    PBUFF_begin;
    /* TODO: generate this code automatically to calculate the exact
       size... f_Q are not necessary but are added here as an
       upper limit */
    PBUFF_ensure((FTYPE_size(f_e)+FTYPE_size(f_o)+FTYPE_size(f_Q))*ARITYLIMIT);
    for (i=0; i<ARITYLIMIT; i++) {
      EMIT_e(sizeof(frame_t)+i*sizeof(tagged_t)); /* FrameSize */
      EMIT_op(kontinue);
    }
    PBUFF_finish(contcode);
    contcode = BCoff(contcode, FTYPE_size(f_e));
  }

  /* TODO: move to other module... */
  address_nd_current_instance = CFUN__EVAL_N(def_retry_cinsnp,next_instance,DynamicPreserved);

  LIVEINFO__INIT(prolog_format_print_integer__liveinfo, CONTPAD, 3);
}

/* local declarations */

CBOOL__PROTO(set_predtrace) {
  tagged_t x;
  DEREF(x,X(0));
  CBOOL__TEST(TaggedIsSmall(x));
#if defined(DEBUG_TRACE)
  debug_predtrace = (bool_t)GetSmall(x);
#endif
  CBOOL__PROCEED;
}

/***************************************************************************/ 
/* safe print routines (works even with gc bits on) */

#if defined(DEBUG_TRACE) || defined(USE_RTCHECKS) || defined(USE_DEBUG_INSCOUNT)

static tagged_t safe_deref(tagged_t t) {
  DerefVar(t);
  return GC_UNMARKED(t);
}

CVOID__PROTO_N(print_number, stream_node_t *stream, tagged_t term);
CVOID__PROTO_N(print_variable, stream_node_t *stream, tagged_t term);
CVOID__PROTO_N(print_atom, stream_node_t *stream, tagged_t term);

CVOID__PROTO_N(wr_tagged_rec, tagged_t t) {
  tagged_t temp;
  intmach_t arity,i;

  t = safe_deref(t);

  SwOnAnyTagB(t, t_head_functor, { /* HVA */
    goto variable;
  }, { /* SVA */
    goto variable;
  }, { /* CVA */
  variable:
    CVOID__CALL_N(print_variable,stream_trace,t);
  }, { /* NUM */
  number:
    CVOID__CALL_N(print_number,stream_trace,t);
  }, { /* ATM */
    CVOID__CALL_N(print_atom,stream_trace,t);
  }, { /* LST */
    putc('[', TraceFile);
    temp = *TaggedToCar(t);
    CVOID__CALL_N(wr_tagged_rec,temp);
    temp = *TaggedToCdr(t);
    t = safe_deref(temp);
    while(TaggedIsLST(t))	{
      putc(',', TraceFile);
      temp = *TaggedToCar(t);
      CVOID__CALL_N(wr_tagged_rec,temp);
      temp = *TaggedToCdr(t);
      t = safe_deref(temp);
    }
    if(t!=atom_nil) {
      putc('|', TraceFile);
      CVOID__CALL_N(wr_tagged_rec,t);
    }
    putc(']', TraceFile);
  }, { /* STR(blob) */
    /* todo[ts]: change if more blob types are added */
    goto number;
  }, { /* STR(struct) */
    CVOID__CALL_N(wr_tagged_rec, t_head_functor);
    putc('(', TraceFile);
    arity = Arity(t_head_functor);
    for (i=1; i<=arity; i++) {
      if (i>1) putc(',', TraceFile);
      temp = *TaggedToArg(t,i);
      CVOID__CALL_N(wr_tagged_rec,temp);
    }
    putc(')', TraceFile);
  });
}

CVOID__PROTO_N(wr_tagged, tagged_t t) {
  CVOID__CALL_N(wr_tagged_rec,t);
  putc('\n', TraceFile);
}

void wr_functor_1(definition_t *func) {
  TRACE_PRINTF("%s/%ld", GetString(FuncName(func)), (long)FuncArity(func));
} 

void wr_functor(char *s, definition_t *func) {
  TRACE_PRINTF("%s: ",s);
  wr_functor_1(func);
  putc('\n', TraceFile);
}

CVOID__PROTO_N(wr_functor_spec, tagged_t t) {
  CVOID__CALL_N(wr_tagged,t);
  TRACE_PRINTF("/%ld\n", (long)Arity(t));
}

CVOID__PROTO_N(wr_x, intmach_t i) {
  CVOID__CALL_N(wr_tagged,X(i));
}
#endif

/***************************************************************************/ 

#define PRED_DEBUG(X,Y) RTCHECK(CVOID__CALL_N(dump_call,X,Y));

/* Predicate tracer */
#if defined(DEBUG_TRACE)
#define PRED_TRACE(X,Y) predtrace(X,Y);
static void predtrace(char *s, definition_t *func) {
  extern intmach_t debug_inscount;
  INSCOUNT_NEXT();
  if (debug_predtrace && dump_cond()) {
    TRACE_PRINTF("[time = %ld] ", (long)debug_inscount);
    wr_functor(s, func);
  }
}
#else
#define PRED_TRACE(X,Y)
#endif

/* Predicate profiler */
#if defined(ABSMACH_OPT__profile_calls)
#define PRED_PROFILE(X,Y) if (profile) add_to_profiling(Y);
#else
#define PRED_PROFILE(X,Y)
#endif

/* Profile memory usage */
/* TODO: right now, only the high water mark of stacks */
#if defined(ABSMACH_OPT__mem_profile)
CVOID__PROTO(memprofile);
#define PRED_MEMPROFILE() CVOID__CALL(memprofile);
#else
#define PRED_MEMPROFILE()
#endif

#define PRED_HOOK(X,Y) \
  PRED_MEMPROFILE() \
  PRED_DEBUG(X,Y) \
  PRED_PROFILE(X,Y) \
  PRED_TRACE(X,Y)

/***************************************************************************/ 
/* Instruction profiling hooks */

#if defined(PROFILE_INSFREQ)
#if defined(ABSMACH_OPT__threaded_bytecode_rel16)||defined(ABSMACH_OPT__threaded_bytecode)
#error "PROFILE_INSFREQ is not intended to work with threaded bytecode"
#endif
#define PROFILE_EVENTFREQ 1
#define INS_HOOK_R(I) eventfreq__hook(INSFREQ__I(0, (I)));
#define INS_HOOK_W(I) eventfreq__hook(INSFREQ__I(1, (I)));
#define INSFREQ__I(M,I) (((I) * 2) + (M))
#define EVENTFREQ__FIRST INSFREQ__I(0, 0)
#define EVENTFREQ__TOTAL INSFREQ__I(0, INS_OPCOUNT)
#define EVENTFREQ__INITIAL INSFREQ__I(0, OPCODE(exit_toplevel))
#define EVENTFREQ__DUMPFILE "ciao__insfreq.dump"
#define EVENTFREQ__DUMP_UNUSED TRUE
#define EVENTFREQ__FPRINT(OUT, X) insfreq__fprint(OUT, X)
static void insfreq__fprint(FILE *out, intmach_t x) {
  intmach_t i, j;
  i = x >> 1;
  j = x & 1;
  if (strcmp(ins_name[i], "?") != 0) {
    fprintf(out, "(%s,%c)", ins_name[i], j ? 'r' : 'w');
  } else {
    fprintf(out, "(ilegal(%ld),%c)", (long)i, j ? 'r' : 'w');
  }
}
#elif defined(PROFILE_INS2FREQ)
#if defined(ABSMACH_OPT__threaded_bytecode_rel16)||defined(ABSMACH_OPT__threaded_bytecode)
#error "PROFILE_INS2FREQ is not intended to work with threaded bytecode"
#endif
#define PROFILE_EVENTFREQ 1
static FTYPE_ctype(f_o) ins2freq__prev = OPCODE(exit_toplevel);
#define INS_HOOK_R(I) INS_HOOK_ANY(I)
#define INS_HOOK_W(I) INS_HOOK_ANY(I)
#define INS_HOOK_ANY(I) { eventfreq__hook(INS2FREQ__I(ins2freq__prev, (I))); ins2freq__prev = (I); }
#define INS2FREQ__I(I,J) (((I) * INS_OPCOUNT) + (J))
#define EVENTFREQ__FIRST 0
#define EVENTFREQ__TOTAL INS_OPCOUNT * INS_OPCOUNT
#define EVENTFREQ__INITIAL OPCODE(exit_toplevel) * INS_OPCOUNT
#define EVENTFREQ__DUMPFILE "ciao__ins2freq.dump"
#define EVENTFREQ__DUMP_UNUSED FALSE
#define EVENTFREQ__FPRINT(OUT, X) ins2freq__fprint(OUT, X)
static void ins2freq__printname(FILE *out, intmach_t i) {
  if (strcmp(ins_name[i], "?") != 0) {
    fprintf(out, "%s", ins_name[i]);
  } else {
    fprintf(out, "ilegal(%ld)", (long)i);
  }
}
static void ins2freq__fprint(FILE *out, intmach_t x) {
  intmach_t i, j;
  i = x / INS_OPCOUNT;
  j = x % INS_OPCOUNT;
  fprintf(out, "[");
  ins2freq__printname(out, i);
  fprintf(out, ",");
  ins2freq__printname(out, j);
  fprintf(out, "]");
}
#else
/* TODO: improve INSTRACE mode */
#if defined(DEBUG_INSTRACE)
#define INS_HOOK_TRACE(C,I) TRACE_PRINTF("[ins](%c) %ld (p=%p, next_alt=%p)\n", (C), (long)(I), P, G->next_alt)
#if !defined(ABSMACH_OPT__specmode)
#define INS_HOOK_U(I) INS_HOOK_TRACE('u', (I))
#else
#define INS_HOOK_R(I) INS_HOOK_TRACE('r', (I))
#define INS_HOOK_W(I) INS_HOOK_TRACE('w', (I))
#endif
#else
#if !defined(ABSMACH_OPT__specmode)
#define INS_HOOK_U(I)
#else
#define INS_HOOK_R(I)
#define INS_HOOK_W(I)
#endif
#endif
#endif

/***************************************************************************/ 
/* Profiling of engine instructions using INS_HOOK */

#if defined(PROFILE_EVENTFREQ)
#endif

#if defined(PROFILE_EVENTFREQ)
flt64_t wallclick_to_time(intclick_t click);
intclick_t internal_wallclick_tsc(void);

/* TODO: improve profile and debug code */
#define EVENTFREQ__MIN(X,Y) ((X) >= (Y) ? (Y) : (X))
#define EVENTFREQ__MAX(X,Y) ((X) >= (Y) ? (X) : (Y))
struct eventfreq__info {
  intclick_t c; /* clicks */
  int64_t i; /* number of executions */
  intclick_t minc; /* minimum clicks */
  intclick_t maxc; /* maximum clicks */
};
static struct eventfreq__info eventfreq__info[EVENTFREQ__TOTAL];
static intclick_t eventfreq__wc;
static intclick_t eventfreq__wc0;
static intclick_t eventfreq__overhead;
static intclick_t eventfreq__dc;
static intmach_t eventfreq__last;
static void eventfreq__hook(intmach_t x) {
  eventfreq__wc0 = internal_wallclick_tsc();
  eventfreq__dc = eventfreq__wc0 - eventfreq__wc;
  eventfreq__info[eventfreq__last].c += eventfreq__dc;
  eventfreq__info[eventfreq__last].minc = EVENTFREQ__MIN(eventfreq__info[eventfreq__last].minc, eventfreq__dc);
  eventfreq__info[eventfreq__last].maxc = EVENTFREQ__MAX(eventfreq__info[eventfreq__last].maxc, eventfreq__dc);
  eventfreq__info[eventfreq__last].i++;
  eventfreq__last = x;
  //  eventfreq__wc = eventfreq__wc0;
  eventfreq__wc = internal_wallclick_tsc();
}
#define EVENTFREQ__CLICKTOP 100000000000000LL
static void eventfreq__clear() {
  intmach_t x;
  for (x = EVENTFREQ__FIRST; x < EVENTFREQ__TOTAL; x++) {
    eventfreq__info[x].c = 0;
    eventfreq__info[x].minc = EVENTFREQ__CLICKTOP; /* a big number */
    eventfreq__info[x].maxc = 0LL; /* a small number */
    eventfreq__info[x].i = 0;
  }
  eventfreq__last = EVENTFREQ__INITIAL;
  eventfreq__wc = internal_wallclick_tsc();
}
static void eventfreq__calc_overhead() {
  intmach_t i;
  /* A do-nothing loop to measure the overhead */
  for (i = 0; i < 256; i++) {
    eventfreq__hook(EVENTFREQ__FIRST); 
    eventfreq__hook(EVENTFREQ__FIRST); 
    eventfreq__hook(EVENTFREQ__FIRST); 
    eventfreq__hook(EVENTFREQ__FIRST); 
    eventfreq__hook(EVENTFREQ__FIRST); 
    eventfreq__hook(EVENTFREQ__FIRST); 
    eventfreq__hook(EVENTFREQ__FIRST); 
    eventfreq__hook(EVENTFREQ__FIRST); 
  }
  eventfreq__overhead = eventfreq__info[EVENTFREQ__FIRST].minc;
}
static intmach_t eventfreq__cmp(const void *a, const void *b) {
  intmach_t xa, xb;
  xa = *(intmach_t *)a;
  xb = *(intmach_t *)b;
  if (eventfreq__info[xa].c < eventfreq__info[xb].c) {
    return 1;
  } else if (eventfreq__info[xa].c == eventfreq__info[xb].c) {
    return 0;
  } else {
    return -1;
  }
}
static void eventfreq__dump() {
  intmach_t x;
  FILE *out;
  intclick_t total;
  intclick_t eff_overhead;
  const char *out_filename = EVENTFREQ__DUMPFILE;
  intmach_t sorted_x[EVENTFREQ__TOTAL];

  out = fopen(out_filename, "w");
  if (out == NULL) {
    TRACE_PRINTF("{profile: cannot open %s}\n", out_filename);
    return;
  }
  fprintf(out, "%% file generated with eventfreq\n");
  total = 0;
  /* Calculate the effective overhead (lower the precal overhead if
     some instruction took less time to exec) */
  eff_overhead = eventfreq__overhead;
  for (x = EVENTFREQ__FIRST; x < EVENTFREQ__TOTAL; x++) {
    if (eventfreq__info[x].i == 0) continue;
    eff_overhead = EVENTFREQ__MIN(eventfreq__info[x].minc, eff_overhead);
  }
  /* Substract the effective overhead */
  for (x = EVENTFREQ__FIRST; x < EVENTFREQ__TOTAL; x++) {
    if (eventfreq__info[x].i == 0) continue;
    eventfreq__info[x].c -= eff_overhead * eventfreq__info[x].i;
    eventfreq__info[x].minc -= eff_overhead;
    eventfreq__info[x].maxc -= eff_overhead;
  }
  /* Calculate the total number of clicks */
  for (x = EVENTFREQ__FIRST; x < EVENTFREQ__TOTAL; x++) {
    if (eventfreq__info[x].i == 0) continue;
    total += eventfreq__info[x].c;
  }
  /* Calculate sorted_x (array of sorted indices) */
  for (x = EVENTFREQ__FIRST; x < EVENTFREQ__TOTAL; x++) {
    sorted_x[x] = x;
  }
  qsort((void *)sorted_x, EVENTFREQ__TOTAL, sizeof(intmach_t), eventfreq__cmp);
  /* Print the sorted table */
  fprintf(out, "%% freq(time (ms), mean (ns), min (ns), max (ns), total, name).\n");
  if (total != 0) { /* TODO: do anything if total is 0? */
    intmach_t y;
    for (y = EVENTFREQ__FIRST; y < EVENTFREQ__TOTAL; y++) {
      x = sorted_x[y];
      if (eventfreq__info[x].i == 0) {
	if (EVENTFREQ__DUMP_UNUSED) {
	  fprintf(out, "unused(");
	  EVENTFREQ__FPRINT(out, x);
	  fprintf(out, ").");
	}
      } else {
	fprintf(out,
		"freq(%.9f, %.9f, %.3f, %.3f, %.3f, %llu, ",
		wallclick_to_time(eventfreq__info[x].c),
		100.0*(flt64_t)eventfreq__info[x].c/(flt64_t)total,
		10.0e6*wallclick_to_time(eventfreq__info[x].c)/eventfreq__info[x].i,
		10.0e6*wallclick_to_time(eventfreq__info[x].minc),
		10.0e6*wallclick_to_time(eventfreq__info[x].maxc),
		eventfreq__info[x].i);
	EVENTFREQ__FPRINT(out, x);
	fprintf(out, ").");
      }
      fprintf(out, "\n");
    }
  }
  fprintf(out, " ins overhead %f ns\n", 10.0e6*wallclick_to_time(eventfreq__overhead));
  fprintf(out, " ins effective overhead %f ns\n", 10.0e6*wallclick_to_time(eff_overhead));
  fprintf(out, " total %.9f ms}\n", wallclick_to_time(total));
  fclose(out);
  TRACE_PRINTF("{profile: eventfreq saved in %s}\n", out_filename);
}
#endif

#if defined(PROFILE_BLOCKFREQ)
#define JUMP_HOOK(I) { blockfreq__hook((void *)(I)); }
#else
#define JUMP_HOOK(I)
#endif

#if defined(PROFILE_BLOCKFREQ)
#error "Profile blockfreq does not work"
/* TODO: move JUMP_HOOK to correct places? */
flt64_t wallclick_to_time(intclick_t click);
intclick_t internal_wallclick_tsc(void);
hashtab_t *blockfreq__table;
void blockfreq__init() {
  blockfreq__table = HASHTAB_NEW(64);
}
void blockfreq__hook(void *p) {
  tagged_t key;
  key = (tagged_t)p;
  HASHTAB_LOOKUP(blockfreq__table, key, node, {
    node->value.raw++;
  }, {
    node->value.raw = 0;
  });
}

static void dump_def__bytecode(FILE *out, char *name, intmach_t arity, intmach_t number, bcp_t code, intmach_t size) {
  fprintf(out, "%p: %s/%ld/%ld (size %ld)\n", code, name, (long)arity, (long)number, (long)size);
  fprintf(out, "%p: -- end\n", (char *)code + size);
}
static void dump_def__incoreinfo(FILE *out, char *name, intmach_t arity, incore_info_t *p) {
  emul_info_t *stop;
  emul_info_t *cl;
  intmach_t number;
  
  stop = *p->clauses_tail;
  cl = p->clauses;
  number = 1;
  while (cl != stop) {
    dump_def__bytecode(out, name, arity, number, cl->emulcode, cl->objsize);
    number++;
    cl = cl->next;
  }
}
static void dump_def__intinfo(FILE *out, char *name, intmach_t arity, int_info_t *p) {
  instance_t *n;
  intmach_t number;

  number = 1;
  n = p->first;
  while (n != NULL) {
    dump_def__bytecode(out, name, arity, number, n->emulcode, n->objsize);
    number++;
    n = n->forward;
  }
}
void dump_def(FILE *out, definition_t *f) {
  intmach_t insn;
  intmach_t arity;
  char *name;

  insn = f->predtyp;

  name = GetString(FuncName(f));
  arity = FuncArity(f);

  switch(insn) {
  case ENTER_COMPACTCODE_INDEXED:
  case ENTER_PROFILEDCODE_INDEXED:
  case ENTER_COMPACTCODE:
  case ENTER_PROFILEDCODE:
    dump_def__incoreinfo(out, name, arity, f->code.incoreinfo);
    break;
  case ENTER_INTERPRETED:
    dump_def__intinfo(out, name, arity, f->code.intinfo);
    break;
  default:
    break;
  }
}

void blockfreq__end() {
  FILE *out;
  const char *out_filename = "ciao__blockfreq.dump";
  hashtab_t *table;
  hashtab_node_t *keyval;
  intmach_t j;
  definition_t *d;

  out = fopen(out_filename, "w");
  if (out == NULL) {
    TRACE_PRINTF("{profile: cannot open %s}\n", out_filename);
    return;
  }
  fprintf(out, "{profile: program blocks\n");

  table = predicates_location;
  
  j = HASHTAB_SIZE(table);
  for (--j; j>=0; --j) {           /* Find how many preds. we have called */
    keyval = &table->node[j];
    d = keyval->value.def;
    if (d != NULL &&
        (d->predtyp == ENTER_COMPACTCODE ||
	 d->predtyp == ENTER_COMPACTCODE_INDEXED)) {
      dump_def(out, d);
    }
  }

  HASHTAB_ITERATE(blockfreq__table, node, {
    fprintf(out, "%p: %ld\n", (char *)node->key, (long)node->value.raw);
  });
  hashtab_free(blockfreq__table);

  fprintf(out, "}\n");
  fclose(out);
  TRACE_PRINTF("{profile: blockfreq saved in %s}\n", out_filename);
}
#endif

/***************************************************************************/ 
/* Memory used for each predicate */
/* TODO: extend to inspect each predicate bytecode to implement atom gc */

//#define PROFILE__MEMPRED 1
#if defined(PROFILE__MEMPRED)
/* Size of a try_node chain */
static intmach_t mempred__try_node(try_node_t *t) {
  intmach_t s;
  try_node_t *t1, *t2;

  s = 0;
  for (t1=t; !TRY_NODE_IS_NULL(t1); t1=t2) {
    t2=t1->next;
    s += sizeof(try_node_t);
  }
  return s;
}
/* Size of a hashtab that contains try_node elements */
static intmach_t mempred__hashtab_try(hashtab_t *sw) {
  hashtab_node_t *h1;
  intmach_t i;
  intmach_t size = HASHTAB_SIZE(sw);
  bool_t otherwise = FALSE;
  intmach_t s;

  s = 0;
  for (i=0; i<size; i++) {
    h1 = &sw->node[i];
    if (h1->key || !otherwise) {
      s += mempred__try_node(h1->value.try_chain);
    }
    if (!h1->key) {
      otherwise = TRUE;
    }
  }
  s += TAILED_SIZE_TO_BYTES(hashtab_t, size);
  return s;
}
static intmach_t mempred__incoreinfo(incore_info_t *p) {
  emul_info_t *stop;
  emul_info_t *cl, *cl1;
  intmach_t s;

  s = 0;
  stop = *p->clauses_tail;
  for (cl=p->clauses; cl!=stop; cl=cl1) {
    cl1 = cl->next;
    s += cl->objsize;
  }
  s += sizeof(incore_info_t);
  return s;
}
static intmach_t mempred_def__intinfo(int_info_t *p) {
  instance_t *n;
  intmach_t s;

  s = 0;
  n = p->first;
  while (n != NULL) {
    s += n->objsize;
    n = n->forward;
  }
  return s;
}
static intmach_t mempred_def(FILE *out, definition_t *f) {
  intmach_t arity;
  char *name;
  intmach_t s;

  name = GetString(FuncName(f));
  arity = FuncArity(f);

  s = 0;
  switch(f->predtyp) {
  case ENTER_COMPACTCODE_INDEXED:
  case ENTER_PROFILEDCODE_INDEXED:
    {
      incore_info_t *i;
      i = f->code.incoreinfo;
      s += mempred__try_node(i->lstcase);
      s += mempred__hashtab_try(i->othercase);
      goto enter_nonindexed;
    }
  case ENTER_COMPACTCODE:
  case ENTER_PROFILEDCODE:
  enter_nonindexed:
    {
      incore_info_t *i;
      i = f->code.incoreinfo;
      s += mempred__try_node(i->varcase);
      s += mempred__incoreinfo(i);
    }
    // fprintf(out, "e %s/%ld: %ld bytes\n", name, (long)arity, (long)s);
    break;
  case ENTER_INTERPRETED:
    {
      int_info_t *i;
      hashtab_t *indexer;
      intmach_t size;
      i = f->code.intinfo;
      s += mempred_def__intinfo(i);
      /* TODO: what does happen with indexer entries? */
      indexer = i->indexer;
      size = HASHTAB_SIZE(indexer);
      s += TAILED_SIZE_TO_BYTES(hashtab_t, size);
      s += sizeof(int_info_t);
    }
    if (s > 10000) fprintf(out, "i %s/%ld: %ld bytes\n", name, (long)arity, (long)s);
    break;
  default:
    break;
  }
  return s;
}

CBOOL__PROTO(show_mempred) {
  FILE *out;
  intmach_t s;
  out = stdout;
  s = 0;
  HASHTAB_ITERATE(predicates_location, node, {
    definition_t *def;
    def = node->value.def;
    if (def != NULL) s += mempred_def(out, def);
  });
  fprintf(out, "t %ld bytes\n", (long)s);
  CBOOL__PROCEED;
}
#endif

/***************************************************************************/ 

/* Attributed variables support */
extern definition_t *address_pending_unifications;
extern definition_t *address_uvc;
extern definition_t *address_ucc;

/***************************************************************************/ 

/* TODO: 'workaround' is a temporary hack to avoid a gcc bug in 3.?.?, but it
   seems faster with it even if it is not necessary (why? check again) */
#if defined(ABSMACH_OPT__threaded_bytecode_rel16)||defined(ABSMACH_OPT__threaded_bytecode)
#define COMPUTED_GOTO(Where) goto *(Where)
#else
#define COMPUTED_GOTO(Where) ({ __label__ workaround; goto workaround; workaround: goto *(Where); })
#endif

#if !defined(ABSMACH_OPT__specmode)
#define GOTOFAIL goto u_fail
#else
#define GOTOFAIL goto r_fail
#endif

/* TODO: dirty, I need a lot of preconditions here... */
void set_predtyp_indexed(definition_t *d) {
  set_predtyp(d, d->predtyp+1);
}

void update_predtyp(definition_t *d) {
  set_predtyp(d, d->predtyp);
}

FTYPE_ctype(f_o) get_enterop(intmach_t predtyp) {
  switch (predtyp) {
  case ENTER_UNDEFINED:
    return OPCODEenc(func__enter_undefined);
  case ENTER_INTERPRETED:
    return OPCODEenc(func__enter_interpreted);
  case ENTER_CBOOL:
    return OPCODEenc(func__enter_cbool);
  case ENTER_CINSNP:
    return OPCODEenc(func__enter_cinsnp);
  case ENTER_CVOID:
    return OPCODEenc(func__enter_cvoid);
  case ENTER_COMPACTCODE_INDEXED:
  case ENTER_PROFILEDCODE_INDEXED:
    return OPCODEenc(func__enter_indexed);
  case ENTER_COMPACTCODE:
  case ENTER_PROFILEDCODE:
    return OPCODEenc(func__enter_nonindexed);
  default:
    SERIOUS_FAULT("undefined predtyp");
  }
}

void set_predtyp(definition_t *d, intmach_t type) {
  d->predtyp = type;
  d->enterop = get_enterop(d->predtyp);
}

CFUN__PROTO_N(handle_event, definition_t *, definition_t *def) {
  intmach_t wake_count;
  tagged_t t0;
  wake_count = WakeCount();
  if (HeapCharAvailable(G->heap_top) < CALLPAD+4*wake_count*sizeof(tagged_t)) {
    SETUP_PENDING_CALL(def);
    def = address_true;
    CVOID__CALL_N(heap_overflow,CALLPAD+4*wake_count*sizeof(tagged_t));
  }
  if (wake_count > 0) {
    if (wake_count==1) {
      SETUP_PENDING_CALL(def);
      CVOID__CALL(collect_one_pending_unification);
      DEREF(t0,X(1));
      if (TaggedIsCVA(t0)) {
#if 0 /* old attributes */
	X(1) = *TaggedToGoal(t0);
#else /* backporting new attributes */
	X(1) = t0;
#endif
	def = address_ucc;
      } else {
	/* TODO: why not pass the dereferenced t0? */
	def = address_uvc;
      }
    } else {
      SETUP_PENDING_CALL(def);
      def = address_pending_unifications;
      CVOID__CALL_N(collect_pending_unifications,wake_count);
    }
  }
  if (OffStacktop(G->frame,Stack_Warn)) {
    SETUP_PENDING_CALL(def);
    def = address_true;
    CVOID__CALL(stack_overflow);
  }
  UnsetEvent();
  if (TestCIntEvent()) {
#if defined(USE_PROLOG_DEBUGGER)
    /* TODO: do not interfere with C-c */
    if (debug_mode) {
      if (debug_status == 1) {
	/* enable tracing for next goal */
	//TRACE_PRINTF("pd:1\n");
	debug_status = 2;
	SetCIntEvent();
      } else if (debug_status == 2) {
	/* call debug_trace */
	/* TODO: is there enough heap space? */
	tagged_t t3;
	tagged_t *pt2;
	EMUL_TO_TERM(def, t3);
	//TRACE_PRINTF("pd:2\n");
	//TRACE_PRINTF("pd(");
	//CVOID__CALL_N(display_term, t3, stream_trace, TRUE);
	//TRACE_PRINTF(")\n");
	X(0) = t3;
	UnsetCIntEvent();
	debug_status = 0;
	def = address_trace;
      }
    } else {
#endif
      SETUP_PENDING_CALL(def);
      def = address_help;
      UnsetCIntEvent();
      CVOID__CALL(control_c_normal);
#if defined(USE_PROLOG_DEBUGGER)
    }
#endif
  }
  CFUN__PROCEED(def);
}

/* TODO: for compilation to C: merge with handle_event */
CVOID__PROTO_N(v__handle_event, intmach_t arity) {
  intmach_t i;
  frame_t *a;

  if (HeapCharAvailable(G->heap_top) < CALLPAD) {
    CODE_ALLOC(a);
    a->x[0] = TaggedZero;
    for (i=0; i<arity; i++)
      a->x[i+1] = X(i);
    CODE_CFRAME(a, CONTCODE(arity+1));
    //
    CVOID__CALL_N(heap_overflow,CALLPAD);
    //
    for (i=0; i<arity; i++)
      X(i) = a->x[i+1];
    SetLocalTop(a);
    DEALLOCATE(a);
  }
  if (OffStacktop(G->frame,Stack_Warn)) {
    CODE_ALLOC(a);
    a->x[0] = TaggedZero;
    for (i=0; i<arity; i++)
      a->x[i+1] = X(i);
    CODE_CFRAME(a, CONTCODE(arity+1));
    //
    frame_t *old_frame = G->frame;
    CVOID__CALL(stack_overflow);
    a = StackCharOffset(a, (char *)G->frame-(char *)old_frame); /* relocate frame pointer */
    //
    for (i=0; i<arity; i++)
      X(i) = a->x[i+1];
    SetLocalTop(a);
    DEALLOCATE(a);
  }
  UnsetEvent();
}

/* We have finished the execution of the instance bytecode:
   execute instance_cont */
/* TODO: change order? use switch? declare X(3) values as macros? */
CVOID__PROTO_N(prolog_unlock_predicate, int_info_t *root);
CBOOL__PROTO_N(instance_to_ref, instance_t *ptr, tagged_t t);
CVOID__PROTO_N(prolog_erase_ptr, instance_t *node);

#define CODE_KONTINUE(F) { \
  (F) = TaggedToFunctor(Y(0)); \
  { intmach_t arity = FuncArity((F)); intmach_t i; for (i=0; i<arity; i++) X(i) = Y(i+1); } \
}

#define PDEF ((definition_t *)(P))

/* TODO: do something special with CVAs? study how the index tree is created... it may have some conservative case for CVA that can be optimized here <- unsure */
/* todo[ts]: how are STR(blob(bignum)) and STR(blob(float)) indexed? */
#define INCORE_INDEX_ALT(ARG, ALT) { \
  tagged_t t0, t1; \
  t0 = (ARG); \
  DerefSw_HVAorCVAorSVA_Other(t0, { /* HVA CVA SVA */ \
    (ARG) = t0; \
    (ALT) = PDEF->code.incoreinfo->varcase; \
    goto indexed_tryeach; \
  }, { \
    (ARG) = t0; \
    Sw_NUMorATM_LST_STR(t0, { /* NUM ATM */ \
      t1 = t0; \
      goto indexed_hashtb; \
    }, { /* LST */ \
      (ALT) = PDEF->code.incoreinfo->lstcase; \
      goto indexed_tryeach; \
    }, { /* STR */ \
      t1 = TaggedToHeadfunctor(t0); \
      goto indexed_hashtb; \
    }); \
  }); \
 indexed_hashtb: \
  { /* input: t1 */ \
    intmach_t i; \
    uintmach_t mask; \
    tagged_t t2; \
    uintmach_t t3; \
    hashtab_t *tab; \
    hashtab_node_t *tabNode; \
    tab = PDEF->code.incoreinfo->othercase; \
    mask = tab->mask; \
    t2 = t1; \
    for (i=0, t3 = HASHTAGGED(t1) & mask; \
	 ; \
	 i+=sizeof(hashtab_node_t), t3=(t3+i) & mask) { \
      tabNode = HASHNODE(tab, t3); \
      if (tabNode->key==t2 || !tabNode->key) { \
	(ALT) = tabNode->value.try_chain; \
        goto indexed_tryeach; \
      } \
    } \
    (ALT) = PDEF->code.incoreinfo->varcase; \
    goto indexed_tryeach; \
  } \
 indexed_tryeach: {} \
}

CVOID__PROTO_N(wam__2, bcp_t start_p);

void wam(goal_descriptor_t *desc) {
  definition_t *func;
  WITH_WORKER(desc->worker_registers, {
    DEBUG__TRACE(debug_threads, 
		 "Worker state address is %lx\n", (long)desc);

    /* set error code for halt/0, abort/0, reinitialise/0 */
    w->misc->exit_code = 0; 

    func = NULL;
  again:
    EXCEPTION__CATCH({
      bcp_t p;
      if (func != NULL) {
	p = DEF_INSNP(func);
      } else {
	/* TODO: check if the next line is correct */ /* Probably... */
	if (desc && (desc->action & BACKTRACKING)) {
	  p = FAIL_INSNP;
	} else {
	  p = SUCCESS_INSNP;
	}
      }
      CVOID__CALL_N(wam__2, p);
      /* We may have been signaled and jumped here from enter_predicate: */
      if (Stop_This_Goal(w)) {
	w->misc->exit_code = WAM_INTERRUPTED;
      }
    }, {
      /* TODO: Use absmach_def:neck/0, neck0/0? or maybe_neck/0? */
      CODE_MAYBE_NECK_TRY();     /* Force neck if not done */
      X(0) = MakeSmall(ErrCode); /* Error code */
      X(1) = GET_ATOM(ErrFuncName); /* Builtin name */
      X(2) = MakeSmall(ErrFuncArity); /* Builtin arity */
      X(4) = Culprit; /* Culprit arg. */
      X(3) = MakeSmall(ErrArgNo); /* w. number */
      func = address_error;
      goto again;
    });
    DEBUG__TRACE(debug_threads, "Goal %p returning!\n", desc);
  });
}

void init_profile() {
  /* Profile */
#if defined(DEBUG_TRACE)
  TRACE_PRINTF("{trace: debug trace activated}\n");
  RESET_COUNTER;
#endif
#if defined(USE_RTCHECKS)
  TRACE_PRINTF("{trace: superfluous rtchecks enabled}\n");
#endif
#if defined(USE_DEBUG_INSCOUNT)
  init_debug_inscount();
#endif

#if defined(PROFILE_EVENTFREQ)
  eventfreq__clear();
  eventfreq__calc_overhead();
  eventfreq__clear();
#endif

#if defined(PROFILE_BLOCKFREQ)
  blockfreq__init();
#endif
}

CVOID__PROTO(finish_profile) {
  /* TODO: use a different flag */
#if defined(ABSMACH_OPT__profile_calls)
  if (profile) dump_profile();
#endif
#if defined(PROFILE_EVENTFREQ)
  eventfreq__dump();
#endif
#if defined(PROFILE_STATS)  
  (void)CBOOL__SUCCEED(statistics);
#endif
#if defined(PROFILE_BLOCKFREQ)
  blockfreq__end();
#endif
#if defined(USE_TRACE_OUTPUT)
  if (stream_trace_file != stderr) fclose(stream_trace_file);
#endif
}

#define Yb(I) (*CharOffset(E,(I))) /* I as bytecode operand */
#define Y(I) (E->x[(I)]) /* I as zero-based */

/***************************************************************************/ 
/* Bytecode disassembler */

#if defined(USE_DISASSEMBLER)
CFUN__PROTO_N(dump_instr, bcp_t, bcp_t p, bcp_t begin, FILE *out);
CVOID__PROTO_N(display_term, tagged_t term, stream_node_t *stream, bool_t quoted);

CBOOL__PROTO(disasm) {
  bcp_t p;
  bcp_t begin;
  bcp_t end;
  FILE *out;
  emul_info_t *ref;

  out = Output_Stream_Ptr->streamfile;

  DEREF(X(0),X(0)); /* Bytecode object */
  ref = TaggedToEmul(X(0));
  p = ref->emulcode;
  begin = p;
  end = BCoff(ref, ref->objsize);
  p = BCoff(p, FTYPE_size(f_i));

  fprintf(out, "  bytecode - 0x%lx bytes - start at 0x%08lx, det at 0x%08lx\n",
	  (long)((char *)end - (char *)p),
	  (long)FTYPE_size(f_i),
	  (long)BCOp(ref->emulcode, FTYPE_ctype(f_i), 0));
  while (p != NULL && p < end) {
    p = CFUN__EVAL_N(dump_instr, p, begin, out);
  }

  CBOOL__PROCEED;
}

void disasm__z(FILE *out, intmach_t j) {
  if (j&1) {
    fprintf(out, "(unsafe)%ld", (long)Y_OffsetToIndex(j+1));
  } else {
    fprintf(out, "%ld", (long)Y_OffsetToIndex(j));
  }
}

/* todo[ts]: make optional (only when alignment is required) */
void check_align(FILE *out, bcp_t p, intmach_t size) {
  /* TODO: move to some code verifier... this indicates a compiler bug! */
  if (((intp_t)p) % size != 0) { 
    fprintf(out, "!UNALIGNED!");
  }
}

#define FTYPE_name(ID) ftype_name[(intmach_t)(ID)]
char *ftype_name[] = {
  [(intmach_t)FTYPE_id(f_o)]="o",
  [(intmach_t)FTYPE_id(f_e)]="e",
  [(intmach_t)FTYPE_id(f_f)]="f",
  [(intmach_t)FTYPE_id(f_i)]="i",
  [(intmach_t)FTYPE_id(f_l)]="l",
  [(intmach_t)FTYPE_id(f_g)]="g",
  [(intmach_t)FTYPE_id(f_p)]="p",
  [(intmach_t)FTYPE_id(f_t)]="t",
  [(intmach_t)FTYPE_id(f_x)]="x",
  [(intmach_t)FTYPE_id(f_y)]="y",
  [(intmach_t)FTYPE_id(f_z)]="z",
  [(intmach_t)FTYPE_id(f_C)]="C",
  [(intmach_t)FTYPE_id(f_E)]="E",
  [(intmach_t)FTYPE_id(f_Q)]="Q",
  [(intmach_t)FTYPE_id(f_Y)]="Y",
  [(intmach_t)FTYPE_id(f_Z)]="Z",
  [(intmach_t)FTYPE_id(f_b)]="b"
};

/* TODO: O(n) complexity! build a reverse hash table for builtins and functions? */
tagged_t reverse_ptr_lookup(hashtab_t *sw, char *ptr) {
  HASHTAB_ITERATE(sw, n, {
    if (n->value.proc == ptr) {
      return n->key;
    }
  });
  return ERRORTAG;
}

CFUN__PROTO_N(dump_instr, bcp_t, bcp_t p, bcp_t begin, FILE *out) {
  const char *name;
  const ftype_typeid_t *format;
  definition_t *d;
  tagged_t t;
  intmach_t i, j, k;
  intmach_t arity;
  FTYPE_ctype(f_o) opcode;

  fprintf(out, "  0x%08lx: ", (long)(p - begin));
  check_align(out, p, FTYPE_size(f_o));
  opcode = BCOp(p, FTYPE_ctype(f_o), 0);
#if defined(ABSMACH_OPT__threaded_bytecode_rel16)||defined(ABSMACH_OPT__threaded_bytecode)
  fprintf(stderr, "threaded bytecode not working at line %d\n", __LINE__);
#endif
  p = BCoff(p, FTYPE_size(f_o));
  if (opcode >= INS_OPCOUNT) {
    fprintf(out, "? [?] invalid opcode 0x%lx\n", (long)opcode);
    CFUN__PROCEED(NULL);
  }
  
  name = ins_name[opcode];
  format = FTYPE_str__args(abscurr.ins_info, opcode);
  arity = FTYPE_str__arity(abscurr.ins_info, opcode);
  
  fprintf(out, "%s", name);
  
  for (i = 0; i < arity; i++) {
    fprintf(out, " %s(", FTYPE_name(format[i]));
    switch (format[i]) {
    case FTYPE_id(f_e):
      check_align(out, p, FTYPE_size(f_e));
      fprintf(out, "%ld", (long)BCOp(p, FTYPE_ctype(f_e), 0)); p = BCoff(p, FTYPE_size(f_e)); break;
    case FTYPE_id(f_f):
      check_align(out, p, FTYPE_size(f_f));
      t = BCOp(p, FTYPE_ctype(f_f), 0); fprintf(out, "%s/%ld", GetString(t), (long)Arity(t)); p = BCoff(p, FTYPE_size(f_f)); break;
    case FTYPE_id(f_i):
      check_align(out, p, FTYPE_size(f_i));
      fprintf(out, "%ld", (long)BCOp(p, FTYPE_ctype(f_i), 0)); p = BCoff(p, FTYPE_size(f_i)); break;
    case FTYPE_id(f_l):
      check_align(out, p, FTYPE_size(f_l));
      fprintf(out, "%ld", (long)(intmach_t)BCOp(p, FTYPE_ctype(f_l), 0)); p = BCoff(p, FTYPE_size(f_l)); break;
    case FTYPE_id(f_g):
      /* TODO: expand in two sub-formats! use compound definitions! */
      fprintf(out, "h:");
      check_align(out, p, FTYPE_size(f_l)); 
      fprintf(out, "%ld", (long)BCOp(p, FTYPE_ctype(f_l), 0)); p = BCoff(p, FTYPE_size(f_l));
      fprintf(out, ",a:");
      check_align(out, p, FTYPE_size(f_i));
      fprintf(out, "%ld", (long)BCOp(p, FTYPE_ctype(f_i), 0)); p = BCoff(p, FTYPE_size(f_i));
      break;
    case FTYPE_id(f_p):
      check_align(out, p, FTYPE_size(f_p));
      fprintf(out, "0x%lx", (long)(BCOp(p, FTYPE_ctype(f_p), 0) - begin)); p = BCoff(p, FTYPE_size(f_p)); break;
    case FTYPE_id(f_t):
      check_align(out, p, FTYPE_size(f_t));
      t = BCOp(p, FTYPE_ctype(f_t), 0); CVOID__CALL_N(display_term, t, Output_Stream_Ptr, TRUE); p = BCoff(p, FTYPE_size(f_t)); break;
    case FTYPE_id(f_x):
      check_align(out, p, FTYPE_size(f_x));
      fprintf(out, "%ld", (long)X_OffsetToIndex(BCOp(p, FTYPE_ctype(f_x), 0))); p = BCoff(p, FTYPE_size(f_x)); break;
    case FTYPE_id(f_y):
      check_align(out, p, FTYPE_size(f_y));
      fprintf(out, "%ld", (long)Y_OffsetToIndex(BCOp(p, FTYPE_ctype(f_y), 0))); p = BCoff(p, FTYPE_size(f_y)); break;
    case FTYPE_id(f_z):
      check_align(out, p, FTYPE_size(f_z));
      disasm__z(out, BCOp(p, FTYPE_ctype(f_z), 0)); p = BCoff(p, FTYPE_size(f_z)); break;
    case FTYPE_id(f_C):
      check_align(out, p, FTYPE_size(f_C));
      {
	tagged_t t;
	char *ptr;
	ptr = BCOp(p, FTYPE_ctype(f_C), 0);
	t = reverse_ptr_lookup(switch_on_builtin, ptr);
	if (t != ERRORTAG) goto c_resolved;
	t = reverse_ptr_lookup(switch_on_function, ptr);
	if (t != ERRORTAG) goto c_resolved;
	fprintf(out, "!UNKNOWN!");
	goto c_resolved_end;
      c_resolved:
	fprintf(out, "%s/%ld", GetString(t), (long)Arity(t));
      }
    c_resolved_end:
      p = BCoff(p, FTYPE_size(f_C));
      break;
    case FTYPE_id(f_E):
      check_align(out, p, FTYPE_size(f_E));
      d = BCOp(p, FTYPE_ctype(f_E), 0); fprintf(out, "%s/%ld", GetString(FuncName(d)), (long)FuncArity(d)); p = BCoff(p, FTYPE_size(f_E)); break;
    case FTYPE_id(f_Q):
      check_align(out, p, FTYPE_size(f_Q));
      p = BCoff(p, FTYPE_size(f_Q)); break;
    case FTYPE_id(f_Y):
      check_align(out, p, FTYPE_size(f_i));
      k = BCOp(p, FTYPE_ctype(f_i), 0); p = BCoff(p, FTYPE_size(f_i));
      for (j = 0; j < k; j++) {
	if (j > 0) fprintf(out, " ");
	check_align(out, p, FTYPE_size(f_y));
	fprintf(out, "%ld", (long)Y_OffsetToIndex(BCOp(p, FTYPE_ctype(f_y), 0))); p = BCoff(p, FTYPE_size(f_y));
      }
      break;
    case FTYPE_id(f_Z):
      check_align(out, p, FTYPE_size(f_i));
      k = BCOp(p, FTYPE_ctype(f_i), 0); p = BCoff(p, FTYPE_size(f_i));
      for (j = 0; j < k; j++) {
	if (j > 0) fprintf(out, " ");
	check_align(out, p, FTYPE_size(f_z));
	disasm__z(out, BCOp(p, FTYPE_ctype(f_z), 0)); p = BCoff(p, FTYPE_size(f_z));
      }
      break;
    case FTYPE_id(f_b):
      /* todo[ts]: change if more blob types are added */
      check_align(out, p, FTYPE_size(f_i));
      t = MakeBlob((tagged_t *)p); CVOID__CALL_N(display_term, t, Output_Stream_Ptr, TRUE); p = BCoff(p, BlobFunctorSizeAligned(*(tagged_t *)p)+sizeof(functor_t)); break;
    default:
      fprintf(out, "?");
      goto end;
    }
    fprintf(out, ")");
  }
 end:
  fprintf(out, "\n");
  CFUN__PROCEED(p);
}
#endif

#define USE_DECTOK 1
#if defined(USE_DECTOK)

/* TODO: incomplete, it should be the reverse function of
   make_bytecode_object */
CFUN__PROTO_N(dectok_instr, bcp_t, bcp_t p, bcp_t begin, FILE *out);

CBOOL__PROTO(dectok) {
  bcp_t p;
  bcp_t begin;
  bcp_t end;
  FILE *out;
  emul_info_t *ref;

  out = Output_Stream_Ptr->streamfile;

  DEREF(X(0),X(0)); /* Bytecode object */
  ref = TaggedToEmul(X(0));
  p = ref->emulcode;
  end = BCoff(ref, ref->objsize);
  p = BCoff(p, FTYPE_size(f_i));
  begin = p;

  /* bytecode(Size, Opcodes). */
  fprintf(out, "bytecode(0x%lx, [", (long)((char *)end - (char *)p));
  if (p != NULL && p < end) {
    p = CFUN__EVAL_N(dectok_instr, p, begin, out);
  }
  while (p != NULL && p < end) {
    fprintf(out, ",");
    p = CFUN__EVAL_N(dectok_instr, p, begin, out);
  }
  fprintf(out, "]).\n");

  CBOOL__PROCEED;
}

CFUN__PROTO_N(dectok_instr, bcp_t, bcp_t p, bcp_t begin, FILE *out) {
  const char *name MAYBE_UNUSED;
  const ftype_typeid_t *format;
  intmach_t i;
  intmach_t arity;
  FTYPE_ctype(f_o) opcode;

  opcode = BCOp(p, FTYPE_ctype(f_o), 0);
#if defined(ABSMACH_OPT__threaded_bytecode_rel16)||defined(ABSMACH_OPT__threaded_bytecode)
  fprintf(stderr, "threaded bytecode not working at line %d\n", __LINE__);
#endif
  p = BCoff(p, FTYPE_size(f_o));
  
  if (opcode >= INS_OPCOUNT) {
    fprintf(out, "? [?] invalid opcode 0x%lx\n", (long)opcode);
    CFUN__PROCEED(NULL);
  }
  fprintf(out, "%ld", (long)opcode);
  
  name = ins_name[opcode];
  format = FTYPE_str__args(abscurr.ins_info, opcode);
  arity = FTYPE_str__arity(abscurr.ins_info, opcode);
  
  for (i = 0; i < arity; i++) {
    switch (format[i]) {
    case FTYPE_id(f_e): p = BCoff(p, FTYPE_size(f_e)); break;
    case FTYPE_id(f_f): p = BCoff(p, FTYPE_size(f_f)); break;
    case FTYPE_id(f_i): p = BCoff(p, FTYPE_size(f_i)); break;
    case FTYPE_id(f_l): p = BCoff(p, FTYPE_size(f_l)); break;
    case FTYPE_id(f_g): p = BCoff(p, FTYPE_size(f_l) + FTYPE_size(f_i)); break; /* TODO: use compound operands */
    case FTYPE_id(f_p): p = BCoff(p, FTYPE_size(f_p)); break;
    case FTYPE_id(f_t): p = BCoff(p, FTYPE_size(f_t)); break;
    case FTYPE_id(f_x): p = BCoff(p, FTYPE_size(f_x)); break;
    case FTYPE_id(f_y): p = BCoff(p, FTYPE_size(f_y)); break;
    case FTYPE_id(f_z): p = BCoff(p, FTYPE_size(f_z)); break;
    case FTYPE_id(f_C): p = BCoff(p, FTYPE_size(f_C)); break;
    case FTYPE_id(f_E): p = BCoff(p, FTYPE_size(f_E)); break;
    case FTYPE_id(f_Q): p = BCoff(p, FTYPE_size(f_Q)); break;
    case FTYPE_id(f_Y): { intmach_t k = BCOp(p, FTYPE_ctype(f_i), 0); p = BCoff(p, FTYPE_size(f_i) + k * FTYPE_size(f_y)); } break;
    case FTYPE_id(f_Z): { intmach_t k = BCOp(p, FTYPE_ctype(f_i), 0); p = BCoff(p, FTYPE_size(f_i) + k * FTYPE_size(f_z)); } break;
    case FTYPE_id(f_b): p = BCoff(p, BlobFunctorSizeAligned(*(tagged_t *)p)+sizeof(functor_t)); break;
    default:
      goto end;
    }
  }
 end:
  CFUN__PROCEED(p);
}
#endif

#if defined(ABSMACH_OPT__mem_profile)
intmach_t hwm_global = 0;
intmach_t hwm_local = 0;
intmach_t hwm_trail = 0;
intmach_t hwm_choice = 0;
CVOID__PROTO(memprofile) {
#define MAX(A,B) ((A)>(B)?(A):(B))
  hwm_global = MAX(hwm_global, HeapCharDifference(Heap_Start,G->heap_top));
  frame_t *newa;
  GetFrameTop(newa,w->choice,G->frame);
  hwm_local = MAX(hwm_local, StackCharUsed(newa));
  hwm_trail = MAX(hwm_trail, TrailCharDifference(Trail_Start,G->trail_top));
  hwm_choice = MAX(hwm_choice, ChoiceCharDifference(Choice_Start,w->choice));
}
#endif
