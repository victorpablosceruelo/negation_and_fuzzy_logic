#include <engine/basiccontrol.native.h>

CBOOL__PROTO(retry_cut) {
  tagged_t number;
  choice_t *nd;

  DEREF(X(0),X(0));
  CBOOL__TEST(TaggedIsSmall(X(0)));
  for (nd = w->choice;
       ChoiceYounger(nd,Choice_Start);
       nd = ChoiceCont(nd)) {
    DEREF(number,nd->x[0]);
    if (nd->x[3]==atom_retry_hook && number<=X(0)) {
      nd->x[1] = X(1);	/* always dereferenced */
      SetChoice(nd);
      break;
    }
  }
  CBOOL__LASTTEST(ChoiceYounger(nd,Choice_Start));
}

CBOOL__PROTO(spypoint) {
  tagged_t *junk;
  definition_t *func;
  
  DEREF(X(0),X(0));
  func = find_definition(X(0),&junk,FALSE);
  CBOOL__TEST(func != NULL);

  CBOOL__UnifyCons((func->properties.spy ? atom_on : atom_off), X(1));

  DEREF(X(2),X(2));
  func->properties.spy = (X(2)==atom_on);

  /* TODO: change enter type? */
  /* SetEnterInstr(func,func->predtyp); */
  CBOOL__PROCEED;
}

CBOOL__PROTO(set_debugger_mode) {
#if defined(DEBUG)
  if (debug_gc) TRACE_PRINTF("Thread %d is changing debbuger mode\n", (int)Thread_Id);
#endif
  DEREF(Current_Debugger_Mode,X(0));
  if (Current_Debugger_Mode != atom_off) {
    //SetCIntEvent();
    debug_mode = TRUE;
    debug_status = 2;
  } else {
    /* TODO: does it interfere with C-c? */
    UnsetCIntEvent();
    debug_mode = FALSE;
    debug_status = 0;
  }
  CBOOL__PROCEED;
}

CINSNP__PROTO(code_call1);

CVOID__PROTO_N(display_term, tagged_t term, stream_node_t *stream, bool_t quoted);

CINSNP__PROTO(code_notracecall1) {
#if defined(USE_PROLOG_DEBUGGER)
  if (debug_mode) {
    //TRACE_PRINTF("dc_(");
    //CVOID__CALL_N(display_term, X(0), stream_trace, TRUE);
    //TRACE_PRINTF(")\n");
    /* enable tracing for next goal */
    debug_status = 1;
    SetCIntEvent();
  }
#endif
  CINSNP__LASTCALL(code_call1);
}

CBOOL__PROTO(code_start_trace) {
#if defined(USE_PROLOG_DEBUGGER)
  if (debug_mode) {
    SetCIntEvent();
    debug_status = 2;
  }
#endif
  CBOOL__PROCEED;
}

CBOOL__PROTO(code_stop_trace) {
#if defined(USE_PROLOG_DEBUGGER)
  if (debug_mode) {
    UnsetCIntEvent();
    debug_status = 0;
  }
#endif
  CBOOL__PROCEED;
}

