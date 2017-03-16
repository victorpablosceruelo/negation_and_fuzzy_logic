
/* kernel primitives for concurrency */

#include "datadefs.h"
#include "instrdefs.h"
#include "support.h"
#include "wam.h"                                   /* Several macros here */
#include "predtyp.h"
#include "kprim.h"
#include <stdio.h>


#if defined(MARKERS)

extern definition_t *address_call;                   /* In initial.h */
extern sw_on_key_t **predicates_location;            /* In initial.h */
extern int wam();

insn_t markercode[20];                                     /* Enough space? */

/* | CALLQ | 0 | address_call | padding | FrameSize | ExitToplevel | */

void init_markercode()
{
  bcp_t b = markercode;

  *b++ = CALLQ;
  *b++ = 0; 
  *(definition_t **)b = address_call;
  b += BPTP;		                               /* padding */
  *b++ = EToY0*sizeof(tagged_t);	/* initial FrameSize -- is this size right? */
  *b++ = EXIT_TOPLEVEL;          /* Here, direct code to a closing marker */
                /* i.e., CLOSE_GOAL or whatever, instead of EXIT_TOPLEVEL */
}


#if defined(zero)        /* This was working; I just want to try sth else */
bool_t prolog_launch_goal(Arg)
     Argdecl;
{
  int return_code;
  bcp_t saved_insn;

                                      /* Taken from BUILTIN_CALL in wam.c */
#if defined(REENTRANT_WAM)
  DEREF(X(0),X(0));                                  /* First X(0) was t0 */
  /* 
     Setfunc(find_definition(predicates_location,t0,&w->structure,FALSE)); 
     if (Func==NULL) return FALSE;
  */

 /* We cannot use push_frame/pop_frame because we do not have the address of
   the code to be executed, only the predicate name and arguments */

  saved_insn = w->next_insn;                /* so we save this by hand... */
  w->next_insn = markercode;
  return_code = wam(w);
  printf("launch_goal returned with code %d\n", return_code);
  w->next_insn = saved_insn;             /* ...and  recover it afterwards */
  return TRUE;
#endif
}
#endif /* zero */

#if defined(zero)                                  /* This is not working */
bool_t prolog_launch_goal(Arg)
     Argdecl;
{
  int return_code;
  bcp_t saved_insn;
  bcp_t P;                           /* Implicitly used by several macros */
  int enter_type;
  tagged_t *pt1, *pt2, t0;

                                      /* Taken from BUILTIN_CALL in wam.c */
#if defined(REENTRANT_WAM)
  t0 = X(0); 
  DerefSwitch(t0,X(0),;);          /* Structure (predicate functor) in t0 */

                         /* This sets P to the address of the entry point */
  Setfunc(find_definition(predicates_location,t0,&w->structure,FALSE)); 
  if (Func == NULL) {
    fprintf(stderr, "Launching a non-existent predicate\n");
    return FALSE;
  }

  enter_type = Func->enter_instr;

  switch (enter_type){
  case ENTER_COMPACTCODE:
  case ENTER_PROFILEDCODE:
  case ENTER_COMPACTCODE_INDEXED:
  case ENTER_PROFILEDCODE_INDEXED:
             /* The arity of the predicate should be that of the functor  */
    t0 = Func->arity;
    fprintf(stderr, "Launching a %d-args goal\n", t0);
    if (t0){                       /* If there is any argument to load... */
      StoreH;
      pt1 = w->term;                                    /* X(i) registers */
      pt2 = w->structure;           /* Is this pointing to the arguments? */
      do
        PushRefHeapNext(pt1,pt2)
      while (--t0);
      LoadH;
    }
    break; 
  default:
    fprintf(stderr, 
   "I don't know what to do --- trying to launch a non-compiled predicate\n");
    return FALSE;
  }
  
  /*saved_insn = w->next_insn;*/ /* so we save this by hand... */
  /*w->next_insn = EXIT_TOPLEVEL;*/
  /*w->next_insn = markercode;*/
  /*return_code = wam(w);*/
  /*P_From_Worker = P;*/                         /* Pass local P to wam() */
  printf("launching goal\n");
  return_code = wam(Arg, enter_type);
  printf("launched goal returned with code %d\n", return_code);
 /*w->next_insn = saved_insn;*/          /* ...and  recover it afterwards */
  return TRUE;
#endif
}
#endif                                                            /* zero */


bool_t prolog_launch_goal(Arg)
     Argdecl;
{
  definition_t *func;
  int return_code;
  bcp_t saved_insn;
  CIAO_REGISTER try_node_t *alt;
  CIAO_REGISTER int n;
  CIAO_REGISTER node_t *b, *saved_node;
  int arity = 1;                                       /* that is, call/1 */
  CIAO_REGISTER tagged_t *pt1;                                 /* Just to hold E */
  tagged_t goal;

                                      /* Taken from BUILTIN_CALL in wam.c */
#if defined(REENTRANT_WAM)
  DEREF(X(0),X(0));                                  /* First X(0) was t0 */

  /* Setfunc(find_definition(predicates_location,t0,&w->structure,FALSE));
     if (Func==NULL) return FALSE; */

 /* We cannot use push_frame/pop_frame because we do not have (yet) the
    address of the code to be executed, only the predicate name and
    arguments */

             /* Create a marker with a failing alternative */

                         /* Give sensible values */
#ifdef UNDEFINED
  alt = fail_alt;
  n = alt->node_offset;
  saved_node = w->node;                        /* Save current choicepoint */
  b = ChoiceCharOffset(saved_node,n);                   /* New choicepoint */

                      /* Compute new WAM registers  */
  ComputeA(w->local_top,w->node); 
  w->node = b;                                    
  NewShadowregs(w->global_top);
  
                  /* Save values on to the choicepoint */

  b->trail_top = w->trail_top;
  SaveGtop(b,w->global_top);
  b->next_alt = w->next_alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  SaveLtop(b);
  
  w->next_alt = alt;
#endif /* UNDEFINED */


  /*  
  ComputeA(w->local_top,w->node); 
  SetE(w->local_top);
  E->next_insn = w->next_insn;
  E->frame = w->frame;
  w->frame = E;
  w->next_insn = markercode;                                       
  w->local_top = (frame_t *)Offset(E,EToY0+arity);
  {
    CIAO_REGISTER int i;

    for(i=0; i<arity; i++) Y(i) = X(i);
  }
*/


  /*
  n = OffsetToArity(n);
  while (n>0)
    ChoicePush(b0,X(--n));
  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
    choice_overflow(CHOICEPAD);
    */

  DEREF(goal, X(0));
  /*func = find_definition(&prolog_predicates,goal,&w->structure,FALSE);*/
  func = find_definition(predicates_location,goal,&w->structure,FALSE);

  if ((func->enter_instr != ENTER_COMPACTCODE) &&
      (func->enter_instr != ENTER_COMPACTCODE_INDEXED)) {
    printf("Launching a non-compiled predicate\n");
    return FALSE;
  }

  saved_insn = w->next_insn;
  w->next_insn = markercode;
  return_code = wam(w, ENTER_CODEARRAY);
  printf("launch_goal returned with code %d\n", return_code);
  w->next_insn = saved_insn;


  /* Do we have to pop it here? */

  /*w->next_insn = E->next_insn;*/


  /* exit_TOPLEVEL recovers E->frame and E->next_insn */

  /*
  w->frame = E->frame;
  w->next_insn = E->next_insn;
  */

  /*w->node = saved_node;*/

  /*
    We can either unwind the trail here, or let the next choicepoint 
    to do it.  Let's let the machine to do it for us... */

  /* 
  SetB(w->node);
  if (TrailYounger(pt2=w->trail_top,t1=(tagged_t)TagToPointer(B->trail_top))) {
    do
      PlainUntrail(pt2,
                   t0,
                   {printf("calling CONT code from launch_goal\n");}
                   )
      while (TrailYounger(pt2,t1));
      w->trail_top = pt2;
  }


  RestoreGtop(saved_node);
  w->next_alt = saved_node->next_alt;
  w->frame = saved_node->frame;
  w->next_insn = saved_node->next_insn;
  RestoreLtop(saved_node);
  SetShadowregs(w->node);
*/

  /*cunify(Arg, X(1), (tagged_t)MakeInteger((SP_INTEGER)b));*/

  return TRUE;
#endif  /* REENTRANT_WAM */
}



bool_t prolog_wait_for_goal(Arg)
     Argdecl;
{
  /*
    If goal is finished
      then continue, return success
    else
      launch goal;
      when finished, return here
      return either success or failure
  */

  return TRUE;
}


bool_t prolog_fail_goal(Arg)
     Argdecl;
{
  return TRUE;
}


bool_t prolog_redo_goal(Arg)
     Argdecl;
{
  return TRUE;
}

#endif  /* MARKERS */
