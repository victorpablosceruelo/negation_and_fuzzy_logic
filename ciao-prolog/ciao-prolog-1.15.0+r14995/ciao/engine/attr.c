/*****************************************************************
 *  DMCAI Clp 1.2                                                *
 *  (c) Copyright 1992                                           *
 *  Department of Medical Cybernetics & Artificial Intelligence  *
 *  University of Vienna                                         *
 *  Freyung 6                                                    *
 *  A-1010 Vienna, Austria                                       * 
 *                                                               *
 *  Permission to use this software for any purpose is subject   *
 *  to the USER AGREEMENT between the DMCAI and the User.        *
 *                                                               *
 *  File: clp.c                                                  *
 *  Author: Christian Holzbaur [christian@ai.univie.ac.at]       *
 *****************************************************************/


#include "datadefs.h"
#include "support.h"
#include "predtyp.h"

/* declarations for global functions accessed here */

#include "start_defs.h"
#include "attr_defs.h"
#include "stacks_defs.h"

/* local declarations */


    
extern definition_t *address_true;                         /* Shared */

tagged_t
  atm_var, atm_attv, atm_float, atm_int, atm_str, atm_atm, atm_lst;/* Shared */

tagged_t fu1_type(Arg,t0)
     Argdecl;
     CIAO_REGISTER tagged_t t0;
{
  DEREF(t0,t0);
  switch (TagOf(t0)) {
    case UBV:
    case SVA:
    case HVA:
      return atm_var;
    case CVA:
      return atm_attv;
    case STR:
      if (STRIsLarge(t0)) 
        return LargeIsFloat(t0) ? atm_float : atm_int;
      return atm_str;
    case ATM:
      return atm_atm;
    case LST:
      return atm_lst;
    case NUM:
      return atm_int;
  }
  return (tagged_t)NULL;                                  /* avoid warnings */
} 

/* ------------------------------------------------------------------------ */
 
tagged_t fu1_get_attribute(Arg,x)
     Argdecl;
     CIAO_REGISTER tagged_t x;
{  
  CIAO_REGISTER tagged_t t;

  DerefSwitch(x,t,{
    if ( VarIsCVA(x) ) { 
      return CTagToGoal(x);
    }
  }); 
  return ERRORTAG;				                  /* fail */
}



/* 
   suspend a goal on a variable. Stolen from suspend_goal in misc.c .
*/


bool_t bu2_attach_attribute(Arg,var,constr) 
     Argdecl;
     CIAO_REGISTER tagged_t var,constr;
{  
  CIAO_REGISTER tagged_t t0;
  CIAO_REGISTER tagged_t *h = w->global_top;
  CIAO_REGISTER tagged_t *tr = w->trail_top;
        
  DerefSwitch(constr,t0,{USAGE_FAULT("attach_attribute/2: type error");}); 
  DEREF(var,var);
  if (TagIsHVA(var)) {
    LoadCVA(t0,h);
    if (CondHVA(var))	{
      TrailPush(tr,var);
      BindingOfHVA(var) = t0;
    }
    else CTagToHVA(var) = t0;
  } 
  else
    if (TagIsSVA(var)) { 			          /* unsafe value */
      CIAO_REGISTER tagged_t *ptr = h;
      LoadHVA(t0,ptr);
      h = ptr;
      BindSVA(var,t0);
      var = t0;
      LoadCVA(t0,h);
      CTagToHVA(var) = t0;
    } else 
      USAGE_FAULT("attach_attribute/2: type error");
  
  HeapPush(h,constr);
  HeapPush(h,PointerToTerm(address_true));	                  /* func */
  
  w->global_top = h;
  w->trail_top = tr;
  if (ChoiceYounger(w->node,TrailOffset(tr,CHOICEPAD)))
    choice_overflow(Arg,CHOICEPAD); 
  return TRUE;
}  


                             /* a la defrost */

bool_t bu1_detach_attribute(Arg,x)
     Argdecl;
     CIAO_REGISTER tagged_t x;
{ 
  CIAO_REGISTER tagged_t t; 
  CIAO_REGISTER tagged_t *h = w->global_top;
  
  DerefSwitch(x,t,{
    if ( VarIsCVA(x) ) {
      LoadHVA(t,h);
      BindCVA(x,t);  				               /* trailed */
      w->global_top = h;
      return TRUE;
    }
  });
  USAGE_FAULT("detach_attribute/2: type error");
}  
 


                /* think about optimizations a la setarg */

bool_t bu2_update_attribute(Arg,x,constr)
     Argdecl;
     CIAO_REGISTER tagged_t x,constr;
{ 
  CIAO_REGISTER tagged_t t;
  CIAO_REGISTER tagged_t *h = w->global_top;
              
  DerefSwitch(constr,t,{USAGE_FAULT("update_attribute/2: type error");}); 
  DerefSwitch(x,t,{
    if ( VarIsCVA(x) ) { 
      LoadCVA(t,h); 
      HeapPush(h,constr);
      HeapPush(h,PointerToTerm(address_true));	                  /* func */
      BindCVA(x,t); 				               /* trailed */
      w->global_top = h;
      return TRUE;
    }
  }); 
  USAGE_FAULT("update_attribute/2: type error"); 
}  
 

/*  
   Called from wam.c
   Collect all constraints that have been woken "recently" by
   scanning the newest trail segment.  Also, excise such entries
   belonging to the newest heap segment.  
   Each pending unification pushes 4 heap elems - cf enter_predicate: wam.c
*/

void collect_pending_unifications(Arg,wake_count)
     Argdecl;
     int wake_count;
{
  int sofar=0;
  CIAO_REGISTER tagged_t *tr = w->trail_top;
  CIAO_REGISTER tagged_t *h = w->global_top;
  tagged_t *tr0 = NULL;
  tagged_t *limit = TagToPointer(w->node->trail_top);  
   
  X(0) = atom_nil;
  while (sofar<wake_count && TrailYounger(tr,limit))  {
    tagged_t ref, value;
    
    ref = TrailPop(tr);
    if (!TagIsCVA(ref))
      continue;
    RefCVA(value,ref); 
    if (value==ref) { 
      SERIOUS_FAULT("wake - unable to find all goals");  
    }
    
    sofar++; 
    CTagToPointer(ref) = ref;     		               /* untrail */
    
    HeapPush( h, ref); 
    HeapPush( h, value);  
    HeapPush( h, Tag(LST,h-2));
    HeapPush( h, X(0));
    X(0) = Tag(LST,h-2);
    
    if ( !CondCVA(ref))	
      tr0=tr, *tr=0; 
  }
  w->global_top = h;
  Heap_Warn_Soft = Heap_Start;			     /* make WakeCount==0 */
  
  if (sofar<wake_count) {
    SERIOUS_FAULT("wake - unable to find all goals");
  }
  
                                                /* now compress the trail */
  
  if (tr0) {
    h = tr = tr0;
    while (TrailYounger(w->trail_top,tr)){
      tagged_t ref;
      
      if ((ref = TrailNext(tr)))
        TrailPush(h,ref);
    }
    w->trail_top = h;
  }
}                  

void collect_one_pending_unification(Arg)
     Argdecl;
{
  int sofar=0;
  CIAO_REGISTER tagged_t *tr = w->trail_top;
  tagged_t *tr0 = NULL;
  tagged_t *limit = TagToPointer(w->node->trail_top);  
  
  while ( !sofar && TrailYounger(tr,limit)) {
    tagged_t ref, value;
    
    ref = TrailPop(tr);
    if (!TagIsCVA(ref))
      continue;
    RefCVA(value,ref); 
    if (value==ref) { 
      SERIOUS_FAULT("wake - unable to find all goals");  
    }
    
    sofar++; 
    CTagToPointer(ref) = ref;     		               /* untrail */
    
    /*   X(0) = CTagToGoal(ref);*/
    X(0) = ref ;
    X(1) = value;
    
    if ( !CondCVA(ref))	
      tr0=tr, *tr=0; 
  }
  Heap_Warn_Soft = Heap_Start;			     /* make WakeCount==0 */
  
  if ( !sofar ) {
    SERIOUS_FAULT("wake - unable to find all goals");
  }
  
                                                /* now compress the trail */
  
  if (tr0) {
    tagged_t *h = tr = tr0;
    while (TrailYounger(w->trail_top,tr))
      {
        tagged_t ref;
        
        if ((ref = TrailNext(tr)))
          TrailPush(h,ref);
      }
    w->trail_top = h;
  }
}


