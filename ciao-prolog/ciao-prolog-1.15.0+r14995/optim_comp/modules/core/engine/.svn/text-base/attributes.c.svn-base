#include <engine/basiccontrol.native.h>

extern definition_t *address_true;

CFUN__PROTO_N(fu1_get_attribute, tagged_t, tagged_t x) {
  DerefSw_CVA_Other(x,{
    CFUN__PROCEED(*TaggedToGoal(x));
  }, {
    CFUN__PROCEED(ERRORTAG);
  });
}

#define ATTACH_BODY(SUSP) \
  DerefSw_HVAorCVAorSVA_Other(constr, { \
    USAGE_FAULT("attach_attribute/2: type error"); \
  }, { \
    tagged_t t0; \
    tagged_t *h = G->heap_top; \
    tagged_t *tr = G->trail_top; \
 \
    DEREF(var,var); \
    if (TaggedIsHVA(var)) { \
      LoadCVA(t0,h); \
      if (CondHVA(var)) TrailPush(tr,var); \
      *TagpPtr(HVA, var) = t0; \
    } else { \
      if (TaggedIsSVA(var)) { /* unsafe value */ \
	tagged_t *ptr = h; \
	LoadHVA(t0,ptr); \
	h = ptr; \
	BindSVA(var,t0); \
	var = t0; \
	LoadCVA(t0,h); \
	*TagpPtr(HVA, var) = t0; \
      } else {  \
	USAGE_FAULT("attach_attribute/2: type error"); \
      } \
    } \
   \
    HeapPush(h,constr); \
    HeapPush(h,SUSP);	                  /* func */ \
   \
    G->heap_top = h; \
    G->trail_top = tr; \
    TEST_CHOICE_OVERFLOW(w->choice, CHOICEPAD); \
    CBOOL__PROCEED; \
  }); 

CBOOL__PROTO_N(bu2_attach_attribute, tagged_t var, tagged_t constr) {  
  ATTACH_BODY(PointerToTerm(address_true));
}  

CBOOL__PROTO_N(bu2_attach_attribute_weak, tagged_t var, tagged_t constr) {  
  ATTACH_BODY(MakeSmall(0));
}  

/* a la defrost */

CBOOL__PROTO_N(bu1_detach_attribute, tagged_t x) { 
  DerefSw_CVA_Other(x,{
    tagged_t t; 
    tagged_t *h = G->heap_top;
    LoadHVA(t,h);
    BindCVANoWake(x,t); /* trailed */
    G->heap_top = h;
    CBOOL__PROCEED;
  }, {
    USAGE_FAULT("detach_attribute/2: type error");
  });
}  
 
/* TODO: think about optimizations a la setarg */

CBOOL__PROTO_N(bu2_update_attribute, tagged_t x, tagged_t constr) { 
  DerefSw_HVAorCVAorSVA_Other(constr,{
    USAGE_FAULT("update_attribute/2: type error");
  }, {
    DerefSw_CVA_Other(x,{
      tagged_t t;
      tagged_t *h = G->heap_top;
      LoadCVA(t,h); 
      HeapPush(h,constr);
      HeapPush(h,PointerToTerm(address_true)); /* func */
      BindCVANoWake(x,t); /* trailed */
      G->heap_top = h;
      CBOOL__PROCEED;
    }, {
      USAGE_FAULT("update_attribute/2: type error");
    });
  }); 
}  
