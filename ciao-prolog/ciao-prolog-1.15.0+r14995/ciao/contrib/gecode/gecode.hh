/////////////////////////////////////////
//// C++ Heads //////////////////////////
/////////////////////////////////////////

#include <iostream> 
#include <cstdlib>

/////////////////////////////////////////
//// GECODE /////////////////////////////
/////////////////////////////////////////

#include "/usr/local/include/gecode/driver.hh"
// #include <gecode/int.hh>
// #include <gecode/minimodel.hh>

/////////////////////////////////////////
//// CIAO ENGINE ////////////////////////
/////////////////////////////////////////


extern "C" 
{
  namespace Ciao 
  {
#include <ciao_prolog.h>
#include <termdefs.h>
#include <nondet_defs.h>
#include "../tabling/chat_tabling.h"
  }
#include <stdlib.h> 
}

using namespace Ciao;

#define EMPTY_LIST 0xA00000F0
#define MkInt(INT) (ensure_make_integer(w,(INT)))
#define MkList(HEAD,TAIL) (ensure_make_list(w,(HEAD),(TAIL)))
#define HeadOfTerm(TERM) (CTagToPointer(TERM))
#define TailOfTerm(TERM) (*(TagToPointer(TERM) + 1))
#define ArgOfTerm(A,TERM) (CTagToArg(TERM,A))
#define NameOfFunctor(FUNCTOR) (((atom_t *)TagToAtom(SetArity(TagToHeadfunctor(FUNCTOR),0)))->name)
#define ArityOfFunctor(FUNCTOR)(Arity(TagToHeadfunctor(FUNCTOR)))
#define IsApplTerm(TERM) (TagIsSTR(TERM) && !IsNumber(TERM))

tagged_t ensure_make_integer(worker_t *w, int i) 
{
  ciao_ensure_heap(w->misc->goal_desc_ptr, 4);  
  return MakeInteger(w, i);
}

tagged_t ensure_make_list(worker_t *w, tagged_t head, tagged_t tail) 
{
  tagged_t list;
  ciao_ensure_heap(w->misc->goal_desc_ptr, 3);
  MakeLST(list, head, tail);
  return list;
}

/////////////////////////////////////////
//// DEFINES ////////////////////////////
/////////////////////////////////////////

#define	TRUE		1
#define	FALSE		0
#define INI_SIZE_G      1
#define FACTOR_G        2
#define MAX             10000//INT_MAX

/////////////////////////////////////////
//// GLOBAL_VARS ////////////////////////
/////////////////////////////////////////

node_t *last_cp;
try_node_t *address_gecode_labeling_nd_cc;

#define Unify(TERM1,TERM2) (cunify(Arg,(TERM1),(TERM2)))
