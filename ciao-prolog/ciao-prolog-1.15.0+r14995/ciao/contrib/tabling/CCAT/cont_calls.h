
#include "datadefs.h"
#include "support.h"
#include "task_areas.h"
#include "wam.h"

#include "engine.h"
#include "tries.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --------------------------- */
/*           Defines           */
/* --------------------------- */

#define	TRUE		1
#define	FALSE		0
#define READY 0
#define EVALUATING 1
#define COMPLETE 2
#define MEMSIZE         512*2*2*2*4*4*4

#define TABLING_GLOBALSTKSIZE  (4800*kCells-1) 
#define TABLING_LOCALSTKSIZE   (3000*kCells-1)
#define TABLING_CHOICESTKSIZE  (3000*kCells-1)
#define TABLING_TRAILSTKSIZE   (3000*kCells-1)

struct EACH_CALL *sid_stack[2048];
int isid_stack;

long memory[MEMSIZE*1024];
long *memory_free = &memory[0];
long *memory_end = &memory[MEMSIZE*1024];

TrNode node_top = NULL;
struct EACH_CALL *TOP_SF=NULL;
try_node_t *address_nd_consume_answer_c;
try_node_t *address_nd_resume_cons_c;
try_node_t *address_nd_prueba_c;

/* --------------------------- */
/*           Macros            */
/* --------------------------- */
#define CHECK_MEM { \
    if (memory_free > memory_end) {\
      panic("memory exhausted - exiting\n");}}



#define DELE_AND_FREE_WAITS(Xwaits)	\
	{ if (Xwaits) { \
		Xwaits = NULL; } }


#define SETMIN(x, y) if (y < x) x = y;

/* --------------------------- */
/*           Structs           */
/* --------------------------- */

struct untrail_list 
{
  ENGINE_Term value;
  struct untrail_list* next;
};

//struct answer_list{
//  struct subs_factor* sub_fact;
//  struct answer_list* sig;
//};

struct consumer{
  struct subs_factor* sub_fact;
  struct subs_factor* sub_factPARENT;
  frame_t *frame;
  bcp_t next_insn;
  trie_node_t *last_answer;
  struct EACH_CALL *parent_id;
  struct consumer *sig;
};

struct EACH_CALL {

  long dfn, plink;                //to implement completion algorithm 
  long comp;                      //state of the call

  node_t *node;

  struct subs_factor* sub_fact;   //substitution factor of the generator.
//  struct answer_list* answers;  //substitution factor of the answers (frozen on the heap).

  struct consumer* cons;          //list of consumers

  trie_node_t *trie_answers; //To check for repetitions.

  trie_node_t *first_answer; 
  trie_node_t *last_answer;

  frame_t *frame;

  struct EACH_CALL *previous;
};


/* --------------------------- */
/*             API             */
/* --------------------------- */


void panic (char *what);
inline void *mem_alloc(int size);

ENGINE_Term my_make_var(goal_descriptor_t *state)
{
  ciao_ensure_heap(state, 1);
  ENGINE_Term resul = TagHVA(GTOP);
  HeapPush(GTOP, resul);
  return resul;
}			      

ENGINE_Term my_make_integer(goal_descriptor_t *state, int i) 
{
  ciao_ensure_heap(state, 4);  //change to in my_heap
  return MakeInteger(REGISTERS, i);
}

ENGINE_Term my_make_float(goal_descriptor_t *state, double f) 
{
  ciao_ensure_heap(state, 4);
  return MakeFloat(REGISTERS, f);
}

ENGINE_Term my_make_list(goal_descriptor_t *state, ENGINE_Term head, ENGINE_Term tail) 
{
  ENGINE_Term list;
  ciao_ensure_heap(state, 3);
  worker_t * w = REGISTERS;
  MakeLST(list, head, tail);
  return list;
}

ENGINE_Term my_make_functor(goal_descriptor_t *state, char* name, int arity, ENGINE_Term *args) 
{
  worker_t * w = REGISTERS;
  if (arity == 0) return MakeString((char *)name);
  else if (strcmp(name, ".") == 0 && arity == 2) 
    {
      ENGINE_Term list;
      ciao_ensure_heap(state, 3);
      MakeLST(list, args[0], args[1]);
      return list;
    } 
  else 
    {
      int i;
      ciao_ensure_heap(state, 2 + arity);
      HeapPush(P_GTOP, SetArity(MakeString((char *)name), arity));
      for (i = 0; i < arity; i++) HeapPush(P_GTOP, args[i]);
      return Tag(STR, HeapOffset(P_GTOP, -(arity+1)));
    }
}
