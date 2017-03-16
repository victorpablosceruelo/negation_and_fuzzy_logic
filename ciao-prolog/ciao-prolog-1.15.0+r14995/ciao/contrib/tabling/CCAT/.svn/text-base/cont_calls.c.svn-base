/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "ciao_prolog.h"
#include "cont_calls.h"
#include "tries.c"

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

tagged_t *ini = NULL;
long int totalTrail = 0;
long int minTrail = 0;
ENGINE_Term *array[TERM_STACK_SIZE];
int iarray;
frame_t *fromArgFrame[10000];
frame_t *fromArgNodeFrame[10000];

void show_term(ENGINE_Term t)
{
  DEREF(t,t);
  if (IsTrieVar(t)) printf("TRIE %d",TrieVarIndex(t)/2); 
  else if (ENGINE_IsVarTerm(t)) printf("%p",t);
  else if (ENGINE_IsAtomTerm(t)) 
    {
      if (t == EMPTY_LIST) printf("[]");
      else printf("%s",ENGINE_AtomName(t));
    }
  else if (ENGINE_IsIntTerm(t)) printf("%d",ENGINE_IntOfTerm(t));
  else if (ENGINE_IsFloatTerm(t)) printf("%f",ENGINE_FloatOfTerm(t));
  else if (ENGINE_IsPairTerm(t)) {
    printf("[");
    while (ENGINE_IsPairTerm(t))
      {
	show_term(ENGINE_HeadOfTerm(t));
	DEREF(t,ENGINE_TailOfTerm(t));
	if (ENGINE_IsPairTerm(t)) printf(",");
      }
    printf("]");
  } else if (ENGINE_IsApplTerm(t)) {
    if (!strcmp(ENGINE_NameOfFunctor(t),",") && ENGINE_ArityOfFunctor(t)  == 2) 
      {
	show_term(ENGINE_ArgOfTerm(1, t));
	printf(",");
	DEREF(t,ENGINE_ArgOfTerm(2, t));
	while (ENGINE_IsApplTerm(t) && 
	       !strcmp(ENGINE_NameOfFunctor(t),",") && 
	       ENGINE_ArityOfFunctor(t)  == 2);
	  {
	    show_term(ENGINE_ArgOfTerm(1, t));
	    printf(",");
	  } 
	    show_term(t);
      } 
    else 
      {
	int i;
	printf("%s(",ENGINE_NameOfFunctor(t));
	for (i = 1; i <= ENGINE_ArityOfFunctor(t); i++)
	  {
	    show_term(ENGINE_ArgOfTerm(i, t));
	    if (i < ENGINE_ArityOfFunctor(t)) printf(",");
	  }
	printf(")");
      }
  } else fprintf(stderr, "\nShowing: unknown type tag\n");
  
  return;
}

void show_sub_fact(struct subs_factor *sub_fact)
{
  int i;

  printf("\nMostrando el sub_fact %p\n",sub_fact);
  for (i = 0; i < sub_fact->size; i++)
    {
      printf("\nTermino %d = ",i+1);
      show_term(sub_fact->vars[i]);
      printf("\n");
    }
  return;
}

void show_frames(frame_t* ini, frame_t* fin, int tamFrame)
//  int tamFrame = (FrameSize(Arg->next_insn)/sizeof(tagged_t)) - EToY0; 
{
  int i;
  printf("\nMostrando frames\n");

  for (; ini != fin ; ini = ini->frame) 
    {
      //Translating pointers to heap top (lastFrame is updated)
      for (i = 0; i < tamFrame; i++) 
	{
	  printf("%p = ",&(ini->term[i]));
	  show_term(ini->term[i]);
	  printf("\n");
	}

      tamFrame = FrameSize(ini->next_insn)/sizeof(tagged_t) - EToY0;
      printf("\n-----------------------------\n");
    }

  for (i = 0; i < tamFrame; i++) 
    {
      printf("%p = ",&(ini->term[i]));
      show_term(ini->term[i]);
      printf("\n");
    }
  printf("\n-----------------------------\n");

  return;
}



void freeze_stacks(Argdecl, frame_t *topFrame) //warning about problematics stackyounger 
{
  Arg->global_uncond = TagHVA((unsigned int)Arg->global_top - MallocBase);
  //Last choice is already frozen
  if ((tagged_t*)&HeapFReg == Arg->node->global_top)
    {
      HeapFReg = Arg->global_top;
      StackFReg = topFrame;
      return;
    }

  //Updating new values
  HeapFReg = Arg->global_top;
  StackFReg = topFrame;

  //Updating pointers from not frozen choice points.
  node_t *ind;
  for (ind = Arg->node; 
       ind->global_top != (ENGINE_Term *)&HeapFReg;
       ind = ChoiceCharOffset(ind,-ind->next_alt->node_offset))
    {
      ind->global_top = (ENGINE_Term *)&HeapFReg;
      ind->local_top = (frame_t *)&StackFReg;      
    }
}

inline void make_ground_cell(Argdecl, ENGINE_Term *cell)
{
  ENGINE_Term t;
  ENGINE_Term resul;
  int i, arity;

  DEREF(t, *cell);
    if (TagIsLST(t))
    {
      *cell = Tag(LST, Arg->global_top);
      cell = TagToPointer(t);
      HeapPush(Arg->global_top, *cell);
      cell++;
      HeapPush(Arg->global_top, *cell);
      
    }
  else if (TagIsSTR(t) && !IsNumber(t))
    {
      *cell = Tag(STR, Arg->global_top);
      cell = TagToPointer(t);
      arity = Arity(TagToHeadfunctor(t));
      
      for (i = 0; i <= arity; i++)
	{
	  HeapPush(Arg->global_top, *cell);
	  cell++;
	}
      
    }
  else *cell = t;
}

//inline void make_ground_cell(Argdecl, ENGINE_Term *cell)
//{
//  ENGINE_Term t;
//  ENGINE_Term resul;
//  ENGINE_Term *top;
//  int i, arity;
//
//  DEREF(t, *cell);
////  printf("\n%p -> DEREF %p = %p\n",cell,*cell,t);
//  if (IsTrieTerm(t))
//    {
////      printf("\n%p es Trie Term\n",t);
//      if (t != *cell)
//	{
////	  printf("\n *cell != t -> igualo\n");
//	  *cell = (ENGINE_Term)TrieTermNew(t);
//	}
////      printf("\n *cell == t\n");
//    }
//  else 
//    if (TagIsLST(t))
//      {
////	printf("\n%p es lista\n",t);
//	if (IsTrieTerm(CTagToPointer(t))) 
//	  *cell = Tag(LST, TrieTermNewPos(t));
//	else
//	  {
//	    *cell = Tag(LST, Arg->global_top);
//	    cell = TagToPointer(t);
//	    top = Arg->global_top;
//	    HeapPush(Arg->global_top, *cell);
//	    cell++;
//	    HeapPush(Arg->global_top, *cell);
//	    make_ground_cell(Arg,top);
//	    make_ground_cell(Arg,top+1);
//	    
//	    array[iarray++] = (ENGINE_Term*) *top;
//	    array[iarray++] = top;
//	    
//	    array[iarray++] = (ENGINE_Term*) CTagToPointer(t);
//	    array[iarray++] = TagToPointer(t);
//	    
//	    CTagToPointer(t) = ENGINE_VarTrie | ((iarray-4) << 1);
//	  }
//      }
//    else if (TagIsSTR(t) && !IsNumber(t))
//      {
//	if (IsTrieTerm(CTagToPointer(t))) 
//	  *cell = Tag(STR, TrieTermNewPos(t));
//	else
//	  {
//	    *cell = Tag(STR, Arg->global_top);
//	    cell = TagToPointer(t);
//	    arity = Arity(TagToHeadfunctor(t));
//	    top = Arg->global_top;
//	
//	    for (i = 0; i <= arity; i++)
//	      {
//		HeapPush(Arg->global_top, *cell);
//		cell++;
//	      }
//
//	    for (i = 1; i <= arity; i++)
//	      {
//		make_ground_cell(Arg,top);
//		top++;
//	      }
//	
//	    array[iarray++] = (ENGINE_Term*) *top;
//	    array[iarray++] = top;
//	    
//	    array[iarray++] = (ENGINE_Term*) CTagToPointer(t);
//	    array[iarray++] = TagToPointer(t);
//	    
//	    CTagToPointer(t) = ENGINE_VarTrie | ((iarray-4) << 1);
//	  }
//      }
//    else *cell = t;
//}

struct consumer* get_consumer(Argdecl, struct subs_factor *sub_fact, struct EACH_CALL *parent_id)
{
  struct consumer* res = (struct consumer*) malloc (sizeof(struct consumer));
  int i, j, tam;
  ENGINE_Term *indHeap;

  res->frame = NULL;
  res->sub_fact = sub_fact;

//  printf("\nCONSUMIDOR ORIGEN\n");
//  show_sub_fact(sub_fact);
//  show_sub_fact(parent_id->sub_fact);
//  show_frames(iniA,fin,(FrameSize(Arg->next_insn)/sizeof(tagged_t)) - EToY0);


  //Copying frames

  ENGINE_Term *startHeap = Arg->global_top;

  //Copying own frames
  frame_t *indFrame;
  frame_t *lastFrame = NULL;

  //Overwritten own frames

  //first frame which is not only for the consumer
  frame_t *topOwnFrame;
  frame_t *generatorFrame = sid_stack[isid_stack - 1]->frame;

  
  frame_t *index;

  if (Arg->node > sid_stack[isid_stack - 1]->node) topOwnFrame = generatorFrame;
  else
    {
      for (index = Arg->frame, i = 0; index != generatorFrame ; index = index->frame)
	{
	  fromArgFrame[i++] = index;
	  if (StackYounger(index,StackFReg)) break;
	}
      fromArgFrame[i] = index;
      
      for (index = Arg->node->frame, j = 0; index != generatorFrame ; index = index->frame)
	{
	  fromArgNodeFrame[j++] = index;
	  if (StackYounger(index,StackFReg)) break;
	}
      fromArgNodeFrame[j] = index;
      
      i--; j--;
      for (; (fromArgFrame[i] == fromArgNodeFrame[j]) && (i >= 0) && (j >= 0); i--, j--);
      
      topOwnFrame = fromArgFrame[i+1];
    }

  int tamFrame = (FrameSize(Arg->next_insn)/sizeof(tagged_t)) - EToY0;

  indFrame = Arg->frame;
  if (indFrame != topOwnFrame) res->frame = indFrame;
  for (; indFrame != topOwnFrame; indFrame = indFrame->frame)
    {
      //Translating pointers to heap top
      for (i = 0; i < tamFrame; i++) make_ground_cell(Arg, &(indFrame->term[i]));

      lastFrame = indFrame;
      tamFrame = FrameSize(indFrame->next_insn)/sizeof(tagged_t) - EToY0;
    }
  
  //Copying shared frames
  frame_t *topFrame; //where next frame is going to be copied
  ComputeA(topFrame,Arg->node);

  if (res->frame == NULL) res->frame = topFrame;
  for ( ; indFrame != generatorFrame ; indFrame = indFrame->frame)
    {
      //Translating pointers to heap top (lastFrame is updated)
      if (lastFrame != NULL) lastFrame->frame = topFrame;

      topFrame->next_insn = indFrame->next_insn;
      for (i = 0; i < tamFrame; i++)
	{
	  topFrame->term[i] = indFrame->term[i];
	  make_ground_cell(Arg, &(topFrame->term[i]));
	}
      lastFrame = topFrame;
      topFrame = (frame_t *)Offset(topFrame,(EToY0+tamFrame));
      tamFrame = FrameSize(lastFrame->next_insn)/sizeof(tagged_t) - EToY0;
    }

  if (lastFrame != NULL) lastFrame->frame = indFrame;
  res->sub_factPARENT = (struct subs_factor*) malloc (sizeof(struct subs_factor));
  res->sub_factPARENT->size = parent_id->sub_fact->size;
  res->sub_factPARENT->vars = (tagged_t*) malloc (res->sub_factPARENT->size * sizeof(tagged_t));

  for (i = 0; i < res->sub_factPARENT->size; i++)
    {
      res->sub_factPARENT->vars[i] = parent_id->sub_fact->vars[i];
      make_ground_cell(Arg, &(res->sub_factPARENT->vars[i]));
    }

  // NOT NEEDED WITH SHARED STRUCTURES
  for (indHeap = startHeap; indHeap < Arg->global_top; indHeap++)
    {
      make_ground_cell(Arg, indHeap);
    }
  //End copying frames

  res->next_insn = Arg->next_insn; //continuation
  res->last_answer = NULL;
  res->parent_id = parent_id;

  //Reinstalling shared structures!
//  for (iarray--; iarray > 0; iarray = iarray - 4) 
//    *(array[iarray]) = (ENGINE_Term) array[iarray-1];
//  iarray = 0;

  freeze_stacks(Arg, topFrame);

//  printf("\nCONSUMIDOR CREADO %p\n",res);
//  show_sub_fact(res->sub_fact);
//  show_sub_fact(res->sub_factPARENT);
//  show_frames(iniB,fin,(FrameSize(Arg->next_insn)/sizeof(tagged_t)) - EToY0);
  return res;
}


///* -------------------------- */
///*            API             */     
///* -------------------------- */

bool_t table_copy_c(Arg)
     Argdecl;
{
  ENGINE_Term *ini = Arg->global_top;

  printf("Termino %p\n",ENGINE_ARG1);
  show_term(ENGINE_ARG1);
  fflush(stdout);
  printf("\n");
  make_ground_cell(Arg,&(ENGINE_ARG1));

  for (iarray--; iarray > 0; iarray = iarray - 4)
    {
      printf("\n%p - %p - %p - %p\n", 
	     array[iarray-3], array[iarray-2],array[iarray-1],array[iarray]);
      *(array[iarray]) = (ENGINE_Term) array[iarray-1];
    }
  iarray = 0;

  printf("\nTermino ORIGEN %p\n",ENGINE_ARG1);
  show_term(ENGINE_ARG1);
  fflush(stdout);
  printf("\n");

  printf("\nLo que crea\n");
  show_term(Tag(STR,ini));
  for (;ini < Arg->global_top; ini++)
    printf("%p = %p\n",ini,*ini);

  
  return TRUE;
}


inline void *mem_alloc(int size) 
{
    void *tmp;
    tmp = (void *)memory_free;

    memory_free += (size / sizeof(long));
    CHECK_MEM;
    return (tmp);
}

bool_t clean_tables_c(Arg)
     Argdecl;
{
  memory_free = &memory[0];
//  tabled_top = ini;

//  printf("\nTotalTrail = %d\n",totalTrail);
//  printf("\nMinTrail = %d\n",minTrail);
//  totalTrail = 0;
//  minTrail = 0;
  node_top = NULL;
  return TRUE;
}

struct EACH_CALL *handlecall(Argdecl, ENGINE_Term Call, struct subs_factor* sub_fact) 
{	     
  struct EACH_CALL *callid;
  TrNode node = put_trie_entry(node_top, Call, sub_fact);

  if (node->child == NULL) 
    {
      callid = (struct EACH_CALL*) mem_alloc (sizeof(struct EACH_CALL));
      node->child = (trie_node_t *) callid;

      callid->sub_fact = sub_fact;

      callid->trie_answers = open_trie();
      callid->first_answer = NULL;
      callid->last_answer = NULL;

      callid->cons = NULL;

      callid->frame = Arg->frame;
//      printf("\nCallid %p tiene frame %p\n",callid,callid->frame);

      if(TOP_SF==NULL) //First tabling predicate
	{
	  HeapFReg = NodeGlobalTop(Arg->node);
	  Arg->node->global_top = (tagged_t*)&HeapFReg;

	  StackFReg = NodeLocalTop(Arg->node);
	  Arg->node->local_top = (frame_t*)&StackFReg;
	  callid->dfn = 0;
	  callid->plink = 0;
	} 
      else //Seconds tabling predicates
	{
	  callid->dfn = ((struct EACH_CALL *)TOP_SF)->dfn + 1;
	  callid->plink = ((struct EACH_CALL *)TOP_SF)->dfn + 1;	
	}
      
      callid->previous = TOP_SF;
      TOP_SF = callid;
      callid->comp = READY;
      return callid;
    }
  else return (struct EACH_CALL *)(node->child);
}

int complete (struct EACH_CALL *call, struct EACH_CALL *parentcallid) 
{
  struct EACH_CALL *ccall;

  if (call->plink == call->dfn) 
    {
      do 
	{
	  if ((long)TOP_SF == (long)call) break;
	  ccall = (struct EACH_CALL *)TOP_SF;
	  ccall->comp = COMPLETE;
	  //DELE_AND_FREE_WAITS(ccall->waits);
	  //Liberar estructuras
	  TOP_SF = TOP_SF->previous;
	} while ((long)TOP_SF != (long)call);
      call->comp = COMPLETE;	    
      //DELE_AND_FREE_WAITS(call->waits);	
      //Liberar estructuras
      TOP_SF = TOP_SF->previous;    
      if (TOP_SF == NULL) 
	{
//	  //Liberar congelados, con que valores???
//	  Arg->node->global_top = HeapFReg;
//	  Arg->node->local_top = StackFReg;
	}
    } 
  return TRUE;
}

bool_t end_computation(Argdecl, struct EACH_CALL *callid, struct subs_factor *sub_fact) 
{
  if (isid_stack && callid->comp != COMPLETE) 
    {
//      printf("\n%p depende de %p\n",sid_stack[isid_stack - 1],callid);
      SETMIN(sid_stack[isid_stack - 1]->plink, callid->plink);  
    }
//  else
//    printf("\nNo hay nuevas dependencias\n");


  //Reading actual answers
  if (callid->comp == COMPLETE) 
    {
//      printf("\nCONSUMIDOR de %p COMPLETO -> leyendo respuestas\n",callid);
      if (callid->first_answer != NULL)
	{
	  TrNode node = callid->first_answer;
	  
	  push_choicept(Arg,address_nd_consume_answer_c);
	  Arg->node->term[0] = (ENGINE_Term)sub_fact;
	  Arg->node->term[1] = (ENGINE_Term)node;
	  Arg->node->term[2] = (ENGINE_Term)callid;
	  
	  struct subs_factor *answer = get_trie_answer(Arg,node);
	  
	  int iVars;
	  for (iVars = 0; iVars < answer->size; iVars++)
	    {
	      if (!ENGINE_Unify(sub_fact->vars[iVars],answer->vars[iVars])) 
		{
		  printf("\nPROBLEMAS III\n");		  
		  DEREF(sub_fact->vars[iVars],sub_fact->vars[iVars]);
		  return FALSE;
		}
	    }
	  return TRUE;        
	}
      else 
	{
	  return FALSE;
	}
    }

//  printf("\nCreamos consumidor\n"); fflush(stdout);
  struct consumer* consumer = get_consumer(Arg, sub_fact, sid_stack[isid_stack - 1]);
  consumer->last_answer = NULL;
//  printf("\nFin creamos consumidor %p\n",consumer); fflush(stdout);

  consumer->sig = callid->cons;
  callid->cons = consumer;

  return FALSE;
}

bool_t tabled_call_c(Arg)
     Argdecl;
{
//  printf("\nEntramos en tabled_call de: \n");

  if (node_top == NULL) node_top = open_trie();

  struct subs_factor *sub_fact = (struct subs_factor*) malloc (sizeof(struct subs_factor));

  push_choicept(Arg, address_nd_resume_cons_c);

  struct EACH_CALL *callid = handlecall(Arg, ENGINE_ARG1, sub_fact); 

  if (callid->comp == READY) 
    {
//      printf("\nGENERADOR %p con subfactor %p\n",callid,sub_fact);
      callid->comp = EVALUATING;
      callid->node = Arg->node;
      sid_stack[isid_stack++] = callid;
      Arg->node->term[0] = (ENGINE_Term)callid; //Poner algo para GC
      Arg->node->term[1] = (ENGINE_Term)NULL; //Poner algo para GC
      Arg->node->term[2] = (ENGINE_Term)NULL; //Poner algo para GC
      Arg->node->term[3] = (ENGINE_Term)NULL; //Poner algo para GC
 
      init_state_from_WAM(Arg->misc->goal_desc_ptr);
      if (StackYounger(StackFReg,Arg->frame)) printf("\nPROBLEMONES\n");
      ciao_commit_call_term(ciao_refer(ENGINE_ARG1));
      re_ciao_frame_end();

      return FALSE;
    }
//  printf("\nCONSUMIDOR de %p\n",callid); fflush(stdout);


  pop_choicept(Arg);

  return end_computation(Arg,callid,sub_fact);
}

bool_t nd_consume_answer_c(Arg)
     Argdecl;
{
//  printf("\nLeemos nueva respuesta\n");
  TrNode node = (TrNode) ENGINE_ARG2;
  struct subs_factor *sub_fact = (struct subs_factor*) Arg->node->term[0];

  if(node->child == NULL) 
    {
      pop_choicept(Arg);
      return FALSE;
    }

  node = node->child;
  Arg->node->term[1] = (ENGINE_Term)node;

  struct subs_factor *answer = get_trie_answer(Arg,node);

  int iVars;
  for (iVars = 0; iVars < answer->size; iVars++)
    {
      if (!ENGINE_Unify(sub_fact->vars[iVars],answer->vars[iVars])) 
	{
	  printf("\nPROBLEMAS IV\n");		  
	  return FALSE;
	}
    }
  return TRUE;
}

bool_t nd_prueba_c(Arg)
     Argdecl;
{
  pop_choicept(Arg);
  return TRUE;
}

bool_t nd_resume_cons_c(Arg)
     Argdecl;
{
//  printf("\nEntramos en resume\n"); fflush(stdout);
  struct EACH_CALL *callid = (struct EACH_CALL *) ENGINE_ARG1;
  struct EACH_CALL *sf = (struct EACH_CALL *) ENGINE_ARG2;
  struct consumer *aux = (struct consumer *) ENGINE_ARG3;

  if (ENGINE_ARG4 != (ENGINE_Term)NULL)
      aux->parent_id->sub_fact = (struct subs_factor *)ENGINE_ARG4;

  isid_stack--;

  if (sf == NULL)
    {
      sf = TOP_SF;
      aux = sf->cons;
    }
  struct EACH_CALL *topeCall = NULL;

  do
    {
      for (; aux != NULL; aux = aux->sig)
	{ 
	  struct trie_node *Answer;
	  if (aux->last_answer == NULL) Answer = sf->first_answer;
	  else Answer = aux->last_answer->child;
	  
	  if (Answer != NULL)
	    {
	      aux->last_answer = Answer;
	      
	      //Updating sub_fact of parent
	      Arg->node->term[1] = (ENGINE_Term)sf; //GC!!
	      Arg->node->term[2] = (ENGINE_Term)aux; //GC!!
	      Arg->node->term[3] = (ENGINE_Term)aux->parent_id->sub_fact; //GC!!
	      
	      aux->parent_id->sub_fact = aux->sub_factPARENT;
	      
	      //	      printf("\nModifico valor de %p\n",aux->parent_id); fflush(stdout);
	      
	      //Updating completion stack
	      sid_stack[isid_stack++] = aux->parent_id; 
	      
	      //Reinstalling subtitution factor from answer
	      
	      struct subs_factor *answer = get_trie_answer(Arg,Answer);
	      int iVars;
	      
	      //	      printf("\nRelanzamos consumidor %p y respuesta %p\n",aux,Answer);
	      //
	      //	      printf("\nPRE\n");
	      //	      show_sub_fact(aux->sub_fact);
	      
	      for (iVars = 0; iVars < answer->size; iVars++)
		{
		  //		  printf("\nUnificando subsfact %p con respuesta %p\n",
		  //			 aux->sub_fact->vars[iVars],answer->vars[iVars]);
		  //		  show_term(answer->vars[iVars]);
		  //		  printf("\n");
		  if (!ENGINE_Unify(aux->sub_fact->vars[iVars],answer->vars[iVars])) 
		    {
		      printf("\nPROBLEMAS XII %p - %p\n",
			     aux->sub_fact->vars[iVars],answer->vars[iVars]);
		      DEREF(aux->sub_fact->vars[iVars],
			    aux->sub_fact->vars[iVars]);
		      DEREF(answer->vars[iVars],answer->vars[iVars]);
		      printf("\nPROBLEMAS XII %p - %p\n",
			     aux->sub_fact->vars[iVars],answer->vars[iVars]);		  
		      return FALSE;
		    }
		}
	      
	      Arg->next_insn = aux->next_insn;
	      Arg->frame = aux->frame;
	      
	      //	      printf("\nPOST\n");
	      //	      show_sub_fact(aux->sub_fact);
	      
	      return TRUE;
	    }
	}
      
      if (topeCall == sf) break;
      if (topeCall == NULL) topeCall = sf;
      if (sf == callid) sf = TOP_SF;
      else sf = sf->previous;
      aux = sf->cons; 
    }
  while (1);
  
//  printf("\nNo quedan consumidores -> probar si se puee completar\n");
  complete(callid,callid);


//  printf("\nUltimo resume %d -> %d\n", consDeep+1, consDeep); fflush(stdout);

  pop_choicept(Arg);

  return end_computation(Arg,callid,callid->sub_fact);
}

bool_t set_diff_answer_trie(void)
{
  struct EACH_CALL *call = sid_stack[isid_stack - 1];
  TrNode node;

//  printf("Para respuesta uso subfact %p\n",call->sub_fact);
//  show_sub_fact(call->sub_fact);

  node = put_trie_answer(call->trie_answers, call->sub_fact);

  if (node->child == NULL && node != call->last_answer)
    {
//      printf("\nNueva respuesta %p\n",node);

      if (call->first_answer == NULL) call->first_answer = node;

      if(call->last_answer != NULL) call->last_answer->child = node;
      
      call->last_answer = node;
      return TRUE;
    }

//  printf("\nRespuesta %p repetida\n",node);
  
  return FALSE;
}

bool_t new_answer_c(Arg)
     Argdecl;
{

//  if (set_diff_answer_trie(callid, ENGINE_ARG1)) 
//    {
//      insert_answer(Arg, callid, ENGINE_ARG1); freezing on the heap
//    }

  set_diff_answer_trie();
  return FALSE;  
}

bool_t check_c(Arg)
     Argdecl;
{
  return TRUE;  
}

bool_t initial_c(Arg)
     Argdecl;
{
  ini = NULL;
  isid_stack = 0;
  memory_free = &memory[0];
  node_top = NULL;
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,3);
  address_nd_resume_cons_c = def_retry_c(nd_resume_cons_c,4);
  address_nd_prueba_c = def_retry_c(nd_prueba_c,0);
  init_tries_module();

  if (Heap_End != HeapOffset(Heap_Start, TABLING_GLOBALSTKSIZE))
    {
      int size = (TABLING_GLOBALSTKSIZE -
		  HeapDifference(Heap_Start, w->global_top))/2;
      heap_overflow(Arg,size);
    }

  if (Stack_End != StackOffset(Stack_Start, TABLING_LOCALSTKSIZE))
    {
      tagged_t *new_Stack_Start;
      int reloc_factor;

      new_Stack_Start = checkrealloc
	(Stack_Start, StackDifference(Stack_Start,Stack_End)*sizeof(tagged_t),
	 TABLING_LOCALSTKSIZE*sizeof(tagged_t));

      reloc_factor = (char *)new_Stack_Start - (char *)Stack_Start;
      stack_overflow_adjust_wam(w, reloc_factor);

      /* Final adjustments */
      Stack_Start = new_Stack_Start;		/* new bounds */
      Stack_End = StackOffset(new_Stack_Start,TABLING_LOCALSTKSIZE);
    }

  if (Trail_End != TrailOffset(Trail_Start, TABLING_CHOICESTKSIZE +
			       TABLING_TRAILSTKSIZE))
    {
      tagged_t *choice_top = (tagged_t *)w->node+w->value_trail;

      int size = (TABLING_CHOICESTKSIZE + TABLING_TRAILSTKSIZE -
		  ChoiceDifference(Choice_Start, choice_top) -
		  TrailDifference(Trail_Start, w->trail_top)) / 2;

      choice_overflow(Arg,size);
    }  

  return TRUE;
}
