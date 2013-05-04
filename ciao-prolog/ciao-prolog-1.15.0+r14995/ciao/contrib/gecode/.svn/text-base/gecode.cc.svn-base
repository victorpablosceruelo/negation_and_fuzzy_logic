/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "gecode.hh"

using namespace Gecode;

#include "gecode_space.hh"

//GLOBAL VARIABLE GECODE SPACE

IGecode* gecode;

/* -------------------------- */
/*     Gecode Interface       */
/* -------------------------- */

extern "C" bool_t gecode_print_cc(worker_t *w)
{
#ifdef DEBUG_ALL
  printf("\nEntering print\n"); //fflush(stdout);
#endif
  gecode->print();

  return TRUE;
}

extern "C" bool_t gecode_print_variable_cc(worker_t *w)
{
#ifdef DEBUG_ALL
  printf("\nEntering print_variable\n"); //fflush(stdout);
#endif
  DEREF(X(0),X(0));
  int id = GetInteger(X(0));

  gecode->print_variable(id);

  return TRUE;
}

extern "C" bool_t gecode_get_space_cc(worker_t *w)
{
#ifdef DEBUG_ALL
  printf("\nEntering get_space\n"); //fflush(stdout);
#endif
  IGecode* prev_gecode = gecode;
  if (last_cp != w->node)
    gecode = (IGecode*)gecode->clone();

  Unify(X(0),MkInt((int)prev_gecode));
  Unify(X(1),MkInt((int)last_cp));

  last_cp = w->node;

  return TRUE;
}

extern "C" bool_t gecode_backtracking_space_cc(worker_t *w)
{
#ifdef DEBUG_ALL
  printf("\nEntering backtracking_space\n"); //fflush(stdout);
#endif
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  IGecode *prev_gecode = (IGecode*)GetInteger(X(0));
  last_cp = (node_t*)GetInteger(X(1));

  if (prev_gecode != gecode) delete gecode;

  gecode = prev_gecode;

  last_cp = NULL;

  return TRUE;
}

extern "C" bool_t gecode_equal_integer_cc(worker_t *w)
{
#ifdef DEBUG_ALL
  printf("\nEntering equal_integer\n"); //fflush(stdout);
#endif
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  gecode->equal_integer(id1,value);

  return gecode->status();
}

extern "C" bool_t gecode_equal_variable_cc(worker_t *w)
{
#ifdef DEBUG_ALL
  printf("\nEntering equal_variable\n"); //fflush(stdout);
#endif
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  gecode->equal_variable(id1,id2);

  return gecode->status();

}

extern "C" bool_t gecode_less_integer_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  gecode->less_integer(id1,value);

  return gecode->status();
}

extern "C" bool_t gecode_less_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  gecode->less_variable(id1,id2);

  return gecode->status();

}

extern "C" bool_t gecode_less_equal_integer_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  gecode->less_equal_integer(id1,value);

  return gecode->status();
}

extern "C" bool_t gecode_less_equal_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  gecode->less_equal_variable(id1,id2);

  return gecode->status();
}

extern "C" bool_t gecode_greater_integer_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  gecode->greater_integer(id1,value);

  return gecode->status();
}

extern "C" bool_t gecode_greater_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  gecode->greater_variable(id1,id2);

  return gecode->status();
}

extern "C" bool_t gecode_greater_equal_integer_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  gecode->greater_equal_integer(id1,value);

  return gecode->status();
}

extern "C" bool_t gecode_greater_equal_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  gecode->greater_equal_variable(id1,id2);

  return gecode->status();
}

extern "C" bool_t gecode_different_integer_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  gecode->different_integer(id1,value);

  return gecode->status();
}

extern "C" bool_t gecode_different_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  gecode->different_variable(id1,id2);

  return gecode->status();
}

extern "C" bool_t gecode_add_integer_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  int res = gecode->add_integer(id1,value);

  Unify(X(2),MkInt(res));

  return TRUE;
}

extern "C" bool_t gecode_add_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  int res = gecode->add_variable(id1,id2);

  Unify(X(2),MkInt(res));

  return TRUE;
}

extern "C" bool_t gecode_sub_integer_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  int res = gecode->sub_integer(id1,value);

  Unify(X(2),MkInt(res));

  return TRUE;
}

extern "C" bool_t gecode_sub_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  int res = gecode->sub_variable(id1,id2);

  Unify(X(2),MkInt(res));

  return TRUE;
}

extern "C" bool_t gecode_mul_integer_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int value = GetInteger(X(1));

  int res = gecode->mul_integer(id1,value);

  Unify(X(2),MkInt(res));

  return TRUE;
}

extern "C" bool_t gecode_mul_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));


  int res = gecode->mul_variable(id1,id2);

  Unify(X(2),MkInt(res));

  return TRUE;
}

extern "C" bool_t gecode_new_variable_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int lb = GetInteger(X(0));
  int ub = GetInteger(X(1));

  int id = gecode->new_variable(lb,ub);

  Unify(X(2),MkInt(id));

  return TRUE;
}

extern "C" bool_t gecode_get_lb_cc(worker_t *w)
{
  DEREF(X(0),X(0));

  int id = GetInteger(X(0));
  int lb = gecode->get_lb(id);

  return Unify(X(1),MkInt(lb));
}

extern "C" bool_t gecode_get_ub_cc(worker_t *w)
{
  DEREF(X(0),X(0));

  int id = GetInteger(X(0));
  int ub = gecode->get_ub(id);

  return Unify(X(1),MkInt(ub));
}

extern "C" bool_t gecode_get_values_cc(worker_t *w)
{
  DEREF(X(0),X(0));

  int id = GetInteger(X(0));
  int *values = gecode->get_values(id);

  //creating a list of values
  tagged_t tail = EMPTY_LIST;
  for (int i = values[0]; i > 0; i--)
    tail = MkList(MkInt(values[i]),tail);

  free(values);

  return Unify(X(1),tail);
}

extern "C" bool_t gecode_labeling_cc(worker_t *w)
{
  DEREF(X(0),X(0));
  gecode->put_branch(X(0));

  Search::Options opt;

  DFS<IGecode> *dfs = new DFS<IGecode>(gecode,opt);
    
  IGecode *s = dfs->next();

  if (s != NULL) 
    {
      push_choicept(w,address_gecode_labeling_nd_cc);
      w->node->term[0] = (tagged_t)dfs; 
      w->node->term[1] = (tagged_t)s; 
      return TRUE;
    }

  return FALSE;
}

extern "C" bool_t gecode_labeling_nd_cc(worker_t *w)
{
  DFS<IGecode> *dfs = (DFS<IGecode>*) w->node->term[0];

  IGecode *s = (IGecode*) w->node->term[1];
  delete s;

  s = dfs->next();
  if (s != NULL) 
    {
      w->node->term[1] = (tagged_t)s; 
      return TRUE;
    }

  delete dfs;
  pop_choicept(w);
  return FALSE;
}

extern "C" void* gecode_clone_cc()
{
#ifdef DEBUG_ALL
  printf("\nEntering clone\n"); //fflush(stdout);
#endif
  return (void*) gecode->clone();
}

extern "C" bool_t initial_cc(worker_t *w)
{
  last_cp = NULL;
  gecode = new IGecode;

  // address_gecode_labeling_nd_cc = def_retry_c(gecode_labeling_nd_cc,2);

  return TRUE;
}
