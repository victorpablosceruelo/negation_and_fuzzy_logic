#include <engine/basiccontrol.native.h>
#include <engine/engine__ciao_prolog.h>

#include <unistd.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/stat.h>
#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>
#if defined(__svr4__) || defined(DARWIN)             /* Solaris or DARWIN */
#include <unistd.h>                                            /* sbrk () */
#include <stdlib.h>                                           /* malloc() */
#else                                                            /* SunOS */
#include <sys/types.h>
#include <malloc.h>
#endif
#include <sys/param.h>

#if defined(Solaris)
#include <alloca.h>
#endif

#if defined(DARWIN)
#include <stdlib.h>
#else
#include <malloc.h>
#endif
#include <string.h>

void ciao_exit(int result);

#define LongToTagged(X) IntmachToTagged((X))
#define TaggedToLong(X) TaggedToIntmach((X))
#define BlobFunctorFixLong BlobFunctorFixIntmach

/* --------------------------------------------------------------------------- */

ciao_state ciao_implicit_state;

/* --------------------------------------------------------------------------- */

extern char source_path[];

extern char *library_directory;

/* Memory management routines --- now only interfaces to library, but they
   might evolve in access to a custom memory management library */

void *ciao_malloc(long size) {
  return malloc(size);
}

void ciao_free(void *pointer) {
  free(pointer);
}


/* Low level term operations */

ciao_term ciao_ref(ciao_state state, tagged_t x);
tagged_t ciao_unref(ciao_state state, ciao_term term);

struct _ciao_query {
  ciao_state state;
  ciao_choice base_choice;
};

/* --------------------------------------------------------------------------- */ 

void ciao_ensure_heap(ciao_state state, int size) {
  WITH_WORKER(state->worker_registers, {
    TEST_HEAP_OVERFLOW(G->heap_top, CONTPAD + size, 0);
  });
}

/* --------------------------------------------------------------------------- */ 

ciao_bool ciao_is_char_code_list(ciao_state state, ciao_term term) {
  tagged_t cdr, car;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TaggedIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!TaggedIsSmall(car) || (car<TaggedZero) || (car>=MakeSmall(256))) break;
    DerefCdr(cdr,cdr);
  }
  return cdr == atom_nil;
}

long ciao_is_int_list(ciao_state state, ciao_term term) {
  tagged_t cdr, car;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TaggedIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!IsInteger(car)) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? 1 : 0;
}

long ciao_list_length(ciao_state state, ciao_term term) {
  tagged_t cdr;
  long len;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  for (len=0; cdr!=atom_nil; len++) {
    if (IsVar(cdr)) break;
    if (!TaggedIsLST(cdr)) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? len : (-1);
}

#define TEMPLATE(Name, X, XC) \
void Name(ciao_state state, ciao_term list, long length, X *array) { \
  long i; \
  tagged_t car, cdr; \
  cdr = ciao_unref(state, list); \
  DEREF(cdr, cdr); \
  for (i = 0; i < length; i++) { \
    DerefCar(car,cdr); \
    array[i] = XC(car); \
    DerefCdr(cdr,cdr); \
  } \
}
TEMPLATE(ciao_list_to_byte_array_l, char, GetSmall)
TEMPLATE(ciao_list_to_int_array_l, long, TaggedToLong)
TEMPLATE(ciao_list_to_double_array_l, double, TaggedToFloat)
#undef TEMPLATE

#define TEMPLATE(Name, X, NameL) \
X *Name(ciao_state state, ciao_term list) { \
  X *array; \
  long length; \
  length = ciao_list_length(state, list); \
  if (length == 0) return NULL; /* sure? */ \
  array = (X *)ciao_malloc(sizeof(X) * length); \
  NameL(state, list, length, array); \
  return array; \
}
TEMPLATE(ciao_list_to_byte_array, char, ciao_list_to_byte_array_l)
TEMPLATE(ciao_list_to_int_array, long, ciao_list_to_int_array_l)
#undef TEMPLATE

char *ciao_list_to_str(ciao_state state, ciao_term list) {
  char *string;
  long length;
  length = ciao_list_length(state, list);
  string = (char *)ciao_malloc(sizeof(char) * (length + 1));
  ciao_list_to_byte_array_l(state, list, length, string);
  string[length] = 0;
  return string;
}

#define TEMPLATE(Name, X, XC, XS) \
ciao_term Name(ciao_state state, X *s, long length) { \
  long i; \
  tagged_t cdr; \
  WITH_WORKER(state->worker_registers, { \
    ciao_ensure_heap(state, (length * XS) * sizeof(tagged_t)); \
    cdr = atom_nil; \
    s += length; \
    for (i = 0; i < length; i++) { \
      s--; \
      MakeLST(cdr, XC, cdr); \
    } \
  }); \
  return ciao_ref(state, cdr); \
}
TEMPLATE(ciao_byte_listn, const char, MakeSmall(*s), 2)
TEMPLATE(ciao_int_listn, long, LongToTagged(*s), 4)
TEMPLATE(ciao_double_listn, double, BoxFloat(*s), 8)
#undef TEMPLATE

ciao_term ciao_str_to_list(ciao_state state, const char *string) {
  long length;
  length = strlen(string);
  return ciao_byte_listn(state, string, length);
}

ciao_term ciao_pointer_to_address(ciao_state state, void *pointer) {
  return ciao_structure_s(state, "$address", 1, ciao_integer_s(state, (long)pointer));
}

void *ciao_address_to_pointer(ciao_state state, ciao_term term) {
  return (void *)ciao_to_integer_s(state, ciao_structure_arg_s(state, term, 1));
}

ciao_bool ciao_is_address(ciao_state state, ciao_term term) {
  return (ciao_is_structure_s(state, term) && strcmp(ciao_structure_name_s(state, term), "$address") == 0 && ciao_structure_arity_s(state, term) == 1);
}

/* --------------------------------------------------------------------------- */
/* Initialization */

/* Parse options before initialization */

int ciao_opts(const char *program_name, int programc, const char **programv, int optc, const char **optv) {
  prolog_argc = programc;
  prolog_argv = (char **)programv;

  engine_set_opts((char **)optv, optc);

  engine_init();
  return 0;
}

/* Initialization */

extern bcp_t call_code;
extern bcp_t default_code;
extern bcp_t null_code;
extern try_node_t nullgoal_alt;
extern try_node_t defaultgoal_alt;
extern try_node_t startgoal_alt;

void ciao_init() {
  engine_init();
}

void ciao_finish(ciao_state state) {
  WITH_WORKER(state->worker_registers, {
    CVOID__CALL(engine_finish);
  });
}

/* Reinitialization */

void ciao_reinit() {
  glb_init_each_time();
}

/* --------------------------------------------------------------------------- */
/* WAM creation */

ciao_state ciao_state_new() {
  return gimme_a_new_gd();
}

void ciao_state_free(ciao_state state) {
  release_goal_desc(state);
}

/* --------------------------------------------------------------------------- */
/* Term creation */

ciao_term ciao_var_s(ciao_state state) {
  tagged_t *pt;
  tagged_t to;
  ciao_ensure_heap(state, sizeof(tagged_t));
  WITH_WORKER(state->worker_registers, {
    pt = G->heap_top;
    to = Tagp(HVA, pt);
    HeapPush(pt, to);
    G->heap_top = pt;
  });
  return ciao_ref(state, to);
}

ciao_term ciao_var() {
  return ciao_var_s(ciao_implicit_state);
}

ciao_term ciao_structure_a_s(ciao_state state, const char *name, long arity, ciao_term *args) {
  tagged_t x;
  WITH_WORKER(state->worker_registers, {
    if (arity == 0) {
      x = GET_ATOM((char *)name);
    } else if (strcmp(name, ".") == 0 && arity == 2) {
      tagged_t list;
      ciao_ensure_heap(state, 3 * sizeof(tagged_t));
      MakeLST(list, ciao_unref(state, args[0]), ciao_unref(state, args[1]));
      x= list;
    } else {
      long i;
      tagged_t *pt;
      tagged_t *s;
      tagged_t functor;
      ciao_ensure_heap(state, (2 + arity) * sizeof(tagged_t));
      functor = deffunctor((char *)name, arity);
      pt = G->heap_top;
      s = pt;
      HeapPush(pt, functor);
      for (i = 0; i < arity; i++) {
	HeapPush(pt, ciao_unref(state, args[i]));
      }
      G->heap_top = pt;  
      x = Tagp(STR, s);
    }
  });
  return ciao_ref(state, x);
}

ciao_term ciao_structure_a(const char *name, long arity, ciao_term *args) {
  return ciao_structure_a_s(ciao_implicit_state, name, arity, args);
}

ciao_term ciao_integer_s(ciao_state state, long i) {
  tagged_t x;
  ciao_ensure_heap(state, 4 * sizeof(tagged_t));
  WITH_WORKER(state->worker_registers, {
    x = LongToTagged(i);
  });
  return ciao_ref(state, x);
}

ciao_term ciao_integer(long i) {
  return ciao_integer_s(ciao_implicit_state, i);
}

ciao_term ciao_float_s(ciao_state state, double f) {
  tagged_t x;
  ciao_ensure_heap(state, 4 * sizeof(tagged_t));
  WITH_WORKER(state->worker_registers, {
    x = BoxFloat(f);
  });
  return ciao_ref(state, x);
}

ciao_term ciao_float(double f) {
  return ciao_float_s(ciao_implicit_state, f);
}

ciao_bool ciao_is_integer_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsInteger(t);
}

ciao_bool ciao_is_integer(ciao_term term) {
  return ciao_is_integer_s(ciao_implicit_state, term);
}

ciao_bool ciao_fits_in_int_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TaggedIsSmall(t) || (TaggedIsSTR(t) && TaggedToHeadfunctor(t) == BlobFunctorFixLong);
}

ciao_bool ciao_fits_in_int(ciao_term term) {
  return ciao_fits_in_int_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_variable_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsVar(t);
}

ciao_bool ciao_is_variable(ciao_term term) {
  return ciao_is_variable_s(ciao_implicit_state, term);
}

long ciao_to_integer_s(ciao_state state, ciao_term term) {
  /* PRECONDITION: ciao_is_integer(state, term) */
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TaggedToLong(t);
}

long ciao_to_integer(ciao_term term) {
  return ciao_to_integer_s(ciao_implicit_state, term);
}

ciao_bool ciao_to_integer_check_s(ciao_state state, ciao_term term, long *res) {
  if (ciao_fits_in_int_s(state, term)) {
    *res = ciao_to_integer_s(state, term);
    return TRUE;
  } else return FALSE;
}

ciao_bool ciao_to_integer_check(ciao_term term, long *res) {
  return ciao_to_integer_check_s(ciao_implicit_state, term, res);
}

ciao_bool ciao_is_number_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsNumber(t);
}

ciao_bool ciao_is_number(ciao_term term) {
  return ciao_is_number_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_float_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsFloat(t);
}

ciao_bool ciao_is_float(ciao_term term) {
  return ciao_is_float_s(ciao_implicit_state, term);
}

double ciao_to_float_s(ciao_state state, ciao_term term) {
  /* PRECONDITION: ciao_is_number(state, term) */
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TaggedToFloat(t);
}

double ciao_to_float(ciao_term term) {
  return ciao_to_float_s(ciao_implicit_state, term);
}

char *ciao_get_number_chars_s(ciao_state state, ciao_term term) {
  tagged_t number;
  char *number_result;

  WITH_WORKER(state->worker_registers, {
    number = ciao_unref(state, term);
    /* number_to_string() handles al kinds of numbers; it leaves the
       result in Atom_Buffer */
    CVOID__CALL_N(number_to_string, number, GetSmall(current_radix));
    number_result = ciao_malloc(strlen(Atom_Buffer) + 1);
    strcpy(number_result, Atom_Buffer);
  });
  return number_result;
}

char *ciao_get_number_chars(ciao_term term) {
  return ciao_get_number_chars_s(ciao_implicit_state, term);
}

/* PRECONDITION: number_result should really represent a number */
/* TODO: raise a proper exception */
ciao_term ciao_put_number_chars_s(ciao_state state, char *number_string) {
  tagged_t result;
  WITH_WORKER(state->worker_registers, {
    (void)CBOOL__SUCCEED_N(string_to_number,
			   (unsigned char *)number_string,
			   GetSmall(current_radix),
			   &result,
			   0);
  });
  return ciao_ref(state, result);
} 

ciao_term ciao_put_number_chars(char *number_string) {
  return ciao_put_number_chars_s(ciao_implicit_state, number_string);
}


ciao_bool ciao_is_atom_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TaggedIsATM(t);
}

ciao_bool ciao_is_atom(ciao_term term) {
  return ciao_is_atom_s(ciao_implicit_state, term);
}

const char *ciao_atom_name_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TaggedIsATM(t)) {
    return (const char *)NULL;
  } else { 
    atom_t *atomptr;
    atomptr = TaggedToAtom(t);
    return atomptr->name;
  }
}

const char *ciao_atom_name(ciao_term term) {
  return ciao_atom_name_s(ciao_implicit_state, term);
}

char *ciao_atom_name_dup_s(ciao_state state, ciao_term term) {
  const char *s2;
  char *s;
  s2 = ciao_atom_name_s(state, term);
  s = (char *)ciao_malloc(sizeof(char *) * (strlen(s2) + 1));
  strcpy(s, s2);
  return s;
}

char *ciao_atom_name_dup(ciao_term term) {
  return ciao_atom_name_dup_s(ciao_implicit_state, term);
}

const char *ciao_structure_name_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TaggedIsSTR(t)) {
    return (const char *)NULL;
  } else {
    tagged_t f;
    f = TaggedToHeadfunctor(t);
    return GetString(f);
  }
}

const char *ciao_structure_name(ciao_term term) {
  return ciao_structure_name_s(ciao_implicit_state, term);
}

long ciao_structure_arity_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TaggedIsSTR(t)) {
    return 0;
  } else { 
    tagged_t f;
    f = TaggedToHeadfunctor(t);
    return Arity(f);
  }
}

long ciao_structure_arity(ciao_term term) {
  return ciao_structure_arity_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_list_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TaggedIsLST(t);
}

ciao_bool ciao_is_list(ciao_term term) {
  return ciao_is_list_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_empty_list_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TaggedIsATM(t) && t == GET_ATOM("[]");
}

ciao_bool ciao_is_empty_list(ciao_term term) {
  return ciao_is_empty_list_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_structure_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TaggedIsSTR(t);
}

ciao_bool ciao_is_structure(ciao_term term) {
  return ciao_is_structure_s(ciao_implicit_state, term);
}

ciao_term ciao_structure_arg_s(ciao_state state, ciao_term term, long i) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TaggedIsSTR(t)) return CIAO_ERROR;
  a = *TaggedToArg(t, i);
  return ciao_ref(state, a);
}

ciao_term ciao_structure_arg(ciao_term term, long i) {
  return ciao_structure_arg_s(ciao_implicit_state, term, i);
}

ciao_term ciao_list_head_s(ciao_state state, ciao_term term) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  a = *TaggedToCar(t);
  return ciao_ref(state, a);
}

ciao_term ciao_list_head(ciao_term term) {
  return ciao_list_head_s(ciao_implicit_state, term);
}

ciao_term ciao_list_tail_s(ciao_state state, ciao_term term) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  a = *TaggedToCdr(t);
  return ciao_ref(state, a);
}

ciao_term ciao_list_tail(ciao_term term) {
  return ciao_list_tail_s(ciao_implicit_state, term);
}

/* Helper functions */

ciao_term ciao_atom_s(ciao_state state, const char *name) {
  return ciao_structure_s(state, name, 0);
}

ciao_term ciao_atom(const char *name) {
  return ciao_atom_s(ciao_implicit_state, name);
}

ciao_term ciao_empty_list_s(ciao_state state) {
  return ciao_atom_s(state, "[]");
}

ciao_term ciao_empty_list() {
  return ciao_empty_list_s(ciao_implicit_state);
}

ciao_term ciao_list_s(ciao_state state, ciao_term head, ciao_term tail) {
  return ciao_structure_s(state, ".", 2, head, tail);
}

ciao_term ciao_list(ciao_term head, ciao_term tail) {
  return ciao_list_s(ciao_implicit_state, head, tail);
}

ciao_term ciao_dlist_a_s(ciao_state state, long len, ciao_term *args, ciao_term tail) {
  /* PRECONDITION: len >= 1 */ 
  long i;
  ciao_term list;
  
  list = tail;
  for (i = len - 1; i >= 0; i--) {
    list = ciao_list_s(state, args[i], list);
  }

  return list;
}

ciao_term ciao_dlist_a(long len, ciao_term *args, ciao_term tail) {
  return ciao_dlist_a_s(ciao_implicit_state, len, args, tail);
}

ciao_term ciao_listn_a_s(ciao_state state, long len, ciao_term *args) {
  return ciao_dlist_a_s(state, len, args, ciao_empty_list(state));
}

ciao_term ciao_listn_a(long len, ciao_term *args) {
  return ciao_listn_a_s(ciao_implicit_state, len, args);
}

#define GETARGS(LENGTH) \
  ciao_term *args; \
  long i; \
  va_list p; \
  args = alloca(sizeof(ciao_term) * LENGTH); \
  va_start(p, LENGTH); /* last argument before '...' */ \
  for (i = 0; i < LENGTH; i++) { \
    args[i] = va_arg(p, ciao_term); \
  } \
  va_end(p);

ciao_term ciao_structure_s(ciao_state state, const char *name, long arity, ...) {
  GETARGS(arity)
  return ciao_structure_a_s(state, name, arity, args);
}

ciao_term ciao_structure(const char *name, long arity, ...) {
  GETARGS(arity)
  return ciao_structure_a(name, arity, args);
}

ciao_term ciao_listn_s(ciao_state state, long length, ...) {
  GETARGS(length)
  return ciao_listn_a_s(state, length, args);
}

ciao_term ciao_listn(long length, ...) {
  GETARGS(length)
  return ciao_listn_a(length, args);
}

ciao_term ciao_dlist_s(ciao_state state, long length, ...) {
  GETARGS(length)
  return ciao_dlist_a_s(state, length - 1, args, args[length - 1]);
}

ciao_term ciao_dlist(long length, ...) {
  GETARGS(length)
  return ciao_dlist_a(length - 1, args, args[length - 1]);
}


ciao_term ciao_copy_term_s(ciao_state src_state, ciao_term src_term, ciao_state dst_state) {
  tagged_t x;
  WITH_WORKER(dst_state->worker_registers, {
    x = CFUN__EVAL_N(cross_copy_term, ciao_unref(dst_state, src_term));
  });
  return ciao_ref(dst_state, x);
}

ciao_term ciao_copy_term(ciao_term src_term) {
  return ciao_copy_term_s(ciao_implicit_state, src_term, ciao_implicit_state);
}

ciao_bool ciao_unify_s(ciao_state state, ciao_term x, ciao_term y) {
  ciao_bool ok;
  WITH_WORKER(state->worker_registers, {
    ok = CBOOL__SUCCEED_N(cunify, ciao_unref(state, x), ciao_unref(state, y));
  });
  return ok;
}

ciao_bool ciao_unify(ciao_term x, ciao_term y) {
  return ciao_unify_s(ciao_implicit_state, x, y);
}

ciao_bool ciao_equal_s(ciao_state state, ciao_term x, ciao_term y) {
  tagged_t a, b;
  a = ciao_unref(state, x);
  b = ciao_unref(state, y);
  DEREF(a, a);
  DEREF(b, b);
  return a == b;
}

ciao_bool ciao_equal(ciao_term x, ciao_term y) {
  return ciao_equal_s(ciao_implicit_state, x, y);
}

void ciao_exit(int result) {
  engine_exit(result);
}

/* Here with G->next_insn set up -- see local_init_each_time(). (MCL) */

JMP_BUF abort_env;                                              /* Shared */

CBOOL__PROTO(flush_output);

int ciao_firstgoal(ciao_state state, ciao_term goal) {
  int i;
  WITH_WORKER(state->worker_registers, {
    i = CFUN__EVAL_N(call_firstgoal, ciao_unref(state, goal), state);
  });
  return i;
}

int ciao_boot(ciao_state state) {
  return ciao_firstgoal(state, ciao_structure_s(state, "internals:boot", 0));
}

/* ------------------------------------------------------------------------- */

ciao_choice ciao_get_choice(ciao_state state) {
  ciao_choice c;
  WITH_WORKER(state->worker_registers, {
    c = ChoiceToTagged(w->choice);
  });
  return c;
}

ciao_bool ciao_more_solutions(ciao_state state, ciao_choice choice) {
  return ciao_get_choice(state) > choice;
}

void ciao_cut(ciao_state state, ciao_choice choice) {
  choice_t *c;
  if (!ciao_more_solutions(state, choice)) return;
  WITH_WORKER(state->worker_registers, {
    c = ChoiceFromTagged(choice);
    SetChoice(c);
  });
}

void ciao_fail(ciao_state state) {
  wam(state);
}

/* --------------------------------------------------------------------------- */

ciao_bool ciao_query_next(ciao_query *query) {
  if (!ciao_query_ok(query)) return FALSE;
  query->state->action = BACKTRACKING | KEEP_STACKS;
  ciao_fail(query->state);
  return ciao_query_ok(query);
}

ciao_bool ciao_query_ok(ciao_query *query) {
  ciao_bool ok;
  try_node_t *alt;

  WITH_WORKER(query->state->worker_registers, {
    alt = G->next_alt;
    if (alt == &nullgoal_alt ||
	(alt == NULL && w->choice->next_alt == &nullgoal_alt)) {
      ok = FALSE;
    } else {
      ok = TRUE;
    }
  });
  return ok;
}

void ciao_query_end(ciao_query *query) {
  choice_t *b;
  ciao_state state;

  state = query->state;

  WITH_WORKER(state->worker_registers, {
    if (ciao_query_ok(query)) {
      ciao_cut(state, query->base_choice);
    }
    b = w->choice;
    b = ChoiceCont(b);
    SetChoice(b);
    SetDeep();
  });

  ciao_free(query);
}

ciao_query *ciao_query_begin_term_s(ciao_state state, ciao_term goal) {
  choice_t *b;
  ciao_query *query;

  goal = ciao_structure_s(state, "call", 1, goal);

  WITH_WORKER(state->worker_registers, {
    DEREF(X(0), ciao_unref(state, goal));

    /* push null choice */

    G->next_insn = default_code;

    CODE_CHOICE_NEW(b, &defaultgoal_alt);

    b->frame = G->frame;
    b->next_insn = G->next_insn;
    b->local_top = G->local_top;
    
    SetDeep();

    query = (ciao_query *)ciao_malloc(sizeof(ciao_query));
    query->state = state;
    query->base_choice = ciao_get_choice(state);
  
    /* push choice for starting goal */
  
    G->next_insn = call_code;

    CODE_CHOICE_NEW(b, &startgoal_alt);

    b->frame = G->frame;
    b->next_insn = G->next_insn;
    b->local_top = G->local_top;
    b->x[0] = X(0);    /* arg. of a call/1 */
    
    SetDeep();
  });

  state->action = BACKTRACKING | KEEP_STACKS;
  wam(state);
  return query;
}

ciao_query *ciao_query_begin_term(ciao_term goal) {
  return ciao_query_begin_term_s(ciao_implicit_state, goal);
}

ciao_query *ciao_query_begin_s(ciao_state state, const char *name, long arity, ...) {
  GETARGS(arity);
  return ciao_query_begin_term_s(state, ciao_structure_a_s(state, name, arity, args));
}

ciao_query *ciao_query_begin(const char *name, long arity, ...) {
  GETARGS(arity);
  return ciao_query_begin_term(ciao_structure_a(name, arity, args));
}

ciao_bool ciao_commit_call_term_s(ciao_state state, ciao_term goal) {
  ciao_bool ok;
  ciao_query *query;

  query = ciao_query_begin_term_s(state, goal);
  ok = ciao_query_ok(query);
  ciao_query_end(query);

  return ok;
}

ciao_bool ciao_commit_call_term(ciao_term goal) {
  return ciao_commit_call_term_s(ciao_implicit_state, goal);
}

ciao_bool ciao_commit_call_s(ciao_state state, const char *name, long arity, ...) {
  GETARGS(arity)
  return ciao_commit_call_term_s(state, ciao_structure_a_s(state, name, arity, args));
}

ciao_bool ciao_commit_call(const char *name, long arity, ...) {
  GETARGS(arity)
  return ciao_commit_call_term(ciao_structure_a(name, arity, args));
}

/* --------------------------------------------------------------------------- */

#include <setjmp.h>

jmp_buf ciao_gluecode_jmpbuf;

void ciao_raise_exception_s(ciao_state state, ciao_term exception) {
  WITH_WORKER(state->worker_registers, {
    X(0) = ciao_unref(state, exception);
  });
  longjmp(ciao_gluecode_jmpbuf, 1);
}

void ciao_raise_exception(ciao_term exception) {
  ciao_raise_exception_s(ciao_implicit_state, exception);
}

/* --------------------------------------------------------------------------- */

#define GARBAGE_PROTECTION

#ifdef GARBAGE_PROTECTION 

#define REF_TABLE_PAD 4
#define REF_TABLE_CHUNK_SIZE 32
#define REF_TABLE_CHUNKS 1

tagged_t create_ref_table(ciao_state state, long chunks) {
  long i, j;
  tagged_t *pt, *pt0;
  tagged_t functor;
  tagged_t x;

  ciao_ensure_heap(state, (REF_TABLE_CHUNK_SIZE * chunks + 1) * sizeof(tagged_t));
  WITH_WORKER(state->worker_registers, {
    functor = deffunctor("$reftable", REF_TABLE_CHUNK_SIZE - 1);
    pt = G->heap_top;
    pt0 = pt;
    for (j = 0; j < chunks - 1; j++) {
      HeapPush(pt, functor);
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 2; i++) {
	HeapPush(pt, Tagp(HVA, pt));
      }
      HeapPush(pt, Tagp(STR, pt + 1));
    }
    if (chunks > 0) {
      HeapPush(pt, functor);
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 1; i++) {
	HeapPush(pt, Tagp(HVA, pt));
      }
    }
    G->heap_top = pt;
    x = Tagp(STR, pt0);
  });
  return x;
}

CFUN__PROTO_N(ciao_ref__2, ciao_term, ciao_state state, tagged_t x) {
  ciao_term term;
  frame_t *frame;
  long next, chunks;

  frame = G->frame;

  next = GetSmall(frame->x[0]);
  {
    tagged_t ta;
    ta = *TaggedToArg(frame->x[2], next);
    if (!CBOOL__SUCCEED_N(cunify,ta,x)) { goto fail; }
    goto ok;
  }
 fail:
  /* fatal error */
  SERIOUS_FAULT("Error registering term");
 ok:
  term = next;
  next++;
  if ((next & (REF_TABLE_CHUNK_SIZE - 1)) == (REF_TABLE_CHUNK_SIZE - 1)) 
    next++; /* skip functor */
  if ((next & (REF_TABLE_CHUNK_SIZE - 1)) == 0) 
    next++; /* skip str tag */
  frame->x[0] = MakeSmall(next);

  chunks = GetSmall(frame->x[1]);
  if (chunks * REF_TABLE_CHUNK_SIZE - next < REF_TABLE_PAD) {
    /* chunk overflow! */
    tagged_t *x, *y;
    tagged_t new_table;
    long i, j, new_chunks, k;

    new_chunks = chunks * 2;
    /* old table is in frame->x[2] so don't care about gc here */  
    new_table = create_ref_table(state, new_chunks); 

    x = TaggedToArg(frame->x[2], 0);
    y = TaggedToArg(new_table, 0);
    k = 0;
    for (j = 0; j < chunks - 1; j++) {
      k++;
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 2; i++) {
	y[k] = x[k];
	if (k == term) goto end;
	k++;
      }
      k++;
    }
    if (chunks > 0) {
      k++;
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 1; i++) {
	y[k] = x[k];
	if (k == term) goto end;
	k++;
      }
    }
  end:
    frame->x[2] = new_table;
    frame->x[1] = MakeSmall(new_chunks);
  }
  CFUN__PROCEED(term);
}

ciao_term ciao_ref(ciao_state state, tagged_t x) {
  ciao_term term;
  WITH_WORKER(state->worker_registers, {
    term = CFUN__EVAL_N(ciao_ref__2, state, x);
  });
  return term;
}

tagged_t ciao_unref(ciao_state state, ciao_term term) {
  frame_t *frame;
  tagged_t x;

  WITH_WORKER(state->worker_registers, {
    frame = G->frame;
    x = *TaggedToArg(frame->x[2], term);
  });
  return x;
}

void ciao_frame_begin_s(ciao_state state) {
  WITH_WORKER(state->worker_registers, {
    frame_t *frame;
    long arity;
    arity = 3;
    CODE_ALLOC(frame);
    CODE_CFRAME(frame, CONTCODE(arity));
    frame->x[0] = MakeSmall(1); /* next free ref */
    frame->x[1] = MakeSmall(REF_TABLE_CHUNKS); /* chunks */
    frame->x[2] = create_ref_table(state, REF_TABLE_CHUNKS);
  });
}

void ciao_frame_begin() {
  ciao_frame_begin_s(ciao_implicit_state);
}

void ciao_frame_end_s(ciao_state state) {
  WITH_WORKER(state->worker_registers, {
    frame_t *a;
    a = G->frame; 
    SetLocalTop(a);
    DEALLOCATE(a);
  });
}

void ciao_frame_end() {
  ciao_frame_end_s(ciao_implicit_state);
}

#else

ciao_term ciao_ref(ciao_state state, tagged_t x) {
  return (ciao_term)x;
}

tagged_t ciao_unref(ciao_state state, ciao_term term) {
  return (tagged_t)term;
}

void ciao_frame_begin_s(ciao_state state) {
}
  
void ciao_frame_end_s(ciao_state state) {
}

void ciao_frame_begin() {
}
  
void ciao_frame_end() {
}

#endif


