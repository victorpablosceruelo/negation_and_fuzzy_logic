#include <engine/basiccontrol.native.h>

/* ------------------------------------------------------------------ */

void predicate_def__interpreted(definition_t *f, bool_t concurrent);
void predicate_def__incore(definition_t *f);
CVOID__PROTO_N(make_undefined, definition_t *f);

CBOOL__PROTO_N(predicate_def, definition_t *f, intmach_t bits) {
  intmach_t type;
  intmach_t current_mem = total_mem_count;

  type = ((bits & DEFBITS(dynamic)) != 0 ? ENTER_INTERPRETED :
	  ENTER_COMPACTCODE);

  /* Definition of multifile predicates is shared */
  if (f->properties.multifile) {
    INC_MEM_PROG((total_mem_count - current_mem));
    /* Check that the types are compatible */
    switch(f->predtyp) {
    case ENTER_COMPACTCODE_INDEXED:
    case ENTER_PROFILEDCODE_INDEXED:
    case ENTER_COMPACTCODE:
    case ENTER_PROFILEDCODE:
      if (type == ENTER_COMPACTCODE) break;
      goto bad;
    case ENTER_INTERPRETED:
      if (type == ENTER_INTERPRETED) break;
      goto bad;
    default:
      goto bad;
    }
    CBOOL__PROCEED;
  bad:
    MAJOR_FAULT("defining incompatible multifile predicates");
    CBOOL__FAIL;
  }

  /* Undefine the predicate */
  if (f->predtyp != ENTER_UNDEFINED) {
    CVOID__CALL_N(make_undefined, f);
  }

  /* Define it */

  num_of_predicates++; 

  switch (type) {
  case ENTER_INTERPRETED:
    predicate_def__interpreted(f, (bits & DEFBITS(concurrent)) ? 1 : 0);
    break;
  default:
    predicate_def__incore(f);
    break;
  }

  if ((bits & DEFBITS(multifile)) != 0) {
    f->properties.multifile = 1;
  }
  if ((bits & DEFBITS(hardrtexp)) != 0) {
    f->properties.hardrtexp = 1;
  }

  INC_MEM_PROG((total_mem_count - current_mem));
  CBOOL__PROCEED;
}

CBOOL__PROTO(define_predicate) {
  definition_t *f;
  intmach_t bits;

  DEREF(X(0),X(0));
  f = resolve_definition(X(0));
  CBOOL__TEST(f != NULL);

  DEREF(X(1),X(1));
  CBOOL__TEST(TaggedIsSmall(X(1)));
  bits = GetSmall(X(1));

  CBOOL__LASTCALL_N(predicate_def, f, bits);
}

CBOOL__PROTO_N(defmultifile, definition_t *func) {
  CBOOL__LASTCALL_N(predicate_def, func,
		    DEFBITS(dynamic) | DEFBITS(multifile));
}

/* ------------------------------------------------------------------ */
/* current_predicate/2 */

try_node_t *address_nd_current_predicate;

CBOOL__PROTO(current_predicate) {
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (!IsVar(X(1))) {
    tagged_t *junk;
    definition_t *d;

    d = find_definition(X(1),&junk,FALSE);

    CBOOL__TEST(d!=NULL && d->predtyp!=ENTER_UNDEFINED);
    CBOOL__UnifyCons(FuncName(d),X(0));
    CBOOL__PROCEED;
  } else {
    X(2) = MakeSmall(HASHTAB_SIZE(predicates_location));
    X(3) = PointerToTerm(predicates_location);
    CVOID__CALL_N(push_choicept,address_nd_current_predicate);
    CBOOL__LASTCALL(nd_current_predicate);
  }
}

/* true if A is a variable or a functor with the same names (and maybe different arity) */
#define INDEXNVCMP(A,B) (IsVar((A)) ? TRUE : (AtomIndex((A)) == AtomIndex((B))))
CBOOL__PROTO(nd_current_predicate) {
  hashtab_t *table = TermToPointer(hashtab_t, X(3));
  hashtab_node_t *keyval;
  intmach_t j = GetSmall(X(2));
  definition_t *d;

  for (--j; j>=0; --j) {
    keyval = &table->node[j];
    if (INDEXNVCMP(X(0), (tagged_t)keyval->key) &&
        (d = keyval->value.def) &&
        d->predtyp != ENTER_UNDEFINED){
      if (j==0) {
        CVOID__CALL(pop_choicept);
      } else {
        w->choice->x[2] = MakeSmall(j);
      }
      CBOOL__UnifyCons(FuncName(d),X(0));
      CBOOL__LASTUNIFY(CFUN__EVAL_N(make_structure, FuncFunctor(d)), X(1));
    }
  }
  CVOID__CALL(pop_choicept);
  CBOOL__FAIL;
}

/* ------------------------------------------------------------------ */
/* predicate_property/2 */

try_node_t *address_nd_predicate_property;

static uintmach_t predicate_property_bits(definition_t *d) {
  return
      (d->properties.concurrent ?  0x1 : 0)
    | (d->properties.dynamic    ?  0x2 : 0)
    | (d->properties.multifile  ?  0x8 : 0)
    ;
}

CBOOL__PROTO(predicate_property) {
  DEREF(X(0),X(0));
  if (!IsVar(X(0))) {
    definition_t *d;
    d = resolve_definition(X(0));
    CBOOL__TEST(d!=NULL && d->predtyp!=ENTER_UNDEFINED);
    CBOOL__UnifyCons(MakeSmall(d->predtyp),X(1));
    CBOOL__UnifyCons(MakeSmall(predicate_property_bits(d)),X(2));
    CBOOL__PROCEED;
  } else {
    X(3) = MakeSmall(HASHTAB_SIZE(predicates_location));
    X(4) = PointerToTerm(predicates_location);
    CVOID__CALL_N(push_choicept,address_nd_predicate_property);
    CBOOL__LASTCALL(nd_predicate_property);
  }
}

CBOOL__PROTO(nd_predicate_property) {
  hashtab_t *table = TermToPointer(hashtab_t, X(4));
  intmach_t j = GetSmall(X(3));
  hashtab_node_t *keyval;
  definition_t *d;
  for (--j; j>=0; --j) {
    keyval = &table->node[j];
    if ((d = keyval->value.def) &&
	d->predtyp != ENTER_UNDEFINED) {
      if (j==0) {
	CVOID__CALL(pop_choicept);
      } else {
	w->choice->x[3] = MakeSmall(j);
      }
      CBOOL__UnifyCons(MakeSmall(d->predtyp),X(1));
      CBOOL__UnifyCons(MakeSmall(predicate_property_bits(d)),X(2));
      CBOOL__LASTUNIFY(CFUN__EVAL_N(make_structure,FuncFunctor(d)),X(0));
    }
  }
  CVOID__CALL(pop_choicept);
  CBOOL__FAIL;
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(prolog_new_user_module_id) {
  // ERR__FUNCTOR("rt_exp:$new_user_module_id", 2);
  intmach_t new_atom_length;
  char *s, *s1;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  CBOOL__TEST(TaggedIsATM(X(0)));

  s1 = GetString(X(0));
  new_atom_length = GetAtomLen(X(0)) + 1 + 1;

  GET_ATOM_BUFFER(s, new_atom_length);

  /* Append the two strings in atom_buffer */
  *s++ = '+';
  while (*s1) *s++ = *s1++;
  *s = '\0';

  CBOOL__LASTUNIFY(GET_ATOM(Atom_Buffer),X(1));
}

#define IS_USER_MODULE_ID(X) (((X)[0]) == '+')

CBOOL__PROTO(prolog_user_module_id) {
  // ERR__FUNCTOR("rt_exp:$user_module_id", 1);
  char *s1;
  DEREF(X(0),X(0));
  CBOOL__TEST(TaggedIsATM(X(0)));
  s1 = GetString(X(0));
  CBOOL__LASTTEST(IS_USER_MODULE_ID(s1));
}

CBOOL__PROTO(prolog_module_concat) {
  // ERR__FUNCTOR("rt_exp:$module_concat", 3);
  intmach_t new_atom_length;
  char *s, *s1, *s2;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));

  if (!TaggedIsATM(X(0))) {
    /* Do nothing if the first argument is not an atom */
    CBOOL__LASTUNIFY(X(0),X(2));
  }
  
  CBOOL__TEST(TaggedIsATM(X(1)));

  s1 = GetString(X(1));
  s2 = GetString(X(0));
  new_atom_length = GetAtomLen(X(0)) + GetAtomLen(X(1)) + 1 + 1;

  GET_ATOM_BUFFER(s, new_atom_length);

  /* Append module+':'+functor in atom_buffer */
  if (IS_USER_MODULE_ID(s1)) {
    s1 = "user";
  }
  while (*s1) *s++ = *s1++;
  *s++ = ':';
  while (*s2) *s++ = *s2++;
  *s = '\0';

  CBOOL__LASTUNIFY(GET_ATOM(Atom_Buffer),X(2));
}

/* TODO: use a user_predicates_location table!!! */
CBOOL__PROTO_N(module_user_defines, tagged_t modname, tagged_t f, intmach_t a, tagged_t *mfp) {
  hashtab_node_t *keyval;
  char *s1;
  char *s2;
  char *s;
  intmach_t new_atom_length;

  s1 = GetString(modname);
  CBOOL__TEST(IS_USER_MODULE_ID(s1));

  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  f = X(1);
  DEREF(X(2), X(2));
  CBOOL__TEST(TaggedIsSmall(X(2)));
  a = GetSmall(X(2));

  /* TODO: share with module concat */
  s1 = "user:";
  s2 = GetString(f);
  new_atom_length = 5 + GetAtomLen(f) + 1;

  GET_ATOM_BUFFER(s, new_atom_length);

  /* Append s1+functor in atom_buffer */
  while (*s1) *s++ = *s1++;
  while (*s2) *s++ = *s2++;
  *s = '\0';
  tagged_t mf = GET_ATOM(Atom_Buffer);
  
  /* Search the predicate and ensure that it does exist */
  /* TODO: necessary? ... yes, todo: implement as a user pred hashtab */
  tagged_t mfa = SetArity(mf, a);
  keyval = hashtab_get(predicates_location, (hashtab_key_t)mfa);
  CBOOL__TEST(keyval->key != ERRORTAG);
  definition_t *d = keyval->value.def;
  CBOOL__TEST(d!=NULL && d->predtyp!=ENTER_UNDEFINED);

  *mfp = mf;
  CBOOL__PROCEED;
}



