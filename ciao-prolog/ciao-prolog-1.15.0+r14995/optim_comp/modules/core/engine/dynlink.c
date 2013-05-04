/*
  Low level module system implementation

  Tue Apr 19 19:12:29 CEST 2005   
  --jfran
*/

/* TODO: optimize disabling using the local resolve cache, replacing
   uses by a poiner to modules?! */
/* TODO: clean error messages */
/* TODO: clean code... avoid code duplications */
/* TODO: use locks? */
/* TODO: transform #if 0 blocks to optional trace code */
/* TODO: use make_module_undefined */

#include <engine/basiccontrol.native.h>

#if defined(USE_DYNLINK_NATIVE)
#  include <dlfcn.h>
#  include <string.h>
#  if defined(SunOS4)
#    include <sys/param.h>
#  endif
#endif

#include <time.h> /* for time() */

#if defined(USE_DYNLINK_NATIVE)
#if !defined(ABSMACH_OPT__dynlink_mod)
#error "dynlink_native requires dynlink_mod"
#endif
#endif

hashtab_t *modules_location = NULL;
#if defined(ABSMACH_OPT__oo_extensions)
hashtab_t *objfunctor_table = NULL;
#endif

#define GETMODULE(MODULE, X) { \
  hashtab_node_t *keyval; \
  tagged_t t1; \
  DEREF(t1,X); \
  CBOOL__TEST(TaggedIsATM(t1)); \
  keyval = hashtab_get(modules_location, (hashtab_key_t)t1); \
  CBOOL__TEST(keyval->key != ERRORTAG); \
  MODULE = keyval->value.module; \
}

#define OOGETCLASS(MODULE, X) { \
  hashtab_node_t *keyval; \
  tagged_t t1; \
  DEREF(t1,X); \
  if (TaggedIsATM(t1)) { \
  } else if (TaggedIsSTR(t1)) { \
    t1 = TaggedToHeadfunctor(t1); \
  } else { \
    CBOOL__FAIL; \
  } \
  keyval = hashtab_get(objfunctor_table, (hashtab_key_t)t1); \
  CBOOL__TEST(keyval->key != ERRORTAG); \
  MODULE = keyval->value.module; \
}

#if defined(ABSMACH_OPT__cache_local_resolve)
#define EMPTY_LOCALCACHE { \
  if (module->local_cache != NULL) { \
    hashtab_free(module->local_cache); \
    module->local_cache = NULL; \
  } \
}
#endif

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(qread_end);
CBOOL__PROTO_N(qread_begin, FILE *file);
CBOOL__PROTO_N(qread1, tagged_t *term);

/* ------------------------------------------------------------------------- */

/* TODO: some bytes can be lost in buffering... always end reading the file! */
/* TODO: by now, all the modules loaded here are marked as static */

extern bool_t qleof();

CBOOL__PROTO_N(load_module, char *native_so_name, bool_t is_static);
CBOOL__PROTO_N(load_module_pack, FILE *qfile) {
  // bool_t ok;
  CBOOL__CALL_N(qread_begin, qfile);
  // ok = TRUE;
  while (1) {
    /* TODO: cannot distinguish 'no more modules' from an error, throw
       exceptions when an error is found */
    if (!CBOOL__SUCCEED_N(load_module, "", TRUE)) break;
  }
  CBOOL__CALL(qread_end);
  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------------- */
/* Load bytecode */

CBOOL__PROTO_N(predicate_def, definition_t *f, intmach_t bits);

tagged_t module__speckey;

#if defined(ABSMACH_OPT__regmod)
extern tagged_t regmod__mark;
#endif

CBOOL__PROTO_N(add_bytecode_clause,
	       definition_t *f,
	       emul_info_t *ref,
	       uintmach_t type,
	       tagged_t key) {
  incore_info_t *d;
  uintmach_t bitmap;
  intmach_t current_mem = total_mem_count;

  /* add a new clause. */
  d = f->code.incoreinfo;

#if defined(ABSMACH_OPT__regmod)
  ref->mark = regmod__mark;
#endif

  /* Insert the clause */
  *d->clauses_tail = ref;
  d->clauses_tail = &ref->next;

  /* Insert the clause in the index tree */
  /* TODO: see compiler(bytecode__compiler) to see the meaning of those bits */
  bitmap = 
    (type&0x1 &&  !f->properties.nonvar ? 0x1 : 0) | /* var   */
    (type&0x8 &&  !f->properties.var    ? 0x2 : 0) | /* lst   */
    (type&0x16 && !f->properties.var    ? 0x4 : 0) ; /* other */
  if ((type&0x21) == 0x21) f->properties.nonvar = 1;
  if ((type&0x3e) == 0x3e) f->properties.var = 1;

  if (!(f->predtyp&1) && bitmap!=0x7) {
    set_predtyp_indexed(f);
    d->lstcase = incore_copy(d->varcase);
    d->othercase = HASHTAB_NEW_DEF(2, incore_copy(d->varcase));
  }

  if (!(f->predtyp&1)) {
    incore_insert(&d->varcase,FuncArity(f),ref);
  } else {
    if (bitmap&0x1) incore_insert(&d->varcase,FuncArity(f),ref);
    if (bitmap&0x2) incore_insert(&d->lstcase,FuncArity(f),ref);
    if (bitmap&0x4) incore_puthash(&d->othercase,FuncArity(f),ref,key);
  }

  INC_MEM_PROG((total_mem_count - current_mem));
  CBOOL__PROCEED;
}

CFUN__PROTO(compile_term_aux, instance_t *);
CBOOL__PROTO_N(insertz, int_info_t *root, instance_t *n);

CBOOL__PROTO_N(add_interpreted, definition_t *d) { /* input: X(0) X(1) */
  /* TODO: assert (d!=NULL) && (d->predtyp==ENTER_INTERPRETED) */
  instance_t *ptr;
  ptr = CFUN__EVAL(compile_term_aux);
  CBOOL__LASTCALL_N(insertz, d->code.intinfo, ptr);
}

#if defined(USE_DYNLINK_NATIVE)
CBOOL__PROTO_N(link_native, module_t *module, char *so_filename);
#endif

CBOOL__PROTO_N(abolish_module, module_t *module);

CBOOL__PROTO_N(defmultifile, definition_t *func);

tagged_t define_predicate_functor;
tagged_t functor__c;
tagged_t functor__e;
tagged_t functor__b2;
tagged_t interpreted_clause_functor;
tagged_t interpreted_fact_functor;
tagged_t atom_true;
tagged_t end_functor;

tagged_t functor__meta;
tagged_t functor__colon;

tagged_t functor__u;
tagged_t functor__exports;
tagged_t functor__defines;
tagged_t functor__imports_all;
tagged_t functor__imports;
tagged_t functor__meta_args;
tagged_t functor__context;
#if defined(ABSMACH_OPT__oo_extensions)
tagged_t functor__instvar;
tagged_t functor__instdata;
#endif

tagged_t functor_specmod_d;
definition_t *def_specmod_d;
definition_t *def_meta_args_d;
definition_t *def_context_d;

definition_t *def_slowpgcall;

CBOOL__PROTO(init_dynlink) {
  functor__c = deffunctor("c", 0); 
  functor__e = deffunctor("e", 0); 
  define_predicate_functor = deffunctor("d", 2); 
  functor__b2 = deffunctor("b", 2);
  interpreted_clause_functor = deffunctor("i", 2);
  interpreted_fact_functor = deffunctor("f", 1);
  end_functor = deffunctor("e", 1); 
  atom_true = GET_ATOM("basiccontrol:true");

  functor__meta = deffunctor("$:", 1);
  functor__colon = deffunctor(":", 2);

  functor__u = deffunctor("u", 0);
  functor__exports = deffunctor("exports", 0);
  functor__defines = deffunctor("defines", 0);
  functor__imports_all = deffunctor("imports_all", 0);
  functor__imports = deffunctor("imports", 0);
  functor__meta_args = deffunctor("meta_args", 0);
  functor__context = deffunctor("context", 0);
#if defined(ABSMACH_OPT__oo_extensions)
  functor__instvar = deffunctor("instvar", 0);
  functor__instdata = deffunctor("instdata", 0);
#endif

  functor_specmod_d = deffunctor("multifile:$specmod", 2);
  def_specmod_d = LOOKUP_DEF(functor_specmod_d);
  def_meta_args_d = LOOKUP_DEF(deffunctor("multifile:$meta_args", 1));
  def_context_d = LOOKUP_DEF(deffunctor("multifile:$context", 3));

  def_slowpgcall = LOOKUP_DEF(deffunctor("rt_exp:rt_slowpgcall", 2));
  
  /* TODO: remove unused entries */
  CBOOL__CALL_N(defmultifile, def_specmod_d);
  CBOOL__CALL_N(defmultifile, def_meta_args_d);
  CBOOL__CALL_N(defmultifile, def_context_d);

  CBOOL__PROCEED;
}

#define NOT_IMPORTED ((void *)0)
#define IMPORTS_ALL ((void *)1)

void module_add_imports_all(module_t *module, tagged_t im) {
  hashtab_node_t *keyval;
  if (module->imports == NULL) module->imports = HASHTAB_NEW(2);
  keyval = hashtab_lookup(&module->imports, (hashtab_key_t)im);
  if (keyval->value.tab == IMPORTS_ALL) {
    return; /* nothing else can be done */
  } else if (keyval->value.tab == NOT_IMPORTED) {
    keyval->value.tab = IMPORTS_ALL;
    EMPTY_LOCALCACHE;
  } else { /* not all the predicates of IM are imported */
    /* delete table */
    hashtab_free(keyval->value.tab);
    keyval->value.tab = IMPORTS_ALL;
    EMPTY_LOCALCACHE;
  }
#if 0
  TRACE_PRINTF("%s imports all from %s\n", GetString(module->name), GetString(im));
#endif
}

void module_add_imports(module_t *module, tagged_t im, tagged_t f, intmach_t a, tagged_t mf) {
  hashtab_node_t *keyval;
  hashtab_node_t *imval;
  if (module->imports == NULL) module->imports = HASHTAB_NEW(2);
  keyval = hashtab_lookup(&module->imports, (hashtab_key_t)im);
  if (keyval->value.tab == IMPORTS_ALL) {
    return; /* nothing else can be done */
  } else if (keyval->value.tab == NOT_IMPORTED) {
    /* create a new table for IM */
    keyval->value.tab = HASHTAB_NEW(2);
  } else { /* not all the predicates of IM are imported */
    /* use existing table for IM */
  }

  /* Insert entry */
  imval = hashtab_lookup(&keyval->value.tab, (hashtab_key_t)SetArity(f, a));
  if (imval->value.tagged == ERRORTAG) {
    imval->value.tagged = mf;
  }
  EMPTY_LOCALCACHE;
#if 0
  TRACE_PRINTF("%s imports from %s %s/%d, points to %s\n",
	  GetString(module->name),
	  GetString(im),
	  GetString(f),
	  a,
	  GetString(mf));
#endif
}	    

/* remove the imports table */
void module_delete_imports(module_t *module) {
#if 0
  TRACE_PRINTF("deleting imports\n");
#endif
  if (module->imports == NULL) return;
  HASHTAB_ITERATE(module->imports, n, {
    if (n->value.tab == IMPORTS_ALL) {
    } else if (n->value.tab == NOT_IMPORTED) {
    } else {
      hashtab_free(n->value.tab);
    }
  });
  hashtab_free(module->imports);
  EMPTY_LOCALCACHE;
}

/* remove imports entries no longer exported by imported module */
void module_purge_imports(module_t *module, tagged_t im) {
  module_t *imported_module;
  hashtab_node_t *keyval;
  hashtab_t *imtab;

#if 0
  TRACE_PRINTF("purging imports\n");
#endif
  
  if (module->imports == NULL) return; /* nothing to purge */
  /* Find the IM table */
  keyval = hashtab_get(module->imports, (hashtab_key_t)im);
  if (keyval->key == ERRORTAG || keyval->value.tab == NOT_IMPORTED) {
    return; /* not imported, nothing to purge */
  } else if (keyval->value.tab == IMPORTS_ALL) {
    return; /* nothing to purge */
  } else { /* not all the predicates of IM are imported */
    /* use existing table for IM */
  }
  imtab = keyval->value.tab;

  /* Find the imported module */
  keyval = hashtab_get(modules_location, (hashtab_key_t)im);
  if (keyval->key == ERRORTAG) {
    /* TODO: panic...? */
    return;
  }
  imported_module = keyval->value.module;
  
  /* TODO: use hash table removal code? */
  /* TODO: should remove the table if nothing is left */
  HASHTAB_ITERATE(imtab, n, {
    tagged_t fa;
    if (n->value.tagged != ERRORTAG) {
      fa = n->key; /* unexpanded functor */
      keyval = hashtab_get(imported_module->exports, (hashtab_key_t)fa);
      if (keyval->key == ERRORTAG) {
	/* not imported, remove the entry of imtab */
	n->value.tagged = ERRORTAG;
      }
    }
  });
  EMPTY_LOCALCACHE;
}

void module_remove_imports(module_t *module, tagged_t im) {
  hashtab_node_t *keyval;
#if 0
  TRACE_PRINTF("remove imports\n");
#endif
  if (module->imports == NULL) return; /* nothing to remove */
  /* Find the IM table */
  keyval = hashtab_get(module->imports, (hashtab_key_t)im);
  if (keyval->key == ERRORTAG || keyval->value.tab == NOT_IMPORTED) {
    /* not imported */
  } else if (keyval->value.tab == IMPORTS_ALL) {
    /* unset entry */
    keyval->value.tab = NOT_IMPORTED;
  } else { /* not all the predicates of IM are imported */
    /* delete table and unset entry */
    hashtab_free(keyval->value.tab);
    keyval->value.tab = NOT_IMPORTED;
  }
  EMPTY_LOCALCACHE;
}

/* add_imports_all(M, IM) */
CBOOL__PROTO(prolog_add_imports_all) {
  module_t *module;
  tagged_t im;

  GETMODULE(module, X(0));

  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  im = X(1);
  
  module_add_imports_all(module, im);
  CBOOL__PROCEED;
}

CBOOL__PROTO_N(module_resolve, module_t *module, tagged_t im, tagged_t fa, tagged_t *mfp);

/* '$mod_resolve'(M, IM, F, A, MF) */
CBOOL__PROTO(prolog_module_resolve) {
  module_t *module;
  tagged_t im;
  tagged_t f;
  tagged_t a;
  tagged_t fa;
  tagged_t mf;
  
  GETMODULE(module, X(0));

  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  im = X(1);
  DEREF(X(2), X(2));
  if (TaggedIsSmall(X(2))) {
    /* unify to complain later */
    /* TODO: correct? why not fail */
    CBOOL__UnifyCons(X(2), X(4));
    CBOOL__PROCEED;
  }
  CBOOL__TEST(TaggedIsATM(X(2)));
  f = X(2);
  DEREF(X(3), X(3));
  CBOOL__TEST(TaggedIsSmall(X(3)));
  a = X(3);

  fa = SetArity(f, GetSmall(a));
  CBOOL__CALL_N(module_resolve, module, im, fa, &mf);

  CBOOL__UnifyCons(mf, X(4));

  CBOOL__PROCEED;
}

CBOOL__PROTO_N(module_user_defines, tagged_t modname, tagged_t f, intmach_t a, tagged_t *mfp);

CBOOL__PROTO_N(module_resolve, module_t *module, tagged_t im, tagged_t fa, tagged_t *mfp) {
  hashtab_node_t *keyval;
  hashtab_t *tab;
  tagged_t mf;

  if (im == atom_dash) {
    intmach_t i;
#if defined(ABSMACH_OPT__cache_local_resolve)    
    hashtab_node_t *cachekeyval;
    if (module->local_cache == NULL) {
      module->local_cache = HASHTAB_NEW(2);
    }
    /* Search in the local cache table */
    cachekeyval = hashtab_lookup(&module->local_cache, (hashtab_key_t)fa);
    if (cachekeyval->value.tagged != ERRORTAG) {
      /* Found */
      *mfp = cachekeyval->value.tagged;
      CBOOL__PROCEED;
    }
    /* Not found in the cache, resolve */
#endif
    /* Search in the local table */
    keyval = hashtab_get(module->defines, (hashtab_key_t)fa);
    if (keyval->key != ERRORTAG) {
      mf = keyval->value.tagged;
      goto local_found;
    }
    /* Search the IM imports */
    CBOOL__TEST(module->imports != NULL);
    for (i = 0; i < module->uses_size; i++) {
      /* TODO: IMPORTANT! we must check each module in the same order
	 than it was imported; if not we can resolve different
	 predicates than those that we get at compile time (or we must
	 use a import hash table that preserves the order... a special
	 hash table for the '-' case) */
      im = module->uses[i];
      /* TODO: share with previous predicate! */
      keyval = hashtab_get(module->imports, (hashtab_key_t)im);
      if (keyval->key == ERRORTAG || keyval->value.tab == NOT_IMPORTED) {
	/* TODO: error, it should not happen */
	continue;
      } else if (keyval->value.tab == IMPORTS_ALL) {
	/* TODO: replace 'uses' by an array to module_t* */
	module_t *imported_module;
	keyval = hashtab_get(modules_location, (hashtab_key_t)im);
	CBOOL__TEST(keyval->key != ERRORTAG);
	imported_module = keyval->value.module;
	/* Search in exported predicates of module IM */
	tab = imported_module->exports;
      } else {
	/* Search in the imported predicates from IM table */
	tab = keyval->value.tab;
      }
      keyval = hashtab_get(tab, (hashtab_key_t)fa);
      if (keyval->key == ERRORTAG) continue;
      if (keyval->value.tagged == ERRORTAG) continue; // it was erased
      mf = keyval->value.tagged;
      goto local_found;
    }
    /* Not found, try with user predicates */
    CBOOL__LASTCALL_N(module_user_defines, module->name, FUNCTOR_NAME(fa), Arity(fa), &mf);
    goto local_found;
  local_found:
    /* Store in the cache */
#if defined(ABSMACH_OPT__cache_local_resolve)    
    cachekeyval->value.tagged = mf;
#endif
    *mfp = mf;
    CBOOL__PROCEED;
  } else {
    if (im == module->name) {
      /* Search in the local table */
      tab = module->defines;
    } else if (im == atom_user) {
      CBOOL__LASTCALL_N(module_user_defines, module->name, FUNCTOR_NAME(fa), Arity(fa), mfp);
    } else {
      /* Consult the IM imports */
      CBOOL__TEST(module->imports != NULL);
      keyval = hashtab_get(module->imports, (hashtab_key_t)im);
      if (keyval->key == ERRORTAG || keyval->value.tab == NOT_IMPORTED) {
	CBOOL__FAIL;
      } else if (keyval->value.tab == IMPORTS_ALL) {
	module_t *imported_module;
	keyval = hashtab_get(modules_location, (hashtab_key_t)im);
	CBOOL__TEST(keyval->key != ERRORTAG);
	imported_module = keyval->value.module;
	/* Search in exported predicates of module IM */
	tab = imported_module->exports;
      } else {
	/* Search in the imported predicates from IM table */
	tab = keyval->value.tab;
      }
    }
    keyval = hashtab_get(tab, (hashtab_key_t)fa);
    CBOOL__TEST(keyval->key != ERRORTAG);
    CBOOL__TEST(keyval->value.tagged != ERRORTAG); // just in case it was erased
    mf = keyval->value.tagged;
    goto found;
  found:
    *mfp = mf;
    CBOOL__PROCEED;
  }
}

/* add_imports(M, IM, F, A, MF) */
CBOOL__PROTO(prolog_add_imports) {
  module_t *module;
  tagged_t im;
  tagged_t f;
  tagged_t a;
  tagged_t mf;
  
  GETMODULE(module, X(0));

  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  im = X(1);
  DEREF(X(2), X(2));
  CBOOL__TEST(TaggedIsATM(X(2)));
  f = X(2);
  DEREF(X(3), X(3));
  CBOOL__TEST(TaggedIsSmall(X(3)));
  a = X(3);
  DEREF(X(4), X(4));
  CBOOL__TEST(TaggedIsATM(X(4)));
  mf = X(4);

  module_add_imports(module, im, f, GetSmall(a), mf);
  CBOOL__PROCEED;
}

/* purge_imports(M, IM) */
CBOOL__PROTO(prolog_purge_imports) {
  module_t *module;
  tagged_t im;
  
  GETMODULE(module, X(0));

  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  im = X(1);
  
  module_purge_imports(module, im);
  CBOOL__PROCEED;
}

/* remove_imports(M, IM) */
CBOOL__PROTO(prolog_remove_imports) {
  module_t *module;
  tagged_t im;
  
  GETMODULE(module, X(0));

  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  im = X(1);

  module_remove_imports(module, im);
  CBOOL__PROCEED;
}

CFUN__PROTO_N(get_objfunctor, tagged_t, tagged_t module_name, intmach_t arity);

/* This internal predicate does not check errors */
/* note: clears X registers */
CBOOL__PROTO_N(load_module, char *native_so_name, bool_t is_static) {
  /* TODO: also, note that X registers are lost after cterm ... right? */
  tagged_t t;
  tagged_t t1;
  tagged_t functor;
  bool_t ok;

  intmach_t defs_size;
  intmach_t defs_i;
  definition_t **defs;
  module_t *module;

  hashtab_node_t *keyval = NULL;

  definition_t *d = NULL;

  defs_i = 0;
  module = NULL;

  /* TODO: is this enough to avoid bugs on heap overflows? */
  X(0) = MakeSmall(0);
  
  /* TODO: the address_nd_repeat here does not matter, we only create
     a choice point to move backwards the heap pointer... it's a
     HACK!! */
  extern try_node_t *address_nd_repeat;
  CVOID__CALL_N(push_choicept, address_nd_repeat); 

  ok = TRUE;

  /* TODO: remember to unload all the definitions if the load fails */
  module = CHECKALLOC(module_t);
  module->defs_count = 0;
  module->defs_size = 0;
  module->defs = NULL;
#if defined(USE_DYNLINK_NATIVE)
  module->so_handle = NULL;
  module->end_func = NULL;
#endif
  module->is_static = is_static;
  module->is_initialized = FALSE;
  module->enable_goaltrans = FALSE;
  module->timestamp = time(NULL); /* TODO: set the timestamp here or just before finishing? */
#if defined(ABSMACH_OPT__oo_extensions)
  module->instvars_size = 0;
  module->instvars = NULL;
  module->instvars_table = NULL;
  module->instdatas_size = 0;
  module->instdatas = NULL;
  module->instdatas_table = NULL;
  module->objfunctor = ERRORTAG;
#endif
  module->exports = HASHTAB_NEW(2);
  module->defines = HASHTAB_NEW(2);
  module->imports = NULL;
  module->uses_size = 0;
  module->uses = NULL;
#if defined(ABSMACH_OPT__cache_local_resolve)    
  module->local_cache = NULL;
#endif

#define QREADATM(T) \
  if (!CBOOL__SUCCEED_N(qread1,&(T))) goto bad; \
  DEREF((T),(T)); /* TODO: assert T is atom */ \
  G->heap_top = w->choice->heap_top;
#define QREADSMALL(N) { \
  tagged_t t; \
  if (!CBOOL__SUCCEED_N(qread1,&t)) goto bad; \
  DEREF(t,t); /* TODO: assert T is small */ \
  G->heap_top = w->choice->heap_top; \
  (N) = GetSmall(t); \
}

  /* Read the module name clause */
  if (!CBOOL__SUCCEED_N(qread1,&t)) goto no_more;
  DEREF(t,t); /* TODO: assert T is atom */
  G->heap_top = w->choice->heap_top;

  //#define TRACE_MODULE_LOADING 1
#if defined(TRACE_MODULE_LOADING)
  TRACE_PRINTF("reading module %s\n", GetString(t));
#endif
  module->name = t;
#if defined(ABSMACH_OPT__regmod) 
  regmod__mark = t;
#endif

  /* Unload previous module */
  keyval = hashtab_get(modules_location, (hashtab_key_t)module->name);
  if (keyval->key != ERRORTAG) {
    CBOOL__CALL_N(abolish_module, keyval->value.module);
    /* remove entry */
    /* TODO: temporal... */
    keyval->key = ERRORTAG;
    keyval->value.module = NULL;
    modules_location->count--;
  }
  /* Read speckey */
  if (!CBOOL__SUCCEED_N(qread1,&t)) goto bad;
  DEREF(t,t);
  module__speckey = t;
  {
    tagged_t *pt1;
    tagged_t *s;
    /* build '$specmod'(SpecKey, Module) term */
    pt1 = G->heap_top;
    s = pt1;
    HeapPush(pt1,functor_specmod_d);
    HeapPush(pt1,t);
    HeapPush(pt1,module->name);
    G->heap_top=pt1;
    X(0) = Tagp(STR,s);
  }
  X(1) = atom_true;
  if (!CBOOL__SUCCEED_N(add_interpreted, def_specmod_d)) goto bad;
  G->heap_top = w->choice->heap_top;

  /* Read the name of the initialization predicate */
  if (!CBOOL__SUCCEED_N(qread1,&t)) goto no_more;
  DEREF(t,t); /* TODO: assert T is atom */
  G->heap_top = w->choice->heap_top;
#if defined(TRACE_MODULE_LOADING)
  TRACE_PRINTF("reading init_name %s\n", GetString(t));
#endif
  module->init_name = t;

  /* Read the name of the finalization predicate */
  if (!CBOOL__SUCCEED_N(qread1,&t)) goto no_more;
  DEREF(t,t); /* TODO: assert T is atom */
  G->heap_top = w->choice->heap_top;
#if defined(TRACE_MODULE_LOADING)
  TRACE_PRINTF("reading end_name %s\n", GetString(t));
#endif
  module->end_name = t;

  /* Metadata read loop */
  while (1) {
    QREADATM(t);
#if defined(TRACE_MODULE_LOADING)
    TRACE_PRINTF("metadata %s\n", GetString(t));
#endif
    if (t == functor__c) {
      QREADSMALL(defs_size);
      defs = CHECKALLOC_ARRAY(definition_t *, defs_size);
      defs_i = 0;
      module->defs_count = 0;
      module->defs_size = defs_size;
      module->defs = defs;
      break;
    } else if (t == functor__u) {
      intmach_t i;
      intmach_t n;
      QREADSMALL(n);
      module->uses_size = n;
      module->uses = CHECKALLOC_ARRAY(tagged_t, n);
      for (i = 0; i < n; i++) {
	QREADATM(t);
	module->uses[i] = t;
      }
    } else if (t == functor__exports) {
      intmach_t i;
      intmach_t n;
      QREADSMALL(n);
      for (i = 0; i < n; i++) {
	tagged_t f, mf;
	intmach_t a;
	hashtab_node_t *keyval;
	QREADATM(f);
	QREADSMALL(a);
	QREADATM(mf);
	/* Include in the exports table */
	keyval = hashtab_lookup(&module->exports, (hashtab_key_t)SetArity(f, a));
	if (keyval->value.tagged == ERRORTAG) { /* TODO: in lookup key is always initialized */
	  keyval->value.tagged = mf;
	}
      }
    } else if (t == functor__defines) {
      intmach_t i;
      intmach_t n;
      QREADSMALL(n);
      for (i = 0; i < n; i++) {
	tagged_t f, mf;
	intmach_t a;
	hashtab_node_t *keyval;
	QREADATM(f);
	QREADSMALL(a);
	QREADATM(mf);
	/* Include in the defines table */
	keyval = hashtab_lookup(&module->defines, (hashtab_key_t)SetArity(f, a));
	if (keyval->value.tagged == ERRORTAG) {
	  /* TODO: in lookup key is always initialized */
	  keyval->value.tagged = mf;
	}
      }
    } else if (t == functor__imports_all) {
      intmach_t i;
      intmach_t n;
      QREADSMALL(n);
      for (i = 0; i < n; i++) {
	QREADATM(t);
	module_add_imports_all(module, t);
      }
    } else if (t == functor__imports) {
      intmach_t i;
      intmach_t n;
      QREADSMALL(n);
      for (i = 0; i < n; i++) {
	tagged_t f, im, mf;
	intmach_t a;
	QREADATM(f);
	QREADSMALL(a);
	QREADATM(im);
	QREADATM(mf);
	module_add_imports(module, im, f, a, mf);
      }
    } else if (t == functor__meta_args) {
      intmach_t i;
      intmach_t n;
      d = def_meta_args_d;
      QREADSMALL(n);
      for (i = 0; i < n; i++) {
	if (!CBOOL__SUCCEED_N(qread1,&t)) goto bad;
	DEREF(t,t);
	X(0) = t;
	X(1) = atom_true;
	if (!CBOOL__SUCCEED_N(add_interpreted, d)) goto bad;
	G->heap_top = w->choice->heap_top;
      }
    } else if (t == functor__context) {
      intmach_t i;
      intmach_t n;
      d = def_context_d;
      QREADSMALL(n);
      for (i = 0; i < n; i++) {
	if (!CBOOL__SUCCEED_N(qread1,&t)) goto bad;
	DEREF(t,t);
	X(0) = t;
	X(1) = atom_true;
	if (!CBOOL__SUCCEED_N(add_interpreted, d)) goto bad;
	G->heap_top = w->choice->heap_top;
      }
#if defined(ABSMACH_OPT__oo_extensions)    
    } else if (t == functor__instvar) {
      intmach_t i;
      intmach_t n;
      QREADSMALL(n);
#if 0
      TRACE_PRINTF("ivs %d\n", n);
#endif
      module->instvars_size = n;
      module->instvars = CHECKALLOC_ARRAY(tagged_t, n);
      if (module->instvars_table == NULL) module->instvars_table = HASHTAB_NEW(2);
      for (i = 0; i < n; i++) {
	hashtab_node_t *keyval;
	QREADATM(t);
	module->instvars[i] = t;
#if 0
	TRACE_PRINTF("%d: iv - %s -\n", i + 1, GetString(t));
#endif
	/* Include in the instvars table */
	keyval = hashtab_lookup(&module->instvars_table, (hashtab_key_t)t);
	if (keyval->value.raw == 0) {
	  /* TODO: in lookup key is always initialized */
	  keyval->value.raw = i + 1;
	}
      }
    } else if (t == functor__instdata) {
      intmach_t i;
      intmach_t n;
      QREADSMALL(n);
#if 0
      TRACE_PRINTF("ids %d\n", n);
#endif
      module->instdatas_size = n;
      module->instdatas = CHECKALLOC_ARRAY(tagged_t, n);
      if (module->instdatas_table == NULL) module->instdatas_table = HASHTAB_NEW(2);
      for (i = 0; i < n; i++) {
	hashtab_node_t *keyval;
	intmach_t a;
	tagged_t mf;
	QREADATM(t);
	QREADSMALL(a);
	mf = SetArity(t, a);
	module->instdatas[i] = mf;
#if 0
	TRACE_PRINTF("%d: id - %s/%d -\n", i + 1 + module->instvars_size, GetString(t), a);
#endif
	/* Include in the instdatas table */
	keyval = hashtab_lookup(&module->instdatas_table, (hashtab_key_t)mf);
	if (keyval->value.raw == 0) {
	  /* TODO: in lookup key is always initialized */
	  keyval->value.raw = i + 1 + module->instvars_size;
	}
      }
#endif
    } else {
      goto bad;
    }
  }

  /* Read predicates */
  while (1) {
    if (!CBOOL__SUCCEED_N(qread1,&t)) break;
    DEREF(t,t);
    functor = TaggedToHeadfunctor(t);
    if (functor == define_predicate_functor) {
      intmach_t bits;
      DerefArg(X(0),t,1);
      d = resolve_definition(X(0));
      if (d == NULL) goto bad;
      DerefArg(X(1),t,2);
      if (!TaggedIsSmall(X(1))) goto bad;
      bits = GetSmall(X(1));
      if (!CBOOL__SUCCEED_N(predicate_def, d, bits)) goto bad;
      /* Save definition pointer */
      if (defs_i >= defs_size) goto bad; /* panic: specified predicate count exceeded reading bytecode */
      defs[defs_i] = d;
      defs_i++;
    } else if (functor == functor__b2) {
      emul_info_t *ref;
      uintmach_t type;
      tagged_t key;
      tagged_t type_key;
      /* TODO: flat third argument? */
      /* TODO: add d != NULL rt check */
      DerefArg(t1,t,1);
      ref = TaggedToEmul(t1);
      DerefArg(type_key,t,2);
      DerefArg(t1,type_key,1);
      type = GetSmall(t1);
      DerefArg(key,type_key,2);
      if (IsVar(key)) {
	key = ERRORTAG;
      } else if (TaggedIsSTR(key)) {
	key = TaggedToHeadfunctor(key);
      }
      if (!CBOOL__SUCCEED_N(add_bytecode_clause, d, ref, type, key)) goto bad;
    } else if (functor == interpreted_clause_functor) {
      DerefArg(X(0),t,1);
      DerefArg(X(1),t,2);
      goto assertz;
    } else if (functor == interpreted_fact_functor) {
      DerefArg(X(0),t,1);
      X(1) = atom_true;
    assertz:
      if (!CBOOL__SUCCEED_N(add_interpreted, d)) goto bad;
    } else if (functor == end_functor) {
      break;
    } else {
      goto bad;
    }
    G->heap_top = w->choice->heap_top;
  }
  if (native_so_name[0] != 0) { /* name different than "" */
    if (!CBOOL__SUCCEED_N(link_native, module, native_so_name)) goto bad;
  }
 end:
  if (module != NULL) module->defs_count = defs_i; /* TODO: use 'ok'? */

  if (ok && module != NULL) {
#if defined(ABSMACH_OPT__oo_extensions)
    intmach_t instslots;
    instslots = module->instvars_size + module->instdatas_size;
    if (instslots > 0) {
      /* fill objfunctor */
      module->objfunctor = CFUN__EVAL_N(get_objfunctor, module->name, instslots);
#if 0
      TRACE_PRINTF("of %s\n", GetString(module->objfunctor));
#endif
      /* register in the objfunctor table */
      hashtab_node_t *keyval;
      keyval = hashtab_lookup(&objfunctor_table, (hashtab_key_t)module->objfunctor);
      if (keyval->value.module == NULL) {
	keyval->value.module = module;
      }
    }
#endif
  }

  G->heap_top = w->choice->heap_top; // todo: necessary?
#if defined(ABSMACH_OPT__regmod) 
  regmod__mark = ERRORTAG;
#endif
  CVOID__CALL(pop_choicept);

  if (ok == TRUE) {
    /* register if everything went ok */
    keyval->key = module->name;
    keyval->value.module = module;
    modules_location->count+=1;
    if (modules_location->count * 2 > HASHTAB_SIZE(modules_location)) {
      HASHTAB_EXPAND(&modules_location);
    }
  }
  CBOOL__LASTTEST(ok);
 bad:
  /* TODO: use MAJOR_FAULT (not possible right now because we still
     have work to do before failing */
  PANIC_FAULT("unexpected error loading bytecode");
  ok = FALSE;
  goto end;
 no_more:
#if defined(TRACE_MODULE_LOADING)
  TRACE_PRINTF("{trace: no more modules}\n");
#endif
  /* TODO: no more modules, document */
  ok = FALSE;
  goto end;
}

CBOOL__PROTO(prolog_module_timestamp) {
  module_t *module;
  GETMODULE(module, X(0));
  CBOOL__LASTUNIFY(IntmachToTagged(module->timestamp),X(1));
}

/* '$mod_setgoaltrans'(M, Status) - activates or deactivates the goaltrans module flag (when activated, fast pgcall is disabled) */
CBOOL__PROTO(prolog_module_setgoaltrans) {
  module_t *module;
  GETMODULE(module, X(0));
  DEREF(X(1), X(1));
  module->enable_goaltrans = GetSmall(X(1));
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_static_module) {
  module_t *module;
  GETMODULE(module, X(0));
  CBOOL__LASTTEST(module->is_static);
}

CBOOL__PROTO(prolog_is_initialized_module) {
  module_t *module;
  GETMODULE(module, X(0));
  CBOOL__LASTTEST(module->is_initialized);
}

CBOOL__PROTO(prolog_set_initialized_module) {
  module_t *module;
  GETMODULE(module, X(0));
  module->is_initialized = TRUE;
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_load_module) {
  bool_t ok;
  FILE *bytecode;
  char *native_so_name;

  DEREF(X(0), X(0));
  CBOOL__TEST(TaggedIsATM(X(0)));
  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  
  bytecode = fopen(GetString(X(0)), "r");
  CBOOL__TEST(bytecode != NULL);
  
  native_so_name = GetString(X(1));

  CBOOL__CALL_N(qread_begin, bytecode);
  ok = CBOOL__SUCCEED_N(load_module, native_so_name, FALSE);
  CBOOL__CALL(qread_end);
  fclose(bytecode);
  CBOOL__LASTTEST(ok);
}

CBOOL__PROTO(prolog_load_module_pack) {
  /* TODO: see engine__start todo */
  bool_t ok;
  FILE *bytecode;
  DEREF(X(0), X(0));
  CBOOL__TEST(TaggedIsATM(X(0)));
  bytecode = fopen(GetString(X(0)), "r");
  CBOOL__TEST(bytecode != NULL);
  ok = CBOOL__SUCCEED_N(load_module_pack, bytecode);
  fclose(bytecode);
  CBOOL__LASTTEST(ok);
}

/* ------------------------------------------------------------------------- */

#if defined(USE_DYNLINK_NATIVE)
static CVOID__PROTO_N(unload_so, module_t *module);

/* Dynamic linking options.  This is really OS dependant. */
/* And any other object can reference objects in the one we are loading */

#if defined(Solaris) || defined(IRIX)
# if defined(RTLD_PARENT)
#  define LIB_LOADING_TYPE RTLD_LAZY | RTLD_GLOBAL | RTLD_PARENT
# else
#  define LIB_LOADING_TYPE RTLD_LAZY | RTLD_GLOBAL
# endif
#elif defined(LINUX) || defined(DARWIN)
# define LIB_LOADING_TYPE RTLD_LAZY | RTLD_GLOBAL
#elif defined(SunOS4)
# define LIB_LOADING_TYPE 1                          /* SunOS man pages... */
#endif

CBOOL__PROTO_N(link_native, module_t *module, char *so_filename) {
  char libfunction[STATICMAXATOM];
  void *so_handle;
  CVOID__PROTO((*init_func));
  CVOID__PROTO((*end_func));
  char errmsg[1024];

  /* Unload native code */
  CVOID__CALL_N(unload_so, module);

  /* Load dynamic native code */
  DEBUG__TRACE(debug_c, "Loading native code %s\n", so_filename);
  so_handle = dlopen(so_filename, LIB_LOADING_TYPE); /* Open the .so library */

  if (so_handle == NULL) {
    sprintf(errmsg, "dynlink/2: could not load library\n %s", dlerror());
    USAGE_FAULT(errmsg);
  }

  module->so_handle = so_handle;

  /* Get the address of init_func */ 
  sprintf(libfunction, "%s", GetString(module->init_name));
  init_func = (CVOID__PROTO((*)))dlsym(so_handle, libfunction);
  if (init_func == NULL){
    dlclose(so_handle);
    USAGE_FAULT("dynlink/2: could not find initialization function");
  }

  /* Get the address of the end_func */
  sprintf(libfunction, "%s", GetString(module->end_name));
  end_func = (CVOID__PROTO((*)))dlsym(so_handle, libfunction);     
  if (end_func == NULL) {
    dlclose(so_handle);
    USAGE_FAULT("dynlink/2: could not find end function");
  }
  module->end_func = end_func;

  /* Call the init function with the name of the module */
  CVOID__CALL((*init_func));

  CBOOL__PROCEED;
}

CVOID__PROTO_N(unload_so, module_t *module) {
  if (module->so_handle != NULL) {
    DEBUG__TRACE(debug_c, "Unloading native code %s\n", GetString(module->name));
    CVOID__CALL((*module->end_func));
    dlclose(module->so_handle);
    module->so_handle = NULL;
    module->end_func = NULL;
  }
}

#else /* !defined(USE_DYNLINK_NATIVE) */

CBOOL__PROTO(link_native, module_t *module, char *so_filename) {
  PANIC_FAULT("native code loading capabilities are not included in this executable");
  CBOOL__FAIL;
}

#endif

#if defined(ABSMACH_OPT__regmod)
tagged_t regmod__mark = ERRORTAG;
void regmod__clean_trychain(try_node_t **tfirst, tagged_t mark) {
  try_node_t *t;
  try_node_t *last;
  t = *tfirst;
  if (!TRY_NODE_IS_NULL(t)) {
    last = NULL; /* the last seen valid try_node */
    do {
      if (t->clause->mark == mark) {
	try_node_t *tnext;
	tnext = t->next;
	/* free the node */
	CHECKDEALLOC0(try_node_t, t);
	t = tnext; /* advance forward */
      } else {
	/* make links */
	if (last == NULL) {
	  *tfirst = t;
	  /* (*tfirst)->previous update is delayed until we know the
	     last node of the chain */
	} else {
	  last->next = t;
	  t->previous = last;
	}
	/* move forward */
	last = t;
	t = t->next;
      }
    } while (!TRY_NODE_IS_NULL(t));
    
    if (last == NULL) {
      /* there was no valid nodes at all */
      *tfirst = fail_alt;
    } else {
      TRY_NODE_SET_DET(last, last->clause);
      /* update (*tfirst)->previous */
      (*tfirst)->previous = last;
    }
  }
}
void regmod__clean_hash(hashtab_t **psw, tagged_t mask) {
  intmach_t i;
  hashtab_node_t *h1;
  try_node_t *otherwise = NULL;
  intmach_t size = HASHTAB_SIZE(*psw);

  /* TODO: share sw traversal code skeleton with other incore
     operations */
  for (i=0; i<size; i++) {
    h1 = &(*psw)->node[i];
    if (h1->key) {
      regmod__clean_trychain(&h1->value.try_chain,mask);
    } else {
      /* empty entries share the same try_chain */
      if (!otherwise) {
	regmod__clean_trychain(&h1->value.try_chain,mask);
	otherwise = h1->value.try_chain;
      } else {
	h1->value.try_chain = otherwise;
      }
    }
  }
}
CBOOL__PROTO_N(regmod__clean_incore, definition_t *f, tagged_t mark) {
  intmach_t current_mem = total_mem_count;
  incore_info_t *d;
  emul_info_t *c;
  emul_info_t *cc;
  emul_info_t **last;

  d = f->code.incoreinfo;
  regmod__clean_trychain(&d->varcase, mark);
  regmod__clean_trychain(&d->lstcase, mark);
  regmod__clean_hash(&d->othercase, mark);

  /* Remove marked clauses */
  last = &d->clauses;
  c = d->clauses;
  while (c != NULL) {
    if (c->mark == mark) {
      *last = c->next;
      cc = c->next;
      free_emulinfo(c);
      c = cc;
    } else {
      last = &c->next;
      c = c->next;
    }
  }
  d->clauses_tail = last;
  INC_MEM_PROG((total_mem_count - current_mem));

  /* Abolish if empty definition */
  if (d->clauses_tail == &d->clauses) {
    CBOOL__CALL_N(abolish, f);
  }
  CBOOL__PROCEED;
}
CVOID__PROTO_N(prolog_erase_ptr, instance_t *node);
CVOID__PROTO_N(prolog_unlock_predicate, int_info_t *root);
CBOOL__PROTO_N(regmod__clean_interpreted, definition_t *f, tagged_t mark) {
  /* TODO: study if this code is correct --jf */
  /* TODO: what happens with concurrent predicates? */
  int_info_t *root;
  instance_t *i, *j;

  root = f->code.intinfo;
  for (i = root->first; i; i=j) {
    j = i->forward;
    if (i->mark == mark) {
      CVOID__CALL_N(prolog_erase_ptr, i);
    }
  }
  /* TODO: necessary? */
  CVOID__CALL_N(prolog_unlock_predicate, root);

  /* TODO: check if it has any problems... */
  /* Abolish if predicate is empty */
  if (root->first == NULL) {
    CBOOL__CALL_N(abolish, f);
  }
  CBOOL__PROCEED;
}
#endif

CBOOL__PROTO_N(abolish_multifile, definition_t *f, tagged_t mod) {
#if defined(ABSMACH_OPT__regmod)
  if (f->predtyp == ENTER_INTERPRETED) {
    CBOOL__LASTCALL_N(regmod__clean_interpreted, f, mod);
  } else { 
    CBOOL__LASTCALL_N(regmod__clean_incore, f, mod);
  }
#else
  CBOOL__PROCEED;
#endif
}

/* Abolish all the definitions in Ptr */
CBOOL__PROTO_N(abolish_module, module_t *module) {
  intmach_t i;

#if defined(USE_DYNLINK_NATIVE)
  CVOID__CALL_N(unload_so, module);
#endif

#if defined(ABSMACH_OPT__regmod)
  tagged_t mark;
  mark = module->name;

  /* TODO: remove unused entries */
  CBOOL__CALL_N(abolish_multifile, def_specmod_d, mark);
  CBOOL__CALL_N(abolish_multifile, def_meta_args_d, mark);
  CBOOL__CALL_N(abolish_multifile, def_context_d, mark);
#endif

#if defined(ABSMACH_OPT__oo_extensions)
  /* Remove from the objfunctor table */ 
  if (module->objfunctor != ERRORTAG) {
    hashtab_node_t *keyval;
    keyval = hashtab_get(objfunctor_table, (hashtab_key_t)module->objfunctor);
    if (keyval->key) {
      /* remove entry */
      /* TODO: temporal... */
      keyval->key = ERRORTAG;
      keyval->value.module = NULL;
      objfunctor_table->count--;
    }
  }
#endif

  /* Delete all predicate entries */
  for (i = 0; i < module->defs_count; i++) {
    definition_t *f;
    f = module->defs[i];
#if defined(ABSMACH_OPT__regmod)
    if (f->properties.multifile) {
      CBOOL__CALL_N(abolish_multifile, f, mark);
    } else {
#endif
      CBOOL__CALL_N(abolish, f);
#if defined(ABSMACH_OPT__regmod)
    }
#endif
  }
  
  /* Free the module structure */
  if (module->defs != NULL) CHECKDEALLOC0_ARRAY(definition_t *, module->defs, module->defs_size);
  if (module->exports != NULL) hashtab_free(module->exports);
  if (module->defines != NULL) hashtab_free(module->defines);
  if (module->uses != NULL) CHECKDEALLOC0_ARRAY(tagged_t, module->uses, module->uses_size);
#if defined(ABSMACH_OPT__oo_extensions)
  if (module->instvars != NULL) CHECKDEALLOC0_ARRAY(tagged_t, module->instvars, module->instvars_size);
  if (module->instdatas != NULL) CHECKDEALLOC0_ARRAY(tagged_t, module->instdatas, module->instdatas_size);
  if (module->instvars_table != NULL) hashtab_free(module->instvars_table);
  if (module->instdatas_table != NULL) hashtab_free(module->instdatas_table);
#endif
#if defined(ABSMACH_OPT__cache_local_resolve)    
  if (module->local_cache != NULL) hashtab_free(module->local_cache);
#endif

  module_delete_imports(module);
  CHECKDEALLOC0(module_t, module);

  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_abolish_module) {
  module_t *module;
  GETMODULE(module, X(0));
  CBOOL__CALL_N(abolish_module, module);
  /* remove entry */
  /* TODO: do not remove, just mark the module as undefined */
  hashtab_node_t *keyval;
  keyval = hashtab_get(modules_location, (hashtab_key_t)X(0));
  keyval->key = ERRORTAG;
  keyval->value.module = NULL;
  modules_location->count--;
  CBOOL__PROCEED;
}

/* '$mod_exports'(M, F, N, MF) */
CBOOL__PROTO(prolog_module_exports) {
  module_t *module;
  GETMODULE(module, X(0));

  /* Get functor */
  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  DEREF(X(2), X(2));
  CBOOL__TEST(TaggedIsSmall(X(2)));
  tagged_t f = SetArity(X(1), GetSmall(X(2)));

  /* Search the functor */
  hashtab_node_t *keyval;
  keyval = hashtab_get(module->exports, (hashtab_key_t)f);
  CBOOL__TEST(keyval->key != ERRORTAG);
  CBOOL__UnifyCons(keyval->value.tagged, X(3));
  CBOOL__PROCEED;
}

/* '$mod_defines'(M, F, N, MF) */
CBOOL__PROTO(prolog_module_defines) {
  module_t *module;
  GETMODULE(module, X(0));

  /* Get functor */
  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  DEREF(X(2), X(2));
  CBOOL__TEST(TaggedIsSmall(X(2)));
  tagged_t f = SetArity(X(1), GetSmall(X(2)));

  /* Search the functor */
  hashtab_node_t *keyval;
  keyval = hashtab_get(module->defines, (hashtab_key_t)f);
  CBOOL__TEST(keyval->key != ERRORTAG);
  CBOOL__UnifyCons(keyval->value.tagged, X(3));
  CBOOL__PROCEED;
}

/* '$mod_set_defines'(M, F, N, MF) */
CBOOL__PROTO(prolog_module_set_defines) {
  module_t *module;
  GETMODULE(module, X(0));

  /* Get functor */
  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  DEREF(X(2), X(2));
  CBOOL__TEST(TaggedIsSmall(X(2)));
  tagged_t f = SetArity(X(1), GetSmall(X(2)));

  /* Set entry */
  hashtab_node_t *keyval;
  keyval = hashtab_lookup(&module->defines, (hashtab_key_t)f);
  DEREF(X(3), X(3));
  CBOOL__TEST(TaggedIsATM(X(3)));
  keyval->value.tagged = X(3);
  EMPTY_LOCALCACHE;
  CBOOL__PROCEED;
}

/* '$mod_current'(Module) - checks that a module is loaded or
   enumerates all loaded modules */

try_node_t *address_nd_current_module;

CBOOL__PROTO(nd_current_module);
CBOOL__PROTO(current_module) {
  DEREF(X(0),X(0));
  if (!IsVar(X(0))) {
    hashtab_node_t *keyval;
    /* TODO: share module lookup code!!! */
    CBOOL__TEST(TaggedIsATM(X(0)));
    keyval = hashtab_get(modules_location, (hashtab_key_t)X(0));
    CBOOL__LASTTEST(keyval->key != ERRORTAG);
  } else {
    X(1) = MakeSmall(HASHTAB_SIZE(modules_location));
    CVOID__CALL_N(push_choicept,address_nd_current_module);
    CBOOL__LASTCALL(nd_current_module);
  }
}

CBOOL__PROTO(nd_current_module) {
  intmach_t j;
  hashtab_node_t *keyval;
  module_t *module;
  j = GetSmall(X(1));
  for (--j; j>=0; --j) {
    keyval = &modules_location->node[j];
    module = keyval->value.module;
    if (module != NULL) {
      if (j==0) {
	CVOID__CALL(pop_choicept);
      } else {
	w->choice->x[1] = MakeSmall(j);
      }
      CBOOL__UnifyCons(module->name,X(0));
      CBOOL__PROCEED;
    }
  }
  CVOID__CALL(pop_choicept);
  CBOOL__FAIL;
}

/* '$mod_add_uses'(Module, IM) - include IM in the uses list */

CBOOL__PROTO(module_add_uses) {
  intmach_t j;
  tagged_t im;
  module_t *module;
  GETMODULE(module, X(0));

  CBOOL__TEST(module->uses != NULL); /* TODO: why? */

  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  im = X(1);
  
  for (j = 0; j < module->uses_size; j++) {
    if (module->uses[j] == im) {
      CBOOL__PROCEED;
    }
  }
  /* Not found, insert */
  module->uses = CHECKREALLOC0_ARRAY(tagged_t, module->uses, module->uses_size, module->uses_size + 1);
  module->uses[module->uses_size] = im;
  module->uses_size++;
  EMPTY_LOCALCACHE;
  CBOOL__PROCEED;
}

/* '$mod_del_uses'(Module, IM) - remove IM from the uses list */

CBOOL__PROTO(module_del_uses) {
  intmach_t j;
  tagged_t im;
  module_t *module;
  GETMODULE(module, X(0));

  CBOOL__TEST(module->uses != NULL);

  DEREF(X(1), X(1));
  CBOOL__TEST(TaggedIsATM(X(1)));
  im = X(1);
  
  for (j = 0; j < module->uses_size; j++) {
    if (module->uses[j] == im) goto found;
  }
  /* Not found */
  CBOOL__FAIL;

 found:
  for (; j < module->uses_size - 1; j++) {
    module->uses[j] = module->uses[j + 1];
  }
  module->uses = CHECKREALLOC0_ARRAY(tagged_t, module->uses, module->uses_size, module->uses_size - 1);
  module->uses_size--;
  EMPTY_LOCALCACHE;
  CBOOL__PROCEED;
}

/* '$mod_uses'(Module) - checks that a module is loaded or
   enumerates all loaded modules. note: predicate not indexed */

try_node_t *address_nd_module_uses;

CBOOL__PROTO(nd_module_uses);
CBOOL__PROTO(module_uses) {
  X(2) = MakeSmall(0);
  CVOID__CALL_N(push_choicept,address_nd_module_uses);
  CBOOL__LASTCALL(nd_module_uses);
}

CBOOL__PROTO(nd_module_uses) {
  intmach_t j;
  module_t *module;
  GETMODULE(module, X(0));

  CBOOL__TEST(module->uses != NULL);

  j = GetSmall(X(2));
  if (j>=module->uses_size - 1) {
    CVOID__CALL(pop_choicept);
  } else {
    w->choice->x[2] = MakeSmall(j + 1);
  }
  CBOOL__TEST(j < module->uses_size);

  DEREF(X(1),X(1));
  CBOOL__UnifyCons(module->uses[j],X(1));
  CBOOL__PROCEED;
}

/*------------------------------------------------------------------------- */
/* TODO: find a better place to put this predicate? (rt_exp or
   hiord_rt or basiccontrol?) */

CINSNP__PROTO(code_call1) {
  definition_t *d;
  tagged_t t0;
  tagged_t *args;
  tagged_t *pt1;
  t0 = X(0);
  DerefVar(t0);
  X(0) = t0;
  d = find_definition(t0,&args,FALSE);
  if (d == NULL) {
    d = address_undefined_goal;
  } else {
    t0 = FuncArity(d);
    if (t0) {
      pt1 = G->x;
      do {
        *pt1++ = *args++;
      } while (--t0);
    }
  }
  return DEF_INSNP(d);
}

/* '$xcall'(A, M, MF) is equivalent to
   A=..[_|Args],NA=..[MF|Args],'$meta_call'(NA)
 */

CINSNP__PROTO(prolog_xcall) {
  definition_t *d;
  tagged_t fa;
  tagged_t mf;
  tagged_t term;
  tagged_t *args;
  tagged_t *pt1;
  intmach_t a;

  module_t *module;
  tagged_t im;
  
  /* TODO: redefine this predicate as an engine instruction and include
     the module pointer with a relocation */

  GETMODULE(module, X(1));
  if (module->enable_goaltrans) goto slowpgcall;

  im = atom_dash;

  term = X(0);
  DerefSw_HVAorCVAorSVA_Other(term,{goto undefined;},{});
  X(0) = term;
 again:
  SwGoalTerm(f, term, { /* STR(blob) */
    goto undefined;
  }, { /* STR(struct) */
    fa = f;
    args = TaggedToArg(term,1);
    a = Arity(fa);
  }, { /* LST */
    fa = functor_lst;
    args = TagpPtr(LST, term);
    a = 2;
  }, { /* ATM */
    fa = term;
    args = NULL;
    a = 0;
  }, { /* Other */
    goto undefined;
  });

  if (fa == functor__meta) { /* '$:'(A) */
    DerefArg(term, term, 1);
    SwGoalTerm(f, term, { /* STR(blob) */
      goto undefined;
    }, { /* STR(struct) */
      fa = f;
      args = TaggedToArg(term,1);
      a = Arity(fa);
    }, { /* LST */
      fa = functor_lst;
      args = TagpPtr(LST, term);
      a = 2;
    }, { /* ATM */
      fa = term;
      args = NULL;
      a = 0;
    }, { /* Other */
      goto undefined;
    });
#if defined(ABSMACH_OPT__functor_table)
  /* TODO: optimize */
    d = TaggedToFunc(fa);
#else
    d = insert_definition(fa,FALSE);
#endif
    goto putargs_and_call;
  } else if (fa == functor__colon) { /* QM:A */
    DerefArg(im, term, 1);
    DerefArg(term, term, 2);
    if (IsVar(im)) im = atom_dash;
    /* TODO: this recursion is too wide... qm:qm:qm... should not be allowed */
    if (!TaggedIsATM(im)) goto undefined;
    goto again;
  }

  if (!CBOOL__SUCCEED_N(module_resolve, module, im, fa, &mf))
    goto slowpgcall;

#if defined(ABSMACH_OPT__functor_table)
  /* TODO: optimize when using functor table */
  d = TaggedToFunc(SetArity(mf,a));
#else
  d = insert_definition(SetArity(mf,a),FALSE);
#endif
  if (d == NULL) goto slowpgcall; /* may require a context expansion */
  if (d->properties.hardrtexp) goto slowpgcall; /* cannot be called directly by pgcall */
 putargs_and_call:
  if (a) {
    pt1 = G->x;
    do {
      *pt1++ = *args++;
    } while (--a);
  }
  return DEF_INSNP(d);
 undefined:
  return DEF_INSNP(address_undefined_goal);
 slowpgcall:
  return DEF_INSNP(def_slowpgcall);
}

/*------------------------------------------------------------------------- */

#if defined(ABSMACH_OPT__oo_extensions)
/* TODO: not really necessary */
CFUN__PROTO_N(get_objfunctor, tagged_t, tagged_t module_name, intmach_t arity) {
  CFUN__PROCEED(SetArity(module_name, arity));
}
#endif

#if defined(ABSMACH_OPT__oo_extensions)

#define CBOOL__MODULE_V(MODULE, ATT, I) { \
  tagged_t t1; \
  hashtab_node_t *keyval; \
  DEREF(t1, (ATT)); \
  CBOOL__TEST(TaggedIsATM(t1)); \
  keyval = hashtab_get((MODULE)->instvars_table, (hashtab_key_t)t1); \
  CBOOL__TEST(keyval->key != ERRORTAG); \
  (I) = keyval->value.raw; \
}

CBOOL__PROTO(prolog_class_v) {
  module_t *module;
  intmach_t i;
  OOGETCLASS(module, X(0));
  CBOOL__MODULE_V(module, X(1), i);
  CBOOL__LASTUNIFY(MakeSmall(i), X(2));
}

CBOOL__PROTO(prolog_class_i) {
  module_t *module;
  tagged_t t1;
  intmach_t i;
  hashtab_node_t *keyval;

  OOGETCLASS(module, X(0));

  DEREF(t1,X(1));
  if (TaggedIsATM(t1)) {
  } else if (TaggedIsSTR(t1)) {
    t1 = TaggedToHeadfunctor(t1);
  } else {
    CBOOL__FAIL;
  }
  keyval = hashtab_get(module->instdatas_table, (hashtab_key_t)t1);
  CBOOL__TEST(keyval->key != ERRORTAG);
  i = keyval->value.raw;

  CBOOL__LASTUNIFY(MakeSmall(i), X(2));
}

CBOOL__PROTO(prolog_attrtype_v) {
  module_t *module;
  tagged_t t1;
  intmach_t i;

  OOGETCLASS(module, X(0));

  DEREF(t1,X(1));
  CBOOL__TEST(TaggedIsSmall(t1));
  i = GetSmall(t1) - 1;
  CBOOL__TEST(i >= 0 && i < module->instvars_size);
  t1 = module->instvars[i];
  CBOOL__LASTUNIFY(t1, X(2));
}

CBOOL__PROTO(prolog_attrtype_i) {
  module_t *module;
  tagged_t t1;
  intmach_t i;

  OOGETCLASS(module, X(0));

  DEREF(t1,X(1));
  CBOOL__TEST(TaggedIsSmall(t1));
  i = GetSmall(t1) - module->instvars_size - 1;
  CBOOL__TEST(i >= 0 && i < module->instdatas_size);
  t1 = module->instdatas[i];
  CBOOL__UNIFY(FUNCTOR_NAME(t1), X(2));
  CBOOL__LASTUNIFY(MakeSmall(Arity(t1)), X(3));
}

CBOOL__PROTO(prolog_ooget) {
  module_t *module;
  intmach_t i;
  tagged_t t0;
  OOGETCLASS(module, MTHIS);
  CBOOL__MODULE_V(module, MX0, i);
  /* TODO: put assertions in debug mode to check boundaries, etc. */
  DEREF(t0, MTHIS); /* TODO: cache deref MTHIS */
  t0 = *TaggedToArg(t0, i);
  CBOOL__LASTUNIFY(t0, MX1);
}

CBOOL__PROTO(prolog_ooget2) {
  module_t *module;
  intmach_t i;
  tagged_t t0;
  OOGETCLASS(module, X(0));
  CBOOL__MODULE_V(module, X(1), i);
  /* TODO: put assertions in debug mode to check boundaries, etc. */
  DEREF(t0, X(0)); /* TODO: cache deref MTHIS */
  t0 = *TaggedToArg(t0, i);
  CBOOL__LASTUNIFY(t0, X(2));
}

#endif
