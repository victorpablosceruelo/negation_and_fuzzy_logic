/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */


#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <dlfcn.h>


/* INITIALIZATIONS   and support functions for initializations  */

#include "threads.h"
#include "datadefs.h"
#include "support.h"
#include "predtyp.h"
#include "wambuiltin.h"
#include "wamfunction.h"
#include "task_areas.h"
#include "instrdefs.h"

/* declarations for global functions accessed here */

#include "initial_defs.h"
#include "alloc_defs.h"
#include "interrupt_defs.h"
#include "unix_utils_defs.h"
#include "term_support_defs.h"
#include "support_defs.h"
#include "timing_defs.h"
#include "locks_defs.h"
#include "streams_defs.h"
#include "indexing_defs.h"                               /* JFMC: abolish */
#include "profile_defs.h"

/* local declarations */

static bool_t prolog_atom_mode(Argdecl);
static definition_t *define_builtin(char *pname, int instr, int arity);
static void classify_atom(atom_t *s);
static bool_t prolog_ciao_lib_dir(Argdecl);
static bool_t prolog_ciao_c_headers_dir(Argdecl);
static void deffunction(char *atom, int arity, CInfo proc, int funcno);
static void define_functions(void);


/*
  void compute_cwd();
  void control_c_normal PROTO((Argdecl));
  void init_streams_each_time();
  bool_t prolog_init_radix();
  void enable_conditions();
*/

statistics_t ciao_statistics = {
  0, /*ENG_FLT ss_tick*/
  0, /*ENG_INT ss_global*/
  0, /*ENG_INT ss_local*/
  0, /*ENG_INT ss_control*/
  0, /*ENG_LINT gc_tick*/
  0, /*ENG_INT gc_count*/
  0, /*ENG_INT gc_acc*/

  0, /*ENG_LINT starttick*/
  0, /*ENG_LINT lasttick*/

  /* keep in mind that the frequency values can be redefined to better
     values */

  0, /*ENG_LINT startwalltick*/
  0, /*ENG_LINT lastwalltick*/
  0, /*ENG_LINT wallclockfreq*/

  0, /*ENG_LINT startusertick*/
  0, /*ENG_LINT lastusertick*/
  0, /*ENG_LINT userclockfreq*/

  0, /*ENG_LINT startsystemtick*/
  0, /*ENG_LINT lastsystemtick*/
  0  /*ENG_LINT systemclockfreq*/

};                                /* Shared */

sw_on_key_node_t **atmtab; /* Shared --- but need lock when accessing /
                                   reallocing it! */
sw_on_key_t *ciao_atoms;  /* Shared -- need lock when adding atoms */
static char                  /* Shared -- for adding new atoms; need lock */
  *prolog_chars=NULL,
  *prolog_chars_end=NULL;

CInfo builtintab[64];                                           /* Shared */


                /* Shared -- related with the hashing of arith. functions */
sw_on_key_t *switch_on_function;

#if defined(MARKERS)
extern bool_t prolog_launch_goal PROTO((worker_t *w));
extern bool_t prolog_wait_for_goal PROTO((worker_t *w));
extern bool_t prolog_fail_goal PROTO((worker_t *w));
extern bool_t prolog_redo_goal PROTO((worker_t *w));
#endif

extern worker_t *create_wam_storage PROTO((void));
extern void numstack_init PROTO((Argdecl));

extern bool_t prolog_bootversion PROTO((worker_t *w));
extern bool_t prolog_sourcepath PROTO((worker_t *w));
extern bool_t prolog_open PROTO((worker_t *w));
extern bool_t prolog_close PROTO((worker_t *w));
extern bool_t prolog_unix_popen PROTO((worker_t *w));
extern bool_t prolog_pipe PROTO((worker_t *w));

extern bool_t set_predtrace PROTO((worker_t *w));

extern bool_t prolog_display PROTO((worker_t *w));
extern bool_t prolog_display2 PROTO((worker_t *w));
extern bool_t prolog_displayq PROTO((worker_t *w));
extern bool_t prolog_displayq2 PROTO((worker_t *w));
extern bool_t prolog_stream_code PROTO((worker_t *w));
extern bool_t prolog_current_input PROTO((worker_t *w));
extern bool_t prolog_set_input PROTO((worker_t *w));
extern bool_t prolog_current_output PROTO((worker_t *w));
extern bool_t prolog_set_output PROTO((worker_t *w));
extern bool_t character_count PROTO((worker_t *w));
extern bool_t line_position PROTO((worker_t *w));
extern bool_t line_count PROTO((worker_t *w));
extern bool_t code_class PROTO((worker_t *w));
extern bool_t flush_output PROTO((worker_t *w));
extern bool_t flush_output1 PROTO((worker_t *w));
extern bool_t getct PROTO((worker_t *w));
extern bool_t getct1 PROTO((worker_t *w));
extern bool_t get PROTO((worker_t *w));
extern bool_t get2 PROTO((worker_t *w));
extern bool_t get1 PROTO((worker_t *w));
extern bool_t get12 PROTO((worker_t *w));
extern bool_t peek PROTO((worker_t *w));
extern bool_t peek2 PROTO((worker_t *w));
extern bool_t nl PROTO((worker_t *w));
extern bool_t nl1 PROTO((worker_t *w));
extern bool_t put PROTO((worker_t *w));
extern bool_t put2 PROTO((worker_t *w));
extern bool_t skip PROTO((worker_t *w));
extern bool_t skip2 PROTO((worker_t *w));
extern bool_t skip_line PROTO((worker_t *w));
extern bool_t skip_line1 PROTO((worker_t *w));
extern bool_t tab PROTO((worker_t *w));
extern bool_t tab2 PROTO((worker_t *w));
extern bool_t prolog_clearerr PROTO((worker_t *w));
extern bool_t prolog_erase PROTO((worker_t *w));
extern bool_t prolog_purge PROTO((worker_t *w));
extern bool_t prolog_ptr_ref PROTO((worker_t *w));
extern bool_t inserta PROTO((worker_t *w));
extern bool_t insertz PROTO((worker_t *w));
extern bool_t make_bytecode_object PROTO((worker_t *w));
extern bool_t prolog_name PROTO((worker_t *w));
extern bool_t prolog_atom_codes PROTO((worker_t *w));
extern bool_t prolog_number_codes_2 PROTO((worker_t *w));
extern bool_t prolog_number_codes_3 PROTO((worker_t *w));
extern bool_t prolog_atom_length PROTO((worker_t *w));
extern bool_t prolog_sub_atom PROTO((worker_t *w));
extern bool_t prolog_atom_concat PROTO((worker_t *w));
extern bool_t prolog_copy_term PROTO((worker_t *w));
extern bool_t prolog_copy_term_nat PROTO((worker_t *w));
extern bool_t context_switch PROTO((worker_t *w));
extern bool_t prolog_abolish PROTO((worker_t *w)); /* JFMC (prolog_) */
extern bool_t erase_clause PROTO((worker_t *w));
extern bool_t clause_number PROTO((worker_t *w));
extern bool_t compiled_clause PROTO((worker_t *w));
extern bool_t define_predicate PROTO((worker_t *w));
extern bool_t empty_gcdef_bin PROTO((worker_t *w));
extern bool_t metachoice PROTO((worker_t *w));
extern bool_t metacut PROTO((worker_t *w));
extern bool_t retry_cut PROTO((worker_t *w));
extern bool_t frozen PROTO((worker_t *w));
extern bool_t defrost PROTO((worker_t *w));
extern bool_t setarg PROTO((worker_t *w));
extern bool_t undo PROTO((worker_t *w));
extern bool_t prolog_qread PROTO((worker_t *w));
extern bool_t push_qlinfo PROTO((worker_t *w));
extern bool_t pop_qlinfo PROTO((worker_t *w));
extern bool_t prolog_new_atom PROTO((worker_t *w));
extern bool_t set_glv PROTO((worker_t *w));
extern bool_t get_glv PROTO((worker_t *w));
extern bool_t prolog_global_vars_set_root PROTO((worker_t *w));
extern bool_t prolog_global_vars_get_root PROTO((worker_t *w));
extern bool_t prolog_erase_atom PROTO((worker_t *w));
extern bool_t prolog_current_executable PROTO((worker_t *w));
extern bool_t prompt PROTO((worker_t *w));
extern bool_t unknown PROTO((worker_t *w));
extern bool_t constraint_list PROTO((worker_t *w));
extern bool_t prolog_eq PROTO((worker_t *w));
extern bool_t prolog_repeat PROTO((worker_t *w));
extern bool_t current_atom PROTO((worker_t *w));
extern bool_t current_clauses PROTO((worker_t *w));
extern bool_t close_predicate PROTO((worker_t *w));
extern bool_t open_predicate PROTO((worker_t *w));
extern bool_t first_instance PROTO((worker_t *w));
extern bool_t current_stream PROTO((worker_t *w));
extern bool_t current_predicate PROTO((worker_t *w));
extern bool_t predicate_property PROTO((worker_t *w));
/* extern bool_t prolog_walltime PROTO((worker_t *w)); */
extern bool_t prolog_datime PROTO((worker_t *w));
extern bool_t prolog_time PROTO((worker_t *w));
extern bool_t termheap_usage PROTO((worker_t *w));
extern bool_t envstack_usage PROTO((worker_t *w));
extern bool_t trail_usage PROTO((worker_t *w));
extern bool_t choice_usage PROTO((worker_t *w));
extern bool_t statistics PROTO((worker_t *w));
extern bool_t program_usage PROTO((worker_t *w));
extern bool_t internal_symbol_usage PROTO((worker_t *w));
extern bool_t total_usage PROTO((worker_t *w));
extern bool_t stack_shift_usage PROTO((worker_t *w));
/* extern bool_t leash_mode PROTO((worker_t *w)); */ /* removed (DCG) */
/* extern bool_t maxdepth PROTO((worker_t *w)); */
/* extern bool_t printdepth PROTO((worker_t *w)); */
/* extern bool_t breaklevel PROTO((worker_t *w)); */
extern bool_t compiling PROTO((worker_t *w));
extern bool_t ferror_flag PROTO((worker_t *w));
/*extern bool_t single_var_flag PROTO((worker_t *w));*/
/* extern bool_t character_escapes_flag PROTO((worker_t *w)); */
/* extern bool_t redefine_flag PROTO((worker_t *w)); */
extern bool_t quiet_flag PROTO((worker_t *w));
extern bool_t spypoint PROTO((worker_t *w));
extern bool_t debugger_state PROTO((worker_t *w));
extern bool_t debugger_mode PROTO((worker_t *w));
extern bool_t prolog_show_nodes PROTO((worker_t *w));
extern bool_t prolog_show_all_nodes PROTO((worker_t *w));
extern bool_t start_node PROTO((worker_t *w));
extern bool_t prolog_radix PROTO((worker_t *w));
extern bool_t gc_mode PROTO((worker_t *w));
extern bool_t gc_trace PROTO((worker_t *w));
extern bool_t gc_margin PROTO((worker_t *w));
extern bool_t gc_usage PROTO((worker_t *w));
extern bool_t gc_start PROTO((worker_t *w));


extern bool_t prolog_using_windows PROTO((worker_t *w));
extern bool_t prolog_unix_cd PROTO((worker_t *w));
extern bool_t prolog_unix_shell0 PROTO((worker_t *w));
extern bool_t prolog_unix_shell2 PROTO((worker_t *w));
extern bool_t prolog_unix_system2 PROTO((worker_t *w));
extern bool_t prolog_exec PROTO((worker_t *w));
extern bool_t prolog_wait PROTO((worker_t *w));
extern bool_t prolog_unix_argv PROTO((worker_t *w));
extern bool_t prolog_unix_exit PROTO((worker_t *w));
extern bool_t prolog_unix_mktemp PROTO((worker_t *w));
extern bool_t prolog_unix_access PROTO((worker_t *w));
extern bool_t prolog_directory_files PROTO((worker_t *w));
extern bool_t prolog_file_properties PROTO((worker_t *w));
extern bool_t prolog_touch PROTO((worker_t *w));
extern bool_t prolog_unix_chmod PROTO((worker_t *w));
extern bool_t prolog_unix_umask PROTO((worker_t *w));
extern bool_t prolog_unix_delete PROTO((worker_t *w));
extern bool_t prolog_unix_rename PROTO((worker_t *w));
extern bool_t prolog_unix_mkdir PROTO((worker_t *w));
extern bool_t prolog_current_host PROTO((worker_t *w));
extern bool_t prolog_c_get_env PROTO((worker_t *w));
extern bool_t prolog_c_set_env PROTO((worker_t *w));
extern bool_t prolog_c_del_env PROTO((worker_t *w));
extern bool_t prolog_c_current_env PROTO((worker_t *w));
extern bool_t prolog_c_errno PROTO((worker_t *w));
extern bool_t prolog_c_strerrno PROTO((worker_t *w));
extern bool_t prolog_c_copy_file PROTO((worker_t *w));
extern bool_t prolog_c_winpath PROTO((worker_t *w));
extern bool_t prolog_c_posixpath PROTO((worker_t *w));
extern bool_t prolog_c_winfile PROTO((worker_t *w));
extern bool_t prolog_c_posixfile PROTO((worker_t *w));
/* extern bool_t prolog_pause PROTO((worker_t *w)); */
/* extern bool_t prolog_getpid PROTO((worker_t *w)); */


/*
extern bool_t prolog_load_foreign_files PROTO((worker_t *w));
extern bool_t prolog_prepare_foreign_files PROTO((worker_t *w));
extern bool_t prolog_foreign_base PROTO((worker_t *w));
*/
extern bool_t prolog_find_file            PROTO((worker_t *w));
extern bool_t prolog_path_is_absolute     PROTO((worker_t *w));
extern bool_t prolog_expand_file_name     PROTO((worker_t *w));
extern bool_t prolog_format_print_integer PROTO((worker_t *w));
extern bool_t prolog_format_print_float   PROTO((worker_t *w));
extern bool_t prolog_save                 PROTO((worker_t *w));
extern bool_t prolog_savecode             PROTO((worker_t *w));
#if defined(OLD_DATABASE)
extern bool_t current_key                 PROTO((worker_t *w));
#endif
extern bool_t large_data                  PROTO((worker_t *w));
extern bool_t prolog_interpreted_clause   PROTO((worker_t *w));
extern bool_t set_property                PROTO((worker_t *w));
extern bool_t prolog_address              PROTO((worker_t *w));

#if defined(UNDEFINED)
extern bool_t prolog_launch_goal_plus     PROTO((worker_t *w));
#endif


extern bool_t prolog_eng_call             PROTO((worker_t *w));
extern bool_t prolog_eng_backtrack        PROTO((worker_t *w));
extern bool_t prolog_eng_cut              PROTO((worker_t *w));


#if defined(UNDEFINED)
extern bool_t prolog_launch_goal          PROTO((worker_t *w));
extern bool_t prolog_launch_goal_shv      PROTO((worker_t *w));
extern bool_t prolog_launch_goal_cont     PROTO((worker_t *w));
#endif
extern bool_t prolog_tasks_status         PROTO((worker_t *w));
extern bool_t prolog_kill_thread          PROTO((worker_t *w));
extern bool_t prolog_kill_other_threads   PROTO((worker_t *w));
extern bool_t prolog_thread_self          PROTO((worker_t *w));
extern bool_t prolog_join_goal            PROTO((worker_t *w));
#if defined(UNDEFINED)
extern bool_t prolog_backtrack_goal       PROTO((worker_t *w));
#endif
extern bool_t prolog_release_goal         PROTO((worker_t *w));



extern bool_t prolog_lock_atom            PROTO((worker_t *w));
extern bool_t prolog_unlock_atom          PROTO((worker_t *w));
extern bool_t prolog_lock_atom_state      PROTO((worker_t *w));
extern bool_t prolog_unlock_predicate     PROTO((worker_t *w));

#if defined(GAUGE)
extern bool_t emulated_clause_counters    PROTO((worker_t *w));
extern bool_t counter_values              PROTO((worker_t *w));
extern bool_t reset_counters              PROTO((worker_t *w));
#endif

extern bool_t prolog_runtick              PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_usertick             PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_systemtick           PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_walltick             PROTO((worker_t *w)); /* EMM */

extern bool_t prolog_runtime              PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_usertime             PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_systemtime           PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_walltime             PROTO((worker_t *w)); /* MCL */

extern bool_t prolog_pause                PROTO((worker_t *w)); /* MCL */
extern bool_t prolog_getpid               PROTO((worker_t *w)); /* MCL */
extern bool_t prolog_getuid               PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_getgid               PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_getpwnam             PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_getgrnam             PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_getarch              PROTO((worker_t *w)); /* MCL */
extern bool_t prolog_getos                PROTO((worker_t *w)); /* MCL */
extern bool_t prolog_getdebug             PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_get_eng_location     PROTO((worker_t *w)); /* EMM */
extern bool_t prolog_version              PROTO((worker_t *w)); /* DCG */

extern bool_t prolog_dynlink              PROTO((worker_t *w)); /* MCL */
extern bool_t prolog_dynunlink            PROTO((worker_t *w)); /* JFMC */

extern bool_t prolog_fast_read_in_c       PROTO((worker_t *w)); /* OPA */
extern bool_t prolog_fast_write_in_c      PROTO((worker_t *w)); /* OPA */

extern bool_t compressLZ                  PROTO((worker_t *w)); /* OPA */
extern bool_t copyLZ                      PROTO((worker_t *w)); /* OPA */

extern bool_t cinstance                   PROTO((worker_t *w));
extern bool_t cground                     PROTO((worker_t *w));

#if defined(USE_OVERFLOW_EXCEPTIONS)
extern bool_t undo_heap_overflow_excep PROTO((worker_t *w));
#endif
extern bool_t heap_limit PROTO((worker_t *w));


/* GLOBAL DATA STRUCTURES */
bool_t stop_on_pred_calls     = FALSE;        /* profile or trace -- Shared */
bool_t predtrace              = FALSE;   /* trace predicate calls -- Shared */
#if defined(DEBUG)
bool_t debug_gc               = FALSE;/* debug garbage collection -- Shared */
bool_t debug_threads          = FALSE;   /* debug thread creation -- Shared */
bool_t debug_choicepoints     = FALSE;/* debug choicepoints state -- Shared */
bool_t debug_concchoicepoints = FALSE; /* debug conc. chpt. state -- Shared */
bool_t debug_mem              = FALSE; /* debug memory manegement -- Shared */
bool_t debug_conc             = FALSE;       /* debug concurrency -- Shared */
bool_t debug_c                = FALSE;    /* debugging C linking  -- Shared */
#endif
bool_t profile                = FALSE;       /* profile execution -- Shared */
bool_t profile_eng            = FALSE;
bool_t profile_rcc            = FALSE;
ENG_LINT bytecodes_read       = 0;
ENG_LINT bytecodes_write      = 0;
                               /* byte code instructions executed -- Shared */
#if defined(PROFILE)

void profile__hook_noop(worker_t *w){};
void profile__hook_call_noop(worker_t *w, definition_t *f){};

void (*profile__hook_fail)(worker_t *w)                  = profile__hook_noop;
void (*profile__hook_redo)(worker_t *w)                  = profile__hook_noop;
void (*profile__hook_cut)(worker_t *w)                   = profile__hook_noop;
void (*profile__hook_call)(worker_t *w, definition_t *f) = profile__hook_call_noop;

# if defined(PROFILE__TRACER)
void (*profile__hook_proceed)(worker_t *w)               = profile__hook_noop;
void (*profile__hook_neck_proceed)(worker_t *w)          = profile__hook_noop;
# endif

#endif

/* Shared? Not easy: they have to do with the lifetime of dyn. predicates  */
instance_clock_t def_clock = 0, use_clock = 0;

/* tagged_t *heap_start , *heap_end, *heap_warn, *heap_warn_soft, stack_start,
   *stack_end, *stack_warn, tagged_choice_start, choice_start, choice_end,
   *trail_start, *trail_end; */

/* char *atom_buffer; */ /* Non shared */
/* int atom_buffer_length; */ /* Non shared */

sw_on_key_t  *prolog_predicates = NULL;                    /* Shared */
/*sw_on_key_t *user_predicates = NULL;*/
sw_on_key_t **predicates_location = &prolog_predicates;    /* Shared */

#if defined(THREADS) && defined(USE_POSIX_THREADS)
pthread_attr_t detached_thread;
pthread_attr_t joinable_thread;
#endif

/* All-predicates lock; this might be held for a long time */
SLOCK    prolog_predicates_l;

/* Memory management lock (for internal structures) */
SLOCK    mem_mng_l;

/* We are giving worker IDs from a pool, in order to speed up task
   creation. This pool needs exclusive access. */
SLOCK    worker_id_pool_l;

/* Count for new atom names; if accessed concurrently, we want no repeated
   identifiers.  */
SLOCK    atom_id_l;

/* The creation of new streams should be atomic. */
LOCK stream_list_l;

/* Counts mutually exclusive operations */
#if defined(DBG)
SLOCK ops_counter_l;
#endif

bool_t wam_initialized = FALSE;

/* Event Tracing Flags etc */

#if defined(ANDPARALLEL) || defined(PARBACK)
nagents = 1;
SLOCK nagents_l;
#endif

#if defined(ANDPARALLEL)
// Statistics
int npargoals = 0;
int_par_t *npargoalstaken = NULL;
int_par_t *nlocalbacktr = NULL;
int_par_t *nrembacktr_top = NULL;
int_par_t *nrembacktr_trapped = NULL;
SLOCK npargoals_l;
SLOCK npargoalstaken_l;
SLOCK nlocalbacktr_l;
SLOCK nrembacktr_top_l;
SLOCK nrembacktr_trapped_l;
bool_t measure = TRUE;
#endif

#if defined(ANDPARALLEL) && defined(VISANDOR)
int nacagents = 1;
int maxevents = 20000;  /* maximum number of events (20,000 is default) */
bool_t gen_event_file = FALSE;
float time_at_event_start = 0.0;
#endif

/* support for tokenizer and for writing out atoms */

#define IsLowerChar(X)  (symbolchar[X]==1)
#define IsUpperChar(X)  (symbolchar[X]==2)
#define IsDigit(X)      (symbolchar[X]==3)
#define IsSymbolChar(X) (symbolchar[X]==4)

/* Shared .... */

char symbolchar[256];

#if defined(MARKERS)
 tagged_t atom_success;
 tagged_t atom_failure;
#endif


 tagged_t atom_share;             /* "share" */
 tagged_t atom_noshare;           /* "noshare" */
 tagged_t atom_nil;		/* "[]" */
 tagged_t atom_list;		/* "." */
 tagged_t atom_read;		/* "read"  */
 tagged_t atom_write;		/* "write" */
 tagged_t atom_append;		/* "append" */
 tagged_t atom_socket;		/* "socket" */
 tagged_t atom_symlink;		/* "symlink" */
 tagged_t atom_regular;		/* "regular" */
 tagged_t atom_directory;		/* "directory" */
 tagged_t atom_fifo;		/* "fifo" */
 tagged_t atom_unknown;		/* "unknown" */
 tagged_t atom_prolog;		/* "prolog"  */
 tagged_t atom_lessthan;		/* "<" */
 tagged_t atom_greaterthan;	/* ">" */
 tagged_t atom_equal;		/* "=" */
 tagged_t atom_off;		/* "off" */
 tagged_t atom_on;		/* "on" */
 tagged_t atom_error;		/* "error" */
 tagged_t atom_trace;		/* "trace" */
 tagged_t atom_debug;		/* "debug" */
 tagged_t atom_fail;		/* "fail" */
 tagged_t atom_all;		/* "all" */
 tagged_t atom_terse;		/* "terse" */
 tagged_t atom_verbose;		/* "verbose" */
 tagged_t atom_compiled;		/* "compiled" */
 tagged_t atom_interpreted;	/* "interpreted" */
 tagged_t atom_builtin;		/* "built_in" */
 tagged_t atom_true;		/* "true" */
 tagged_t atom_false;		/* "false" */
 tagged_t atom_retry_hook;	/* "$$retry_hook" */
 tagged_t atom_unprofiled;	/* "unprofiled" */
 tagged_t atom_profiled;   	/* "profiled" */
/* tagged_t atom_public; */		/* "public" */
 tagged_t atom_concurrent;	/* "concurrent" */
 tagged_t atom_wait;		/* "wait" */
 tagged_t atom_dynamic;		/* "dynamic" */
 tagged_t atom_multifile;		/* "multifile" */
 tagged_t atom_default_lib_dir;
 tagged_t atom_default_c_headers_dir;

 tagged_t atom_block;      	/* "block" */
 tagged_t atom_no_block;	        /* "no_block" */


 tagged_t atom_self;                   /* "self" */
 tagged_t atom_create;                 /* "create" */

#if defined(GAUGE)
 tagged_t atom_counter;           /* "counter" */
#endif

#if defined(USE_OVERFLOW_EXCEPTIONS)
  tagged_t atom_undo_heap_overflow_excep;
#endif
  tagged_t atom_heap_limit;

 tagged_t functor_neck;
 tagged_t functor_list;
 tagged_t functor_cut;
 tagged_t functor_minus;
 tagged_t functor_slash;
 tagged_t functor_and;
 tagged_t functor_functor;
 tagged_t functor_tagged;
 tagged_t functor_emul_entry;
 tagged_t functor_builtin;
 tagged_t functor_Dref;
 tagged_t functor_Dstream;
 tagged_t functor_Dlock;
 tagged_t functor_Dhandler;
 tagged_t functor_Dsetarg;
 tagged_t functor_large;
 tagged_t functor_long;


 tagged_t functor_active;
 tagged_t functor_pending;
 tagged_t functor_failed;
 tagged_t functor_available;



 tagged_t current_prompt;
 tagged_t current_unknown;
/*  tagged_t current_leash_mode; */
/*  tagged_t current_maxdepth; */
/*  tagged_t current_printdepth; */
/*  tagged_t current_breaklevel; */
 tagged_t current_compiling;
 tagged_t current_ferror_flag;
/*  tagged_t current_single_var_flag; */
/*  tagged_t current_character_escapes_flag; */
/*  tagged_t current_redefine_flag; */
 tagged_t current_quiet_flag;
 tagged_t current_gcmode;
 tagged_t current_gctrace;
 tagged_t current_gcmargin;
/* tagged_t current_debugger_state; */ /* Now private */
/* tagged_t current_debugger_mode;  */  /* Now private */
 tagged_t current_radix;

 try_node_t *address_nd_repeat;
 try_node_t *address_nd_current_instance;
 try_node_t *address_nd_current_atom;
 try_node_t *address_nd_current_predicate;
 try_node_t *address_nd_predicate_property;
 try_node_t *address_nd_current_stream;
 try_node_t *address_nd_atom_concat;

#if defined(TABLING)
 try_node_t *address_nd_fake_choicept;
#endif

#if defined(PARBACK)
 try_node_t *address_nd_suspension_point;
 bcp_t restart_point_insn;
#endif

 definition_t *address_true;
 definition_t *address_fail;
/*
#if !defined(ATTRVARS)
 definition_t *address_fast_apply;
 definition_t *address_slow_apply;
 definition_t *address_apply;
#endif
*/
 definition_t *address_call;
#if defined(INTERNAL_CALLING)
 definition_t *address_internal_call = NULL;
#endif
 definition_t *address_interpret_goal;
 definition_t *address_call_with_cont;                     /* call/3 */
 definition_t *address_interpret_compiled_goal;
 definition_t *address_interpret_c_goal;
 definition_t *address_undefined_goal;
 definition_t *address_help; 
 definition_t *address_restart; 
 definition_t *address_trace;
 definition_t *address_getct;
 definition_t *address_getct1;
 definition_t *address_get;
 definition_t *address_get2;
 definition_t *address_get1;
 definition_t *address_get12;
 definition_t *address_peek;
 definition_t *address_peek2;
 definition_t *address_skip;
 definition_t *address_skip2;
 definition_t *address_skip_line;
 definition_t *address_skip_line1;
 definition_t *address_error;        /* Handle errors in Prolog (DCG)*/

                                          /* Attributed variables support */
definition_t *address_pending_unifications;
definition_t *address_uvc;
definition_t *address_ucc;



 /*-----------------------------------------------------------------
   INITIALIZATION support functions  
   -----------------------------------------------------------------*/


static void classify_atom(s)
     CIAO_REGISTER atom_t *s;
{
  CIAO_REGISTER unsigned char *cp = (unsigned char *)s->name;
  unsigned char c0 = cp[0];
  unsigned char c1 = cp[1];
  unsigned char c2 = cp[2];
  CIAO_REGISTER unsigned char c;
  bool_t seen_alpha = FALSE;
  bool_t seen_symbol = FALSE;
  
  s->has_dquote = FALSE;	/* TRUE if symbolchars only */
  s->has_squote = FALSE;	/* TRUE if ! ; [] {} OR contains a "'" */
  s->has_special = FALSE;	/* TRUE if needs quoting */
  while ((c = *cp++)) {
    if (c=='\'')
      s->has_squote = s->has_special = TRUE;
    else if (IsLowerChar(c) || IsUpperChar(c) || IsDigit(c))
      seen_alpha = TRUE;
    else if (IsSymbolChar(c))
      seen_symbol = TRUE;
    else s->has_special = TRUE;
  }

  s->has_dquote = (!s->has_special & !seen_alpha);
  s->has_special |= (seen_alpha==seen_symbol);

  /* check cases '!' ';' '[]' '{}' '_...' 'A...' '9...' '.' '/ * ...' */
  /* NB: No point in quoting '... * /' */
    
  if (s->has_special && !s->has_squote &&
      ((c0=='!' && c1==0) ||
       (c0==';' && c1==0) ||
       (c0=='[' && c1==']' && c2==0) ||
       (c0=='{' && c1=='}' && c2==0)))
    s->has_special=FALSE, s->has_squote=TRUE;
  else if (!s->has_special &&
	   (IsUpperChar(c0) ||
	    IsDigit(c0) ||
	    (c0=='.' && c1==0) ||
	    (c0=='/' && c1=='*')))
    s->has_special=TRUE;
}


/* This astonishing piece of code makes an atom from its name and index in
   the hash table.  The memory for the (atom_t) is taken from a linear
   chunk of memory within which a pointer is advanced to reclaim memory for
   the atoms.  When there is not enough room for a new atom, a new area of
   memory is allocated.  MCL.  Unfortunately:

   a) Some space is probably wasted at the end of the prolog_chars memory
   area every time more memory is needed, and

   b) I could not find an easy way to give memory back to the memory manager
   in the event of atom deletion.

   This scheme is supposed to be very fast, because memory is allocated in
   fixed, but I still have to check if just calling the (general) memory
   manager is really that much inefficient.  Doing so would solve completely
   the problem of freeing atom table memory.
*/

/* MCL: changed to solve problems with fixed amounts of increments and the
   (new) variable max_atom_size */

#define MIN_MEM_CHUNK_SIZE 4096
#define MAX(a, b) (a > b ? a : b)

#if defined(USE_ATOM_LEN)
atom_t *new_atom_check(str, str_len, index)  
     unsigned char *str;   
     unsigned int str_len;
     unsigned int index;
#else
atom_t *new_atom_check(str, index)  
     unsigned char *str;   
     unsigned int index;
#endif
{
  atom_t *s;

#if defined(USE_ATOM_LEN)
  int len = sizeof(atom_t) + str_len + 1 - ANY;
#else
  int len = sizeof(atom_t) + strlen(str) + 1 - ANY;
#endif
  
  /* Adjust to a tagged pointer */
  prolog_chars = (char *)((tagged_t)(prolog_chars+3) & ~3); 
  if (prolog_chars+len > prolog_chars_end) {             /* Out of bounds */
    prolog_chars = (char *)checkalloc(MAX(MIN_MEM_CHUNK_SIZE, len));
    prolog_chars_end = prolog_chars + MAX(MIN_MEM_CHUNK_SIZE, len);
  }
  
  s = (atom_t *)prolog_chars;
  prolog_chars += len;
  s->index = index;
  (void) strcpy(s->name,(char *)str);
#if defined(USE_ATOM_LEN)
  s->atom_len = str_len;
#endif
  classify_atom(s);

#if defined(THREADS)
  /*s->atom_lock_l = create_dynamic_lock(); */            /* Already inited */
  /*  Quite amazingly, the latter seems to be faster than just

    s->atom_lock_l = &s->atom_lock_st;
    Init_slock(s->atom_lock_l);

    in a i586, probably because it helps to keep the size of the atoms 
    smaller, and so it favours the cache behavior.
  */
  Init_lock(s->atom_lock_l);
  Init_slock(s->counter_lock);
  s->atom_lock_counter = 1;                           /* MUTEX by default */
#endif
  return s;
}

/*
void reclassify_atoms()
{
  CIAO_REGISTER int i;

  for (i=0; i<ciao_atoms->count; i++)
    classify_atom(atmtab[i]->value.atomp);
}
*/		


/* $atom_mode(+Atom, -Context)
 * Context = 2'000 for alpha
 * Context = 2'001 for quote
 * Context = 2'010 for other
 * Context = 2'100 for punct, depending on how Atom is printed.
 */
static bool_t prolog_atom_mode(Arg)
     Argdecl;
{
  CIAO_REGISTER atom_t *atomptr;

  DEREF(X(0),X(0));
  atomptr = TagToAtom(X(0));
  if (atomptr->has_special)
    Unify_constant(MakeSmall(1),X(1))
  else if (atomptr->has_dquote)
    Unify_constant(MakeSmall(2),X(1))
  else if (atomptr->has_squote)
    Unify_constant(MakeSmall(4),X(1))
  else
    Unify_constant(TaggedZero,X(1))

  return TRUE;
}


static bool_t prolog_ciao_lib_dir(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  return cunify(Arg, X(0), atom_default_lib_dir);
}

static bool_t prolog_ciao_c_headers_dir(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  return cunify(Arg, X(0), atom_default_c_headers_dir);
}

static definition_t *define_builtin(pname,instr,arity)
     char *pname;
     int instr;
     int arity;
{
  definition_t *func;
  ENG_INT current_mem = total_mem_count;
  
  func = insert_definition(predicates_location,MakeString(pname),arity,TRUE);
  SetEnterInstr(func,instr);
  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}

static tagged_t deffunctor(pname,arity)
     char *pname;
     int arity;
{
  return SetArity(MakeString(pname),arity);
}

definition_t *define_c_mod_predicate(module,pname,arity,procedure)
     char *module;                 /* Module in which the definition goes */
     char  *pname;                               /* Prolog predicate name */
     int arity;                                     /* Arity of predicate */
     bool_t (*procedure)();                        /* Pointer to C function */
{
  definition_t *func;
  CIAO_REGISTER sw_on_key_node_t *keyval;
  char mod_pname[STATICMAXATOM]; /* Pred. name with module name prepended */
  tagged_t mod_tagpname;    /* Def. of predicate with module name prepended */
  tagged_t key;
  ENG_INT current_mem = total_mem_count;
               
  if (strlen(module) + strlen(pname) > STATICMAXATOM){     /* Check sizes */
    char errmsg[STATICMAXATOM+STATICMAXATOM+512];

    strcpy(errmsg, "Predicate ");
    strcat(errmsg, pname);
    strcat(errmsg, " in module ");
    strcat(errmsg, module);
    strcat(errmsg, " gave a predicate name too long!");
    USAGE_FAULT(errmsg);
  }

  strcpy(mod_pname, module);/* No need to check length -- already and atom */
  strcat(mod_pname, ":");
  strcat(mod_pname, pname);
  mod_tagpname = MakeString(mod_pname);
  key = SetArity(mod_tagpname, arity);

  Wait_Acquire_slock(prolog_predicates_l);

  keyval = (sw_on_key_node_t *)incore_gethash(prolog_predicates,key);

  if (keyval->key)
    func = keyval->value.def;
  else {
    func = new_functor(mod_tagpname,arity);
    add_definition(predicates_location,keyval,key,func);
  }

  Release_slock(prolog_predicates_l);

  /* Previously in the else-part (DCG) */
  /*func->properties.public = public;*/
  SetEnterInstr(func,ENTER_C);
  func->code.cinfo = procedure;

  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}

void undefine_c_mod_predicate(char *module, char *pname, int arity) {
  definition_t *f;
  char mod_pname[STATICMAXATOM]; /* Pred. name with module name prepended */
  tagged_t mod_tagpname;    /* Def. of predicate with module name prepended */

  strcpy(mod_pname, module);/* No need to check length -- already and atom */
  strcat(mod_pname, ":");
  strcat(mod_pname, pname);
  mod_tagpname = MakeString(mod_pname);

  f = insert_definition(predicates_location, mod_tagpname, arity, FALSE);

  abolish(NULL, f);
}

/*---------------------------------------------------------------
  GENERAL INITIALIZATION
  ------------------------------------------------------------  */

#if 0
static void unused_initialize_intrinsics() {
  /* define_c_predicate is deprecated */
                               /* kprim.c */
#if defined(MARKERS)
  define_c_predicate("launch_goal", prolog_launch_goal, 2);
  define_c_predicate("wait_for_goal", prolog_wait_for_goal, 1);
  define_c_predicate("fail_goal", prolog_fail_goal, 1);
  define_c_predicate("redo_goal", prolog_redo_goal, 1);
#endif
				/* misc.c */
#if defined(UNDEFINED)
  define_c_predicate("launch_goal_plus",prolog_launch_goal_plus,3);
#endif

#if defined(UNDEFINED)
  define_c_predicate("launch_goal",prolog_launch_goal,2);
  define_c_predicate("launch_goal_shv",prolog_launch_goal_shv,2);
  define_c_predicate("launch_goal_cont",prolog_launch_goal_cont,3);
#endif
				/* misc.c */

#if defined(UNDEFINED)
  define_c_predicate("goal_status",prolog_tasks_status,0);
  define_c_predicate("kill_goal",prolog_kill_thread,1);
  define_c_predicate("kill_other_goals",prolog_kill_other_threads,0);
  define_c_predicate("goal_self",prolog_thread_self,1);
  define_c_predicate("join_goal",prolog_join_goal,1);
  define_c_predicate("backtrack_goal",prolog_backtrack_goal,1);
  define_c_predicate("release_goal",prolog_release_goal,1);
#endif
				/* nondet.c */
#if defined(OLD_DATABASE)
  define_c_predicate("$current_key",current_key,4);
#endif
}
#endif

static void deffunction(atom,arity,proc,funcno)
     char *atom;
     CInfo proc;
     int arity, funcno;
{
  tagged_t k = deffunctor(atom,arity);
  sw_on_key_node_t *node = incore_gethash(switch_on_function,k);

  node->key = k;
  node->value.cinfo = builtintab[funcno] = proc;
}

static void deffunction_nobtin(atom,arity,proc)
     char *atom;
     CInfo proc;
     int arity;
{
  tagged_t k = deffunctor(atom,arity);
  sw_on_key_node_t *node = incore_gethash(switch_on_function,k);

  node->key = k;
  node->value.cinfo = proc;
}


static void define_functions()
{
   /* Note: the size of the hash table (64) needs to be expanded when
      new functions are added */

  switch_on_function = new_switch_on_key(64,NULL);

  deffunction("-",1,(CInfo)fu1_minus,0);
  deffunction("+",1,(CInfo)fu1_plus,1);
  deffunction("--",1,(CInfo)fu1_sub1,2); /* shorthand for 'SUB1 FUNCTION' */
  deffunction("++",1,(CInfo)fu1_add1,3); /* shorthand for 'ADD1 FUNCTION' */
  deffunction("integer",1,(CInfo)fu1_integer,4);
  deffunction("truncate",1,(CInfo)fu1_integer,4); /* alias of integer/1 (ISO)*/
  deffunction("float",1,(CInfo)fu1_float,5);
  deffunction("\\",1,(CInfo)fu1_not,6);
  deffunction("+",2,(CInfo)fu2_plus,7);
  deffunction("-",2,(CInfo)fu2_minus,8);
  deffunction("*",2,(CInfo)fu2_times,9);
  deffunction("/",2,(CInfo)fu2_fdivide,10);
  deffunction("//",2,(CInfo)fu2_idivide,11);
  deffunction("rem",2,(CInfo)fu2_rem,12); /* was "mod" (ISO) */
  deffunction("#",2,(CInfo)fu2_xor,13); /* was "^" */
  deffunction("/\\",2,(CInfo)fu2_and,14);
  deffunction("\\/",2,(CInfo)fu2_or,15);
  deffunction("<<",2,(CInfo)fu2_lsh,16);
  deffunction(">>",2,(CInfo)fu2_rsh,17);
  deffunction("mod",2,(CInfo)fu2_mod,18); /* (ISO) */
  deffunction("abs",1,(CInfo)fu1_abs,19); /* (ISO) */
  deffunction("sign",1,(CInfo)fu1_sign,49); /* (ISO) */
  deffunction_nobtin("gcd",2,(CInfo)fu2_gcd);

/* all of these ISO */
  deffunction("float_integer_part",1,(CInfo)fu1_intpart,50);
  deffunction("float_fractional_part",1,(CInfo)fu1_fractpart,51);
  deffunction("floor",1,(CInfo)fu1_floor,52);
  deffunction("round",1,(CInfo)fu1_round,53);
  deffunction("ceiling",1,(CInfo)fu1_ceil,54);
  deffunction("**",2,(CInfo)fu2_pow,55);
  deffunction_nobtin("exp",1,(CInfo)fu1_exp);
  deffunction_nobtin("log",1,(CInfo)fu1_log);
  deffunction_nobtin("sqrt",1,(CInfo)fu1_sqrt);
  deffunction_nobtin("sin",1,(CInfo)fu1_sin);
  deffunction_nobtin("cos",1,(CInfo)fu1_cos);
  deffunction_nobtin("atan",1,(CInfo)fu1_atan);


 /* 41 & 42 are 68 & 79 in SICStus 2.1 */

  deffunction("ARG FUNCTION",2,(CInfo)fu2_arg,41);
  deffunction("COMPARE FUNCTION",2,(CInfo)fu2_compare,42);

}


void init_kanji()
{
  CIAO_REGISTER int i;
  CIAO_REGISTER char *cp;

  for (i=0; i<128; i++)		/* default: whitespace */
      symbolchar[i] = 0;
  for (i=128; i<256; i++)
    symbolchar[i] = 1;		/* accept 128..255 as lowercase */
  
  for (cp="abcdefghijklmnopqrstuvwxyz"; (i = *cp++); )
    symbolchar[i]=1;		/* lowercase */
  for (cp="ABCDEFGHIJKLMNOPQRSTUVWXYZ_"; (i = *cp++); )
    symbolchar[i]=2;		/* uppercase */
  for (cp="0123456789"; (i = *cp++); )
    symbolchar[i]=3;		/* digits */
  for (cp="#$&*+-./:<=>?@^\\`~"; (i = *cp++); )
    symbolchar[i]=4;		/* symbolchars */
  for (cp="!;\"'%(),[]{|}"; (i = *cp++); )
    symbolchar[i]=5;		/* punctuation */
}


void init_latin1()
{
  CIAO_REGISTER int i;

  init_kanji();
  for (i=128; i<161; i++)	/* 128..160 are whitespace */
    symbolchar[i]=0;
  for (i=161; i<192; i++)	/* 161..191 are symbolchars */
    symbolchar[i]=4;
  for (i=192; i<223; i++)	/* 192..222 are uppercase */
    symbolchar[i]=2;
  for (i=223; i<256; i++)	/* 223..255 are lowercase */
    symbolchar[i]=1;
  symbolchar[215]=4;		/* 215 (mult sign) is a symbolchar */
  symbolchar[247]=4;		/* 247 (div sign) is a symbolchar */
}


/* Initializations that need to be made once only. */

void init_locks(){
#if defined(THREADS)
  Init_slock(prolog_predicates_l);
#if defined(DEBUG)
  Init_slock(ops_counter_l);
#endif

  Init_lock(stream_list_l);
  Init_slock(mem_mng_l);
  Init_slock(worker_id_pool_l);
  Init_slock(atom_id_l);
  Init_slock(wam_list_l);

#if defined(ANDPARALLEL)
  Init_slock(stackset_expansion_l);
  Init_slock(wam_circular_list_l);
  Init_slock(nagents_l);
  Init_slock(npargoals_l);
  Init_slock(npargoalstaken_l);
  Init_slock(nlocalbacktr_l);
  Init_slock(nrembacktr_top_l);
  Init_slock(nrembacktr_trapped_l);
#endif

#if defined(PARBACK)
  Init_slock(wam_circular_list_l);
  Init_slock(nagents_l);
#endif
#endif
}

extern char *library_directory;
extern char *c_headers_directory;

extern bool_t fu1_type();
extern bool_t fu1_get_attribute();
extern bool_t bu2_attach_attribute();
extern bool_t bu1_detach_attribute();
extern bool_t bu2_update_attribute();

extern tagged_t atm_var, atm_attv, atm_float, atm_int, atm_str, atm_atm, atm_lst;

void init_once()
{
  CIAO_REGISTER int i;
#if defined(ATOMGC)
  CIAO_REGISTER int j;
#endif
  CIAO_REGISTER char *cp;

  /*struct timeval tp;*/

  /* Init time variables */

  reset_statistics();

#if defined(THREADS) && defined(USE_POSIX_THREADS)
  pthread_attr_init(&detached_thread);
  pthread_attr_setdetachstate(&detached_thread, PTHREAD_CREATE_DETACHED);
  pthread_attr_setscope(&detached_thread, PTHREAD_SCOPE_SYSTEM);

  pthread_attr_init(&joinable_thread);
  pthread_attr_setdetachstate(&joinable_thread, PTHREAD_CREATE_JOINABLE);
  pthread_attr_setscope(&joinable_thread, PTHREAD_SCOPE_SYSTEM);
#endif

#if defined(DBG)
  d_init();
#endif

  /*self = (worker_t *)create_wam_storage();*/

  GETENV(i,cp,"ATMTABSIZE",ATMTABSIZE);
  atmtab =
    (sw_on_key_node_t **)checkalloc(i*sizeof(sw_on_key_node_t *));

  ciao_atoms = new_switch_on_key(2*i,NULL);

#if defined(ATOMGC)
  for (j=0; j < i; j++)
    atmtab[j] = NULL;
#endif

 /*  Database initialization */
  prolog_predicates = new_switch_on_key(2,NULL);

  define_functions();                  /* Uses builtintab up to number 17 */
  
  builtintab[20] = bu1_atom;
  builtintab[21] = bu1_atomic;
  builtintab[22] = bu1_float;
  builtintab[23] = bu1_integer;
  builtintab[24] = bu1_nonvar;
  builtintab[25] = bu1_number;
  builtintab[26] = bu1_var;
  builtintab[27] = bu2_lexeq;
  builtintab[28] = bu2_lexne;
  builtintab[29] = bu2_lexlt;
  builtintab[30] = bu2_lexge;
  builtintab[31] = bu2_lexgt;
  builtintab[32] = bu2_lexle;
  builtintab[33] = bu2_numeq;
  builtintab[34] = bu2_numne;
  builtintab[35] = bu2_numlt;
  builtintab[36] = bu2_numge;
  builtintab[37] = bu2_numgt;
  builtintab[38] = bu2_numle;
  builtintab[39] = bu1_if;
  builtintab[40] = bu2_univ;
  /* builtintab[41] = bu3_arg; */
  /* builtintab[42] = bu3_compare; */
  builtintab[43] = bu3_functor;

  atm_var   = init_atom_check("var");
  atm_attv  = init_atom_check("attv");
  atm_float = init_atom_check("float");
  atm_int   = init_atom_check("integer");
  atm_str   = init_atom_check("structure");
  atm_atm   = init_atom_check("atom");
  atm_lst   = init_atom_check("list"); 
    
  builtintab[44] = fu1_type;                         
  builtintab[45] = fu1_get_attribute;
  builtintab[46] = bu2_attach_attribute;
  builtintab[47] = bu2_update_attribute;
  builtintab[48] = bu1_detach_attribute;
  
#if defined(MARKERS)
  atom_success=init_atom_check("success");
  atom_failure=init_atom_check("failure");
#endif

  atom_share=init_atom_check("share");
  atom_noshare=init_atom_check("noshare");
  atom_read=init_atom_check("read");
  atom_write=init_atom_check("write");
  atom_append=init_atom_check("append");
  atom_socket=init_atom_check("socket");
  atom_symlink=init_atom_check("symlink");
  atom_regular=init_atom_check("regular");
  atom_directory=init_atom_check("directory");
  atom_fifo=init_atom_check("fifo");
  atom_unknown=init_atom_check("unknown");
  atom_prolog=init_atom_check("prolog");
  atom_lessthan=init_atom_check("<");
  atom_greaterthan=init_atom_check(">");
  atom_equal=init_atom_check("=");
  atom_list = init_atom_check(".");
  atom_nil = init_atom_check("[]");
  atom_on = init_atom_check("on");
  atom_off = init_atom_check("off");
  atom_error = init_atom_check("error");
  atom_trace = init_atom_check("trace");
  atom_debug = init_atom_check("debug");
  atom_fail = init_atom_check("fail");
  atom_all = init_atom_check("all");
  atom_terse = init_atom_check("terse");
  atom_verbose = init_atom_check("verbose");
  atom_compiled = init_atom_check("compiled");
  atom_interpreted = init_atom_check("interpreted");
  atom_builtin = init_atom_check("built_in");
  atom_true = init_atom_check("true");
  atom_false = init_atom_check("false");
  atom_retry_hook = init_atom_check("$$retry_hook");

  atom_unprofiled = init_atom_check("unprofiled");
  atom_profiled = init_atom_check("profiled");

  /* atom_public = init_atom_check("public"); */
  atom_concurrent = init_atom_check("concurrent");
  atom_wait = init_atom_check("wait");
  atom_dynamic = init_atom_check("dynamic");
  atom_multifile = init_atom_check("multifile");

  atom_block = init_atom_check("block");
  atom_no_block = init_atom_check("no_block");


  atom_self = init_atom_check("self");
  atom_create = init_atom_check("create");

#if defined(GAUGE)
  atom_counter = init_atom_check("counter");
#endif

  atom_default_lib_dir = init_atom_check(library_directory);
  atom_default_c_headers_dir = init_atom_check(c_headers_directory);

#if defined(USE_OVERFLOW_EXCEPTIONS)
  atom_undo_heap_overflow_excep = init_atom_check("internals:$undo_heap_overflow_excep");
#endif
  atom_heap_limit = init_atom_check("internals:$heap_limit");

  current_gcmode = atom_on;
  current_gctrace = atom_off;
  current_gcmargin = MakeSmall(500); /* Quintus has 1024 */
  current_unknown = atom_error;
/*   current_leash_mode = MakeSmall(0xf); */
/*   current_maxdepth = MakeSmall(100000); */
/*   current_printdepth = MakeSmall(10); */
  current_compiling = atom_unprofiled;
  current_ferror_flag = atom_on;
/*   current_single_var_flag = atom_on; */
/*   current_character_escapes_flag = atom_off; */
/*   current_redefine_flag = atom_on; */
  current_quiet_flag = atom_off;
  
  init_streams();

  functor_neck = deffunctor(":-",2);
  functor_list = deffunctor(".",2);
  functor_cut = deffunctor("!",0);
  functor_minus = deffunctor("-",2);
  functor_slash = deffunctor("/",2);
  functor_and = deffunctor(",",2);
  functor_functor = deffunctor("functor",1);
  functor_tagged = deffunctor("tagged",1);
  functor_emul_entry = deffunctor("emul_entry",1);
  functor_builtin = deffunctor("builtin",1);
  functor_Dref = deffunctor("$ref",2);
  functor_Dstream = deffunctor("$stream",2);
  functor_Dlock = deffunctor("$lock",2);
  functor_Dhandler = deffunctor("$goal_info",1);
  functor_Dsetarg = deffunctor("internals:$setarg",4);
  functor_large = deffunctor("large",2);
  functor_long = deffunctor("long",1);


  functor_active = deffunctor("active", 4);
  functor_pending = deffunctor("pending", 4);
  functor_failed = deffunctor("failed", 3);
  functor_available = deffunctor("available", 1);

#if defined(INTERNAL_CALLING)
  address_internal_call = define_builtin("user:internal_call",ENTER_UNDEFINED,0);
#endif

  address_interpret_goal = define_builtin("basiccontrol:interpret_goal",ENTER_UNDEFINED,2);
  address_call_with_cont = define_builtin("internals:call_with_cont",ENTER_UNDEFINED,1);
  address_interpret_compiled_goal = define_builtin("basiccontrol:interpret_compiled_goal",ENTER_UNDEFINED,2);
  address_undefined_goal = define_builtin("basiccontrol:undefined_goal",ENTER_UNDEFINED,1);
  address_trace = define_builtin("basiccontrol:debug_goal",ENTER_UNDEFINED,1);
  address_help = define_builtin("internals:control_c_handler",ENTER_UNDEFINED,0);
  address_restart = define_builtin("internals:reboot",ENTER_UNDEFINED,0);
  (void) define_builtin("$geler",BUILTIN_GELER,2);
  (void) define_builtin("internals:$instance",BUILTIN_INSTANCE,3);
  (void) define_builtin("internals:dif",BUILTIN_DIF,2);
  (void) define_builtin("internals:$exit",BUILTIN_ABORT,1);
/*
#if !defined(ATTRVARS)
  address_fast_apply = define_builtin("apply",ENTER_UNDEFINED,2);
  address_slow_apply = define_builtin("debug_apply",ENTER_UNDEFINED,2);
#endif
*/
  address_call = define_builtin("hiord_rt:call",BUILTIN_CALL,1);
  (void) define_builtin("hiord_rt:SYSCALL",BUILTIN_SYSCALL,1);
  (void) define_builtin("hiord_rt:$nodebug_call",BUILTIN_NODEBUGCALL,1);
  address_true = define_builtin("basiccontrol:true",BUILTIN_TRUE,0);
  address_fail = define_builtin("basiccontrol:fail",BUILTIN_FAIL,0);
  address_error = define_builtin("internals:error",ENTER_UNDEFINED,5);

 /* Support for attributed variables */
  address_pending_unifications = 
    define_builtin("internals:pending_unifications",    ENTER_UNDEFINED,1);
  address_uvc = define_builtin("internals:uvc",         ENTER_UNDEFINED,2);
  address_ucc = define_builtin("internals:ucc",         ENTER_UNDEFINED,2);

  (void) define_builtin("internals:$current_instance",BUILTIN_CURRENT_INSTANCE,5);
  (void) define_builtin("internals:$compile_term",BUILTIN_COMPILE_TERM,2);
/*
#if !defined(ATTRVARS)
  (void) define_builtin("$apply",BUILTIN_APPLY,2);
#endif
*/
                              /* initial.c */
  
  define_c_mod_predicate("internals","$atom_mode",2,prolog_atom_mode);
  define_c_mod_predicate("system_info","ciao_lib_dir",1,prolog_ciao_lib_dir);
  define_c_mod_predicate("system_info","ciao_c_headers_dir",1,prolog_ciao_c_headers_dir);

				/* streams.c */
  
  define_c_mod_predicate("streams_basic","stream_code",2,prolog_stream_code);
  define_c_mod_predicate("internals","$bootversion",0,prolog_bootversion);
  define_c_mod_predicate("internals","$open",3,prolog_open);
  define_c_mod_predicate("streams_basic","close",1,prolog_close); 
  define_c_mod_predicate("internals","$unix_popen",3,prolog_unix_popen);
  define_c_mod_predicate("streams_basic","character_count",2,character_count);
  define_c_mod_predicate("streams_basic","line_position",2,line_position);
  define_c_mod_predicate("streams_basic","line_count",2,line_count);
  define_c_mod_predicate("streams_basic","current_input",1,prolog_current_input);
  define_c_mod_predicate("streams_basic","set_input",1,prolog_set_input);
  define_c_mod_predicate("streams_basic","current_output",1,prolog_current_output);
  define_c_mod_predicate("streams_basic","set_output",1,prolog_set_output);
  define_c_mod_predicate("streams_basic","pipe",2,prolog_pipe);

  define_c_mod_predicate("io_alias_redirection", "replace_stream",2, prolog_replace_stream);
  define_c_mod_predicate("io_alias_redirection", "get_stream",2, prolog_get_stream);

                              /* objareas.c */

  define_c_mod_predicate("internals","$purge",1,prolog_purge);
  define_c_mod_predicate("internals","$erase",1,prolog_erase);
  define_c_mod_predicate("internals","$ptr_ref",2,prolog_ptr_ref);
  define_c_mod_predicate("internals","$inserta",2,inserta);
  define_c_mod_predicate("internals","$insertz",2,insertz);
  define_c_mod_predicate("internals","$make_bytecode_object",4,make_bytecode_object);

				/* support.c */
  define_c_mod_predicate("terms_check","$instance",2,cinstance);
  define_c_mod_predicate("term_typing","ground",1,cground);

				/* term_support.c */

  define_c_mod_predicate("atomic_basic","name",2,prolog_name);
  define_c_mod_predicate("atomic_basic","atom_codes",2,prolog_atom_codes);
  define_c_mod_predicate("atomic_basic","number_codes",2,prolog_number_codes_2);
  define_c_mod_predicate("atomic_basic","number_codes",3,prolog_number_codes_3);
  define_c_mod_predicate("atomic_basic","atom_length",2,prolog_atom_length);
  define_c_mod_predicate("atomic_basic","sub_atom",4,prolog_sub_atom);
  define_c_mod_predicate("atomic_basic","atom_concat",3,prolog_atom_concat);
  define_c_mod_predicate("term_basic","copy_term",2,prolog_copy_term);
  define_c_mod_predicate("term_basic","copy_term_nat",2,prolog_copy_term_nat);
  define_c_mod_predicate("term_basic","cyclic_term",1,prolog_cyclic_term);

				/* indexing.c */
  define_c_mod_predicate("internals","$abolish",1,prolog_abolish); 
  define_c_mod_predicate("internals","$define_predicate",2,define_predicate);
  define_c_mod_predicate("internals","$erase_clause",1,erase_clause);
  define_c_mod_predicate("internals","$clause_number",2,clause_number);
  define_c_mod_predicate("internals","$compiled_clause",4,compiled_clause);
  define_c_mod_predicate("internals","$empty_gcdef_bin",0,empty_gcdef_bin);
  define_c_mod_predicate("internals","$set_property",2,set_property);

				/* inout.c */
  
  define_c_mod_predicate("io_basic","code_class",2,code_class);
  define_c_mod_predicate("streams_basic","flush_output",0,flush_output);
  define_c_mod_predicate("streams_basic","flush_output",1,flush_output1);
  address_getct = define_c_mod_predicate("io_basic","getct",2,getct);
  address_getct1 = define_c_mod_predicate("io_basic","getct1",2,getct1);
  address_get = define_c_mod_predicate("io_basic","get_code",1,get);
  address_get2 = define_c_mod_predicate("io_basic","get_code",2,get2);
  address_get1 = define_c_mod_predicate("io_basic","get1_code",1,get1);
  address_get12 = define_c_mod_predicate("io_basic","get1_code",2,get12);
  address_peek = define_c_mod_predicate("io_basic","peek_code",1,peek);
  address_peek2 = define_c_mod_predicate("io_basic","peek_code",2,peek2);
  define_c_mod_predicate("io_basic","nl",0,nl);
  define_c_mod_predicate("io_basic","nl",1,nl1);
  define_c_mod_predicate("io_basic","put_code",1,put);
  define_c_mod_predicate("io_basic","put_code",2,put2);
  define_c_mod_predicate("io_basic","tab",1,tab);
  define_c_mod_predicate("io_basic","tab",2,tab2);
  address_skip = define_c_mod_predicate("io_basic","skip_code",1,skip);
  address_skip2 = define_c_mod_predicate("io_basic","skip_code",2,skip2);
  address_skip_line =
    define_c_mod_predicate("io_basic","skip_line",0,skip_line);
  address_skip_line1 =
    define_c_mod_predicate("io_basic","skip_line",1,skip_line1);
  define_c_mod_predicate("io_basic","display",1,prolog_display);
  define_c_mod_predicate("io_basic","display",2,prolog_display2);
  define_c_mod_predicate("io_basic","displayq",1,prolog_displayq);
  define_c_mod_predicate("io_basic","displayq",2,prolog_displayq2);
  define_c_mod_predicate("streams_basic","clearerr",1,prolog_clearerr);
  define_c_mod_predicate("fastrw","fast_read",1,prolog_fast_read_in_c);
  define_c_mod_predicate("fastrw","fast_write",1,prolog_fast_write_in_c);
  define_c_mod_predicate("compressed_bytecode","compressLZ",1,compressLZ);
  define_c_mod_predicate("compressed_bytecode","copyLZ",1,copyLZ);

				/* builtin.c */
  
  define_c_mod_predicate("internals","$ddt",1,set_predtrace);

				/* qread.c */

  define_c_mod_predicate("internals","$qread",2,prolog_qread);
  define_c_mod_predicate("internals","$push_qlinfo",0,push_qlinfo);
  define_c_mod_predicate("internals","$pop_qlinfo",0,pop_qlinfo);

				/* misc.c */
  
  define_c_mod_predicate("prolog_sys", "new_atom", 1, prolog_new_atom);
  define_c_mod_predicate("internals","$set_global_logical_var", 2, set_glv);
  define_c_mod_predicate("internals","$get_global_logical_var", 2, get_glv);
  define_c_mod_predicate("internals","$global_vars_get_root", 1, prolog_global_vars_get_root);
  define_c_mod_predicate("internals","$global_vars_set_root", 1, prolog_global_vars_set_root);
#if defined(ATOMGC)
  define_c_mod_predicate("internals","$erase_atom", 1, prolog_erase_atom);
#endif
  define_c_mod_predicate("system","current_executable",1, prolog_current_executable);
  define_c_mod_predicate("internals","$prompt",2,prompt);
  define_c_mod_predicate("internals","$frozen",2,frozen);
  define_c_mod_predicate("internals","$defrost",2,defrost);
  define_c_mod_predicate("internals","$setarg",4,setarg);
  define_c_mod_predicate("internals","$undo_goal",1,undo);
  define_c_mod_predicate("basiccontrol","$metachoice",1,metachoice);
  define_c_mod_predicate("basiccontrol","$metacut",1,metacut);
  define_c_mod_predicate("internals","$unknown",2,unknown);
  define_c_mod_predicate("internals","$compiling",2,compiling);
  define_c_mod_predicate("internals","$ferror_flag",2,ferror_flag);
  define_c_mod_predicate("internals","$quiet_flag",2,quiet_flag);
  define_c_mod_predicate("debugger_support","$retry_cut",2,retry_cut);
  define_c_mod_predicate("debugger_support","$spypoint",3,spypoint);
  define_c_mod_predicate("debugger_support","$debugger_state",2,debugger_state);
  define_c_mod_predicate("debugger_support","$debugger_mode",0,debugger_mode);
  define_c_mod_predicate("internals","$show_nodes",2,prolog_show_nodes);
  define_c_mod_predicate("internals","$show_all_nodes",0,prolog_show_all_nodes);
  define_c_mod_predicate("internals","$start_node",1,start_node);
  define_c_mod_predicate("internals","$prolog_radix",2,prolog_radix);
  define_c_mod_predicate("internals","$constraint_list",2,constraint_list);
  define_c_mod_predicate("internals","$eq",2,prolog_eq);
  define_c_mod_predicate("internals","$large_data",3,large_data);
  define_c_mod_predicate("internals","$interpreted_clause",2,prolog_interpreted_clause);
  define_c_mod_predicate("internals","$unlock_predicate",1,prolog_unlock_predicate);
  define_c_mod_predicate("internals","$get_address",2,prolog_address);

				/* unix_utils.c */
  define_c_mod_predicate("system","using_windows",0,prolog_using_windows);
  define_c_mod_predicate("system","working_directory",2,prolog_unix_cd);
  define_c_mod_predicate("system","pause", 1, prolog_pause);
  define_c_mod_predicate("system","shell",0,prolog_unix_shell0);
  define_c_mod_predicate("system","shell",2,prolog_unix_shell2);
  define_c_mod_predicate("system","system",2,prolog_unix_system2);
  define_c_mod_predicate("internals","$exec",8,prolog_exec);
  define_c_mod_predicate("system","wait",3,prolog_wait);
  define_c_mod_predicate("internals","$unix_argv",1,prolog_unix_argv);
  define_c_mod_predicate("system","mktemp",2,prolog_unix_mktemp);
  define_c_mod_predicate("system","file_exists",2,prolog_unix_access);
  define_c_mod_predicate("system","directory_files",2,prolog_directory_files);
  define_c_mod_predicate("system","file_properties",6,prolog_file_properties);
  define_c_mod_predicate("system","touch",1,prolog_touch);
  define_c_mod_predicate("system","chmod",2,prolog_unix_chmod);
  define_c_mod_predicate("system","umask",2,prolog_unix_umask);
  define_c_mod_predicate("system","delete_file",1,prolog_unix_delete);
  define_c_mod_predicate("system","rename_file",2,prolog_unix_rename);
  define_c_mod_predicate("system","make_directory",2,prolog_unix_mkdir);
  define_c_mod_predicate("system","delete_directory",1,prolog_unix_rmdir);  
  define_c_mod_predicate("system","c_errno",1,prolog_c_errno);
  define_c_mod_predicate("system","c_strerror",1,prolog_c_strerror);
  define_c_mod_predicate("system","c_copy_file",3,prolog_c_copy_file);
  define_c_mod_predicate("system","c_winpath",2,prolog_c_winpath);
  define_c_mod_predicate("system","c_posixpath",2,prolog_c_posixpath);
  define_c_mod_predicate("system","c_winfile",2,prolog_c_winfile);
  define_c_mod_predicate("system","c_posixfile",2,prolog_c_posixfile);
  define_c_mod_predicate("system","current_host",1,prolog_current_host);
  define_c_mod_predicate("system","c_get_env", 2,prolog_c_get_env);
  define_c_mod_predicate("system","c_set_env", 2,prolog_c_set_env);
  define_c_mod_predicate("system","c_del_env", 1,prolog_c_del_env);
  define_c_mod_predicate("system","c_current_env", 3,prolog_c_current_env);

  define_c_mod_predicate("system","get_pid", 1, prolog_getpid);
  define_c_mod_predicate("system","get_uid", 1, prolog_getuid);
  define_c_mod_predicate("system","get_gid", 1, prolog_getgid);
  define_c_mod_predicate("system","get_pwnam", 1, prolog_getpwnam);
  define_c_mod_predicate("system","get_grnam", 1, prolog_getgrnam);
  define_c_mod_predicate("system_info","get_arch", 1, prolog_getarch);
  define_c_mod_predicate("system_info","get_os", 1, prolog_getos);
  define_c_mod_predicate("system_info","get_debug", 1, prolog_getdebug);
  define_c_mod_predicate("system_info","get_eng_location", 1, prolog_get_eng_location);
  define_c_mod_predicate("system_info","get_ciao_ext", 1, prolog_get_ciao_ext);
  define_c_mod_predicate("system_info","get_exec_ext", 1, prolog_get_exec_ext);
  define_c_mod_predicate("system_info","get_so_ext", 1, prolog_get_so_ext);
  define_c_mod_predicate("internals","$ciao_version", 3, prolog_version);

  define_c_mod_predicate("internals","$find_file",8,prolog_find_file); 
  define_c_mod_predicate("internals","$path_is_absolute",1,prolog_path_is_absolute); 
  define_c_mod_predicate("internals","$expand_file_name",2,prolog_expand_file_name); 

                            /* dynlink.c */

  define_c_mod_predicate("internals","dynlink", 2, prolog_dynlink);
  define_c_mod_predicate("internals","dynunlink", 1, prolog_dynunlink); 

				/* format.c */
  
  define_c_mod_predicate("internals","$format_print_float",3,prolog_format_print_float);
  define_c_mod_predicate("internals","$format_print_integer",3,prolog_format_print_integer);

				/* timing.c */

  define_c_mod_predicate("internals","$runtime",1,prolog_runtime);
  define_c_mod_predicate("internals","$usertime",1,prolog_usertime);
  define_c_mod_predicate("internals","$systemtime",1,prolog_systemtime);  
  define_c_mod_predicate("internals","$walltime",1,prolog_walltime);

  define_c_mod_predicate("system","time",1,prolog_time);
  define_c_mod_predicate("system","datime",9,prolog_datime);
  
                                /* clock/cpu ticks */  

  define_c_mod_predicate("internals","$runtick",1,prolog_runtick);
  define_c_mod_predicate("internals","$usertick",1,prolog_usertick);
  define_c_mod_predicate("internals","$systemtick",1,prolog_systemtick);
  define_c_mod_predicate("internals","$walltick",1,prolog_walltick);

                                /* clock frequency */  

  define_c_mod_predicate("internals","$runclockfreq",1,prolog_runclockfreq);
  define_c_mod_predicate("internals","$userclockfreq",1,prolog_userclockfreq);
  define_c_mod_predicate("internals","$systemclockfreq",1,prolog_systemclockfreq);
  define_c_mod_predicate("internals","$wallclockfreq",1,prolog_wallclockfreq);

				/* stacks.c */

  define_c_mod_predicate("internals","$termheap_usage",1,termheap_usage);
  define_c_mod_predicate("internals","$envstack_usage",1,envstack_usage);
  define_c_mod_predicate("internals","$trail_usage",1,trail_usage);
  define_c_mod_predicate("internals","$choice_usage",1,choice_usage);
  define_c_mod_predicate("internals","$stack_shift_usage",1,stack_shift_usage);

				/* alloc.c */

  define_c_mod_predicate("prolog_sys","statistics",0,statistics);
  define_c_mod_predicate("internals","$program_usage",1,program_usage);
  define_c_mod_predicate("internals","$internal_symbol_usage",1,internal_symbol_usage);
  define_c_mod_predicate("internals","$total_usage",1,total_usage);

                                 /* heapgc.c */

  define_c_mod_predicate("internals","$gc_mode",2,gc_mode);
  define_c_mod_predicate("internals","$gc_trace",2,gc_trace);
  define_c_mod_predicate("internals","$gc_margin",2,gc_margin);
  define_c_mod_predicate("internals","$gc_usage",1,gc_usage);
  define_c_mod_predicate("prolog_sys","garbage_collect",0,gc_start);

				/* nondet.c */
  
  define_c_mod_predicate("basiccontrol","repeat",0,prolog_repeat);
  define_c_mod_predicate("prolog_sys","current_atom",1,current_atom);
  define_c_mod_predicate("streams_basic","current_stream",3,current_stream);
  define_c_mod_predicate("internals","$current_predicate",2,current_predicate);
  define_c_mod_predicate("internals","$predicate_property",3,predicate_property);
  define_c_mod_predicate("internals","$current_clauses",2,current_clauses);

  define_c_mod_predicate("internals","$first_instance",2,first_instance);
  define_c_mod_predicate("internals","$close_predicate",1,close_predicate);
  define_c_mod_predicate("internals","$open_predicate",1,open_predicate);

#if defined(GAUGE)
  				/* gauge.c */

  define_c_mod_predicate("internals","$emulated_clause_counters",4,emulated_clause_counters);
  define_c_mod_predicate("internals","$counter_values",3,counter_values);
  define_c_mod_predicate("internals","$reset_counters",2,reset_counters);
#endif

#if defined(USE_OVERFLOW_EXCEPTIONS)
  define_c_mod_predicate("internals","$undo_heap_overflow_excep",0,undo_heap_overflow_excep);
#endif
  define_c_mod_predicate("internals","$heap_limit",1,heap_limit);

  address_nd_repeat = def_retry_c(nd_repeat,0);
  address_nd_current_atom = def_retry_c(nd_current_atom,2);
  address_nd_current_stream = def_retry_c(nd_current_stream,4);
  address_nd_current_predicate = def_retry_c(nd_current_predicate,4);
  address_nd_predicate_property = def_retry_c(nd_predicate_property,5);
  address_nd_atom_concat = def_retry_c(nd_atom_concat,4);
#if defined(TABLING)
  address_nd_fake_choicept = def_retry_c(nd_fake_choicept,0);
#endif
#if defined(PARBACK)
  address_nd_suspension_point = def_retry_c(nd_suspension_point,1);
  restart_point_insn = (bcp_t) checkalloc(sizeof(insn_t));
  *restart_point_insn = RESTART_POINT;
#endif

#if defined(STATICENG)
#include "addmodules.c"
#endif  

#if defined(MARKERS)
  init_markercode();
#endif
  
#if defined(PROFILE)
  if (profile_eng) {
    void (*profile_init)(void) = (void (*)(void))dlsym(RTLD_DEFAULT, "profile_init");
    void (*profile_enter_call_)(void) = (void (*)(void))dlsym(RTLD_DEFAULT, "profile_enter_call_");
    profile_rcc = TRUE;
    if (profile_init) profile_init();
    if (profile_enter_call_) profile_enter_call_();
  }
#endif

  /*init_worker_entry_table();*/
}


void glb_init_each_time()
{

/*
#if !defined(ATTRVARS)
  address_apply = address_fast_apply;
#endif
*/

  address_interpret_c_goal = address_interpret_goal;

  current_radix = MakeSmall(10);
  prolog_init_radix();
  /*current_breaklevel = TaggedZero;*/
  current_prompt = init_atom_check("|: ");
  enable_conditions();
  compute_cwd();
}



void local_init_each_time(Arg)
     Argdecl;
{
  CIAO_REGISTER node_t *b = InitialNode;
		
  /* Debugger state globals moved to per-thread variables because of
     reallocations of the atoms they point to when expanding the heap
     --- this caused the program to break, since a C variable could be
     pointing to a thread's heap and be reallocated by another thread
     with wrong displacements. */

  /* Initialize debugger variables */

  Current_Debugger_State = atom_nil;
  Current_Debugger_Mode = atom_off;

  /* Worker expansion caused by compiling big clauses */

  Expanded_Worker = NULL;

  /* Initialize garbage collection ciao_statistics */

  Gc_Total_Grey = 0;

  Arg->node = b;		            /* set up initial choicepoint */
  b->frame = Arg->frame = (frame_t *)Stack_Start;

  TopConcChpt = b;           /* Initialize concurrent topmost choicepoint */
  b->next_insn = exitcode;
  b->next_alt = termcode;

  b->local_top = Arg->local_top = (frame_t *)Offset(Arg->frame,EToY0);
  b->global_top = Arg->global_top = Heap_Start;
  b->trail_top = Arg->trail_top = Trail_Start;
  b->term[0] = atom_nil;

  ChoiceptMarkPure(b);
  ChoiceptMarkStatic(b);
  ChoiceptMarkNoCVA(b);
				
  Arg->frame->next_insn = NULL;                     /* set up initial frame */
  Arg->frame->frame = NULL;
  
  Arg->next_insn = bootcode;
  Arg->value_trail = InitialValueTrail;
  NewShadowregs(Arg->global_top);
  Arg->next_alt = NULL;
				
  Stop_This_Goal(Arg) = FALSE;
  Heap_Warn_Soft = Heap_Warn;

#if defined(DEBUG)
  if (debug_threads)
    printf("%d (%d) Initializing WAM %x: node = %x, trail = %x, frame = %x\n",
           (int)Thread_Id, (int)GET_INC_COUNTER, (int)Arg,
           (int)b, (int)Arg->trail_top, (int)Arg->frame);
#endif
  init_streams_each_time(Arg);       /* set misc. variables, handle signals */
  control_c_normal(Arg);                               /* For threads also? */

  /* Initialize global variables root */
#if defined(USE_GLOBAL_VARS)
  GLOBAL_VARS_ROOT = atom_nil;
#endif
}


/* Init. at boot and after abort. */

void init_each_time(Arg)
     Argdecl;
{
  glb_init_each_time();
  local_init_each_time(Arg);
}


void init_streams_each_time(Arg)
     Argdecl;
{
  Input_Stream_Ptr = stream_user_input;
  Output_Stream_Ptr = stream_user_output;
  Error_Stream_Ptr = stream_user_error;
}

/* Cleanup after abort: shrink stacks to initial sizes. */
void reinitialize(Arg)
     Argdecl;
{
  int i, j;
  char *cp;

  wam_initialized = FALSE;                    /* disable recursive aborts */
  GETENV(i,cp,"GLOBALSTKSIZE",GLOBALSTKSIZE);
  if ((j=HeapDifference(Heap_Start,Heap_End)) != i)
    Heap_Start = checkrealloc(Heap_Start,j*sizeof(tagged_t),i*sizeof(tagged_t)),
    Heap_End = HeapOffset(Heap_Start,i);
  GETENV(i,cp,"LOCALSTKSIZE",LOCALSTKSIZE);
  if ((j=StackDifference(Stack_Start,Stack_End)) != i)
    Stack_Start = checkrealloc(Stack_Start,j*sizeof(tagged_t),i*sizeof(tagged_t)),
    Stack_End = StackOffset(Stack_Start,i);
  GETENV(i,cp,"CHOICESTKSIZE",CHOICESTKSIZE);
  GETENV(j,cp,"TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  if ((j=TrailDifference(Trail_Start,Trail_End)) != i) {
    Choice_End = Trail_Start =
      checkrealloc(Trail_Start,j*sizeof(tagged_t),i*sizeof(tagged_t));
    Choice_Start = Trail_End = TrailOffset(Trail_Start,i);

#if defined(USE_TAGGED_CHOICE_START)
    Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif
  }

  /* Create an expandable char array for loading po files */ 

  if (Atom_Buffer_Length != STATICMAXATOM){
    Atom_Buffer = (char *)checkrealloc((tagged_t *)Atom_Buffer,
				       Atom_Buffer_Length, STATICMAXATOM);
    Atom_Buffer_Length = STATICMAXATOM;
  }

  Heap_Warn_Soft = Heap_Warn = HeapOffset(Heap_End,-DEFAULT_SOFT_HEAPPAD);
#if defined(USE_OVERFLOW_EXCEPTIONS)
  SOFT_HEAPPAD = DEFAULT_SOFT_HEAPPAD;
  Heap_Limit = 0;
#endif 

  Stack_Warn = StackOffset(Stack_End,-STACKPAD);
  
  empty_gcdef_bin(Arg);

  fflush(stdout);
  fflush(stderr);
  wam_initialized = TRUE;
}


