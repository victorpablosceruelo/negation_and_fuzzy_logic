#include <engine/basiccontrol.native.h>
#include <string.h>

int engine_start();

CBOOL__PROTO_N(expand_file_name, char *name, char *target);

CBOOL__PROTO_N(load_boot, char *boot_path) {
  char source_path[MAXPATHLEN] = "";
  CBOOL__CALL_N(expand_file_name, boot_path, source_path);

#if defined(Win32)
  {
    intmach_t i;
    i = strlen(source_path)-4;
    if (i > 0 && strcmp(source_path+i,".bat") == 0) {
      source_path[i+1] = 'c';
      source_path[i+2] = 'p';
      source_path[i+3] = 'x';
    } else if (i > 0 && strcmp(source_path+i,".cpx") != 0) {
      strcat(source_path,".cpx");
    }

    if (access(source_path,R_OK)) {
      source_path[strlen(source_path)-4] = '\0'; /* Take out .cpx */
    }
  }
#endif

  {
    FILE *qfile;
    bool_t ok;
#if defined(PROFILE_STATS)
    flt64_t walltime(void);
#endif
#if defined(PROFILE_STATS)
    flt64_t load_time;
#endif

    qfile = fopen(source_path,"r");
    CBOOL__TEST(qfile != NULL);

    /* TODO: add skip script and exec here ONLY in load_module_pack, not
       in any qread1 */
    /* TODO: read bytecode with mmap??? only if it is faster and it
       can be disabled */
#if defined(PROFILE_STATS)
    load_time = walltime();
#endif
    if (CBOOL__SUCCEED_N(load_module_pack, qfile)) {
#if defined(PROFILE_STATS)
      load_time = walltime() - load_time;
      TRACE_PRINTF("modules loaded: %ld modules in %.3f ms (elapsed)\n", (long int)modules_location->count, load_time);
#endif
      ok = TRUE;
    } else {
      ok = FALSE;
    }
    fclose(qfile);
    CBOOL__LASTTEST(ok);
  }
}

CVOID__PROTO(engine_finish);

goal_descriptor_t *default_goal_desc;

int main(int argc, char **argv) {
  int i;
  char *boot_path = NULL;

  /* Split program options and engine options from engine arguments */
  int optc = 0;
  char **optv = NULL;
  prolog_argc = argc;
  prolog_argv = argv;
  for (i=1; i < argc; i++) {
    if (strcmp(argv[i], "-C") == 0) {
      /* set the number of program arguments */
      prolog_argc = i;
      /* skip "-C" */
      i++;
      /* get the boot path */
      boot_path = argv[i];
      i++;
      /* engine options */
      optc = argc - i;
      optv = &argv[i];
      goto done_args;
    }
  }
  /* no "-C" is found */
  prolog_argc = argc;
 done_args:

  /* Set the options */
  engine_set_opts(optv, optc);

  if (boot_path == NULL) {
    PANIC_FAULT("boot file not provided");
  }

  /* Initialize the engine */
  engine_init();

  default_goal_desc = gimme_a_new_gd();

  WITH_WORKER(default_goal_desc->worker_registers, {
    if (CBOOL__SUCCEED_N(load_boot, boot_path)) {
      intmach_t i;
      i = CFUN__EVAL_N(call_firstgoal, GET_ATOM("internals:boot"), default_goal_desc);
      engine_exit(i);
    } else {
      PANIC_FAULT("cannot load boot file");
    }
  });
  return 0;
}
 
/* TODO: exit using exceptions */
void engine_exit(int result) {
#if defined(PROF_AG)
  extern int ptr2gc_count[100];
  extern int ptr2gc_seen[100];
  extern int ptr2gc_sect[100];
  extern int ptr2gc_max;
  int i;
  for (i = 0; i < ptr2gc_max; i++) {
    fprintf(stderr, "%d -> count %d, heap:%d,stack:%d,trail:%d, sect:%d\n",
	    i, ptr2gc_count[i],
	    (ptr2gc_seen[i] & 1) != 0 ? 1:0,
	    (ptr2gc_seen[i] & 2) != 0 ? 1:0,
	    (ptr2gc_seen[i] & 4) != 0 ? 1:0,
	    ptr2gc_sect[i]);
  }
#endif
  WITH_WORKER(default_goal_desc->worker_registers, {
    CVOID__CALL(engine_finish);
  });
  exit(result);
}

