#include <engine/basiccontrol.native.h>

#include <sys/types.h>
#include <signal.h>

static void abortmsg(int rc);

/* I/O predicate at ^C, else NULL */
/* Probably Shared, since only one worker should receive a ^C */
definition_t *int_address = NULL;

static void interrupt_h(int signal_number) {
  /* exit if wam is not initialized */
  if (!in_abort_context) engine_exit(-1);

  WITH_WORKER(get_my_worker(), {
    SetCIntEvent();
#if defined(USE_PROLOG_DEBUGGER)
    debug_status = 0;
#endif
#if defined(LINUX)
    /* From the manpage: Unlike on BSD systems, signals under Linux
       are reset to their default behavior when raised.  Therefore we
       have to SIGNAL() interrupt_h here again.  Be careful of
       restating which signals should interrupt_h respond to. */
#endif 
    if (int_address) LONGJMP(abort_env, -1);
  });
}

CVOID__PROTO(control_c_normal) {
  if (Input_Stream_Ptr->isatty) {
    SIGNAL(SIGINT,interrupt_h);
  }
}

/* Non-control-C exception handling. --GB & MC */

void enable_conditions() {
/* set system exception signal handling */
  SIGNAL(SIGFPE,  abortmsg);
  SIGNAL(SIGSEGV, abortmsg);
  SIGNAL(SIGILL,  abortmsg);
  SIGNAL(SIGBUS,  abortmsg);
#if defined(LINUX)
  /* Signal handlers in LINUX must reinstall the handler for the signal */
#endif
#if !defined(LINUX)              /* No "bad systema call" signal in Linux */
  SIGNAL(SIGSYS, abortmsg);
#endif
}

static void abortmsg(int rc) {
  char message[1024];
  switch(rc) {
  case SIGINT:
    SERIOUS_FAULT("interrupted");
    break;
  case SIGFPE:
    SERIOUS_FAULT("floating point exception");
    break;
  case SIGSEGV:
    PANIC_FAULT("segmentation violation");
    break;
  case SIGILL:
    PANIC_FAULT("illegal instruction");
    break;
  case SIGBUS:
    PANIC_FAULT("bus error");
    break;
#if !defined(LINUX)
  case SIGSYS:
    PANIC_FAULT("bad system call arg");
    break;
#endif
  default:
    sprintf(message, "miscellaneous error condition: received signal number %d\n", rc);
    PANIC_FAULT(message);
    break;
  }
}


