#include <engine/basiccontrol.native.h>

#include <string.h>
#include <time.h>

#include <sys/types.h>

#if (defined(Solaris) || defined(SunOS4) || defined(LINUX) || defined(DARWIN) || defined(IRIX) || defined(Win32)) && !defined(crossWin32i86)

#include <sys/time.h>
#include <sys/resource.h>

int64_t internal_userclick_std(void) {
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((int64_t)rusage.ru_utime.tv_sec) * 1000000 + rusage.ru_utime.tv_usec;
}

int64_t internal_systemclick_std(void) {
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((int64_t)rusage.ru_stime.tv_sec) * 1000000 + rusage.ru_stime.tv_usec;
}

static void init_frequency_info(void) {
  stats.userclockfreq = 1000000;
  stats.systemclockfreq = 1000000;
}

#else

#include <sys/times.h>
#include <sys/param.h>

int64_t internal_userclick_std(void) {
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_utime;
}

int64_t internal_systemclick_std(void) {
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_stime;
}

static void init_frequency_info(void) {
  stats.userclockfreq = HZ;
  stats.systemclockfreq = HZ;
}

#endif

/* userclick is defined as a pointer to a function to let the
   redefinition on the fly for a better timing measurement function */

int64_t (*userclick)(void) = internal_userclick_std;
int64_t (*systemclick)(void) = internal_systemclick_std;

flt64_t usertime(void) {
  return ((flt64_t)userclick()) / stats.userclockfreq;
}


/* walltime(?Time): unifies Time with the time in milliseconds elapsed since
  the last call to walltime/1 . The first call returns walltime since the
  start of the execution.  */

/* Shared but locked?  Initialized in init_once() */

int64_t internal_wallclick_std(void) {
  struct timeval tp;

  gettimeofday(&tp, 0L);
  return (int64_t)tp.tv_sec*1000000 + ((int64_t)tp.tv_usec);
}

#if defined(i86) && defined(LINUX)
/* more precise timing functions available in x86 plattform: click
  represent the more precise time unit.  In Pentium II and better, it
  is equal to 1 cpu - cycle.
*/

/* Returns the clock speed of the system's CPU in Hz, as reported by
  /proc/cpuinfo. On a multiprocessor machine, returns the speed of the
  first CPU. On error returns zero.

  Reference:

  http://www.informit.com/isapi/product_id~%7B480DF8FB-19B8-4C4E-88B8-FC2BF352887D%7D/content/index.asp
*/

static int64_t cpuspeed(void) {
  FILE* fp;
  #define PROC_BUFFER_SIZE 4096
  char buffer[4096];
  size_t bytes_read;
  char* match;
  flt64_t clock_speed;

  /* Read the entire contents of /proc/cpuinfo into the buffer. */
  fp = fopen("/proc/cpuinfo", "r");
  bytes_read = fread(buffer, 1, PROC_BUFFER_SIZE, fp);
  fclose(fp);
  /* Fail if read failed or if buffer isn't big enough. */
  if (bytes_read == 0 || bytes_read == PROC_BUFFER_SIZE) return 0;
  /* NUL-terminate the text. */
  buffer[bytes_read] = '\0';
  /* Locate the line that starts with "cpu MHz". */
  match = strstr(buffer, "cpu MHz");
  if (match == NULL) return 0;
  /* Parse the line to extract the clock speed. */
  sscanf(match, "cpu MHz : %lf", &clock_speed);
  return (int64_t)(clock_speed * 1000000);
}

int64_t internal_wallclick_tsc(void) {
  volatile union {
    uint64_t k;
    uint32_t parts[2];
  } u;
  __asm__ __volatile__("rdtsc" : "=a" ( u.parts[0] ), "=d" ( u.parts[1] )) ;
  return u.k;
}

int64_t (*wallclick)(void) = internal_wallclick_tsc;

void init_timing() {
  init_frequency_info();
  stats.wallclockfreq = cpuspeed();
  if (stats.wallclockfreq==0) {
    // by now, if this method fail, use the standar method
    stats.wallclockfreq = 1000000;
    wallclick = internal_wallclick_std;
  }
}

#else

int64_t (*wallclick)(void) = internal_wallclick_std;

void init_timing(void) {
  init_frequency_info();
  stats.wallclockfreq = 1000000;
}
#endif

/*
  This function returns the walltime in milliseconds
*/

flt64_t wallclick_to_time(int64_t click) {
  return ((flt64_t)click * 1000) / stats.wallclockfreq;
}

flt64_t userclick_to_time(int64_t click) {
  return ((flt64_t)click * 1000) / stats.userclockfreq;
}

flt64_t systemclick_to_time(int64_t click) {
  return ((flt64_t)click * 1000) / stats.systemclockfreq;
}

flt64_t walltime(void) {
  return wallclick_to_time(wallclick());
}

/*
  This function has been modified by Edison Mera to prevents the
  truncation of the microseconds.  Very important in modern
  plattforms where the speed is given in GHz.
 */
static CBOOL__PROTO_N(generic_time,
		   int64_t (*time_function)(void),
		   int64_t startclick,
		   int64_t *lastclick,
		   int64_t clockfreq) {
  /*int st,lt; */
  int64_t st,lt;
  int64_t t;
  tagged_t x;

  t = time_function();
  st = t - startclick;
  lt = t - *lastclick;
  *lastclick = t;

  /* while ciao not support int64_t, lt and st must be cast to
    flt64_t */
  tagged_t k,j;
  k=BoxFloat(((flt64_t)lt)*1000/clockfreq);
  j=BoxFloat(((flt64_t)st)*1000/clockfreq);
  MakeLST(x,k,atom_nil);
  MakeLST(x,j,x);
  CBOOL__LASTUNIFY(x,X(0));  
}

/* runtime returns a list of two floats
 * giving time in milliseconds. The first number gives time from the system 
 * start_up and the second since the last call to runtime */
CBOOL__PROTO(prolog_runtime) {
  CBOOL__LASTCALL_N(generic_time, userclick, stats.startclick, &stats.lastclick, stats.userclockfreq);
}

CBOOL__PROTO(prolog_usertime) {
  CBOOL__LASTCALL_N(generic_time, userclick, stats.startuserclick, &stats.lastuserclick, stats.userclockfreq);
}

CBOOL__PROTO(prolog_systemtime) {
  CBOOL__LASTCALL_N(generic_time, systemclick, stats.startsystemclick, &stats.lastsystemclick, stats.systemclockfreq);
}

CBOOL__PROTO(prolog_walltime) {
  CBOOL__LASTCALL_N(generic_time, wallclick, stats.startwallclick, &stats.lastwallclick, stats.wallclockfreq);
}

/* New time medition functions */
CBOOL__PROTO_N(generic_click,
	    int64_t (*time_function)(void),
	    int64_t startclick,
	    int64_t *lastclick) {
  /*int st,lt; */
  int64_t st,lt;
  int64_t t;
  tagged_t x;

  t = time_function();
  st = t - startclick;
  lt = t - *lastclick;
  *lastclick = t;
  /* while ciao not support int64_t, lt and st must be cast to
    flt64_t */
  tagged_t k,j;
  k = BoxFloat((flt64_t)lt);
  j = BoxFloat((flt64_t)st);
  MakeLST(x,k,atom_nil);
  MakeLST(x,j,x);
  CBOOL__LASTUNIFY(x,X(0));  
}

CBOOL__PROTO(prolog_wallclick) {
  CBOOL__LASTCALL_N(generic_click, wallclick, stats.startwallclick, &stats.lastwallclick);
}

CBOOL__PROTO(prolog_userclick) {
  CBOOL__LASTCALL_N(generic_click, userclick, stats.startuserclick, &stats.lastuserclick);
}

CBOOL__PROTO(prolog_systemclick) {
  CBOOL__LASTCALL_N(generic_click, systemclick, stats.startsystemclick, &stats.lastsystemclick);    
}

CBOOL__PROTO(prolog_runclick) {
  CBOOL__LASTCALL_N(generic_click, userclick, stats.startclick, &stats.lastclick);
}

/* New time medition functions */
inline static CBOOL__PROTO_N(generic_clockfreq, int64_t clockfreq) {
  /* while ciao not support int64_t, return value must be cast to
    flt64_t */
  CBOOL__LASTUNIFY(BoxFloat((flt64_t)clockfreq),X(0));
}

CBOOL__PROTO(prolog_userclockfreq) {
  CBOOL__LASTCALL_N(generic_clockfreq, stats.userclockfreq);
}

CBOOL__PROTO(prolog_systemclockfreq) {
  CBOOL__LASTCALL_N(generic_clockfreq, stats.systemclockfreq);
}

CBOOL__PROTO(prolog_wallclockfreq) {
  CBOOL__LASTCALL_N(generic_clockfreq, stats.wallclockfreq);
}

void reset_timing() {
  stats.lastclick = stats.startclick = userclick();
  stats.startwallclick = wallclick();
  stats.lastwallclick = stats.startwallclick;
  stats.startuserclick = stats.startclick; /*userclick();*/
  stats.lastuserclick = stats.startuserclick;
  stats.startsystemclick = systemclick();
  stats.lastsystemclick = stats.startsystemclick;
}


