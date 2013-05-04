#include <engine/basiccontrol.native.h>

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <dirent.h>
#include <pwd.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>

CBOOL__PROTO(prolog_time) {
  time_t timeofday = time(NULL);
  CBOOL__LASTUNIFY(IntmachToTagged(timeofday),X(0));
}

/* datime(+Time,-Year,-Month,-Day,-Hour,-Min,-Sec,-WeekDay,-YearDay) */

CBOOL__PROTO(prolog_datime) {
  ERR__FUNCTOR("system:datime", 9);
  struct tm *datime;
  time_t inputtime[1];

  DEREF(X(0),X(0));

  if (IsVar(X(0))) {
    inputtime[0] = time(NULL);
    CBOOL__UNIFY(IntmachToTagged(inputtime[0]),X(0));
  } else if (IsInteger(X(0))) {
    inputtime[0] = TaggedToIntmach(X(0));
  } else {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  }

  datime = localtime(inputtime);

  CBOOL__UNIFY(MakeSmall((datime->tm_year)+1900),X(1));
  CBOOL__UNIFY(MakeSmall((datime->tm_mon)+1), X(2));
  CBOOL__UNIFY(MakeSmall(datime->tm_mday),X(3));
  CBOOL__UNIFY(MakeSmall(datime->tm_hour),X(4));
  CBOOL__UNIFY(MakeSmall(datime->tm_min), X(5));
  CBOOL__UNIFY(MakeSmall(datime->tm_sec), X(6));
  CBOOL__UNIFY(MakeSmall(datime->tm_wday),X(7));
  CBOOL__UNIFY(MakeSmall(datime->tm_yday),X(8));
  CBOOL__PROCEED;
}

/* To rename paths like /mounted/... */
/*
#define FIX_PATHS
#define REN_PAIRS {{"/mounted/","/"}, {"/localhome/","/home/"}, {"",""}}
*/

char *library_directory = NULL;

#if 0 /* Already defined in newer solaris versions */
//#if defined(Solaris)
intmach_t gethostname(char *name, intmach_t namelen);
#endif

#if defined(SunOS4)
#include <string.h>
intmach_t system(char *string);
intmach_t gethostname(char *name, intmach_t namelen);
intmach_t readlink(char *path, char *buf, intmach_t bufsiz);
#endif

#if defined(Win32)
#define DriveSelector(path) \
        (isalpha(path[0]) && path[1]==':' && \
         (path[2]=='/' || path[2]=='\\' || path[2]==(char)0))
#endif   

char cwd[MAXPATHLEN+1];/* Should be private --- each thread may cd freely! */

CBOOL__PROTO_N(expand_file_name, char *name, char *target) {

#if !defined(__pwd_h) && !defined(_PWD_H) && !defined(__PWD_H__) && !defined(_PWD_H_)
  extern struct passwd *getpwnam(char *);
#endif

  char *src, *dest;
  char src_buff[MAXPATHLEN+1];

  if (!name[0]) {
    target[0] = (char)0;
    return TRUE;
  }    

#if defined(Win32)
  src = name;
  dest = src_buff;
  while ((*dest = (*src == '\\') ? '/' : *src))
    ++src, ++dest;
#else
  strcpy(src_buff,name);
#endif

  /* contract // to / (non-initial in Win32) */
#if defined(Win32)
  src = dest = src_buff+1;
#else
  src = dest = src_buff;
#endif
  while ((*dest = *src)) {
    while (src[0] == '/' && src[1] == '/') ++src;
    ++dest ; ++src;
  }

  src = src_buff;
  dest = target;

  switch (*src) {
  case '$':        /* environment var */
    ++src;
  envvar:
    switch (*dest++ = *src++) {
    case 0:
    case '/':
      --src, --dest, dest[0] = (char)0;
      if (dest == target) {
        strcpy(target,library_directory);
        dest = target+strlen(target);
      } else {
        if (!(dest = getenv(target)))
          USAGE_FAULT("file name: undefined variable");
        target[0] = (char)0;
        strcpy(target,dest);
        dest = target+strlen(target);
      }
      goto st1;
    default:
      goto envvar;
    }
    break;
  case '~':        /* home directory */
    ++src;
  homedir:
    switch (*dest++ = *src++)
    {
    case 0:
    case '/':
      --src, --dest, dest[0] = (char)0;
      if (dest == target) {
        if (!(dest = getenv("HOME"))) {
       /* TRACE_PRINTF("library_directory = %s\n", library_directory); */
	  dest = library_directory;
	}
        strcpy(target,dest);
        dest = target+strlen(target);
      } else {
        struct passwd *pw;
        if (!(pw = getpwnam(target)))
          USAGE_FAULT("file name: no such user");
        strcpy(target,(char *)pw->pw_dir);
        dest = target+strlen(target);
      }
      goto st1;
    default:
      goto homedir;
    }
    break;
  case '/':        /* absolute path */
    src++;
    *dest++ = '/';
    break;
  default:
#if defined(Win32)
    if (DriveSelector(src)) {    /* c:/ */
      strcpy(dest,"/cygdrive/");
      dest[10] = tolower(src[0]);
      dest += 11;
      src += 2;
      goto st1;
    } else
#endif
      {
        strcpy(target,cwd);
        dest = target+strlen(target);
        if (dest[-1] != '/')
          *dest++ = '/';
      }
  }


 st0: /* prev char is '/' */
  switch (*dest++ = *src++) {
  case 0:
    if (dest-2 > target)
      dest[-2] = 0;
    goto end;
  case '/':
    goto st0;
  case '.':
    if (src[0] == '/' || src[0] == (char)0) {
      if (dest-2 >= target)
        dest -= 2;
    } else if (src[0] == '.' && (src[1] == '/' || src[1] == (char)0))	{
      if (dest-3 >= target) {
        dest -= 3;
        while (--dest, dest[0] != '/')
          ;
        src++;
      }
    }
  }
  
 st1: /* inside file name component */
  switch (*dest++ = *src++) {
    case 0:
      goto end;
    case '/':
      goto st0;
    default:
      goto st1;
  }

 end:
  if (target[0] == (char)0) /* root directory */
    target[0] = '/', target[1] = (char)0;
  return TRUE;
}


#if defined(FIX_PATHS) /* Renaming paths like /mounted/... */

struct ren_pair { char *from; char *to; };

static struct ren_pair rename_path_pairs[] = REN_PAIRS;

bool_t fix_path(char *path) {
  char *from, *p1, buf[MAXPATHLEN+1];
  struct ren_pair *rp;

  for (rp = rename_path_pairs; *(from = rp->from) ; rp++) {
    for (p1 = path ; *from && *p1 ; from++, p1++) {
      if (*from != *p1) {break;}; /* "path" does not start with "from" */
    }
    if (! *from) { /* "path" starts with "from" */
      strcpy(buf,p1);
      strcpy(path,rp->to);
      strcat(path,buf);
      return TRUE;
    }
  }

  return FALSE;
}
#endif

void compute_cwd() {
  if (getcwd(cwd,MAXPATHLEN+1)); /* TODO: do not ignore result */

#if defined(FIX_PATHS)
  fix_path(cwd);
#endif
}

CBOOL__PROTO(prolog_unix_cd) {
  ERR__FUNCTOR("system:working_directory", 2);
  char pathBuf[MAXPATHLEN+1];
  struct stat statbuf;
  CBOOL__UnifyCons(GET_ATOM(cwd),X(0));
  DEREF(X(0), X(0));

  DEREF(X(1), X(1));
  if (IsVar(X(1))){
    BUILTIN_ERROR(INSTANTIATION_ERROR,X(1),2);
  }

  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(1)))
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(1),2);
  /* ORGAMA: check argument domain error */
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(1)),pathBuf))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);

  /* OGRAMA: check file exists */
  if (stat(pathBuf, &statbuf)) 
    if (current_ferror_flag==atom_on)
      BUILTIN_ERROR(PERMISSION_ERROR(ACCESS,PERMISSION_OBJECTS(stream)), X(1), 2);
  /* OGRAMA: If there is another problem ...*/
  if (chdir(pathBuf))
    BUILTIN_ERROR(SYSTEM_ERROR,X(1),2);

  compute_cwd();
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_unix_shell0) {
  ERR__FUNCTOR("system:shell", 0);
  char cbuf[MAXPATHLEN+10];

  strcpy(cbuf,"exec ");
  strcat(cbuf,getenv("SHELL"));
  /* OGRAMA: if can't create a new shell, system_error */
  if (!system(cbuf)) {
    CBOOL__PROCEED;
  } else {
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
  }
}

CBOOL__PROTO(prolog_unix_shell2) {
  ERR__FUNCTOR("system:shell", 2);
  char *p1, *p2;
  intmach_t system_result;
  char *cbuf = CHECKALLOC_ARRAY(char, 2*MAXATOM+MAXPATHLEN+20);

  DEREF(X(0),X(0));

  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0)))
    ERROR_IN_ARG(X(0),1,STRICT_ATOM);

  strcpy(cbuf,"exec ");
  strcat(cbuf,getenv("SHELL"));
  strcat(cbuf," -c ");
  p1 = cbuf+strlen(cbuf);
  for(p2=GetString(X(0)); *p2;)
    *p1++ = '\\',
      *p1++ = *p2++;
  *p1++ = 0;
  /* try to execute, raise an exection if there is some error */
  system_result = system(cbuf);
  CHECKDEALLOC0_ARRAY(char, cbuf, 2*MAXATOM+MAXPATHLEN+20);
  if (system_result)
    BUILTIN_ERROR(SYSTEM_ERROR,X(0),2);
  CBOOL__LASTUNIFY(MakeSmall(system_result),X(1));
}

CBOOL__PROTO(prolog_unix_system2) {
  ERR__FUNCTOR("system:system", 2);

  DEREF(X(0),X(0));
  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);

  CBOOL__LASTUNIFY(MakeSmall(system(GetString(X(0)))),X(1));
}

/* Current executable */

CBOOL__PROTO(prolog_current_executable) {
  char source_path[MAXPATHLEN] = "";
  CBOOL__CALL_N(expand_file_name, prolog_argv[0], source_path);
  DEREF(X(0),X(0));
  CBOOL__LASTUNIFY(GET_ATOM(source_path), X(0));
}

CBOOL__PROTO(prolog_unix_mktemp) {
  ERR__FUNCTOR("system:mktemp", 2);
  char template[STATICMAXATOM];
  intmach_t fildes;

  DEREF(X(0),X(0));

  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);

  /* OGRAMA: check type argument */
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);

  strcpy(template, GetString(X(0)));

  /* OGRAMA: if mkstemp fails, give a system error */
  if ((fildes = mkstemp(template)) <  0) {
    BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);
  } else {
    /* Do not leave it open, since the stream is not seen at Prolog
       level */
    close(fildes);
    CBOOL__LASTUNIFY(GET_ATOM(template), X(1));
  }
}

CBOOL__PROTO(prolog_unix_access) {
  ERR__FUNCTOR("system:file_exists", 2);
  char pathBuf[MAXPATHLEN+1];
  intmach_t mode;

  DEREF(X(0),X(0));

  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument */
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);

  DEREF(X(1),X(1));

  if (!TaggedIsSmall(X(1)) || (mode = GetSmall(X(1))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(1),2,BYTE);

  CBOOL__CALL_N(expand_file_name,GetString(X(0)),pathBuf);

  /* print_syserror("% access in file_exits/2"); --this must be quiet. */
  /*  MINOR_FAULT("access() failed");  */
  /* --MCL: no need to raise any exception */
  CBOOL__TEST(access(pathBuf,mode) == 0);
  CBOOL__PROCEED;
}

/* '$unix_popen'(+Command, +Mode, -Stream) */
CBOOL__PROTO(prolog_unix_popen) {
  FILE *f;
  char *streammode;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  streammode = (X(1) == atom_read ? "r" : "w");

  f = popen(GetString(X(0)),streammode);
  CBOOL__TEST(f != NULL);

  CBOOL__LASTUNIFY(CFUN__EVAL_N(ptr_to_stream, new_stream((tagged_t)0, streammode, f)), X(2));
}

/* directory_files(+Path, FileList) */

CBOOL__PROTO(prolog_directory_files) {
  ERR__FUNCTOR("system:directory_files", 2);
  char pathBuf[MAXPATHLEN+1];
  DIR *dir;
  intmach_t gap;
  struct dirent *direntry;

  /* Using X(2) to build the result - DCG */

  DEREF(X(0),X(0));

  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);

  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);

  /* OGRAMA: check domain argument */
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(0)),pathBuf))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);

  /* OGRAMA: Raise an execetion if can't open the directory */
  if (! (dir = opendir(pathBuf))) {
    /* By Edison Mera: */
    /* First, identifying the error type: */
    switch(errno) {
    case EACCES:
      BUILTIN_ERROR(PERMISSION_ERROR(OPEN,PERMISSION_OBJECTS(stream)),X(0),1);
      break;
    case EMFILE:
      BUILTIN_ERROR(RESOURCE_ERROR,X(0),1);
      break;
    case ENFILE:
      BUILTIN_ERROR(RESOURCE_ERROR,X(0),1);
      break;
    case ENOENT:
      BUILTIN_ERROR(EXISTENCE_ERROR(EXISTENCE_ERRORS(stream)),X(0),1);
      break;
    case ENOMEM:
      BUILTIN_ERROR(RESOURCE_ERROR,X(0),1);
      break;
    case ENOTDIR:
      BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
      break;
    default:
      BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);
      break;
    }
  } else {
    X(2) = atom_nil;
    gap = HeapCharAvailable(G->heap_top) - CONTPAD;
    while ((direntry = readdir(dir))) {
      if ((gap -= 2*sizeof(tagged_t)) < 0) {
	CVOID__CALL_N(explicit_heap_overflow,(CONTPAD+32*sizeof(tagged_t))*2,3);
	gap += 32*sizeof(tagged_t);
      }
      MakeLST(X(2),GET_ATOM(direntry->d_name),X(2));
    }
    closedir(dir);
  }
  CBOOL__LASTUNIFY(X(2),X(1));
}

/* file_properties(+File, Type, Linkto, ModTime, Protection, Size)

   ModTime: the time (in seconds since 1, Jan, 1970, since file File
   (absolute path) was last modified.
 */

CBOOL__PROTO(prolog_file_properties) {
  ERR__FUNCTOR("system:file_properties", 6);
  struct stat statbuf;
  char pathBuf[MAXPATHLEN+1];
  char symlinkName[STATICMAXATOM+1];
  intmach_t len;

  DEREF(X(0),X(0));
  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);
  /* OGRAMA: check argument domain error */
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(0)),pathBuf))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
  DEREF(X(2),X(2));
  if (X(2)!=atom_nil) { /* Link wanted */
    symlinkName[0] = (char) 0;
    if ((len=readlink(pathBuf, symlinkName, STATICMAXATOM)) > 0)
      symlinkName[len] = (char) 0;
    CBOOL__UnifyCons(GET_ATOM(symlinkName),X(2));
  }

  DEREF(X(1),X(1));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));
  if ((X(1)!=atom_nil)
      || (X(3)!=atom_nil)
      || (X(4)!=atom_nil)
      || (X(5)!=atom_nil)) {

    if (stat(pathBuf, &statbuf)) {
      if (current_ferror_flag==atom_on) {
        BUILTIN_ERROR(PERMISSION_ERROR(ACCESS,PERMISSION_OBJECTS(stream)), X(0), 1);
      } else {
	CBOOL__FAIL;
      }
    }

    if (X(1)!=atom_nil) {
      CBOOL__UnifyCons((S_ISREG(statbuf.st_mode) ? atom_regular
                      : S_ISDIR(statbuf.st_mode) ? atom_directory
                      : S_ISLNK(statbuf.st_mode) ? atom_symlink
                      : S_ISFIFO(statbuf.st_mode) ? atom_fifo
                      : S_ISSOCK(statbuf.st_mode) ? atom_socket
                      : atom_unknown), X(1));
    }

    if (X(3)!=atom_nil) {
      /* Cannot be CBOOL__UnifyCons because it may require a bignum */
      CBOOL__UNIFY(IntmachToTagged(statbuf.st_mtime),X(3));  
    }

    if (X(4)!=atom_nil) {
      CBOOL__UnifyCons(MakeSmall(statbuf.st_mode&0xfff),X(4));
    }

    if (X(5)!=atom_nil) {
      CBOOL__UnifyCons(MakeSmall(statbuf.st_size), X(5));
    }
  }
  
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_unix_chmod) {
  ERR__FUNCTOR("system:chmod", 2);
  char pathBuf[MAXPATHLEN+1];
  struct stat statbuf;
  DEREF(X(0),X(0));
  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);
  /* OGRAMA: check domain argument */
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(0)),pathBuf))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
  DEREF(X(1),X(1));
  /* OGRAMA: check instatiation error to the other argument*/
  if (IsVar(X(1))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(1),2);
  /* OGRAMA: and check type argument again */
  if (!TaggedIsSmall(X(1))) BUILTIN_ERROR (TYPE_ERROR(INTEGER),X(1),2);

  /* OGRAMA: check file exists */
  if (stat(pathBuf, &statbuf))
    if (current_ferror_flag==atom_on)
      BUILTIN_ERROR(PERMISSION_ERROR(ACCESS,PERMISSION_OBJECTS(stream)), X(0), 1);
  /* make call to chmod, if there is any proble, raise a system error */
  if (chmod(pathBuf, GetSmall(X(1))))
    BUILTIN_ERROR(SYSTEM_ERROR,X(1),2);
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_unix_umask) {
  ERR__FUNCTOR("system:umask", 2);
  intmach_t i;
  
  DEREF(X(1),X(1));
  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(1))) {
    if (X(1)==X(0)) {
      i = umask(0);
      (void)umask(i);
      CBOOL__LASTUNIFY(MakeSmall(i),X(0));
    } else 
      BUILTIN_ERROR(INSTANTIATION_ERROR,X(1),2);
  } else {
    /* OGRAMA: check type argument*/
    if (!TaggedIsSmall(X(1)))
      BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
    CBOOL__LASTUNIFY(MakeSmall(umask(GetSmall(X(1)))),X(0));
  }
}




CBOOL__PROTO(prolog_unix_delete) {
  ERR__FUNCTOR("system:delete_file", 1);
  char pathBuf[MAXPATHLEN+1];
  struct stat statbuf;
  DEREF(X(0),X(0));
  /* OGRAMA: check argument instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);
  /* OGRAMA: check argument domain error */
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(0)),pathBuf))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);
      /* OGRAMA: verify that the file exists */
  if (stat(pathBuf, &statbuf))
    if (  (errno != EACCES) &&
	  (current_ferror_flag==atom_on)  )
      BUILTIN_ERROR(PERMISSION_ERROR(ACCESS,PERMISSION_OBJECTS(stream)), X(0), 1);
  /* try to unlink, if anything go wrong, raise a system error */
  if (unlink(pathBuf))
    BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);
  CBOOL__PROCEED;
}


CBOOL__PROTO(prolog_unix_rename) {
  ERR__FUNCTOR("system:rename_file", 2);
  char 
    orig_name[MAXPATHLEN+1],
    new_name[MAXPATHLEN+1];
  struct stat statbuf;

  DEREF(X(0),X(0));
  /* OGRAMA: check instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);

  DEREF(X(1),X(1));
  /* OGRAMA: check instantiation error to the other argument */
  if (IsVar(X(1))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(1),2);
  /* OGRAMA: check type the other argument*/
  if (!TaggedIsATM(X(1))) ERROR_IN_ARG(X(1),2,STRICT_ATOM);
  /* check domain of the two arguments */
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(0)),orig_name))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(1)),new_name))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);
  /* OGRAMA: check file exists */
  if (stat(orig_name, &statbuf))
    if (current_ferror_flag==atom_on)
      BUILTIN_ERROR(PERMISSION_ERROR(ACCESS,PERMISSION_OBJECTS(stream)), X(0), 1);
  /* if anything fails, raise and exception */
  if (rename(orig_name, new_name))
    BUILTIN_ERROR(SYSTEM_ERROR,X(1),2);
  CBOOL__PROCEED;
}


CBOOL__PROTO(prolog_unix_mkdir) {
  ERR__FUNCTOR("system:make_directory", 2);
  char dirname[MAXPATHLEN+1];
  intmach_t mode;

  DEREF(X(0),X(0));
  /* OGRAMA: check instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);
  /* OGRAMA: check domain argument */
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(0)),dirname))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
  DEREF(X(1),X(1));
  /* OGRAMA: check instantiation error */
  if (IsVar(X(1))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(1),2);
  /* OGRAMA: check type argument*/
  if (!TaggedIsSmall(X(1))) BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);

  mode = GetSmall(X(1));
  /* call to mkdir, if there is a problem, raise a system error */
  if (mkdir(dirname, mode))
    BUILTIN_ERROR(SYSTEM_ERROR,X(1),2);
  CBOOL__PROCEED;
}
CBOOL__PROTO(prolog_unix_rmdir) {
  ERR__FUNCTOR("system:delete_directory", 1);
  char dirname[MAXPATHLEN+1];
  struct stat statbuf;
  DEREF(X(0),X(0));
  /* OGRAMA: check instantiation error */
  if (IsVar(X(0))) BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsATM(X(0))) ERROR_IN_ARG(X(0),1,STRICT_ATOM);
  /* OGRAMA: Check domain error */
  if (!CBOOL__SUCCEED_N(expand_file_name,GetString(X(0)),dirname))
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
  /* OGRAMA: check that file exists */
  if (stat(dirname, &statbuf))
    if (current_ferror_flag==atom_on)
      BUILTIN_ERROR(PERMISSION_ERROR(ACCESS,PERMISSION_OBJECTS(stream)), X(0), 1);
  /* OGRAMA: and try to make rmdir, else, system_error */
  if (rmdir(dirname))
    BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);
  CBOOL__PROCEED;
}



/*
 *  current_host(?HostName).
 */
CBOOL__PROTO(prolog_current_host) {
  ERR__FUNCTOR("system:current_host", 1);
  char hostname[MAXHOSTNAMELEN*4];
  
  if (gethostname(hostname, sizeof(hostname)) < 0)
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
    
  if (!strchr(hostname, '.')) {
    struct hostent *host_entry;
    char **aliases;
    
    /* If the name is not qualified, then pass the name through the name
       server to try get it fully qualified */
    /* OGRAMA: if null, its a system error */
    if ((host_entry = gethostbyname(hostname)) == NULL)
      BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
    strcpy(hostname, host_entry->h_name);
    
    /* If h_name is not qualified, try one of the aliases */
    
    if ((aliases=host_entry->h_aliases)) {
      while (!strchr(hostname, '.') && *aliases)
	strcpy(hostname, *aliases++);
      if (!strchr(hostname, '.'))
	strcpy(hostname, host_entry->h_name);
    }
    
#if HAS_NIS
    /* If still unqualified, then get the domain name explicitly.
       This code is NIS specific, and causes problems on some machines.
       Apollos don't have getdomainname, for example. */
    if (!strchr(hostname, '.')) {
      char domain[MAXHOSTNAMELEN*3];
      
      if (getdomainname(domain, sizeof(domain)) < 0)
	BUILTIN_ERROR(SYSTEM_ERROR,Arg,1);
      strcat(hostname, ".");
      strcat(hostname, domain);
    }
#endif
    /*free(host_entry);*/
  }

  DEREF(X(0),X(0));
  CBOOL__LASTUNIFY(GET_ATOM(hostname), X(0));
}

#if 0 /* Already defined in newer solaris versions */
//#if defined(Solaris)
/* emulate setenv in terms of putenv (from rpm 2.0.9) */
intmach_t setenv(const char *name, const char *value, intmach_t overwrite)
{
  intmach_t len;
  if (!overwrite && getenv(name)) return 0;
  len = strlen(name) + strlen(value) + 2;
  if (len < 255) {
    char buf[256];
    strcpy(buf, name);
    strcat(buf, "=");
    strcat(buf, value);
    return putenv(buf);
  } else {
    char *buf = malloc(len);
    strcpy(buf, name);
    strcat(buf, "=");
    strcat(buf, value);
    return putenv(buf);
  }
}
#endif

/* By Edison Mera: */
#define BUF_MAX 65536
CBOOL__PROTO(prolog_c_copy_file) {
  ERR__FUNCTOR("system:copy_file", 2);
  char *source, *destination;
  intmach_t fd_source, fd_destination;
  ssize_t s;
  char buffer[BUF_MAX];

  DEREF(X(0),X(0));
  source = GetString(X(0));
  fd_source = open(source, O_RDONLY);
  if(fd_source==-1) {
    /* First, identifying the error type: */
    switch(errno) {
    case EISDIR:
      BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
      break;
    case EACCES:
      BUILTIN_ERROR(PERMISSION_ERROR(OPEN,PERMISSION_OBJECTS(stream)),X(0),1);
      break;
    case ENAMETOOLONG:
      BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
      break;
    case ENOENT:
      BUILTIN_ERROR(EXISTENCE_ERROR(EXISTENCE_ERRORS(stream)),X(0),1);
      break;
    case ENOTDIR:
      BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(0),1);
      break;
    case EMFILE:
      BUILTIN_ERROR(RESOURCE_ERROR,X(0),1);
      break;
    case ENFILE:
      BUILTIN_ERROR(RESOURCE_ERROR,X(0),1);
      break;
    case ENOMEM:
      BUILTIN_ERROR(RESOURCE_ERROR,X(0),1);
      break;
    default:
      BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);
      break;
    }
  }

  DEREF(X(1),X(1));
  destination = GetString(X(1));
  fd_destination = open(destination, O_WRONLY|O_CREAT|O_EXCL,
			S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
  if (fd_destination==-1) {
    /* Now we must close source */
    close(fd_source);
    /* Identifying the error type: */
    switch(errno) {
    case EEXIST:
      BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);
      break;
    case EISDIR:
      BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);
      break;
    case EACCES:
      BUILTIN_ERROR(PERMISSION_ERROR(OPEN,PERMISSION_OBJECTS(stream)),X(1),2);
      break;
    case ENOENT:
      BUILTIN_ERROR(PERMISSION_ERROR(OPEN,PERMISSION_OBJECTS(stream)),X(1),2);
      break;
    case ENAMETOOLONG:
      BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);
      break;
    case ENOTDIR:
      BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);
      break;
    case EMFILE:
      BUILTIN_ERROR(RESOURCE_ERROR,X(1),2);
      break;
    case ENFILE:
      BUILTIN_ERROR(RESOURCE_ERROR,X(1),2);
      break;
    case ENOMEM:
      BUILTIN_ERROR(RESOURCE_ERROR,X(1),2);
      break;
    default:
      BUILTIN_ERROR(SYSTEM_ERROR,X(1),2);
      break;
    }
  }
  while((s=read(fd_source,buffer,BUF_MAX))!=0){
    if(s==-1){
      close(fd_source);
      close(fd_destination);
      BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);
    }
    else
    {
      if(write(fd_destination,buffer,s)==-1){
	close(fd_source);
	close(fd_destination);
        BUILTIN_ERROR(SYSTEM_ERROR,X(1),2);
      }
    }
  }
  close(fd_source);
  close(fd_destination);
  CBOOL__PROCEED;
}


CBOOL__PROTO(prolog_c_get_env) {
  char *name, *value;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  name = GetString(X(0));
  value = getenv(name);
  if(value==NULL)
    CBOOL__FAIL;
  else
    CBOOL__LASTUNIFY(GET_ATOM(value), X(1));
}

CBOOL__PROTO(prolog_c_set_env) {
  ERR__FUNCTOR("system:set_env", 2);
  char *name, *value;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  name = GetString(X(0));
  value = GetString(X(1));
  if(setenv(name,value,1))
    BUILTIN_ERROR(RESOURCE_ERROR,X(0),1);
  else
    CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_c_del_env) {
  char *name;
  DEREF(X(0),X(0));
  name = GetString(X(0));
#if defined(Solaris)
  putenv(name);
#else
  unsetenv(name);
#endif
  CBOOL__PROCEED;
}

extern char ** environ;

CBOOL__PROTO(prolog_c_current_env) {
  intmach_t n, index;
  char *name, *value;
  tagged_t name_atom;
  tagged_t value_atom;
  DEREF(X(0),X(0));
  index = TaggedToIntmach(X(0));
  name = environ[index];
  CBOOL__TEST(name!=NULL);
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  value = strchr(environ[index],'=');
  n = (intmach_t)(value-name);
  value++;
  name = memcpy((char *)malloc((intmach_t)(value-name)*sizeof(char)),
		name,(intmach_t)(value-name)*sizeof(char));
  name[n] = '\0';
  name_atom = GET_ATOM(name);
  value_atom = GET_ATOM(value);
  free(name);
  CBOOL__UNIFY(name_atom, X(1));
  CBOOL__LASTUNIFY(value_atom, X(2));
}

/*
  pause(+Seconds): make this process sleep for Seconds seconds
*/

CBOOL__PROTO(prolog_pause) {
  ERR__FUNCTOR("system:pause", 1);
  tagged_t x0;
  intval_t time;
  
  DEREF(x0, X(0));
  /* OGRAMA: check instantiation_error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  /* OGRAMA: check type argument*/
  if (!TaggedIsSmall(x0))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  time = GetSmall(x0);

  sleep(time);

  CBOOL__PROCEED;
}


/*
  get_pid(?PID): PID is unified with  the process identificator number 
  of this process 
*/

CBOOL__PROTO(prolog_getpid) {
  tagged_t x0;

  DEREF(x0, X(0));
  CBOOL__LASTUNIFY(x0, MakeSmall(getpid()));
}


/* $find_file(+LibDir,+Path,+Opt,+Suffix,?Found,-AbsPath,-AbsBase,-AbsDir)
 * string LibDir	a library in which to search for Path
 * string Path		a path, may be absolute or relative. If LibDir
 *			is specified then Path must be relative to LibDir.
 * string Opt           an optional suffix to Path, must precede Suffix, is
 *                      included in AbsBase
 * string Suffix        an optional suffix to Path, not included in AbsBase
 * atom   Found         true or fail
 * string AbsPath       the absolute pathname of Path
 * string AbsBase       the absolute pathname of Path, without Suffix
 * string AbsDir        the absolute pathname of the directory of Path
 *
 * Description: Try to find in LibDir, in this order:
 *   Path+Opt+Suffix
 *   Path+Suffix
 *   Path
 *   Path/Path+Opt+Suffix
 *   Path/Path+Suffix
 *   Path/Path
 * if any found, unify Found with true, and return in AbsPath, AbsBase and
 * AbsDir the appropriate values, else unify Found with false, and return in
 * AbsPath, AbsBase and AbsDir the values corresponding to the last option
 * (no Opt nor Suffix).
 */

#if !defined(S_ISDIR)                                 /* Notably, Solaris */
#  define S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#endif

CBOOL__PROTO(prolog_find_file) {
  char *libDir, *path, *opt, *suffix;
  char pathBuf[MAXPATHLEN+8];
  char *relBuf = CHECKALLOC_ARRAY(char, 2*MAXATOM+2);
  char *bp;
  char *cp;
  struct stat file_status;
  time_t t_opt, t_pri;

  DEREF(X(0),X(0));
  libDir = GetString(X(0));
  DEREF(X(1),X(1));
  path = GetString(X(1));
  DEREF(X(2),X(2));
  opt = GetString(X(2));
  DEREF(X(3),X(3));
  suffix = GetString(X(3));
  
  if (path[0] == '/' || path[0] == '$' || path[0] == '~'
#if defined(Win32)
      || path[0] == '\\' || DriveSelector(path)
#endif
      ) {
    strcpy(relBuf,path);
  } else {
    strcpy(relBuf,libDir);
    if (relBuf[strlen(relBuf)-1]!='/')
      strcat(relBuf,"/");
    strcat(relBuf,path);
  }

  if (!CBOOL__SUCCEED_N(expand_file_name,relBuf,pathBuf)){
    CHECKDEALLOC0_ARRAY(char, relBuf, 2*MAXATOM+2);
    CBOOL__FAIL;
  }

#if defined(FIX_PATHS)
  fix_path(pathBuf);
#endif

  cp = pathBuf + strlen(pathBuf);

  t_opt = t_pri = 0;

 searchPath:
  
  if (*opt) {
    strcpy(cp,opt);
    bp = cp + strlen(cp);
    strcpy(bp,suffix);
    if(!access(pathBuf,F_OK)) {
      stat(pathBuf, &file_status);
      if (!S_ISDIR(file_status.st_mode))
        t_opt = file_status.st_mtime;    /* found path+opt+suffix */
    }
  }
  
  bp = cp;

  if (*suffix) {
    strcpy(bp,suffix);
    if(!access(pathBuf,F_OK)) {
      stat(pathBuf, &file_status);
      if (!S_ISDIR(file_status.st_mode))
        t_pri = file_status.st_mtime;    /* found path+suffix */
    }
  }

  if (t_pri > t_opt) { /* path+suffix exists, path+opt+suffix older|absent */
    CBOOL__UnifyCons(atom_true,X(4));
    goto giveVals;
  } else if (t_opt > 0) { /* newer path+opt+suffix exists */
    /* recreate opt+suffix */
    strcpy(cp,opt);
    bp = cp + strlen(cp);
    strcpy(bp,suffix);
    CBOOL__UnifyCons(atom_true,X(4));
    goto giveVals;
  }

  *bp = 0;
  
  if(!access(pathBuf,F_OK)){
    stat(pathBuf, &file_status);
    if (S_ISDIR(file_status.st_mode)) {    /* directory */
      while (*bp!='/') --bp;               /* duplicate dir name */
      *cp++ = *bp++ ;
      while (*bp!='/') *cp++ = *bp++ ;
      *cp = 0;
      goto searchPath;                     /* search inside */
    } else {
      CBOOL__UnifyCons(atom_true,X(4));      /* found path */
      if (*suffix && strcmp(bp -= strlen(suffix), suffix))
        /* does not end in suffix */
        bp = cp;
      goto giveVals;
    }
  }

  CBOOL__UnifyCons(atom_fail,X(4));

 giveVals:

  CBOOL__UnifyCons(GET_ATOM(pathBuf),X(5));

  *bp = 0;

  CBOOL__UnifyCons(GET_ATOM(pathBuf),X(6));

  while (*bp!='/') --bp;
  *bp = 0;

  CBOOL__UnifyCons(GET_ATOM(pathBuf),X(7));
  
  CHECKDEALLOC0_ARRAY(char, relBuf, 2*MAXATOM+2);
  CBOOL__PROCEED;
}

/*
 * exec(+Process, -StdIn, -StdOut, -StdErr): connect to an external process
 */

#define Read  0 
#define Write 1 

#define STDIN  0 
#define STDOUT 1 
#define STDERR 2

CBOOL__PROTO(prolog_exec) {
  ERR__FUNCTOR("system:exec", 3);

  DEREF(X(0), X(0));
  if (IsVar(X(0))) {
    /* OGRAMA: check instantiation error */
    BUILTIN_ERROR(INSTANTIATION_ERROR,X(0),1);
  } else if (!TaggedIsATM(X(0))) {
    /* OGRAMA: check instantiation error */
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);
  } else {
    char *command;
    bool_t dup_stderr;
    int pipe_in[2]; /* Child standard input */
    int pipe_out[2]; /* Child standard output */
    int pipe_err[2]; /* Child standard error */

    stream_node_t *str_in; /* Connection to child standard input  */
    stream_node_t *str_out; /* Connection to child standard output  */
    stream_node_t *str_err = NULL; /* Connection to child standard error  */

    pid_t pid;

    if (pipe(pipe_in)); /* TODO: do not ignore result */
    if (pipe(pipe_out)); /* TODO: do not ignore result */

    DEREF(X(3), X(3));
    if (X(3) == atom_nil) {
      dup_stderr = FALSE;
    } else {
      dup_stderr = TRUE;
      if (pipe(pipe_err)); /* TODO: do not ignore result */
    }

    command = GetString(X(0));

    /* Empty buffers before launching child */
    fflush(NULL);

    pid = fork();

    if (pid == -1) {
      /* OGRAMA: when can't launch a child, system_error */
      BUILTIN_ERROR(SYSTEM_ERROR,X(0),2);
    } else if (pid == 0) {
      /* Child */
      close(pipe_in[Write]);      
      dup2(pipe_in[Read],STDIN);
      close(pipe_out[Read]);
      dup2(pipe_out[Write],STDOUT);
      if (dup_stderr) {
	close(pipe_err[Read]);
	dup2(pipe_err[Write],STDERR);
      }
      /* OGRAMA: try to execute sh ..., if it doesn't work, system error */
      if (execlp("sh", "sh", "-c", command, NULL) < 0) {
	BUILTIN_ERROR(SYSTEM_ERROR,X(0),2);
      } else {
	CBOOL__PROCEED;
      }
    } else {                                                    /* Parent */
      close(pipe_in[Read]);
      str_in  = new_stream(X(0), "w", fdopen(pipe_in[Write], "w"));
      close(pipe_out[Write]);
      str_out = new_stream(X(0), "r", fdopen(pipe_out[Read], "r"));
      if (dup_stderr) {
	close(pipe_err[Write]);
	str_err = new_stream(X(0), "r", fdopen(pipe_err[Read], "r"));
      }
      CBOOL__UNIFY(CFUN__EVAL_N(ptr_to_stream, str_in), X(1));
      CBOOL__UNIFY(CFUN__EVAL_N(ptr_to_stream, str_out), X(2));
      if (dup_stderr) {
	CBOOL__UNIFY(CFUN__EVAL_N(ptr_to_stream, str_err), X(3));
      }
      CBOOL__PROCEED;
    }
  }
}

/* Initialize the library_directory from CIAOROOT environment variable */

#if defined(Win32)
#define SUBDIR_WINDOWS_BIN "/Win32/bin"
#endif

void init_ciaolib() {
  library_directory = getenv("CIAOROOT");
  if (!library_directory) {
#if defined(Win32)
    /* Otherwise, look in the registry (for Windows executables) and set a
       couple more of variables  */ 
    /* These are for the registry */
    HKEY SOFTWAREKey, CiaoPrologKey;
    DWORD buffer_size = MAXPATHLEN;

    /* These are to locate the shell (needed for the shell/1 call) */
    char *temp_path = CHECKALLOC_ARRAY(char, MAXPATHLEN+1);
    char *current_path;
    char *current_path_local;
 
    library_directory = CHECKALLOC_ARRAY(char, MAXPATHLEN+1);
     
    if ((RegOpenKeyEx(HKEY_LOCAL_MACHINE, TEXT("SOFTWARE"), 0, KEY_READ,
		      &SOFTWAREKey) == ERROR_SUCCESS ) &&
	(RegOpenKeyEx(SOFTWAREKey, TEXT("Ciao Prolog"), 0, KEY_READ,
		      &CiaoPrologKey) == ERROR_SUCCESS ) &&
	(RegQueryValueEx(CiaoPrologKey, TEXT("ciao_dir"), NULL, NULL,
			 library_directory, &buffer_size) == ERROR_SUCCESS)) {
      RegCloseKey(SOFTWAREKey);
      RegCloseKey(CiaoPrologKey);
    } else {
      fprintf(stderr,
	      "%s\n%s\n",
	      "Registry key not found. Please remember to install Ciao Prolog"
	      "or to set the CIAOLIB environment variable!");
      engine_exit(1); 
    }

    /* TODO: is this correct? why is this code executed only when
       ciaolib env is not set? */

    /* Now, we adjust a couple of things to be used inside Windows;
       outstandingly, the PATH and the presence of the SHELL variable.
       We assume that ciaoengine.exe (if there is any), cygwin.dll, and
       sh.exe are in the same directory.  This is placed either in the 
       Win32/bin subdir or in the applications subdir (if it is packed).

       The SHELL environment variable, if not already set, should point to
       the sh.exe executable.
    */
    
    /* 
       We need the library directory here.  It either points to an
       installation directory, and it is stored in the registry, or exists
       because we got it from an environment variable, or we reverted to
       the "current directory", for "packed" distributions.  The last one
       not yet implemented. */

    /* Guess which one exists.  Start with the Win32/bin option */

    strcpy(temp_path, library_directory);
    strcat(temp_path, SUBDIR_WINDOWS_BIN);
    if (access(temp_path, F_OK)){ 
      /* Does not exist --- look in the libdir itself */
      strcpy(temp_path, library_directory);
    }

    /* Is it already in the PATH? */
    if (!(current_path = getenv("PATH")) ||               /* No path or */
	!strstr(current_path, temp_path)) {      /* does not contain it */
      /* Add to $PATH at the end */
      if (current_path == NULL)
	current_path_local = temp_path;
      else {                           /* Do not alter the env. itself! */
	current_path_local = CHECKALLOC_ARRAY(char, strlen(current_path) + MAXPATHLEN + 2);
	strcpy(current_path_local, current_path);
	strcat(current_path_local, ":");
	strcat(current_path_local, temp_path);
      }
      setenv("PATH", current_path_local, 1);
    }

    /* Check now if the SHELL variable has been defined --- the
       shell/{0,3} calls depend on it. */
    if (!getenv("SHELL")){
      strcat(temp_path, "/sh.exe");  /* CygWin shell --- MUST be here */
      setenv("SHELL", temp_path, 1);
    }
#else
    /* Revert to installation-defined library directory otherwise */
    library_directory = installibdir;
#endif
  }
}


