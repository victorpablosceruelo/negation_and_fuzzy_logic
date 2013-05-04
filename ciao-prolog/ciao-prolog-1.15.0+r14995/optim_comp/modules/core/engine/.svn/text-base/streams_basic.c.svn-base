#include <engine/basiccontrol.native.h>

#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h> 
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#if !defined(crossWin32i86)
# include <sys/param.h>
# include <sys/errno.h>
#endif

#define ENG_NOFILES 20

try_node_t *address_nd_current_stream;

CVOID__PROTO(init_inout) {
  address_nd_current_stream = CFUN__EVAL_N(def_retry_cbool,nd_current_stream,4);
}

/* -------------------------------------------------------------- */
/* Functions to check types (useful to find when exceptions should be raised)*/

/* ISO Prolog does not allow stream aliases to be used instead of stream
   terms in many cases.  We are relaxing this here. */

CBOOL__PROTO_N(is_var_or_alias_or_stream, tagged_t cell) {
  CBOOL__LASTTEST(
		  IsVar(cell) || 
		  (TaggedIsATM(cell) &&
		   (cell == atom_user_input ||
		    cell == atom_user_output ||
		    cell == atom_user_error
		    )
		   ) ||
		  (TaggedIsSTR(cell) &&
		   TaggedToHeadfunctor(cell) == functor_Dstream));
}

/* -------------------------------------------------------------- */
/*  USAGE:  open(+,+,-) only  */

CBOOL__PROTO(prolog_open) {
  ERR__FUNCTOR("streams_basic:$open", 3);
  struct stat statbuf;
  FILE *fileptr;
  char *modecodif, modespec[2];
  extern int errno;
  enum {
    STRANGE_SOURCE_SINK,
    INEXISTENT_SOURCE_SINK,
    CANNOT_OPEN,
    SYS_ERROR,
    FINISHED_RESOURCES
  } what_happened;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  modecodif = GetString(X(1));

  modespec[0] = modecodif[0];
  modespec[1] = 0;

  fileptr = (TaggedIsATM(X(0)) ? fopen(GetString(X(0)),modespec) :
	     TaggedIsSmall(X(0)) ? fdopen(GetSmall(X(0)),modespec) :
	     NULL);

  if (fileptr==NULL) {
    what_happened = SYS_ERROR;                            /* Just in case */
    if (errno==ENOENT || errno==ENOTDIR || errno==ENXIO ||errno==EBADF)
      what_happened = INEXISTENT_SOURCE_SINK;
    else if (errno==EEXIST || errno==EISDIR || errno==EISDIR || 
               errno==EBADF || errno==EROFS)
      what_happened = CANNOT_OPEN;
    else if (errno==ENOMEM || errno==EMFILE || errno==ENFILE)
      what_happened = FINISHED_RESOURCES;
    goto bombit;
    } else {
        if (fstat(fileno(fileptr), &statbuf) || 
            (statbuf.st_mode & S_IFMT) == S_IFDIR) {
          fclose(fileptr);
          what_happened = CANNOT_OPEN;
          goto bombit;
        }
    }

  {
    char locking = modecodif[1];

    if (locking == 'l' || locking == 'b') /* file locking */ {
      struct flock sflo;
      intmach_t cmd = (locking == 'l' ? F_SETLK : F_SETLKW);

      sflo.l_whence = 0; sflo.l_start = 0; sflo.l_len = 0;
      sflo.l_type = 
        (modecodif[2] == 'r' || 
         (modecodif[2] =='\0' && modecodif[0] == 'r') ? F_RDLCK
         : F_WRLCK);
      if (fcntl(fileno(fileptr), cmd, &sflo) < 0) {
        fclose(fileptr);
        CBOOL__FAIL;
      }
    }
  }

  CBOOL__LASTUNIFY(CFUN__EVAL_N(ptr_to_stream,new_stream(X(0),modespec,fileptr)),X(2));

 bombit:
  CBOOL__TEST(current_ferror_flag == atom_on);
  switch (what_happened) {
  case STRANGE_SOURCE_SINK:
    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)), X(0), 1);
  case INEXISTENT_SOURCE_SINK:
    BUILTIN_ERROR(EXISTENCE_ERROR(EXISTENCE_ERRORS(source_sink)), X(0), 1);
  case CANNOT_OPEN:
    BUILTIN_ERROR(PERMISSION_ERROR(OPEN, PERMISSION_OBJECTS(source_sink)),X(0),1);
  case SYS_ERROR:
    BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);
  case FINISHED_RESOURCES:
    BUILTIN_ERROR(RESOURCE_ERROR,X(0),1);
  default:
    CBOOL__FAIL;
  }
}

/*   --------------------------------------------------------------  */

/* as Quintus closing a stream object referring to user_input,user_output */
/*   or user_error will succeed but cause no changes */

CBOOL__PROTO(prolog_close) {
  ERR__FUNCTOR("streams_basic:close", 1);
  stream_node_t *stream;

  stream = stream_to_ptr(X(0), 'x');
  if (stream==NULL) {
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS), X(0), 1);
  }

  /* When closing a redirected input/output stream, reset it to
     the default value */
  if (stream==Input_Stream_Ptr) {
    Input_Stream_Ptr = stream_user_input;
  } else if (stream==Output_Stream_Ptr) {
    Output_Stream_Ptr = stream_user_output;
  }

  /* Do nothing it the stream is one of the default ones */
  if (stream==stream_user_input) CBOOL__PROCEED;
  if (stream==stream_user_output) CBOOL__PROCEED;
  if (stream==stream_user_error) CBOOL__PROCEED;

  /* Really close the stream */

  if (stream->streammode != 's')        /* Not a socket -- has FILE * */
    fclose(stream->streamfile);  /* Releases file locks automatically */
  else
    close(TaggedToIntmach(stream->label)); /* Needs a lock here */


  /* We are twiggling with a shared structure: lock the access to it */

  Wait_Acquire_lock(stream_list_l);

  stream->label = ERRORTAG;
  stream->backward->forward = stream->forward;
  stream->forward->backward = stream->backward;

  /* now ensure that no choicepoints point at the stream */
  {
    choice_t *b;
    tagged_t t1, t2;
    
    t1 = PointerToTerm(stream);
    t2 = PointerToTerm(stream->forward);
	
    for (b = w->choice;
	 ChoiceYounger(b,Choice_Start);
	 b = ChoiceCont(b)) {
      if (b->next_alt == address_nd_current_stream && b->x[3] == t1)
	b->x[3] = t2;
    }
  }
  
  CHECKDEALLOC0(stream_node_t, stream);
  Release_lock(stream_list_l);

  CBOOL__PROCEED;
}

/* -------------------------------------------------------------- */

/* ISO Behavior (MCL): current_input and current_output shall unify its
   argument with the current input (re. output) stream, _not_ stream_alias.
   This is a bit relaxed here: we allow also for stream aliases to be passed
   in and out without raising an exception.  Same goes for current_output */ 

CBOOL__PROTO(prolog_current_input) {
  ERR__FUNCTOR("streams_basic:current_input", 1);
  DEREF(X(0), X(0));

  if (CFUN__EVAL_N(is_var_or_alias_or_stream, X(0))) {
    CBOOL__LASTUNIFY(CFUN__EVAL_N(ptr_to_stream,Input_Stream_Ptr), X(0));
  } else {
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS), X(0), 1);
  }
}

CBOOL__PROTO(prolog_set_input) {
  ERR__FUNCTOR("streams_basic:set_input", 1);
  intmach_t errcode;
  stream_node_t *stream;

  DEREF(X(0),X(0));
  stream = stream_to_ptr_check(X(0), 'r', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  Input_Stream_Ptr = stream;
  CBOOL__PROCEED;
}

/*   --------------------------------------------------------------  */

CBOOL__PROTO(prolog_current_output) {
  ERR__FUNCTOR("streams_basic:current_output", 1);
  DEREF(X(0), X(0));

  if (CFUN__EVAL_N(is_var_or_alias_or_stream, X(0))) {
    CBOOL__LASTUNIFY(CFUN__EVAL_N(ptr_to_stream,Output_Stream_Ptr), X(0));
  } else {
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS), X(0), 1);
  }
}

CBOOL__PROTO(prolog_set_output) {
  ERR__FUNCTOR("streams_basic:set_output", 1);
  intmach_t errcode;
  stream_node_t *stream;

  DEREF(X(0),X(0));
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    DEBUG__TRACE(TRUE, "stream to pointer returned null, errcode is %ld\n", (long)errcode);
    BUILTIN_ERROR(errcode, X(0), 1);
  }
  Output_Stream_Ptr = stream;
  CBOOL__PROCEED;
}

/*   --------------------------------------------------------------  */

CBOOL__PROTO(prolog_current_error) {
  CBOOL__LASTUNIFY(CFUN__EVAL_N(ptr_to_stream,stream_user_error),X(0));
}

/*   --------------------------------------------------------------  */

/* prolog_stream_code(?Stream,?Stream_code)
 * stream Stream	A prolog Stream
 * integer Stream_code  A unique number associated with Stream
 *
 * Description: Stream can be used by prolog predicates to perform IO
 * Stream_code can be used by C functions to somehow perform IO.
 * There is a problem due to the ambiguity of 'user':
 *	stream_code(user,X)
 * will return the stream code associated with user_output.
 */

CBOOL__PROTO(prolog_stream_code) {
  ERR__FUNCTOR("streams_basic:stream_code", 2);
  intmach_t errcode;
  stream_node_t *s;

  DEREF(X(1), X(1));
  if (IsVar(X(1))) {
    s = stream_to_ptr_check(X(0), 'x', &errcode);
    if (!s) BUILTIN_ERROR(errcode,X(0),1);

    if (s->streammode != 's'){                            /* Not a socket */
      CBOOL__UnifyCons(MakeSmall(fileno(s->streamfile)),X(1));
    } else {                                                  /* DCG, MCL */
      CBOOL__UnifyCons(s->label,X(1)); /* Can't be this done above as well? */
    }
    CBOOL__PROCEED;
  } else if (X(1) >= TaggedZero && X(1) < MakeSmall(ENG_NOFILES)) {
    for (s = root_stream_ptr->backward;
	 s != root_stream_ptr && s->label != X(1);
	 s = s->backward)
      ;
    if (s != root_stream_ptr && s->label == X(1)) {
      CBOOL__LASTUNIFY(CFUN__EVAL_N(ptr_to_stream,s),X(0));
    } else {
      CBOOL__FAIL;
    }
  } else if (IsInteger(X(1))) {
    CBOOL__FAIL;
  } else {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
  }
}

CBOOL__PROTO(character_count) {
  ERR__FUNCTOR("streams_basic:character_count", 2);
  intmach_t errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream)
    BUILTIN_ERROR(errcode,X(0),1);

  if (stream->isatty)
    stream = root_stream_ptr;
  CBOOL__LASTUNIFY(IntmachToTagged(stream->char_count),X(1));
}

CBOOL__PROTO(line_position) {
  ERR__FUNCTOR("streams_basic:line_position", 2);
  intmach_t errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream)
    BUILTIN_ERROR(errcode,X(0),1);

  if (stream->isatty)
    stream = root_stream_ptr;
  CBOOL__LASTUNIFY(IntmachToTagged(stream->char_count-stream->last_nl_pos),X(1));
}

CBOOL__PROTO(line_count) {
  ERR__FUNCTOR("streams_basic:line_count", 2);
  intmach_t errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream)
    BUILTIN_ERROR(errcode,X(0),1);

  if (stream->isatty)
    stream = root_stream_ptr;
  CBOOL__LASTUNIFY(IntmachToTagged(stream->nl_count),X(1));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(flush_output) {
  if ((Output_Stream_Ptr->streammode != 's')
      && fflush(Output_Stream_Ptr->streamfile)) {
    print_syserror("% fflush in flush_output/1");
  }
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(flush_output1) {
  ERR__FUNCTOR("streams_basic:flush_output", 1);
  intmach_t errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);

  if ((s->streammode != 's') && fflush(s->streamfile)) {
    print_syserror("% fflush in flush_output/1");
  }
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(prolog_clearerr) {
  ERR__FUNCTOR("streams_basic:clearerr", 1);
  intmach_t errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) BUILTIN_ERROR(errcode,X(0),1);
  
  if (s->streammode != 's') clearerr(s->streamfile);

  CBOOL__PROCEED;
}

/*----------------------------------------------------
  THE BUILTIN C-PREDICATE       CURRENT_STREAM/3
  -----------------------------------------------------------------------*/

static CBOOL__PROTO_N(current_stream_data, stream_node_t *streamptr) {
  CBOOL__UnifyCons(streamptr->streamname,X(0));
  switch (streamptr->streammode)
    {
    case 'a':
      CBOOL__UnifyCons(atom_append,X(1));
      break;
    case 'r':
      CBOOL__UnifyCons(atom_read,X(1));
      break;
    case 'w':
      CBOOL__UnifyCons(atom_write,X(1));
      break;
    case 's':
      CBOOL__UnifyCons(atom_socket,X(1));
      break;
    }
  CBOOL__PROCEED;
}

CBOOL__PROTO(current_stream) {
  stream_node_t *streamptr;

  DEREF(X(2),X(2));
  if (!IsVar(X(2)) && (streamptr=stream_to_ptr(X(2),'y'))) {
    CBOOL__LASTCALL_N(current_stream_data,streamptr);
  }

  streamptr = root_stream_ptr->forward;
  /* skip over system streams */
  while (streamptr!=root_stream_ptr &&
	 streamptr->streamname==ERRORTAG) {
    streamptr = streamptr->forward;
  }
  CBOOL__TEST(streamptr!=root_stream_ptr);

  if (streamptr->forward!=root_stream_ptr) {
    X(3) = PointerToTerm(streamptr->forward);
    CVOID__CALL_N(push_choicept,address_nd_current_stream);
  }

  CBOOL__UNIFY(CFUN__EVAL_N(ptr_to_stream,streamptr),X(2));
  CBOOL__LASTCALL_N(current_stream_data,streamptr);
}

CBOOL__PROTO(nd_current_stream) {
  stream_node_t *streamptr = TaggedToStream(X(3));

  if (streamptr==root_stream_ptr) {
    /* zero alts due to close */
    CVOID__CALL(pop_choicept);
    CBOOL__FAIL;
  }

  if (streamptr->forward==root_stream_ptr) {
    /* last alt */
    CVOID__CALL(pop_choicept);
  } else {
    w->choice->x[3]=PointerToTerm(streamptr->forward);
  }
  CBOOL__UNIFY(CFUN__EVAL_N(ptr_to_stream,streamptr),X(2));
  CBOOL__LASTCALL_N(current_stream_data,streamptr);
}


/* '$stream'(<address>,<id>) <-- (stream_node_t *) */

CFUN__PROTO_N(ptr_to_stream_noalias, tagged_t, stream_node_t *n) {
  tagged_t *pt1 = G->heap_top;
  HeapPush(pt1,functor_Dstream);
  HeapPush(pt1,PointerToTerm(n));
  HeapPush(pt1,n->label);
  G->heap_top=pt1;
  CFUN__PROCEED(Tagp(STR,HeapCharOffset(pt1,-3*sizeof(tagged_t))));
}

/* '$stream'(<address>,<id>) <-- (stream_node_t *) */
/* or */
/* <stream_alias> <-- (stream_node_t *) */
CFUN__PROTO_N(ptr_to_stream, tagged_t, stream_node_t *n) {
  if (n==stream_user_input) {
    CFUN__PROCEED(atom_user_input);
  } else if (n==stream_user_output) {
    CFUN__PROCEED(atom_user_output);
  } else if (n==stream_user_error) {
    CFUN__PROCEED(atom_user_error);
  } else {
    CFUN__LASTCALL_N(ptr_to_stream_noalias, n);
  }
}

