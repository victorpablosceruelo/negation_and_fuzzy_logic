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

/* -------------------------------------------------------------- */
/* Replacing the stream aliases pointer */

/* replace_stream(StreamAlias, NewStream) */

CBOOL__PROTO(prolog_replace_stream) {
  ERR__FUNCTOR("io_alias_redirection:replace_stream", 2);
  tagged_t which_stream;
  tagged_t which_atom;
  stream_node_t *node;
  intmach_t errcode;

  DEREF(which_atom, X(0));
  DEREF(which_stream, X(1));

  if ((which_atom == atom_user_error) ||
      (which_atom == atom_user_output))
    node = stream_to_ptr_check(which_stream, 'w', &errcode);    
  else if (which_atom == atom_user_input) 
    node = stream_to_ptr_check(which_stream, 'r', &errcode);    
  else {
    /* Not exactly: should be "alias"*/
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS),X(0),1);
  }
  
  if (node == NULL) BUILTIN_ERROR(errcode,X(0),1);

  if (which_atom == atom_user_input) 
    stream_user_input = node;
  else if (which_atom == atom_user_output)
    stream_user_output = node;
  else if (which_atom == atom_user_error)
    stream_user_error = node;

  CBOOL__PROCEED;
}

/* get_stream(StreamAlias, CurrentStream) */

CBOOL__PROTO(prolog_get_stream) {
  ERR__FUNCTOR("io_alias_redirection:get_stream", 2);
  tagged_t which_atom;
  stream_node_t *node;

  DEREF(which_atom, X(0));
  if (which_atom == atom_user_input) 
    node = stream_user_input;
  else if (which_atom == atom_user_output)
    node = stream_user_output;
  else if (which_atom == atom_user_error)
    node = stream_user_error;
  else {
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS),X(0),1);
  }
   
  CBOOL__LASTUNIFY(X(1), CFUN__EVAL_N(ptr_to_stream_noalias, node));
}

