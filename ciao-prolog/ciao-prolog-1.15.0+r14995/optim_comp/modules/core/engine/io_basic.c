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

/*
 * prolog_format_print_float(formatChar,arg,precision):
 *   formatChar;	Selects type of format.
 *   arg;		Value to be printed.
 *   precision; 	Precision of printed item.
 *
 * Description: Print a FLOAT value on file file according to the format
 * specified by formatChar and the precision specified by precision.
 * Precision is number of digits after decimal point for some formats and
 * total number of digits for some FORMATS.
 */

#define MAX_OUTPUT_DIGITS 1023
#define BUFF_SIZE         2048

CBOOL__PROTO(prolog_format_print_float) {
  intmach_t precision;
  char buf[BUFF_SIZE], formatChar;
  flt64_t f;

  DEREF(X(0),X(0));
  formatChar = TaggedToIntmach(X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  precision = TaggedToIntmach(X(2));
  
  /* New code (Edison): */

  f = TaggedToFloat(X(1));

  if(formatChar=='f' && precision > 1023)
    precision = 1023;
  else if(precision<0)
    precision = 6;
  float_to_string(buf, precision, formatChar, f, 10);
  print_string(Output_Stream_Ptr, buf);

  CBOOL__PROCEED;
}

/*
 * prolog_format_print_integer(formatChar,arg,precision):
 *   formatChar; Selects type of format.
 *   arg         Value to be printed.
 *   precision;	 Precision or radix of printed item.
 *
 * Description: Print an INTEGER value on file file according to the format
 * specified by formatChar and the precision specified by precision.
 * Precision is number of digits after decimal point for some formats and
 * radix for some formats.
 */

CFUN__PROTO_N(fu1_integer, tagged_t, tagged_t X0);

extern liveinfo_t prolog_format_print_integer__liveinfo;

CBOOL__PROTO(prolog_format_print_integer) {
  char formatChar;
  intmach_t precision;
  intmach_t base;
  
  DEREF(X(0),X(0));
  formatChar = GetSmall(X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  precision = TaggedToIntmach(X(2));

  if (formatChar=='r')
    base = ((precision<2 || precision>36) ? 8 : precision);
  else if (formatChar=='R')
    base = ((precision<2 || precision>36) ? -8 : -precision);
  else
    base = 10;

  if (IsFloat(X(1))) {
    w->liveinfo = prolog_format_print_integer__liveinfo;
    X(1) = CFUN__EVAL_N(fu1_integer, X(1));
  }
  CVOID__CALL_N(number_to_string, X(1), base);
  
  if ((formatChar=='d' || formatChar=='D') && precision > 0)
    {
      intmach_t usilen = strlen(Atom_Buffer) - (Atom_Buffer[0]=='-');
      intmach_t n = precision-usilen+1;
      
      if (n>0)
	{
	  intmach_t i;
	  intmach_t dig1 = (Atom_Buffer[0] == '-');
	  intmach_t slen = strlen(Atom_Buffer);
	  
	  while (slen+n+1 > Atom_Buffer_Length) {
	    Atom_Buffer = CHECKREALLOC0_ARRAY(char, Atom_Buffer, Atom_Buffer_Length, 2 * Atom_Buffer_Length);
	    Atom_Buffer_Length <<= 1;
	  }
	  
	  for (i=slen; i>=dig1; i--)
	    Atom_Buffer[i+n] = Atom_Buffer[i];
	  for (i=dig1+n-1; i>=dig1; i--)
	    Atom_Buffer[i] = '0';
	}

      {
	intmach_t i;
	intmach_t slen = strlen(Atom_Buffer);
	intmach_t ppos = slen-precision;
	
	if (slen+2 > Atom_Buffer_Length) {
	  Atom_Buffer = CHECKREALLOC0_ARRAY(char, Atom_Buffer, Atom_Buffer_Length, 2 * Atom_Buffer_Length);
	  Atom_Buffer_Length <<= 1;
	}
	
	for (i=slen; i>=ppos; i--)
	  Atom_Buffer[i+1] = Atom_Buffer[i];
	Atom_Buffer[ppos] = '.';
      }
    }
  if (formatChar=='D')
    {
      intmach_t i, count;
      intmach_t slen = strlen(Atom_Buffer);
      intmach_t dig1 = (Atom_Buffer[0]=='-');
      intmach_t ppos = slen;
      
      for (i=dig1, count=0; i<ppos; i++)
	{
	  if (Atom_Buffer[i]=='.') ppos=i;
	  else count++;
	}
      count = (count-1)/3;
      
      if (count>0)
	{
	  if (slen+count+1 > Atom_Buffer_Length) {
	    Atom_Buffer = CHECKREALLOC0_ARRAY(char, Atom_Buffer, Atom_Buffer_Length, 2 * Atom_Buffer_Length);
	    Atom_Buffer_Length <<= 1;
	  }
	  
	  for (i=slen; i>=ppos; i--)
	    Atom_Buffer[i+count] = Atom_Buffer[i];
	  for (i=ppos-1; count>0; count--)
	    Atom_Buffer[i+count] = Atom_Buffer[i], i--,
	    Atom_Buffer[i+count] = Atom_Buffer[i], i--,
	    Atom_Buffer[i+count] = Atom_Buffer[i], i--,
	    Atom_Buffer[i+count] = ',';
	}
    }
  
  print_string(Output_Stream_Ptr, Atom_Buffer);
  CBOOL__PROCEED;
}

CVOID__PROTO_N(display_term, tagged_t term, stream_node_t *stream, bool_t quoted);
CBOOL__PROTO(code_class);
CBOOL__PROTO(peek);
CBOOL__PROTO(peek2);

CBOOL__PROTO(code_class) {
  ERR__FUNCTOR("io_basic:code_class", 2);
  intmach_t i;
  
  DEREF(X(0),X(0));
  if (!TaggedIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) { /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(symbolchar[i]));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(getct) {
  ERR__FUNCTOR("io_basic:getct", 2);
  intmach_t i;

  i = readchar(Input_Stream_Ptr,GET,address_getct);

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);

  CBOOL__UNIFY(X(0),MakeSmall(i));
  CBOOL__LASTUNIFY(X(1),MakeSmall(i == -1 ? -1 : symbolchar[i]));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(getct1) {
  ERR__FUNCTOR("io_basic:getct1", 2);
  intmach_t i;
  
  i = readchar(Input_Stream_Ptr,GET1,address_getct1); /* skip whitespace */

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);

  CBOOL__UNIFY(X(0),MakeSmall(i)); 
  CBOOL__LASTUNIFY(X(1),MakeSmall(i == -1 ? -1 : symbolchar[i]));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get) {
  ERR__FUNCTOR("io_basic:get_code", 1);
  intmach_t i;

  i = readchar(Input_Stream_Ptr,GET,address_get);

  if (i < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  CBOOL__LASTUNIFY(X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get2) {
  ERR__FUNCTOR("io_basic:get_code", 2);
  intmach_t i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  i = readchar(s,GET,address_get2);

  if (i < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get1) {
  ERR__FUNCTOR("io_basic:get1_code", 1);
  intmach_t i;
  
  i = readchar(Input_Stream_Ptr,GET1,address_get1); /* skip whitespace */

  if (i < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  CBOOL__LASTUNIFY(X(0),MakeSmall(i));
}


/*----------------------------------------------------------------*/

CBOOL__PROTO(get12) {
  ERR__FUNCTOR("io_basic:get1_code", 2);
  intmach_t i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  i = readchar(s,GET1,address_get12); /* skip whitespace */

  if (i < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(peek) {
  ERR__FUNCTOR("io_basic:peek_code", 1);
  intmach_t i;

  i = readchar(Input_Stream_Ptr,PEEK,address_peek);

  if (i < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  CBOOL__LASTUNIFY(X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(peek2) {
  ERR__FUNCTOR("io_basic:peek_code", 2);
  intmach_t i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  i = readchar(s,PEEK,address_peek2);

  if (i < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(nl) {
  writechar('\n',1,Output_Stream_Ptr);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(nl1) {
  ERR__FUNCTOR("io_basic:nl", 1);
  intmach_t errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  writechar('\n',1,s);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(put) {
  ERR__FUNCTOR("io_basic:put_code", 1);
  intmach_t i;

  DEREF(X(0),X(0));
  if (!TaggedIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) {
    /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE);
  }

  writechar(i,1,Output_Stream_Ptr);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(put2) {
  ERR__FUNCTOR("io_basic:put_code", 2);
  intmach_t i;
  stream_node_t *s;

  s = stream_to_ptr_check(X(0), 'w', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  DEREF(X(1),X(1));
  if (!TaggedIsSmall(X(1)) || (i = GetSmall(X(1))) & ~255) {
    /* Not a byte */
    ERROR_IN_ARG(X(1),2,BYTE);
  }

  writechar(i,1,s);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/
/* output stream always write or append */

CBOOL__PROTO(tab) {
  ERR__FUNCTOR("io_basic:tab", 1);
  DEREF(X(0),X(0));
  if (!IsInteger(X(0))) ERROR_IN_ARG(X(0),1,INTEGER);

  writechar(' ',TaggedToIntmach(X(0)),Output_Stream_Ptr);
  CBOOL__PROCEED;
}


/*----------------------------------------------------------------*/

CBOOL__PROTO(tab2) {
  ERR__FUNCTOR("io_basic:tab", 2);
  intmach_t errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  if (!IsInteger(X(1))) ERROR_IN_ARG(X(1),2,INTEGER);

  writechar(' ',TaggedToIntmach(X(1)),s);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip) {
  ERR__FUNCTOR("io_basic:skip_code", 1);
  intmach_t i, ch;

  DEREF(X(0),X(0));
  if (!TaggedIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) {
    /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE);
  }

  for (ch=i+1; ch!=i && ch>=-1;)
    ch = readchar(Input_Stream_Ptr,i,address_skip);

  if (ch < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip2) {
  ERR__FUNCTOR("io_basic:skip_code", 2);
  intmach_t i, ch;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  DEREF(X(1),X(1));
  if (!TaggedIsSmall(X(1)) || (i = GetSmall(X(1))) & ~255) {
    /* Not a byte */
    ERROR_IN_ARG(X(1),2,BYTE);
  }

  for (ch=i+1; ch!=i && ch>=-1;) {
    ch = readchar(s,i,address_skip2);
  }

  if (ch < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip_line) {
  /* ERR__FUNCTOR("io_basic:skip_line", 0); */
  int ch;

  for (ch=0; ch!=0xa && ch!=0xd && ch>=0;) {
    ch = readchar(Input_Stream_Ptr,SKIPLN,address_skip_line);
  }

  if (ch == 0xd) {
    /* Delete a possible 0xa (win end-of-line) */
    readchar(Input_Stream_Ptr,DELRET,address_skip_line);
  }

  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip_line1) {
  ERR__FUNCTOR("io_basic:skip_line", 1);
  intmach_t errcode, ch;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  for (ch=0; ch!=0xa && ch!=0xd && ch>=0;) {
    ch = readchar(s,SKIPLN,address_skip_line1);
  }

  if (ch == 0xd) {
    /* Delete a possible 0xa (win end-of-line) */
    readchar(s,DELRET,address_skip_line1);
  }

  return TRUE;
}

/*----------------------------------------------------------------*/
/*                       Moved from streams.c (DCG)               */
/*----------------------------------------------------------------*/

CVOID__PROTO_N(print_variable, stream_node_t *stream, tagged_t term) {
  intmach_t n;
  print_string(stream, "_");
  if (TaggedIsSVA(term)) {
    n = (TaggedToPointer(term) - Stack_Start) * sizeof(tagged_t) + HeapCharSize;
  } else {
    n = (TaggedToPointer(term) - Heap_Start) * sizeof(tagged_t);
  }
  CVOID__CALL_N(number_to_string, IntmachToTagged(n), 10);
  print_string(stream, Atom_Buffer);
}

CVOID__PROTO_N(print_number, stream_node_t *stream, tagged_t term) {
  CVOID__CALL_N(number_to_string, term, 10);
  print_string(stream, Atom_Buffer);
}

CVOID__PROTO_N(print_atom, stream_node_t *stream, tagged_t term) {
  atom_t *atomptr = TaggedToAtom(term);
  
  if (!atomptr->has_special) {
    print_string(stream, atomptr->name);
  } else {
    char *buf = CHECKALLOC_ARRAY(char, 2*MAXATOM+3);
    char *ch = atomptr->name;
    char *bp = buf;
    intmach_t i;
      
    *bp++ = '\'';
    if (atomptr->has_squote)
      while ((i = *ch++))
	{
	  if (i=='\'' || i=='\\')
	    *bp++ = i;
	  *bp++ = i;
	}
    else
      while ((i = *ch++))
	{
	  if (i=='\\')
	    *bp++ = i;
	  *bp++ = i;
	}
    *bp++ = '\'';
    *bp++ = 0;
    print_string(stream, buf);
    CHECKDEALLOC0_ARRAY(char, buf, 2*MAXATOM+3);
  }
}

/*   --------------------------------------------------------------  */	 

CVOID__PROTO_N(display_term, tagged_t term, stream_node_t *stream, bool_t quoted) {
  tagged_t aux;
  intmach_t arity,i;

  SwOnAnyTagB(term, t_head_functor, { /* HVA */
    goto variable;
  }, { /* SVA */
    goto variable;
  }, { /* CVA */
  variable:
    CVOID__CALL_N(print_variable,stream,term);
  }, { /* NUM */
  number:
    CVOID__CALL_N(print_number,stream,term);
  }, { /* ATM */
    if (quoted) {
      CVOID__CALL_N(print_atom,stream,term);
    } else {
      print_string(stream,TaggedToAtom(term)->name);
    }
  }, { /* LST */
    writechar('[',1,stream);
    DerefCar(aux,term);
    CVOID__CALL_N(display_term, aux, stream, quoted);
    DerefCdr(term,term);
    while (TaggedIsLST(term)) {
      writechar(',',1,stream);
      DerefCar(aux,term);
      CVOID__CALL_N(display_term, aux, stream, quoted);
      DerefCdr(term,term);
    }
    if(term!=atom_nil){
      writechar('|',1,stream);
      CVOID__CALL_N(display_term, term, stream, quoted);
    }
    writechar(']',1,stream);
  }, { /* STR(blob) */
    /* todo[ts]: change if more blob types are added */
    goto number;
  }, { /* STR(struct) */
    CVOID__CALL_N(display_term, t_head_functor, stream, quoted);
    writechar('(',1,stream);
    arity = Arity(t_head_functor);
    for (i=1; i<=arity; i++){
      if (i>1) writechar(',',1,stream);
      DerefArg(aux,term,i);
      CVOID__CALL_N(display_term, aux, stream, quoted);
    }
    writechar(')',1,stream);
  });
}

CBOOL__PROTO(prolog_display) {
  DEREF(X(0),X(0));
  CVOID__CALL_N(display_term, X(0),Output_Stream_Ptr, FALSE);
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_display2) {
  ERR__FUNCTOR("io_basic:display", 2);
  intmach_t errcode;
  stream_node_t *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  CVOID__CALL_N(display_term, X(1),stream, FALSE);
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_displayq) {
  DEREF(X(0),X(0));
  CVOID__CALL_N(display_term, X(0), Output_Stream_Ptr, TRUE);
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_displayq2) {
  ERR__FUNCTOR("io_basic:displayq", 2);
  intmach_t errcode;
  stream_node_t *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  CVOID__CALL_N(display_term, X(1), stream, TRUE);
  CBOOL__PROCEED;
}

/*----------------------------------------------------*/

#define DISPLAY_STRING__BUFFER_SIZE 1024
#define DISPLAY_STRING__FLUSH { \
  if (fwrite(buffer, sizeof(unsigned char), buffer_n, out)) {} else {} \
  buffer_n = 0; \
}
#define DISPLAY_STRING__PUT(I) { \
  buffer[buffer_n++] = (I); \
  if (buffer_n >= DISPLAY_STRING__BUFFER_SIZE) DISPLAY_STRING__FLUSH; \
}
/* Fast string write */
CBOOL__PROTO(prolog_display_string) {
  tagged_t term;
  tagged_t t0;
  FILE *out;
  intmach_t i;
  intmach_t buffer_n;
  unsigned char buffer[DISPLAY_STRING__BUFFER_SIZE];

  out = Output_Stream_Ptr->streamfile;
  
  buffer_n = 0;

  DEREF(term, X(0));
  while(TaggedIsLST(term)) {
    DerefCar(t0, term);
    DerefCdr(term, term);
    if (!TaggedIsSmall(t0)) goto error;
    i = GetSmall(t0);
    if (i < 0 || i > 255) goto error;
    DISPLAY_STRING__PUT(i);
  }
  if (term != atom_nil) goto error;
  DISPLAY_STRING__FLUSH;
  CBOOL__PROCEED;
 error:
  DISPLAY_STRING__FLUSH;
  CBOOL__FAIL;
}

/*----------------------------------------------------*/
/* TODO: is this the correct place? */

CBOOL__PROTO(prolog_copy_stdout) {
  ERR__FUNCTOR("io_basic:$copy_stdout", 1);
  intmach_t i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  if ((i = getc(s->streamfile)) < -1) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }
  
  while (i != EOF) {
    writechar(i,1,Output_Stream_Ptr);
    if ((i = getc(s->streamfile)) < -1) {
      BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
    }
  }
  CBOOL__PROCEED;
}

