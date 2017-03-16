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
#include <sys/time.h>

#if !defined(crossWin32i86)
# include <sys/param.h>
# include <sys/errno.h>
#endif

/****************************************************************************/
/* Atom table */

char symbolchar[256];

hashtab_node_t **atmtab; /* Shared --- but need lock when accessing /
                                   reallocing it! */
hashtab_t *prolog_atoms;  /* Shared -- need lock when adding atoms */
/* Shared -- for adding new atoms; need lock */
static char *prolog_chars=NULL;
static char *prolog_chars_end=NULL;

#define IsLowerChar(X)  (symbolchar[X]==1)
#define IsUpperChar(X)  (symbolchar[X]==2)
#define IsDigit(X)      (symbolchar[X]==3)
#define IsSymbolChar(X) (symbolchar[X]==4)

static void classify_atom(atom_t *s) {
  unsigned char *cp = (unsigned char *)s->name;
  unsigned char c0 = cp[0];
  unsigned char c1 = cp[1];
  unsigned char c2 = cp[2];
  unsigned char c;
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

#if defined(ABSMACH_OPT__atom_len)
atom_t *new_atom_check(str, str_len, index)  
     unsigned char *str;   
     uintmach_t str_len;
     uintmach_t index;
#else
atom_t *new_atom_check(str, index)  
     unsigned char *str;   
     uintmach_t index;
#endif
{
  atom_t *s;

#if defined(ABSMACH_OPT__atom_len)
  intmach_t len = sizeof(atom_t) + str_len + 1;
#else
  intmach_t len = sizeof(atom_t) + strlen(str) + 1;
#endif

  /* Adjust to a tagged pointer */
  prolog_chars = (char *)ALIGN_TO(OWNMALLOC_ALIGN, (intp_t)prolog_chars);
  if (prolog_chars+len > prolog_chars_end) {             /* Out of bounds */
    intmach_t len2;
    len2 = MIN_MEM_CHUNK_SIZE > len ? MIN_MEM_CHUNK_SIZE : len;
    prolog_chars = CHECKALLOC_ARRAY(char, len2);
    prolog_chars_end = prolog_chars + len2;
  }
  
  s = (atom_t *)prolog_chars;
  prolog_chars += len;
  s->index = index;
  (void) strcpy(s->name,(char *)str);
#if defined(ABSMACH_OPT__atom_len)
  s->atom_len = str_len;
#endif
  classify_atom(s);

#if defined(USE_THREADS)
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

void init_symbolchar__kanji();
void init_symbolchar__latin1();

void init_symbolchar() {
  char *lc_ctype;
  lc_ctype = getenv("LC_CTYPE");
  if (lc_ctype!=NULL &&
      (strcmp(lc_ctype,"ja_JP.EUC")==0 || strcmp(lc_ctype,"ja_JP.euc")==0)) {
    init_symbolchar__kanji();
  } else {
    init_symbolchar__latin1();
  }
}

void init_symbolchar__kanji() {
  intmach_t i;
  char *cp;

  /* default: whitespace */
  for (i=0; i<128; i++)	symbolchar[i] = 0;
  /* accept 128..255 as lowercase */
  for (i=128; i<256; i++) symbolchar[i] = 1;

  /* lowercase */
  for (cp="abcdefghijklmnopqrstuvwxyz"; (i = *cp++); ) symbolchar[i]=1;
  /* uppercase */
  for (cp="ABCDEFGHIJKLMNOPQRSTUVWXYZ_"; (i = *cp++); ) symbolchar[i]=2;
  /* digits */
  for (cp="0123456789"; (i = *cp++); ) symbolchar[i]=3;
  /* symbolchars */
  for (cp="#$&*+-./:<=>?@^\\`~"; (i = *cp++); ) symbolchar[i]=4;
  /* punctuation */
  for (cp="!;\"'%(),[]{|}"; (i = *cp++); ) symbolchar[i]=5;
}

void init_symbolchar__latin1() {
  intmach_t i;

  init_symbolchar__kanji();
  /* 128..160 are whitespace */
  for (i=128; i<161; i++) symbolchar[i]=0;
   /* 161..191 are symbolchars */
  for (i=161; i<192; i++) symbolchar[i]=4;
   /* 192..222 are uppercase */
  for (i=192; i<223; i++) symbolchar[i]=2;
  /* 223..255 are lowercase */
  for (i=223; i<256; i++) symbolchar[i]=1;
  /* 215 (mult sign) is a symbolchar */
  symbolchar[215]=4;
  /* 247 (div sign) is a symbolchar */
  symbolchar[247]=4;
}

/* insert atom in global table */
/*  MCL: there is an implicit assumption that the table is not full */

#if defined(ABSMACH_OPT__atom_len)
static hashtab_node_t *atom_gethash(hashtab_t *sw,
				    tagged_t key,
				    char *str,
				    uintmach_t str_len)
#else
static hashtab_node_t *atom_gethash(hashtab_t *sw,
				    tagged_t key,
				    char *str)
#endif
{
  hashtab_node_t *hnode;
#if defined(ABSMACH_OPT__atomgc)
  hashtab_node_t *first_erased = NULL;
#endif
  intmach_t i;
  uintmach_t t0;

  for (i=0, t0= HASHTAGGED(key) & sw->mask;
       ;
       i+=sizeof(hashtab_node_t), t0=(t0+i) & sw->mask) {
    hnode = HASHNODE(sw, t0);
#if !defined(ABSMACH_OPT__atomgc)
    if ((hnode->key==key 
#if defined(ABSMACH_OPT__atom_len)
         && hnode->value.atomp->atom_len == str_len
#endif
         && strcmp(hnode->value.atomp->name, str)==0) ||
        !hnode->key)
      return hnode;
#else
    if ((hnode->key == key) 
#if defined(ABSMACH_OPT__atom_len)
        && hnode->value.atomp->atom_len == str_len
#endif
        && (strcmp(hnode->value.atomp->name, str) == 0))
      return hnode;
    else if (!hnode->key)
      return first_erased ? first_erased : hnode;
    else if (hnode->key == 1 && !first_erased)
      first_erased = hnode;
#endif
  }
}

intmach_t init_atom_check(char *str) {
  hashtab_node_t *hnode;
  uintmach_t hashcode = 0;
  intmach_t count, size;
  intmach_t current_mem = total_mem_count;
  unsigned char *c = (unsigned char *)str;

#if defined(ABSMACH_OPT__atom_len)
  uintmach_t atom_len = 0;
#endif
  
  while (*c) {
    hashcode = (hashcode<<1) + *c++;
#if defined(ABSMACH_OPT__atom_len)
    atom_len++;
#endif
  }

  hashcode = (hashcode<<3)+4;	/* low bits are masked away; ensure it is
				   not 0 --- it cannot be 1, either, which is
				   very important for atom GC */
/*
  while ((hnode=hashtab_get(prolog_atoms, (hashtab_key_t)hashcode)) &&
	 hnode->key==(tagged_t)hashcode &&
	 strcmp(hnode->value.atomp->name, str)!=0)
    hashcode += 233509<<3;         233509 is prime, and so is
				   233509&0x1ffff, 233509&0x0ffff, ...,
				   233509&0x00007
*/

#if defined(ABSMACH_OPT__atom_len)
  hnode = atom_gethash(prolog_atoms, (hashtab_key_t)hashcode, str, atom_len);
#else
  hnode = atom_gethash(prolog_atoms, (hashtab_key_t)hashcode, str);
#endif

#if defined(ABSMACH_OPT__atomgc)
  if (hnode->key && hnode->key != 1) /* if ABSMACH_OPT__atomgc, '1' marks freed position */
#else
  if (hnode->key)
#endif
    return hnode->value.atomp->index;

  if ((count=prolog_atoms->count) > MAX_ATOM_INDEX) {
    SERIOUS_FAULT("the atom table is full");
  }

  /* Check for a full table, and expand if needed */

  if ((count+1)<<1 > (size=HASHTAB_SIZE(prolog_atoms))) {
    hashtab_t *new_table = HASHTAB_NEW(size<<1);
    intmach_t i;
    hashtab_node_t *h1, *h2;

#if defined(ABSMACH_OPT__atomgc)
    DEBUG__TRACE(debug_atomgc,
		 "Reallocing atom table (count = %ld)\n", (long)count);
#endif

    for (i=0; i<count; i++){
#if defined(ABSMACH_OPT__atomgc)   /* Actually, if the table is full, no entry should be
                         null... */
       /* size *= 2; */
      if ((h1 = atmtab[i]) != NULL) { /* There may be holes when doing GC */
#if defined(ABSMACH_OPT__atom_len)
        atmtab[i] = h2 = atom_gethash(new_table, 
                                      h1->key, 
                                      str,
                                      h1->value.atomp->atom_len);
#else
        atmtab[i] = h2 = atom_gethash(new_table, h1->key, str);
#endif
        h2->key = h1->key;
        h2->value.atomp = h1->value.atomp;
      }
#else
      h1 = atmtab[i];
#if defined(ABSMACH_OPT__atom_len)
      atmtab[i] = h2 = atom_gethash(new_table, 
                                    h1->key, 
                                    str,
                                    h1->value.atomp->atom_len);
#else
      atmtab[i] = h2 = atom_gethash(new_table, h1->key, str);
#endif
      h2->key = h1->key;
      h2->value.atomp = h1->value.atomp;
#endif
    }

    atmtab = CHECKREALLOC0_ARRAY(hashtab_node_t *, atmtab, count, 2*count);

#if defined(ABSMACH_OPT__atomgc)
    /* Clean up the upper part of the new atom table */
    for (i = count; i < 2*count; i++) atmtab[i] = NULL;
    new_table->next_index = count;
#endif

    CHECKDEALLOC0_TAILED(hashtab_t, prolog_atoms);
    new_table->count = count;
#if defined(ABSMACH_OPT__atom_len)
    hnode = atom_gethash(new_table, (hashtab_key_t)hashcode, str, atom_len);
#else
    hnode = atom_gethash(new_table, (hashtab_key_t)hashcode, str);
#endif
    prolog_atoms = new_table;
    size = size << 1;
  }
  hnode->key = (hashtab_key_t)hashcode;

#if defined(ABSMACH_OPT__atomgc)
    size = size >> 1;     /* atmtab size is one half of prolog_atoms size */
    count = prolog_atoms->next_index;
    while (atmtab[count])                  /* There must be one free entry */
      count = (count + 1) % size;
    /*prolog_atoms->next_index+1 == size ? 0 : prolog_atoms->next_index+1;*/
    /* next_index should point to a free entry in the table */
    prolog_atoms->next_index = count;
#endif

#if defined(ABSMACH_OPT__atom_len)
  hnode->value.atomp = new_atom_check((unsigned char *)str, atom_len, count);
#else
  hnode->value.atomp = new_atom_check((unsigned char *)str, count);
#endif
  atmtab[count] = hnode;

  prolog_atoms->count++;

  INC_MEM_PROG((total_mem_count - current_mem));

#if defined(ABSMACH_OPT__functor_table)
  hnode->value.atomp->functor = insert_definition0_ft(count, 0, TRUE);
#endif

  return count;
}

/****************************************************************************/
/* Streams */

intmach_t prolog_force_interactive = 0;      /* Shared --- not really relevant? */

#define EXITCOND(op,i) \
  ( op<GET1 || i==EOF || (op==GET1 && symbolchar[i]>0) || \
    (op==SKIPLN && (i==0xa || i==0xd)) || op==i )
#define GIVEBACKCOND(op,i) \
  (op==PEEK || (op==SKIPLN && i==EOF) || (op==DELRET && i!=0xa))

intmach_t (*ENG_read_hook)() = NULL;

stream_node_t *stream_user_input;                          /* Shared */
stream_node_t *stream_user_output;                         /* Shared */
stream_node_t *stream_user_error;                          /* Shared */

#if defined(USE_TRACE_OUTPUT)
stream_node_t *stream_trace; /* Shared */
FILE *stream_trace_file;
#endif

/* The creation of new streams should be atomic. */
LOCK stream_list_l;

stream_node_t *root_stream_ptr;               /* Shared and _locked_ */

void print_syserror(char *s) {
  char *str;
#if !defined(LINUX) && !defined(Win32) && !defined(DARWIN) && !defined(Solaris)
  extern char *sys_errlist[];
  extern int errno;
  str = sys_errlist[errno];
#else
  str = strerror(errno);
#endif
  fprintf(stderr, "%s: %s\n", s, str);
}

void writechar(intmach_t ch,
	       intmach_t i,
	       stream_node_t *s) {
  FILE *f = s->streamfile;
  
  if (s->isatty)
    s = root_stream_ptr;
  while (--i >= 0){
    if (s->streammode != 's')                              /* Not a socket */
      putc(ch, f);
    else {
      char p;
      p = (char)ch;
      /* TODO: do not ignore result */
      if (write(TaggedToIntmach(s->label), &p, (size_t)1));
    }
    INC_COUNTS(ch,s);
  }
}

/* Returns -2 when attempting to read past end of file //) */
intmach_t readchar(stream_node_t *s,
		   intmach_t op_type,  /* DELRET, PEEK, GET, GET1, SKIPLN, or >= 0 for SKIP */
		   definition_t *pred_address) {
  FILE *f = s->streamfile;
  intmach_t i;
  unsigned char ch;

  if (s->isatty) {
    int_address = pred_address;
    while (TRUE) {
      if (root_stream_ptr->char_count==root_stream_ptr->last_nl_pos){
        print_string(stream_user_output,GetString(current_prompt));
	fflush(stream_user_output->streamfile);
      }

      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else {
        i = getc(f);
      }

      if (GIVEBACKCOND(op_type, i)) {
        s->pending_char = i;
      } else {
        INC_COUNTS(i,root_stream_ptr);
      }

      if (i==EOF) clearerr(f);

      if (EXITCOND(op_type,i)) {
        int_address = NULL; 
        return i;
      }
    }
  } else if (s->streammode != 's') { /* Not a socket */

    if (feof(f) && s->pending_char == -100) return -2; /* attempt to read past end of stream */
    
    while (TRUE) {
      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else {
        i = getc(f);
      }

      if (GIVEBACKCOND(op_type,i)) {
        s->pending_char = i;
      } else {
        INC_COUNTS(i,s);
      }
      
      if (EXITCOND(op_type,i)) return i;
    } 
  } else {                                                  /* A socket */
    intmach_t fildes = TaggedToIntmach(s->label);
    
    if (s->socket_eof) return -2; /* attempt to read past end of stream */
    
    while (TRUE) {
      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else {
        switch(read(fildes, (void *)&ch, 1)){
        case 0:
          i = EOF;
          break;
        case 1: 
          i = (intmach_t)ch;
          break;
        default:
          perror("read() in readchar(): ");
          SERIOUS_FAULT("Aborting");
        }
      }
      
      if (GIVEBACKCOND(op_type,i)) {
        s->pending_char = i;
      } else {
        INC_COUNTS(i,s);
        if (i==EOF) s->socket_eof = TRUE;
      }

      if (EXITCOND(op_type,i)) return i;

    }
  }
}

/* This is essentially an open-coded fputs().  
   fputs() starts paying off at string lengths above 50 or so.
 */
void print_string(stream_node_t *stream, char *p) {
  FILE *fileptr = stream->streamfile;
  intmach_t i;

  if (stream->isatty)
    stream = root_stream_ptr;

  if (stream->streammode != 's')                       /* Is not a socket */
    for (i = *p++; i; i = *p++) {
      putc(i,fileptr);
      INC_COUNTS(i,stream);
    }
  else {
    size_t size = 0;
    char *q = p;

    for (i = *q++; i; i = *q++) {
      INC_COUNTS(i,stream);
      size++;
    }
    /* TODO: do not ignore result */
    if (write(TaggedToIntmach(stream->label), p, size));
  }
}

void init_streams() {
#if defined(USE_THREADS)
  Init_lock(stream_list_l);
#endif

  root_stream_ptr = CHECKALLOC(stream_node_t);
  root_stream_ptr->label=ERRORTAG;
  root_stream_ptr->streamname=ERRORTAG;
  root_stream_ptr->forward=root_stream_ptr;
  root_stream_ptr->backward=root_stream_ptr;
  root_stream_ptr->last_nl_pos = 0;               /* used for tty streams */
  root_stream_ptr->nl_count = 0;
  root_stream_ptr->char_count = 0;

  stream_user_input = new_stream(ERRORTAG, "r", stdin);
  stream_user_output = new_stream(ERRORTAG, "a", stdout);
  stream_user_error = new_stream(ERRORTAG, "a", stderr);

#if defined(USE_TRACE_OUTPUT)
  {
#define TRACE_TO_FILE 1
#if defined(TRACE_TO_FILE)
    char trace_filename[MAXPATHLEN+1]; /* problem with buffer overflow */
    strcpy(trace_filename, getenv("CIAOCACHE"));
    strcat(trace_filename, "/tmp/ciao__trace.txt");
    stream_trace_file = fopen(trace_filename, "w");
    if (stream_trace_file == NULL) {
      fprintf(stderr, "{error: cannot open trace file %s}\n", trace_filename);
      abort();
    }
#else
    stream_trace_file = stderr;
#endif
    stream_trace = new_stream(ERRORTAG, "a", stream_trace_file);
  }
#endif
}


/* Protect the creation of streams: several threads might want to create
   streams at once. */

extern LOCK stream_list_l;

stream_node_t *insert_new_stream(stream_node_t *new_stream) {
  Wait_Acquire_lock(stream_list_l);
  new_stream->forward = root_stream_ptr;
  new_stream->backward = root_stream_ptr->backward;
  root_stream_ptr->backward->forward = new_stream;
  root_stream_ptr->backward = new_stream;
  Release_lock(stream_list_l);
  return new_stream;
}

stream_node_t *new_stream(tagged_t streamname,
			  char *streammode,
			  FILE *streamfile) {
  stream_node_t *s;

  s = CHECKALLOC(stream_node_t);
  s->streamname = streamname;
  s->streammode = streammode[0];
  s->pending_char = -100;
  s->socket_eof = FALSE;
  update_stream(s,streamfile);

  return insert_new_stream(s);
}

/* '$stream'(<address>,<id>) --> (stream_node_t *)
                          --> NULL, if invalid
   'r' - read mode,  streammode=[rs]
   'w' - write mode, streammode=[was]
   'x' - any mode,   streammode=[rwas]
   'y' - any mode but no standard streams  */
stream_node_t *stream_to_ptr(tagged_t t, intmach_t mode) {
  DerefSw_HVAorCVAorSVA_Other(t, {
    return NULL;
  }, {
    stream_node_t *n = NULL;

    /* TODO: optimize */
    if (TaggedIsATM(t)) {
      if (mode=='y')
	n = NULL;
      else if (t==atom_user)
	n = (mode=='r' ? stream_user_input : stream_user_output);
      else if (t==atom_user_input)
	n = stream_user_input;
      else if (t==atom_user_output)
	n = stream_user_output;
      else if (t==atom_user_error)
	n = stream_user_error;
    } else if (TaggedIsSTR(t) && (TaggedToHeadfunctor(t) == functor_Dstream)) {
      tagged_t x1;
      tagged_t x2;
      DerefArg(x1,t,1);
      DerefArg(x2,t,2);
      if (!TaggedIsSmall(x1)) {
	n = NULL;
      } else if (!TaggedIsSmall(x2)) {
	n = NULL;
      } else {
	n = TaggedToStream(x1);
	if (n->label != x2) {
	  n = NULL;
	}
      }
    }

    if (((mode=='r') && (n!=NULL) && (n->streammode=='w'||n->streammode=='a')) ||
	((mode=='w') && (n!=NULL) && (n->streammode=='r')))
      return NULL;
    else
      return n;
  });
}

/* Similar to stream_to_ptr(), but giving an error code */

stream_node_t *stream_to_ptr_check(tagged_t t,
				   intmach_t mode, /* not 'y' */
				   intmach_t *errcode) {
  DerefSw_HVAorCVAorSVA_Other(t,{
    *errcode = INSTANTIATION_ERROR;
    return NULL;
  }, {
    stream_node_t *n = NULL;
    if (TaggedIsATM(t)) {
      if (t==atom_user) {
	n = (mode=='r' ? stream_user_input : stream_user_output);
      } else if (t==atom_user_input) {
	n = stream_user_input;
      } else if (t==atom_user_output) {
	n = stream_user_output;
      } else if (t==atom_user_error) {
	n = stream_user_error;
      } else {
	*errcode = DOMAIN_ERROR(STREAM_OR_ALIAS);
	return NULL;
      }
    } else if (TaggedIsSTR(t) && (TaggedToHeadfunctor(t) == functor_Dstream)) {
      tagged_t x1;
      tagged_t x2;
      DerefArg(x1,t,1);
      DerefArg(x2,t,2);
      if (!TaggedIsSmall(x1)) goto existence_error;
      if (!TaggedIsSmall(x2)) goto existence_error;
      n = TaggedToStream(x1);
      if (n->label != x2) goto existence_error;
      goto dstream_ok;
    existence_error:
      *errcode = EXISTENCE_ERROR(EXISTENCE_ERRORS(stream));
      return NULL;
    dstream_ok:
      {}
    } else {
      *errcode = DOMAIN_ERROR(STREAM_OR_ALIAS);
      return NULL;
    }

    if (mode=='r') {
      if (n->streammode=='w'||n->streammode=='a') {
	*errcode = PERMISSION_ERROR(ACCESS,PERMISSION_OBJECTS(stream));  
	return NULL;
      }
    } else if (mode=='w') {
      if (n->streammode=='r') {
	*errcode = PERMISSION_ERROR(MODIFY,PERMISSION_OBJECTS(stream)); 
	return NULL;
      }
    }
    return n;
  });
}

static intmach_t file_is_tty(FILE *file) {
  return (isatty(fileno(file)) ||
          (prolog_force_interactive && fileno(file)<3));
}

void update_stream(stream_node_t *s, FILE *file) {
  s->label = MakeSmall(fileno(file));
  s->streamfile = file;
  if ((s->isatty = file_is_tty(file)))
    s = root_stream_ptr;
  s->last_nl_pos = 0;
  s->nl_count = 0;
  s->char_count = 0; /* less than perfect */
}


#if defined(CREATE_NEW_STREAMS)
void update_std_streams() /* called by restore/1 */
{
  stream_node_t *streamptr = root_stream_ptr
  stream_node_t *next_ptr;

  do {
    next_ptr = streamptr->forward;
    if (streamptr->streamname!=ERRORTAG) fclose(streamptr->streamfile);
    CHECKDEALLOC0(stream_node_t, streamptr);
    streamptr = next_ptr;
  } while (streamptr!=root_stream_ptr);
  init_streams();
  CVOID__CALL(init_streams_each_time);
}
#else
void update_std_streams()		/* called by restore/1 */
{
  stream_node_t *streamptr = root_stream_ptr->forward;

  while (streamptr!=root_stream_ptr) {         /* close any ghost streams */
    if (streamptr->streamname!=ERRORTAG)
      fclose(streamptr->streamfile);
    else			      /* check if std stream is a tty now */
      update_stream(streamptr,streamptr->streamfile);
    streamptr = streamptr->forward;
  }
}
#endif

/****************************************************************************/
/* Faults */

/* TODO: the wam_initialized conditional should not be in this
   code... too complex... */
CVOID__PROTO_N(failc, char *mesg) {
  fprintf(stderr, "{error: %s}\n", mesg);
  if (!in_abort_context) {
    fprintf(stderr, "Wam not initialized, exiting!!!\n");
    engine_exit(-1);
  }
}

/****************************************************************************/
/* Hash tables */

/* get hash table node for a key */
hashtab_node_t *hashtab_get(hashtab_t *sw, hashtab_key_t key) {
  hashtab_node_t *hnode;
  intmach_t i;
  uintmach_t t0;

  for (i=0, t0 = HASHTAGGED(key) & sw->mask;
       ;
       i+=sizeof(hashtab_node_t), t0=(t0+i) & sw->mask) {
    hnode = HASHNODE(sw, t0);
    if (hnode->key==key || !hnode->key) return hnode;
  }
}

hashtab_t *hashtab_new(intmach_t size, tagged_t otherwise) {
  intmach_t i;
  hashtab_t *sw;

  SET_CHECKALLOC_TAILED(sw, hashtab_t, size);
  
  sw->count = 0;
#if defined(ABSMACH_OPT__atomgc)
  sw->next_index = 0;
#endif
  for (i=0; i<size; i++) {
    sw->node[i].key = 0;
    sw->node[i].value.tagged = otherwise;
  }
  return sw;
}

/* free a hashtab, don't care about its elements */
void hashtab_free(hashtab_t *sw) {
  CHECKDEALLOC0_TAILED(hashtab_t, sw);
}

void hashtab_clear(hashtab_t *sw) { 
  hashtab_node_t *node;
  intmach_t i;
  intmach_t size;
  size = HASHTAB_SIZE(sw) * sizeof(hashtab_node_t);
#if defined(ABSMACH_OPT__atomgc)
  sw->next_index = 0;
#endif
  for (i = 0; i < size; i += sizeof(hashtab_node_t)) { 
    node = HASHNODE(sw, i);
    node->key = 0;
    node->value.tagged = 0;
  }
  sw->count = 0;
}

/* like hashtab_get, but expands the hash table if needed */
hashtab_node_t *hashtab_lookup(hashtab_t **swp, hashtab_key_t k) {
  hashtab_node_t *h1;

  h1 = hashtab_get(*swp,k);
  if (h1->key) {
    return h1;
  } else {
    h1->key = k;
    if (((*swp)->count+=1)<<1 <= HASHTAB_SIZE(*swp)) {
      return h1;
    } else {
      HASHTAB_EXPAND(swp);
      return hashtab_get(*swp,k);
    }
  }
}

/****************************************************************************/

void expunge_instance(instance_t *i) {
  instance_t **loc;
  int_info_t *root = i->root;

#if defined(USE_THREADS)
  if (root->behavior_on_failure != DYNAMIC) {
    DEBUG__TRACE(debug_conc,
		 "*** %ld(%ld) expunge_instance: deleting instance %lx!\n",
		 (long)Thread_Id, (long)GET_INC_COUNTER, (long)i);
  }
  RTCHECK({
    if (root->behavior_on_failure != DYNAMIC &&
	Cond_Lock_is_unset(root->clause_insertion_cond))
      TRACE_PRINTF("*** %ld(%ld) expunge_instance: lock not set!\n",
	      (long)Thread_Id, (long)GET_INC_COUNTER);
    if (!root->first)
      TRACE_PRINTF("*** %ld(%ld) expunge_instance: no first instance!\n",
	      (long)Thread_Id, (long)GET_INC_COUNTER);
  });
#endif
  
  if (!i->forward)		/* last ? */
    root->first->backward = i->backward;
  else
    i->forward->backward = i->backward;

  if (i == root->first)	/* first ? */
    root->first = i->forward;
  else
    i->backward->forward = i->forward;
    
  loc = (i->key==ERRORTAG ? &root->varcase :
	 i->key==functor_lst ? &root->lstcase :
	 &hashtab_get(root->indexer,i->key)->value.instp);
  
  if (!i->next_forward)	/* last ? */
    (*loc)->next_backward = i->next_backward;
  else
    i->next_forward->next_backward = i->next_backward;
    
  if (i == (*loc))		/* first ? */
    (*loc) = i->next_forward;
  else
    i->next_backward->next_forward = i->next_forward;
    
  i->rank = ERRORTAG;

  CHECKDEALLOC0_TAILED(instance_t, i);
}

 /* Move all elements of a queue to another queue, and make all of them to
    point to the instance destinst */

void move_queue(instance_handle_t **srcq,
		instance_handle_t **destq,
		instance_t *destinst) {
  instance_handle_t *last, *running = *srcq;

#if defined(USE_THREADS)
#if defined(DEBUG_TRACE)
  int_info_t *root =
    *srcq && (*srcq)->inst_ptr ? (*srcq)->inst_ptr->root : NULL;
  intmach_t counter = 0;

  if (root && Cond_Lock_is_unset(root->clause_insertion_cond)) {
    /* TODO: precondition or trace? */
    DEBUG__TRACE(debug_conc,
		 "*** in move_queue() with lock unset!\n");
  }

  DEBUG__TRACE(debug_conc,
            "*** %ld(%ld) moving queue from %lx to %lx (-> instance %lx)\n",
            (long)Thread_Id, (long)GET_INC_COUNTER, 
            (long)srcq, (long)destq, (long)destinst);
#endif
#endif

  if (running){
    while(running) {
#if defined(USE_THREADS)
#if defined(DEBUG_TRACE)
      counter++;
#endif
#endif
      running->inst_ptr = destinst;
      last = running;
      running = running->next_handle;
    }
    last->next_handle = *destq;
    if (last->next_handle)
      last->next_handle->previous_handle = last;
    *destq = *srcq;
    *srcq = NULL;
  }
#if defined(USE_THREADS)
  DEBUG__TRACE(debug_conc,
	       "*** %ld(%ld) after moving queue made %ld steps\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER, (long)counter);
#endif
}

/*------------------------------------------------------------*/

extern worker_t *create_wam_storage();

/* GLOBAL DATA STRUCTURES */

/* We are giving worker IDs from a pool, in order to speed up task
   creation. This pool needs exclusive access. */
SLOCK    worker_id_pool_l;

/* Count for new atom names; if accessed concurrently, we want no repeated
   identifiers.  */
SLOCK    atom_id_l;

bool_t in_abort_context = FALSE;

/* Shared .... */

tagged_t functor_lst;
tagged_t functor_slash;
tagged_t functor_Dref;
tagged_t functor_Dstream;
tagged_t functor_Dsetarg;

tagged_t functor_active;
tagged_t functor_pending;
tagged_t functor_failed;
tagged_t functor_available;

tagged_t current_prompt;
tagged_t current_unknown;
tagged_t current_ferror_flag;
tagged_t current_quiet_flag;
tagged_t current_radix;

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
definition_t *address_metacall;
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



/*
void reclassify_atoms()
{
  intmach_t i;

  for (i=0; i<prolog_atoms->count; i++)
    classify_atom(atmtab[i]->value.atomp);
}
*/		

/* Initializations that need to be made once only. */

void init_locks() {
#if defined(USE_THREADS)
  Init_slock(prolog_predicates_l);
#if defined(DEBUG_TRACE)
  Init_slock(ops_counter_l);
#endif

  Init_slock(worker_id_pool_l);
  Init_slock(atom_id_l);
  Init_slock(wam_list_l);
  /*  init_dynamic_locks();*/
#endif
}

void compute_cwd();

/* TODO: hmmm... */
void glb_init_each_time() {
  current_radix = MakeSmall(10);
  prolog_init_radix();
  current_prompt = GET_ATOM("|: ");
  enable_conditions();
  compute_cwd();
}

#if defined(USE_PROLOG_DEBUGGER)
// todo: kludge... use one per wam
bool_t debug_mode;
intmach_t debug_status;
#endif

CVOID__PROTO(local_init_each_time) {
  tagged_t x0;
  choice_t *b;

 /* Debugger state globals moved to per-thread variables because of
    reallocations of the atoms they point to when expanding the heap ---
    this caused the program to break, since a C variable could be pointing
    to a thread's heap and be reallocated by another thread with wrong
    displacements. */

  /* Initialize debugger variables */

  GLOBAL_VARS_ROOT = atom_nil;
  Current_Debugger_Mode = atom_off;
#if defined(USE_PROLOG_DEBUGGER)
  debug_mode = FALSE;
  debug_status = 0;
#endif

  /* Initialize garbage collection stats */

  Gc_Total_Grey = 0;

  x0 = X(0); /* save X(0) */

  G->heap_top = Heap_Start;
  G->trail_top = Trail_Start;
  G->next_insn = NULL;
  G->frame = NULL;
  
  {
    frame_t *frame;
    frame = (frame_t *)Stack_Start;
    /* set up initial frame */
    CODE_CFRAME(frame, exitcode);
  }

  /* InitialChoice */
  w->choice = (choice_t *)Choice_Start;
  CODE_CHOICE_NEW0(b, w->choice, termcode);

  X(0) = atom_nil;
  CODE_NECK_TRY(b);
  SetDeep();

  ChoiceptMarkPure(b);
  ChoiceptMarkStatic(b);
  ChoiceptMarkNoCVA(b);

  VALUETRAIL__INIT;
  TopConcChpt = b;           /* Initialize concurrent topmost choicepoint */

  X(0) = x0; /* restore X(0) */
  G->next_insn = bootcode;
  Stop_This_Goal(w) = FALSE;

  w->liveinfo = NULL;

  DEBUG__TRACE(debug_threads,
	       "%ld (%ld) Initializing WAM %lx: choice = %lx, trail = %lx, frame = %lx\n",
	       (long)Thread_Id, (long)GET_INC_COUNTER, (long)w,
	       (long)b, (long)G->trail_top, (long)G->frame);

  CVOID__CALL(init_streams_each_time); /* set misc. variables, handle signals */
  UnsetEvent();
  UnsetCIntEvent();
  CVOID__CALL(control_c_normal); /* For threads also? */
}

CVOID__PROTO(init_streams_each_time) {
  Input_Stream_Ptr = stream_user_input;
  Output_Stream_Ptr = stream_user_output;
}

#define GLOBALSTKSIZE  0x3fff
#define LOCALSTKSIZE   0xfff
#define CHOICESTKSIZE  0xfff
#define TRAILSTKSIZE   0xfff
/* JF: just heuristics */
#define XREGBANKSIZE   (ARITYLIMIT * 8)
#define NESTEDSTACK_SIZE XREGBANKSIZE /* TODO: current c_term cannot
					  use more registers! */

/* size of global atom table  */
#define ATMTABSIZE  (4*1024)	

/* All-predicates lock; this might be held for a long time */
SLOCK prolog_predicates_l;

hashtab_t *predicates_location = NULL; /* Shared */
/* Table of builtins */
hashtab_t *switch_on_builtin;
/* Table of functions */
hashtab_t *switch_on_function;
#if defined(USE_GLOBAL_WORKER)
__thread worker_t *w = NULL;
#endif

/* Creates the wam structure, allocates its areas and initializes them.
   This returns an empty, fresh wam.  We do not add it here to the task
   state list; it needs its own thread, which we have after startwam() */
intmach_t disable_local_init;
worker_t *create_and_init_wam() {
  worker_t *new_worker;
  new_worker = create_wam_storage();
  WITH_WORKER(new_worker, {
    CVOID__CALL(create_wam_areas);
    /* TODO: think in a better solution */
    /* Init local areas */
    if (!disable_local_init) CVOID__CALL(local_init_each_time);
  });
  return new_worker;
}

/* Available workers are enqued here */

worker_t *wam_list = NULL;
SLOCK wam_list_l;

worker_t *free_wam() {
  worker_t *free_wam;

  Wait_Acquire_slock(wam_list_l);
  if (wam_list) {
    free_wam = wam_list;
    wam_list = Next_Worker(free_wam);
    Release_slock(wam_list_l);
    Next_Worker(free_wam) = NULL;
  } else {
    Release_slock(wam_list_l);
    free_wam = create_and_init_wam();
  }
  return free_wam;
}

CVOID__PROTO(release_wam) {
  CVOID__CALL(local_init_each_time);
  Wait_Acquire_slock(wam_list_l);
  Next_Worker(w) = wam_list;
  wam_list = w;
  Release_slock(wam_list_l);
}

worker_t *create_wam_storage() {
  worker_t *new_worker;

  SET_CHECKALLOC_TAILED(new_worker, worker_t, XREGBANKSIZE);
  WITH_WORKER(new_worker, {
    w->misc = CHECKALLOC(misc_info_t);
    /* TODO: move to stream code */
    w->streams = CHECKALLOC(io_streams_t);
  });
  return new_worker;
}

CVOID__PROTO(create_wam_areas) {
  intmach_t i, j;

  Atom_Buffer_Length = STATICMAXATOM;
  Atom_Buffer = CHECKALLOC_ARRAY(char, Atom_Buffer_Length);

  /* Init the pbuff */
#define PBUFF_INITIAL_SIZE 512
  w->misc->pbuff = CHECKALLOC_ARRAY(char, PBUFF_INITIAL_SIZE);
  w->misc->pbuff_length = PBUFF_INITIAL_SIZE;

  /* Init the nestedstack */
  w->misc->nestedstack = CHECKALLOC_ARRAY(nestedstack_entry_t, NESTEDSTACK_SIZE);
  w->misc->nestedstack_length = NESTEDSTACK_SIZE;
  w->misc->nestedstack_count = 0;

  /* heap pointer is first free cell, grows ++ */
  i = resources__get_variable("GLOBALSTKSIZE",GLOBALSTKSIZE);
  Heap_Start = ALLOC_AREA(i*sizeof(tagged_t));
  Heap_End = HeapCharOffset(Heap_Start,i*sizeof(tagged_t));
  UnsetEvent();

  /* stack pointer is first free cell, grows ++ */
  i = resources__get_variable("LOCALSTKSIZE",LOCALSTKSIZE);
  Stack_Start = ALLOC_AREA(i*sizeof(tagged_t));
  Stack_End = (tagged_t *)StackCharOffset(Stack_Start,i*sizeof(tagged_t));

  /* trail pointer is first free cell, grows ++ */
  /* choice pointer is last busy cell, grows -- */
  i = resources__get_variable("CHOICESTKSIZE",CHOICESTKSIZE);
  j = resources__get_variable("TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  Choice_End = Trail_Start = ALLOC_AREA(i*sizeof(tagged_t));
  Choice_Start = Trail_End = TrailCharOffset(Trail_Start, i*sizeof(tagged_t));
}

/* Cleanup after abort: shrink stacks to initial sizes. */
CVOID__PROTO(reinitialize_stacks) {
  intmach_t i, j;

  i = resources__get_variable("GLOBALSTKSIZE",GLOBALSTKSIZE) * sizeof(tagged_t);
  j = HeapCharSize;
  if (j != i) {
    Heap_Start = REALLOC_AREA(Heap_Start, j, i);
    Heap_End = HeapCharOffset(Heap_Start,i);
  }
  i = resources__get_variable("LOCALSTKSIZE",LOCALSTKSIZE) * sizeof(tagged_t);
  j = StackCharSize;
  if (j != i) {
    Stack_Start = REALLOC_AREA(Stack_Start, j, i);
    Stack_End = (tagged_t *)StackCharOffset(Stack_Start, i);
  }
  i = resources__get_variable("CHOICESTKSIZE",CHOICESTKSIZE) * sizeof(tagged_t);
  j = resources__get_variable("TRAILSTKSIZE",TRAILSTKSIZE) * sizeof(tagged_t);
  i += j;
  j = TrailCharDifference(Trail_Start,Trail_End);
  if (j != i) {
    Choice_End = Trail_Start = REALLOC_AREA(Trail_Start, j, i);
    Choice_Start = Trail_End = TrailCharOffset(Trail_Start,i*sizeof(tagged_t));
  }

  /* Create an expandable char array for loading po files */ 

  if (Atom_Buffer_Length != STATICMAXATOM) {
    Atom_Buffer = CHECKREALLOC0_ARRAY(char, Atom_Buffer, Atom_Buffer_Length, STATICMAXATOM);
    Atom_Buffer_Length = STATICMAXATOM;
  }

  UnsetEvent();
  
  CVOID__CALL(empty_gcdef_bin);

  fflush(stdout);
  fflush(stderr);
}

void init_symbol_tables() {
  intmach_t i;
#if defined(ABSMACH_OPT__atomgc)
  intmach_t j;
#endif

  i = resources__get_variable("ATMTABSIZE",ATMTABSIZE);
  atmtab = CHECKALLOC_ARRAY(hashtab_node_t *, i);
#if defined(ABSMACH_OPT__atomgc)
  for (j=0; j < i; j++) atmtab[j] = NULL;
#endif

  prolog_atoms = HASHTAB_NEW(2*i);

  /* Predicate table initialization */
  predicates_location = HASHTAB_NEW(2);

  /* Predicate table initialization */
  modules_location = HASHTAB_NEW(2);
#if defined(ABSMACH_OPT__oo_extensions)
  objfunctor_table = HASHTAB_NEW(2);
#endif  

  switch_on_builtin = HASHTAB_NEW(2);
  switch_on_function = HASHTAB_NEW(2);

#if defined(ABSMACH_OPT__functor_table)
  /* Insert a dummy functor to reserve atom number 0 so that no
     functor will have a key 0 (which is a reserved key value) */
  /* TODO: this is a bit dirty... */
  deffunctor("$@#$-d-u-m-m-y-$#@$", 1);
#endif
}

definition_t *register_cbool__2(char *name, intmach_t arity, void *procedure) {
  definition_t *func;
  intmach_t current_mem = total_mem_count;
  func = LOOKUP_DEF(deffunctor(name, arity));
  set_predtyp(func, ENTER_CBOOL);
  func->code.proc = procedure;
  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}

definition_t *register_cinsnp__2(char *name, intmach_t arity, void *procedure) {
  definition_t *func;
  intmach_t current_mem = total_mem_count;
  func = LOOKUP_DEF(deffunctor(name, arity));
  set_predtyp(func, ENTER_CINSNP);
  func->code.proc = procedure;
  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}

definition_t *register_cvoid__2(char *name, intmach_t arity, void *procedure) {
  definition_t *func;
  intmach_t current_mem = total_mem_count;
  func = LOOKUP_DEF(deffunctor(name, arity));
  set_predtyp(func, ENTER_CVOID);
  func->code.proc = procedure;
  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}

void set_defbits(char *name, intmach_t arity, intmach_t bits) {
  definition_t *func;
  func = LOOKUP_DEF(deffunctor(name, arity));
  /* TODO: other defbits are not used here */
  if ((bits & DEFBITS(hardrtexp)) != 0) {
    func->properties.hardrtexp = 1;
  }
}

definition_t *query_predicate(char *name, intmach_t arity) {
  definition_t *func;
  intmach_t current_mem = total_mem_count;
  func = LOOKUP_DEF(deffunctor(name,arity));
  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}

void unregister_cbool(char *name, intmach_t arity) {
  definition_t *f;
  f = GET_DEF(deffunctor(name, arity));
  WITH_WORKER(NULL, { /* TODO: this work because the predicate is not interpreted! but the code is potentially wrong... */
    (void)CBOOL__SUCCEED_N(abolish, f);
  });
}

void unregister_cinsnp(char *name, intmach_t arity) {
  definition_t *f;
  f = GET_DEF(deffunctor(name, arity));
  WITH_WORKER(NULL, { /* TODO: this work because the predicate is not interpreted! */
    (void)CBOOL__SUCCEED_N(abolish, f);
  });
}

void unregister_cvoid(char *name, intmach_t arity) {
  definition_t *f;
  f = GET_DEF(deffunctor(name, arity));
  WITH_WORKER(NULL, { /* TODO: this work because the predicate is not interpreted! */
    (void)CBOOL__SUCCEED_N(abolish, f);
  });
}

void register_builtin__2(char *atom, intmach_t arity, void *proc) {
  tagged_t k = deffunctor(atom,arity);
  hashtab_node_t *node;
  
  node = hashtab_lookup(&switch_on_builtin, (hashtab_key_t)k);
  node->key = (hashtab_key_t)k;
  node->value.proc = proc;
}

void register_ctagged__2(char *atom, intmach_t arity, void *proc) {
  tagged_t k = deffunctor(atom,arity);
  hashtab_node_t *node;
  
  node = hashtab_lookup(&switch_on_function, (hashtab_key_t)k);
  node->key = (hashtab_key_t)k;
  node->value.proc = proc;
}

/* tagged->intmach hashtab (for compilation to C) */
hashtab_t *register_sw_on_tagged(tagged_t *taggeds, intmach_t length) {
  hashtab_t *sw;
  hashtab_node_t *node;
  intmach_t i;
  
  sw = HASHTAB_NEW(2);

  for (i = 0; i < length; i++) {
    node = hashtab_lookup(&sw, (hashtab_key_t)taggeds[i]);
    node->key = (hashtab_key_t)taggeds[i];
    node->value.idx = i+1;
  }

  return sw;
}
intmach_t tagged_hashtab_get(hashtab_t *sw, tagged_t k) {
  hashtab_node_t *h1;
  h1 = hashtab_get(sw, k);
  if (h1->key) {
    return h1->value.idx;
  } else {
    return 0;
  }
}

/* todo[ts]: extend and change name by init_boxed? currently supports
   floats and bignums, via string_to_number */
CVOID__PROTO_N(init_number, char *string, tagged_t *p) {
  tagged_t t;
  tagged_t *h;
  h = G->heap_top;
  if (!CBOOL__SUCCEED_N(string_to_number, (unsigned char *)string, 10, &t, 0)) {
    /* TODO: raise an exception or insert here a RTCHECK */
    fprintf(stderr, "internal error initializing bignum or float\n");
  }
  copy_blob(TagpPtr(STR,t), p);
  G->heap_top = h;
}

CBOOL__PROTO(init_dynlink);
CVOID__PROTO(init_flags);

void reset_timing();

CVOID__PROTO(init_flags) {
  current_unknown = atom_error;
  current_ferror_flag = atom_on;
  current_quiet_flag = atom_off;
}

CVOID__PROTO(init_once) {
  /*
  assert((sizeof(int32_t) == 4));
  assert((sizeof(int64_t) == 8));
  assert((sizeof(void *) == sizeof(intp_t)));
  assert((sizeof(flt64_t) == 8));
  assert((WORDSIZE == sizeof(tagged_t) * 8));
  */

  /* Ensure that sizes are correct */
  if (sizeof(worker_t) != X_OFFSET_FROM_WORKER) {
    PANIC_FAULT("worker size badly calculated");
  }
  if (sizeof(frame_t) != Y_OFFSET_FROM_FRAME) {
    PANIC_FAULT("frame size badly calculated");
  }

  /* Init time variables */

  reset_timing();

#if defined(USE_THREADS)
  INIT_THREADS;
#endif
  init_gc();

  init_symbol_tables();

  functor_lst = deffunctor(".",2);
  functor_slash = deffunctor("/",2);
  functor_Dref = deffunctor("$ref",2);
  functor_Dstream = deffunctor("$stream",2);
  functor_Dsetarg = deffunctor("internals:$setarg",4);
  functor_active = deffunctor("active", 4);
  functor_pending = deffunctor("pending", 4);
  functor_failed = deffunctor("failed", 3);
  functor_available = deffunctor("available", 1);

  address_metacall = query_predicate("interpreter:metacall",3);
  address_undefined_goal = query_predicate("interpreter:undefined_goal",1);
  address_trace = query_predicate("debugger:debug_trace",1);

  address_help = query_predicate("internals:control_c_handler",0);
  address_restart = query_predicate("internals:reboot",0);
  address_call = query_predicate("hiord_rt:call",1);
  address_true = query_predicate("basiccontrol:true",0);
  address_fail = query_predicate("basiccontrol:fail",0);
  address_error = query_predicate("internals:error",5);
  /* Attributed variables support */
  address_pending_unifications = query_predicate("internals:pending_unifications",1);
  address_uvc = query_predicate("internals:uvc",2);
  address_ucc = query_predicate("internals:ucc",2);
  address_getct = query_predicate("io_basic:getct",2);
  address_getct1 = query_predicate("io_basic:getct1",2);
  address_get = query_predicate("io_basic:get_code",1);
  address_get2 = query_predicate("io_basic:get_code",2);
  address_get1 = query_predicate("io_basic:get1_code",1);
  address_get12 = query_predicate("io_basic:get1_code",2);
  address_peek = query_predicate("io_basic:peek_code",1);
  address_peek2 = query_predicate("io_basic:peek_code",2);
  address_skip = query_predicate("io_basic:skip_code",1);
  address_skip2 = query_predicate("io_basic:skip_code",2);
  address_skip_line = query_predicate("io_basic:skip_line",0);
  address_skip_line1 = query_predicate("io_basic:skip_line1",1);
}


/***************************************************************************/

void compute_cwd();

void init_timing();
CVOID__PROTO(init_inout);

CVOID__PROTO(init_everything);
CBOOL__PROTO(init_dynlink);
CVOID__PROTO(init_flags);
CVOID__PROTO(ciao_initcode);

void engine_set_opts(char **optv, int optc) {
  intmach_t i;
  /* get engine options */
  for (i = 0; i < optc; i++) {
    if (strcmp(optv[i],"-i") == 0) {
      prolog_force_interactive = 1;
#if defined(ABSMACH_OPT__profile_calls)
    } else if (profile__get_opt(optv[i])) { /* Profile option */
#endif
#if defined(DEBUG_TRACE)
    } else if (debug_trace__get_opt(optv[i])) { /* Debug trace option */
#endif
    } else { /* unknown engine option */
      fprintf(stderr,"{error: unknown engine option \"%s\"\n}", optv[i]);
    }
  }
}

void init_profile();
CVOID__PROTO(finish_profile);

void engine_init() {
  /* Basic initializations (before anything else can be done) */
  init_locks();
  init_alloc();
  extern intmach_t disable_local_init;
  goal_descriptor_t *goal_desc;

  init_goal_desc_list();
  disable_local_init = 1; /* TODO: find a better solution */
  goal_desc = init_first_gd_entry();
  WITH_WORKER(goal_desc->worker_registers, {
    /* Streams for input/output (and trace) */
    CVOID__CALL(init_streams);

    /* Init timing */
    init_timing();
    /* Init debug/trace/profile */
    init_profile();

    init_float_to_str(); /* prepares the char digit table */
    init_symbolchar();
    compute_cwd();
    /* Find out the library_directory --- we need it before using '$' anywhere */
    init_ciaolib();

    CVOID__CALL(init_once);
    CVOID__CALL(init_some_bytecode);
    /*mem_prog_count = 0;*/
    CVOID__CALL(init_inout);

    CVOID__CALL(ciao_initcode);

    CVOID__CALL(local_init_each_time);
    glb_init_each_time();

    CVOID__CALL(init_everything);
    (void)CBOOL__SUCCEED(init_dynlink); /* TODO: ignored succeed status */
    CVOID__CALL(init_flags);

    disable_local_init = 0; /* TODO: find a better solution */
  });

  make_goal_desc_free(goal_desc);
}

CVOID__PROTO(engine_finish) {
  CVOID__CALL(finish_profile);
  fflush(stderr);
  fflush(stdout);
  fflush(NULL);
}
