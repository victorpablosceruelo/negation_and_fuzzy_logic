#include <engine/basiccontrol.native.h>

#include <stdlib.h>
#include <string.h>

typedef intp_t relocoffset_t;
typedef intmach_t qreg_t;

/* todo[ts]: change blob save/restore code if more blob types are
   added */

ftype_base_t *ftypedef__basic_new(intmach_t size, intmach_t smethod) {
  ftype_basic_t *f;
  f = CHECKALLOC(ftype_basic_t);
  f->type = FTYPEDEF_BASIC;
  f->size = size;
  f->smethod = smethod;
  return (ftype_base_t *)f;
}

ftype_base_t *ftypedef__str_new(intmach_t arity, ftype_typeid_t *args) {
  ftype_str_t *f;
  f = CHECKALLOC(ftype_str_t);
  f->type = FTYPEDEF_STR;
  f->arity = arity;
  f->args = args;
  return (ftype_base_t *)f;
}

ftype_base_t *ftypedef__array_new(ftype_typeid_t itype, ftype_typeid_t argtype) {
  ftype_array_t *f;
  f = CHECKALLOC(ftype_array_t);
  f->type = FTYPEDEF_ARRAY;
  f->itype = itype;
  f->argtype = argtype;
  return (ftype_base_t *)f;
}

ftype_base_t *ftypedef__blob_new() {
  ftype_blob_t *f;
  f = CHECKALLOC(ftype_blob_t);
  f->type = FTYPEDEF_BLOB;
  return (ftype_base_t *)f;
}

void ftypedef__free(ftype_base_t *f) {
  switch (f->type) {
  case FTYPEDEF_BASIC:
    CHECKDEALLOC0(ftype_basic_t, f);
    break;
  case FTYPEDEF_STR:
    if (((ftype_str_t *)f)->args != NULL) {
      CHECKDEALLOC0_ARRAY(ftype_typeid_t, ((ftype_str_t *)f)->args, ((ftype_str_t *)f)->arity);
    }
    CHECKDEALLOC0(ftype_str_t, f);
    break;
  case FTYPEDEF_ARRAY:
    CHECKDEALLOC0(ftype_array_t, f);
    break;
  case FTYPEDEF_BLOB:
    CHECKDEALLOC0(ftype_blob_t, f);
    break;
  }
}

extern absmachdef_t abscurr;
absmachdef_t *absnext = NULL;

/* max of Ai where input is [[A0|...],[A1|...],...,[An|...]] */
CBOOL__PROTO_N(listtbl_max, tagged_t l, intmach_t *res) {
  tagged_t t;
  tagged_t u;
  tagged_t a;
  intmach_t max;
  intmach_t c;
  /* */
  max = 0;
  DEREF(l, l);
  t = l;
  while (t != atom_nil) {
    CBOOL__TEST(TaggedIsLST(t));
    DerefCar(u, t);
    DerefCdr(t, t);
    /* [A|...] */
    CBOOL__TEST(TaggedIsLST(u));
    DerefCar(a, u);
    CBOOL__TEST(TaggedIsSmall(a));
    /* */
    c = GetSmall(a);
    if (c > max) { max = c; }
  }
  *res = max;
  CBOOL__PROCEED;
}

void absmach__free(absmachdef_t *absmach) {
  intmach_t i;
  if (absmach->ins_info != NULL) {
    for (i = 0; i < absmach->ins_n; i++) {
      if (absmach->ins_info[i] == NULL) continue;
      ftypedef__free(absmach->ins_info[i]);
      absmach->ins_info[i] = NULL;
    }
    CHECKDEALLOC0_ARRAY(ftype_base_t *, absmach->ins_info, absmach->ins_n);
    absmach->ins_info = NULL;
  }
  if (absmach->ftype_info != NULL) {
    for (i = 0; i < absmach->ftype_n; i++) {
      if (absmach->ftype_info[i] == NULL) continue;
      ftypedef__free(absmach->ftype_info[i]);
      absmach->ftype_info[i] = NULL;
    }
    CHECKDEALLOC0_ARRAY(ftype_base_t *, absmach->ftype_info, absmach->ftype_n);
    absmach->ftype_info = NULL;
  }
  CHECKDEALLOC0(absmachdef_t, absmach);
}

/* Fill ftype_info array from info_list */
CBOOL__PROTO_N(absmach__set_ftype_info, tagged_t info_list, ftype_base_t ***ret_ftype_info, intmach_t *ret_ftype_n) {
  tagged_t u;
  tagged_t a;
  tagged_t t;
  intmach_t max;
  intmach_t i;
  ftype_base_t **ftype_info;
  intmach_t ftype_n;

  // list of "ftype_id size smethod"

  CBOOL__CALL_N(listtbl_max, info_list, &max);

  /* create structure */
  //  TRACE_PRINTF("ftype_n: (%ld)\n", (long)(max+1));
  ftype_n = max + 1;
  ftype_info = CHECKALLOC_ARRAY(ftype_base_t *, ftype_n);
  for (i = 0; i < ftype_n; i++) {
    ftype_info[i] = NULL;
  }
  
  t = info_list;
  DEREF(t, t);
  while (t != atom_nil) {
    intmach_t id;
    intmach_t kind;
    CBOOL__TEST(TaggedIsLST(t));
    DerefCar(u, t);
    DerefCdr(t, t);
    /* [A,Kind,Size,SMethod] */
    /* A */
    CBOOL__TEST(TaggedIsLST(u));
    DerefCar(a, u);
    CBOOL__TEST(TaggedIsSmall(a));
    id = (intmach_t)GetSmall(a);
    if (ftype_info[id] != NULL) {
      /* TODO: throw an exception? */
      fprintf(stderr, "{bug: not overwriting definition for ftype %ld (conflict in ftype or instruction identifier)}\n", (long)id);
      continue;
    }
    DerefCdr(u, u);
    /* Kind */
    CBOOL__TEST(TaggedIsLST(u));
    DerefCar(a, u);
    CBOOL__TEST(TaggedIsSmall(a));
    kind = (intmach_t)GetSmall(a);
    DerefCdr(u, u);
    if (kind == 0) {
      intmach_t size;
      intmach_t smethod;
      /* Size */
      CBOOL__TEST(TaggedIsLST(u));
      DerefCar(a, u);
      CBOOL__TEST(TaggedIsSmall(a));
      size = (intmach_t)GetSmall(a);
      DerefCdr(u, u);
      /* SMethod */
      CBOOL__TEST(TaggedIsLST(u));
      DerefCar(a, u);
      CBOOL__TEST(TaggedIsSmall(a));
      smethod = (intmach_t)GetSmall(a);
      DerefCdr(u, u);
      /* insert in the table */
      ftype_info[id] = ftypedef__basic_new(size, smethod);
      //TRACE_PRINTF("%ld = ba %ld %ld\n", (long)id, (long)size, (long)smethod);
    } else if (kind == 1) {
      tagged_t f;
      tagged_t b;
      intmach_t k;
      intmach_t m;
      /* ArgTypes */
      f = u;
      /* count length of rest */
      k = 0;
      u = f;
      while(TaggedIsLST(u)) {
	DerefCar(b, u);
	DerefCdr(u, u);
	CBOOL__TEST(TaggedIsSmall(b));
	//      TRACE_PRINTF(" %ld", (long)GetSmall(b));
	k++;
      }
      //    TRACE_PRINTF(" (%ld) ", (long)k);
      /* build argument list */
      //TRACE_PRINTF("%ld = arr %ld ", (long)id, (long)k);
      ftype_typeid_t *args;
      if (k > 0) {
	args = CHECKALLOC_ARRAY(ftype_typeid_t, k);
	m = 0;
	u = f;
	while(TaggedIsLST(u)) {
	  DerefCar(b, u);
	  DerefCdr(u, u);
	  CBOOL__TEST(TaggedIsSmall(b));
	  //      TRACE_PRINTF(" %ld", (long)GetSmall(b));
	  args[m] = (ftype_typeid_t)GetSmall(b);
	  m++;
	  //TRACE_PRINTF("%ld ", (long)(ftype_typeid_t)GetSmall(b));
	}
      } else {
	args = NULL;
      }
      //TRACE_PRINTF("\n");
      /* insert in the list */
      ftype_info[id] = ftypedef__str_new(k, args);
    } else if (kind == 2) {
      intmach_t itype;
      intmach_t argtype;
      /* Itype */
      CBOOL__TEST(TaggedIsLST(u));
      DerefCar(a, u);
      CBOOL__TEST(TaggedIsSmall(a));
      itype = (intmach_t)GetSmall(a);
      DerefCdr(u, u);
      /* Argtype */
      CBOOL__TEST(TaggedIsLST(u));
      DerefCar(a, u);
      CBOOL__TEST(TaggedIsSmall(a));
      argtype = (intmach_t)GetSmall(a);
      DerefCdr(u, u);
      /* insert in the table */
      ftype_info[id] = ftypedef__array_new(itype, argtype);
      //TRACE_PRINTF("%ld = array %ld %ld\n", (long)id, (long)itype, (long)argtype);
    } else if (kind == 3) {
      /* insert in the table */
      ftype_info[id] = ftypedef__blob_new();
      //TRACE_PRINTF("%ld = blob\n", (long)id);
    } else {
      /* TODO: throw an exception? */
      PANIC_FAULT("bad ftype def");
    }
  }
  *ret_ftype_n = ftype_n;
  *ret_ftype_info = ftype_info;
  CBOOL__PROCEED;
}

CBOOL__PROTO(absnext__set) {
  tagged_t ins_info_list;
  tagged_t ftype_info_list;
  absmachdef_t *absmach;

  if (absnext != NULL) {
    absmach__free(absnext);
    absnext = NULL;
  }

  absmach = absnext = CHECKALLOC(absmachdef_t);
  
  DEREF(X(0),X(0));
  CBOOL__TEST(TaggedIsSmall(X(0)));
  absmach->ftype_id_i = GetSmall(X(0));
  DEREF(X(1),X(1));
  CBOOL__TEST(TaggedIsSmall(X(1)));
  absmach->ftype_id_o = GetSmall(X(1));
  DEREF(X(2),X(2));
  CBOOL__TEST(TaggedIsSmall(X(2)));
  absmach->q_pad1 = GetSmall(X(2));
  DEREF(X(3),X(3));
  CBOOL__TEST(TaggedIsSmall(X(3)));
  absmach->q_pad2 = GetSmall(X(3));
  DEREF(X(4),X(4));
  CBOOL__TEST(TaggedIsSmall(X(4)));
  absmach->tagged_size = GetSmall(X(4));
  DEREF(X(5),X(5));
  CBOOL__TEST(TaggedIsSmall(X(5)));
  absmach->size_align = GetSmall(X(5));

  DEREF(X(6), X(6));
  ins_info_list = X(6);
  DEREF(X(7), X(7));
  ftype_info_list = X(7);

  CBOOL__CALL_N(absmach__set_ftype_info, ins_info_list, &absmach->ins_info, &absmach->ins_n);
  CBOOL__CALL_N(absmach__set_ftype_info, ftype_info_list, &absmach->ftype_info, &absmach->ftype_n);
  CBOOL__PROCEED;
}

/* --------------------------------------------------------------------------- */
/* Buffered input */

typedef struct buffered_in buffered_in_t;
#define BUFFERED_IN__SIZE 1024
struct buffered_in {
  FILE *file;
  intmach_t idx;
  intmach_t end;
  unsigned char buffer[BUFFERED_IN__SIZE];
};

#define BUFFERED_IN__INIT(I, F) { \
  /* Input file */ \
  (I)->file = (F); \
  /* Empty input buffer */ \
  (I)->idx = BUFFERED_IN__SIZE; \
  (I)->end = BUFFERED_IN__SIZE; \
}

#define buffered_in__getc(I) (((I)->idx == (I)->end) ? buffered_in__getc_2((I)) : (intmach_t)(I)->buffer[(I)->idx++])
static intmach_t buffered_in__getc_2(buffered_in_t *in) {
  if (in->end < BUFFERED_IN__SIZE) goto eof;
  in->end = fread(in->buffer, sizeof(unsigned char), BUFFERED_IN__SIZE, in->file);
  if (in->end == 0) goto eof;
  in->idx = 1;
  return (intmach_t)in->buffer[0];
 eof:
  return EOF; /* Could not read after buffer emptied */
}

/* see PUTINT_DEF */
#define GETINT_DEF(T, UT, NAME) \
T NAME(buffered_in_t *in) { \
  T i, j, s; \
  UT k; \
  j = buffered_in__getc(in); /* fetch initial chunk */ \
  s = j & 0x80; /* get sign bit */ \
  i = j & 0x3f; /* load first 6 bits */ \
  if (j & 0x40) { /* continue if bit 7 is on */ \
    k = 6; /* initial bit pointer */ \
    j = buffered_in__getc(in); /* fetch next chunk */ \
    while (j & 0x80) { /* continue while bit 8 is on */ \
      i |= (j & 0x7f) << k; /* load next 7 bits */ \
      k += 7; /* shift the bit pointer */ \
      j = buffered_in__getc(in); /* fetch next chunk */ \
    } \
    i |= (j << k); /* load last 7 bits */ \
  } \
  return s ? -i : i; /* apply the sign */ \
}

GETINT_DEF(int32_t, uint32_t, buffered_in__getint32);
GETINT_DEF(int64_t, uint64_t, buffered_in__getint64);

#if defined(ABSMACH_OPT__tagged64)
#if defined(ABSMACH_OPT__pointer64)
#define buffered_in__getintmach buffered_in__getint64
#else
#define buffered_in__getintmach buffered_in__getint32
#endif
#define buffered_in__getintval buffered_in__getint64
#else
#define buffered_in__getintmach buffered_in__getint32
#define buffered_in__getintval buffered_in__getint32
#endif

#define buffered_in__get_reg buffered_in__getintmach

/* --------------------------------------------------------------------------- */
/* Buffered output */

typedef struct buffered_out buffered_out_t;

#define BUFFER_PAD 32
#define MAXBUFFER 1024

struct buffered_out {
  FILE *output_file;
  unsigned char *buffer_top;
  unsigned char buffer_base[MAXBUFFER];
};

#define BUFFERED_OUT__N(O) ((O)->buffer_top - (O)->buffer_base)
#define BUFFERED_OUT__INIT(O) { \
  (O)->buffer_top = &((O)->buffer_base[0]); \
}

#define I_EST (sizeof(intmach_t)*2) /* estimated output size of a integer */
#define C_EST sizeof(char) /* estimated output size of a character */
#define CHECK_FLUSH_OUTPUT(O, AMOUNT) { \
  if (BUFFERED_OUT__N((O)) + AMOUNT + BUFFER_PAD > MAXBUFFER) flush_buffered_output((O)); \
}

void flush_buffered_output(buffered_out_t *out) {
  if (BUFFERED_OUT__N(out) > MAXBUFFER) {
    fprintf(stderr, "{panic: buffer overflow! (%ld)}\n", (long)BUFFERED_OUT__N(out));
    exit(-1);
  }
  if (fwrite(out->buffer_base, sizeof(unsigned char), BUFFERED_OUT__N(out), out->output_file)) {} else {} // avoid warning
  out->buffer_top = &out->buffer_base[0];
}

#define buffered_out__putc(O,X) { \
  *((O)->buffer_top++) = (X); \
}

/*
  compact encoding of integers:

                   .- this is the continuation bit
     byte 0 : sign|1|data6
     byte 1 : 1|data7
     byte 2 : 1|data7
     ...
     byte i : 0|data7
*/
#define PUTINT_DEF(T, NAME) \
void NAME(buffered_out_t *out, T i) { \
  T s; \
  if (i < 0) { i = -i; s = 0x80; } else { s = 0; } \
  if (i < 0x40) { \
    buffered_out__putc(out, s | (i & 0x3f)); \
  } else { \
    buffered_out__putc(out, s | 0x40 | (i & 0x3f)); \
    i >>= 6; \
    while (i >= 0x80) { \
      buffered_out__putc(out, 0x80 | (i & 0x7f)); \
      i >>= 7; \
    }  \
    buffered_out__putc(out, i & 0x7f); \
  } \
}

PUTINT_DEF(int32_t, buffered_out__putint32);
PUTINT_DEF(int64_t, buffered_out__putint64);

#if defined(ABSMACH_OPT__tagged64)
#if defined(ABSMACH_OPT__pointer64)
#define buffered_out__putintmach buffered_out__putint64
#else
#define buffered_out__putintmach buffered_out__putint32
#endif
#define buffered_out__putintval buffered_out__putint64
#else
#define buffered_out__putintmach buffered_out__putint32
#define buffered_out__putintval buffered_out__putint32
#endif

#define buffered_out__put_reg buffered_out__putintmach

void buffered_out__put_string(buffered_out_t *out, char *string) {
  intmach_t size;
  size = strlen(string) + 1;
  CHECK_FLUSH_OUTPUT(out, size);
  if (size + BUFFER_PAD > MAXBUFFER) { /* too big for the buffer */
    flush_buffered_output(out);
    if (fwrite(string, sizeof(unsigned char), size, out->output_file)) {} else {} // avoid warning
  } else {
    memcpy(out->buffer_top, string, size);
    out->buffer_top += size;
  }
}

/***************************************************************************/

/* TODO: explore alternative methods to serialize data, a memory
     projection using copy_term code??  emit the bytecode of
     compile_term??
*/       
/* TODO: bug: it doesn't work with CVAs or cyclic terms */
/* TODO: bug: it does not share structures that are shared in the
     heap, try to optimize that  */
/* TODO: use DEBUG_QSIZE to shorten the difference between the
     estimated term size obtained through qsaver__term_size and the actual
     term size after the term is loaded into memory */

/***************************************************************************/

/* Control codes to be detected */ 

#define SCRIPT_MARK 35 /* # */

/* Term construction codes */
#define QI_ENSURE_SPACE '%'
#define QI_LOAD_ATOM 'a'
#define QI_LOAD_FUNCTOR 'b'
#define QI_LOAD_SMALL 'c'
#define QI_LOAD_NUMBER_L 'd'
#define QI_LOAD_NUMBER_F 'e'
#define QI_LOAD_VARIABLE 'f'
#define QI_LOAD_NIL 'g'
#define QI_LOAD_LIST_REG 'h'
#define QI_LOAD_LIST_HEAP '<'
#define QI_LOAD_LIST_TAIL_REG '['
#define QI_LOAD_LIST_TAIL_HEAP '('
#define QI_LOAD_TUPLE_REG 'i'
#define QI_LOAD_TUPLE_HEAP '>'
#define QI_LOAD_TUPLE_TAIL_REG ']'
#define QI_LOAD_TUPLE_TAIL_HEAP ')'
#define QI_LOAD_DBNODE 'k'
#define QI_RETURN 'l'
#define QI_RELOC_TAGGED 'm'
#define QI_RELOC_EMUL_ENTRY 'p'
#define QI_RELOC_BUILTIN_ENTRY 'u'

/* --------------------------------------------------------------------------- */
/* qloader */

typedef struct qloader qloader_t;

struct qloader {
  qloader_t *next;
  tagged_t *regs_array;
  qreg_t regs_offset;
  qreg_t regs_limit;

  /* input file and local buffer */
  buffered_in_t in;
  
  /* reloc base (for relocations) */
  char *reloc_base;

  absmachdef_t *absmach;
};

qloader_t *qlcurrl = NULL;

#define GETC(Q) buffered_in__getc(&((Q)->in))

CFUN__PROTO_N(qloader__get_number, tagged_t, qloader_t *q, intmach_t liveregs);
CFUN__PROTO_N(qloader__get_string, char *, qloader_t *q);
CVOID__PROTO_N(qloader__load_bytecode, qloader_t *q, bcp_t insn_p, intmach_t length);

/* plenty at present */
#define QL_ARRAY_SIZE   (2*1024)	

CFUN__PROTO_N(qloader__get_number, tagged_t, qloader_t *q, intmach_t liveregs) {
  intmach_t used_length = 0;
  char *ws = Atom_Buffer;
  tagged_t result;

  while ((ws[used_length++] = GETC(q))) {
    ENSURE_ATOM_BUFFER(used_length, { ws = Atom_Buffer; });
  }

  if (!CBOOL__SUCCEED_N(string_to_number, (unsigned char *)Atom_Buffer, 10, &result, liveregs)) {
    PANIC_FAULT("$qread: wrong number!");
  }
  CFUN__PROCEED(result);
}

CFUN__PROTO_N(qloader__get_string, char *, qloader_t *q) {
  intmach_t used_length = 0;
  char *ws = Atom_Buffer;   /* Try to avoid indirection through WAM */

  while ((ws[used_length++] = GETC(q))) {
    ENSURE_ATOM_BUFFER(used_length, { ws = Atom_Buffer; });
  }
  CFUN__PROCEED(ws);
}

/* TODO: assumes X(0) is live */
#define QGETBYTECODE_LIVE_REGS 1

CVOID__PROTO_N(qloader__load_unboxedsmall, qloader_t *q, ftype_typeid_t ftype_id, intval_t token, bcp_t *ret) {
  bcp_t insn_p = *ret;
  intmach_t lmethod;
  lmethod = FTYPE_basic__lmethod(q->absmach->ftype_info, ftype_id);
  switch (lmethod) {
  case QL_UINT16: 
    *(uint16_t *)insn_p = token;
    insn_p = BCoff(insn_p, sizeof(uint16_t));
    break;
  case QL_UINT32: 
    *(uint32_t *)insn_p = token;
    insn_p = BCoff(insn_p, sizeof(uint32_t));
    break;
  default:
    /* TODO: throw an exception? */
    fprintf(stderr, "{panic: unrecognized load method %ld}\n", (long)lmethod);
    break;
  }
  *ret = insn_p;
}

CVOID__PROTO_N(qloader__load_basic, qloader_t *q, ftype_typeid_t ftype_id, bcp_t *ret, bcp_t base) {
  bcp_t insn_p = *ret;
  intmach_t lmethod;
  lmethod = FTYPE_basic__lmethod(q->absmach->ftype_info, ftype_id);
  switch (lmethod) {
  case QL_UINT16: 
    *(uint16_t *)insn_p = buffered_in__getint32(&q->in);
    insn_p = BCoff(insn_p, sizeof(uint16_t));
    break;
  case QL_UINT32: 
    *(uint32_t *)insn_p = buffered_in__getint32(&q->in);
    insn_p = BCoff(insn_p, sizeof(uint32_t));
    break;
  case QL_UINT64:
    *(uint64_t *)insn_p = buffered_in__getint64(&q->in);
    insn_p = BCoff(insn_p, sizeof(uint64_t));
    break;
  case QL_BASEPTR:
    /* todo[ts]: define BASEPTR32 and BASEPTR64? */
    *(bcp_t *)insn_p = BCoff(base, buffered_in__getintmach(&q->in));
    insn_p = BCoff(insn_p, sizeof(bcp_t));
    break;
  default:
    /* TODO: throw an exception? */
    fprintf(stderr, "{panic: unrecognized load method %ld}\n", (long)lmethod);
    break;
  }
  *ret = insn_p;
}

CVOID__PROTO_N(qloader__load_blob, qloader_t *q, ftype_typeid_t ftype_id, bcp_t *ret, intmach_t length, bcp_t base) {
  bcp_t insn_p = *ret;
  tagged_t *h;
  tagged_t t;

  /* TODO: this is wrong!! length is the length of all the
     bytecode chunk size!!
     
     We ensure that there is at least (length/sizeof(tagged_t)+4)
     available words in the heap. We load the number into the
     heap, copy it to the bytecode, and then move the heap pointer
     back.
  */
  h = G->heap_top;
  TEST_HEAP_OVERFLOW(h, length + 4*sizeof(tagged_t), QGETBYTECODE_LIVE_REGS);
  t = CFUN__EVAL_N(qloader__get_number, q, QGETBYTECODE_LIVE_REGS);
  insn_p = BCoff(insn_p, copy_blob(TagpPtr(STR,t), (tagged_t *)insn_p));
  G->heap_top = h;
  *ret = insn_p;
}

CVOID__PROTO_N(qloader__load_bytecode, qloader_t *q, bcp_t insn_p, intmach_t length) {
  ftype_typeid_t fmt_root[] = {q->absmach->ftype_id_i};
  ftype_typeid_t ftype_id;

  bcp_t base;
  
  base = insn_p;

  FMT_LOOP(q->absmach, fmt_root, ftype_id, i, op, {
    /* GET_OP */
    op = buffered_in__getintmach(&q->in);
    if (op == FINISH_OP) break;
  }, {
    /* EMIT_OP */
#if defined(ABSMACH_OPT__threaded_bytecode_rel16)||defined(ABSMACH_OPT__threaded_bytecode)
    bcp_t old_p = insn_p;
#endif
    CVOID__CALL_N(qloader__load_unboxedsmall, q, ftype_id, op, &insn_p);
#if defined(ABSMACH_OPT__threaded_bytecode_rel16)||defined(ABSMACH_OPT__threaded_bytecode)
    /* TODO: opIdEnc must bit inside uint16_t! */
    //    fprintf(stderr, "newop %x %x\n", BCOp(old_p, FTYPE_ctype(f_o), 0), opIdEnc(BCOp(old_p, FTYPE_ctype(f_o), 0)));
    BCOp(old_p, FTYPE_ctype(f_o), 0) = opIdEnc(BCOp(old_p, FTYPE_ctype(f_o), 0));
#endif
  }, {
    /* GET_ARRAY_I */
    i = buffered_in__getintmach(&q->in);
  }, {
    /* EMIT_ARRAY_I */
    CVOID__CALL_N(qloader__load_unboxedsmall, q, ftype_id, i, &insn_p);
  }, {
    /* EMIT_BLOB */
    //TRACE_PRINTF("fid %ld\n", (long)ftype_id);
    CVOID__CALL_N(qloader__load_blob, q, ftype_id, &insn_p, length, base);
  }, {
    /* EMIT_BASIC */
    //TRACE_PRINTF("fid %ld\n", (long)ftype_id);
    CVOID__CALL_N(qloader__load_basic, q, ftype_id, &insn_p, base);
  });
  return;
 corrupted:
  /* TODO: throw an exception? */
  fprintf(stderr, "{panic: token stream does not match instruction format}\n");
  exit(-1);
}

static CFUN__PROTO_N(qloader__load_dbnode, emul_info_t *, qloader_t *q);

void qloader__expand_regs(qloader_t *q, qreg_t requested_i);

/* TODO: rename QLENSURE and QLCHECK? (one is used before writting the other before reading) */
#define QLreg(Q, I) (Q)->regs_array[I]
#define QLENSURE(Q, I) \
  { if ((I)+(Q)->regs_offset >= (Q)->regs_limit || (I)+(Q)->regs_offset <= 0) qloader__expand_regs(Q, I); }

#define QLCHECK(Q, I) \
  RTCHECK({ if ((I)+(Q)->regs_offset >= (Q)->regs_limit || (I)+(Q)->regs_offset <= 0) { fprintf(stderr, "panic: bad ql register 0x%x! (%s, line %ld in its file)\n", (I), __func__, (long)__LINE__); exit(-1); } });

CBOOL__PROTO_N(qread_begin, FILE *qfile) {
  qloader_t *q;

  q = CHECKALLOC(qloader_t);

  q->next = qlcurrl;
  q->regs_limit = QL_ARRAY_SIZE;
  q->regs_offset = q->regs_limit>>1;
  q->regs_array = CHECKALLOC_ARRAY(tagged_t, q->regs_limit)+q->regs_offset;
  BUFFERED_IN__INIT(&(q->in), qfile);

  /* TODO: note: the loader only supports the current abstract machine definition */
  q->absmach = &abscurr;
  
  qlcurrl = q;
  
  CBOOL__PROCEED;
}

CBOOL__PROTO(qread_end) {
  qloader_t *q;

  q = qlcurrl;
  qlcurrl = q->next;

  CHECKDEALLOC0_ARRAY(tagged_t, q->regs_array-q->regs_offset, q->regs_limit);
  CHECKDEALLOC0(qloader_t, q);

  CBOOL__PROCEED;    
}

void qloader__expand_regs(qloader_t *q, qreg_t requested_i) {
  qreg_t i;
  qreg_t new_regs_limit;
  qreg_t new_regs_offset;

  q->regs_array -= q->regs_offset;

  if (requested_i < 0) requested_i = -requested_i; /* make it possitive */
  new_regs_limit = q->regs_limit;
  new_regs_offset = q->regs_offset;
  while (requested_i >= new_regs_offset) { /* expand */
    new_regs_limit <<= 1;
    new_regs_offset <<= 1;
  }
  
  q->regs_array = CHECKREALLOC0_ARRAY(tagged_t, q->regs_array, q->regs_limit, new_regs_limit);

  for (i = q->regs_limit; i > 0;) {
    i--;
    q->regs_array[i + new_regs_offset - q->regs_offset] = q->regs_array[i];
  }
  q->regs_limit = new_regs_limit;
  q->regs_offset = new_regs_offset;

  q->regs_array += q->regs_offset;
}

CBOOL__PROTO(prolog_qread_begin) {
  stream_node_t *s;
  s = stream_to_ptr(X(0), 'r');
  CBOOL__TEST(s != NULL);
  CBOOL__LASTCALL_N(qread_begin, s->streamfile);
}

static CFUN__PROTO_N(qloader__load_dbnode, emul_info_t *, qloader_t *q) {
  emul_info_t *db;
  intmach_t codelength;

  codelength = buffered_in__getintmach(&q->in);
  
  SET_CHECKALLOC_TAILED(db, emul_info_t, codelength);

  CVOID__CALL_N(qloader__load_bytecode, q, db->emulcode,codelength);
  q->reloc_base = db->emulcode;  
  db->next = NULL;
  CFUN__PROCEED(db);
}

#define RELOC_VAL(T, BASE, VAL, LABEL) ({ \
  char *pos; \
  do { \
    pos = (BASE) + (LABEL); \
    (LABEL) = (relocoffset_t)(*(T *)pos); \
    *(T *)pos = (VAL); \
  } while ((LABEL) != 0); \
}) \

static void reloc_tagged(char *base, tagged_t val, relocoffset_t Label) {
  RELOC_VAL(tagged_t, base, val, Label);
}

static void reloc_ptr(char *base, char *val, relocoffset_t Label) {
  RELOC_VAL(char *, base, val, Label);
}

/* TODO: move script skip to a different function and execute it once per file, not per qread */
/* TODO: assumes X(0) is live */
#define QREAD_LIVE_REGS 1
CBOOL__PROTO_N(qread1, tagged_t *readgoal) {
  qreg_t Li = 0;
  qreg_t Lj = 0;
#if defined(DEBUG_QSIZE)
  tagged_t *oldh = G->heap_top;
#endif
  tagged_t *h = G->heap_top;
  intmach_t pad;
  intmach_t c;
  intmach_t i, arity;
  tagged_t functor;
  qloader_t *q = qlcurrl;

  c = GETC(q);
  while (c != EOF) {
    switch (c)
      {
      case SCRIPT_MARK:
	{
	  intmach_t chr;	    
	  do {
	    chr = GETC(q);
	  } while ((chr != EOF) && (chr != 12));
	}
	break;
      case QI_ENSURE_SPACE:
	/* TODO: important assertion!! this must be the first ins executed before any ref is stored in a QL register!!! (if not, garbage collections may break everything) */
	pad=buffered_in__getintmach(&q->in);
#if defined(DEBUG_QSIZE)
	{
	  extern intmach_t debug_inscount;
	  if (debug_inscount >= 19428237) {
	    TRACE_PRINTF("pad:%ld\n", pad);
	  }
	}
#endif
	TEST_HEAP_OVERFLOW(h, pad, QREAD_LIVE_REGS);
#if defined(DEBUG_QSIZE)
	oldh = G->heap_top;
#endif
	break;
      case QI_LOAD_ATOM:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLreg(q, Li) = GET_ATOM(CFUN__EVAL_N(qloader__get_string, q));
	break;
      case QI_LOAD_FUNCTOR:
	Li = buffered_in__get_reg(&q->in);
	Lj = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLCHECK(q, Lj);
	QLreg(q, Li) = SetArity(QLreg(q, Lj),buffered_in__getintmach(&q->in));
	break;
      case QI_LOAD_SMALL:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLreg(q, Li) = MakeSmall(buffered_in__getintval(&q->in));
	break;
      case QI_LOAD_NUMBER_L:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	G->heap_top = h;
	QLreg(q, Li) = CFUN__EVAL_N(qloader__get_number, q, QREAD_LIVE_REGS);
	h = G->heap_top;
	break;
      case QI_LOAD_NUMBER_F:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	G->heap_top = h;
	QLreg(q, Li) = CFUN__EVAL_N(qloader__get_number, q, QREAD_LIVE_REGS);
	h = G->heap_top;
	break;
      case QI_LOAD_VARIABLE:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li); 
	LoadHVA(QLreg(q, Li),h);
	break;
      case QI_LOAD_NIL:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLreg(q, Li) = atom_nil;
	break;
      case QI_LOAD_LIST_REG:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLreg(q, Li) = Tagp(LST,h);
	goto load_list_common;
      case QI_LOAD_LIST_HEAP:
	HeapPush(h,Tagp(LST,h + 1));
	goto load_list_common;
      load_list_common:
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	HeapPush(h,QLreg(q, Li));
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	HeapPush(h,QLreg(q, Li));
	break;
      case QI_LOAD_LIST_TAIL_REG:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLreg(q, Li) = Tagp(LST,h);
	goto load_list_opt_common;
      case QI_LOAD_LIST_TAIL_HEAP:
	HeapPush(h,Tagp(LST,h + 1));
	goto load_list_opt_common;
      load_list_opt_common:
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	HeapPush(h,QLreg(q, Li));
	/* last argument is written in other QI instruction */
	break;
      case QI_LOAD_TUPLE_REG:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLreg(q, Li) = Tagp(STR,h);
	goto load_tuple_common;
      case QI_LOAD_TUPLE_HEAP:
	HeapPush(h,Tagp(STR,h + 1));
	goto load_tuple_common;
      load_tuple_common:
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	functor = QLreg(q, Li);
	HeapPush(h, functor);
	arity = Arity(functor);
	for (i = 1; i <= arity; i++) {
	  Li = buffered_in__get_reg(&q->in);
	  QLCHECK(q, Li);
	  HeapPush(h,QLreg(q, Li));
	}
	break;
      case QI_LOAD_TUPLE_TAIL_REG:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLreg(q, Li) = Tagp(STR,h);
	goto load_tuple_opt_common;
      case QI_LOAD_TUPLE_TAIL_HEAP:
	HeapPush(h,Tagp(STR,h + 1));
	goto load_tuple_opt_common;
      load_tuple_opt_common:
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	functor = QLreg(q, Li);
	HeapPush(h, functor);
	arity = Arity(functor);
	for (i = 1; i < arity; i++) {
	  Li = buffered_in__get_reg(&q->in);
	  QLCHECK(q, Li);
	  HeapPush(h,QLreg(q, Li));
	}
	/* last argument is written in other QI instruction */
	break;
      case QI_LOAD_DBNODE:
	Li = buffered_in__get_reg(&q->in);
	QLENSURE(q, Li);
	QLreg(q, Li) = PointerToTerm(CFUN__EVAL_N(qloader__load_dbnode, q));
	break;
      case QI_RETURN:
#if defined(DEBUG_QSIZE)
	{
	  extern intmach_t debug_inscount;
	  if (debug_inscount >= 19428237) {
	    TRACE_PRINTF("nowsize:%ld\n", h-oldh);
	    exit(-1);
	  }
	}
#endif
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	*readgoal = QLreg(q, Li);
	G->heap_top = h;	
	CBOOL__PROCEED;
      case QI_RELOC_TAGGED:
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	reloc_tagged(q->reloc_base, QLreg(q, Li), (relocoffset_t)buffered_in__getintmach(&q->in));
	break;
      case QI_RELOC_EMUL_ENTRY:
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	reloc_ptr(q->reloc_base, (char *)LOOKUP_DEF(QLreg(q, Li)), (relocoffset_t)buffered_in__getintmach(&q->in));
	break;
      case QI_RELOC_BUILTIN_ENTRY:
	Li = buffered_in__get_reg(&q->in);
	QLCHECK(q, Li);
	{
	  tagged_t t;
	  char *proc;
	  t = QLreg(q, Li);
	  proc = hashtab_get(switch_on_builtin,t)->value.proc;
	  if (proc == NULL) {
	    /* TODO: throw an exception? */
	    fprintf(stderr, "{panic: builtin %s/%ld not registered!}\n", GetString(t), (long)Arity(t));
	    goto error;
	  }
	  reloc_ptr(q->reloc_base, proc, (relocoffset_t)buffered_in__getintmach(&q->in));
	}
	break;
      default:
	/* TODO: use a better error handling */
	/* TODO: throw an exception? */
	fprintf(stderr, "{panic: unrecognized ql command %ld (%c)}\n", (long)c, (int)c);
	goto error;
      }
    c=GETC(q);
  }
 error: /* or end of file */
  G->heap_top = h;
  CBOOL__FAIL;
}

CBOOL__PROTO(prolog_qread) {
  tagged_t t;
  CBOOL__CALL_N(qread1, &t);
  CBOOL__LASTUNIFY(t,X(0));
}

/* QSAVER ---------------------------------------------------------- */

/* Relocation tables */

DYNSTACK_TEMPLATE(qreg_t)

typedef struct reloc_entry reloc_entry_t; 
struct reloc_entry {
  tagged_t key;
  relocoffset_t val;
};

DYNSTACK_TEMPLATE(reloc_entry_t)

typedef struct reloc_table reloc_table_t;
struct reloc_table {
  hashtab_t *sw;
  DYNSTACK_TYPE(reloc_entry_t) st;
};

void reloc_table__init(reloc_table_t *r, intmach_t size) {
  r->sw = HASHTAB_NEW(size);
  DYNSTACK_INIT(reloc_entry_t, &r->st, size);
}

void reloc_table__free(reloc_table_t *r) {
  hashtab_free(r->sw);
  DYNSTACK_FREE(reloc_entry_t, &r->st);
}

void reloc_table__clear(reloc_table_t *r) {
  hashtab_clear(r->sw);
  DYNSTACK_SET_TOP(reloc_entry_t, &r->st, 0);
}

relocoffset_t reloc_table__set(reloc_table_t *r, tagged_t key, relocoffset_t newval) {
  HASHTAB_LOOKUP(r->sw, key, node, {
    /* found */
    reloc_entry_t *e;
    intmach_t oldval;
    intmach_t i;
    i = node->value.raw;
    e = DYNSTACK_ELEMENT(reloc_entry_t, &r->st, i);
    oldval = e->val;
    e->val = newval;
    return oldval;
  }, {
    reloc_entry_t *e;
    node->value.raw = DYNSTACK_GET_TOP(reloc_entry_t, &r->st);
    DYNSTACK_PUSH(reloc_entry_t, &r->st, e);
    e->key = key;
    e->val = newval;
    return 0;
  });
} 

/* saver code */

typedef enum {
  SYMTBL__FUNCTOR = 0,
  SYMTBL__EMUL_ENTRY, 
  SYMTBL__BUILTIN_ENTRY, 
  SYMTBL__TAGGED,
  SYMTBL__COUNT
} symtbl_t;

typedef struct qsaver qsaver_t;
struct qsaver {
  qreg_t global_reg;
#define ALLOC_Q(Q, X) { (X) = (Q)->global_reg; (Q)->global_reg++; }
#define RESET_Q(Q) { (Q)->global_reg = 1; }

  reloc_table_t reloc_table[SYMTBL__COUNT];
  hashtab_t *var_table;

  DYNSTACK_TYPE(qreg_t) regstack;
  hashtab_t *functor_table;
  hashtab_t *tagged_table;

  qreg_t reloc_reg;
#define ALLOC_R(Q, X) { (X) = (Q)->reloc_reg; (Q)->reloc_reg--; }
#define RESET_R(Q) { (Q)->reloc_reg = -1; }

  relocoffset_t offset;

  absmachdef_t *absmach;
  
  buffered_out_t out;
};

qsaver_t *qlcurrs = NULL;

CFUN__PROTO_N(qsaver__emit_functor, qreg_t, qsaver_t *q, tagged_t functor);
CFUN__PROTO_N(qsaver__emit_term, qreg_t, qsaver_t *q, tagged_t term);
const intmach_t symtbl__qi[] = {QI_RELOC_TAGGED, QI_RELOC_EMUL_ENTRY, QI_RELOC_BUILTIN_ENTRY, QI_RELOC_TAGGED};
typedef CFUN__PROTO_N((*qsaver__emit_function), intmach_t, qsaver_t *, tagged_t);
const qsaver__emit_function symtbl__emit[SYMTBL__COUNT] = {qsaver__emit_functor, qsaver__emit_functor, qsaver__emit_functor, qsaver__emit_term};

#define PUTC(Q,X) buffered_out__putc(&((Q)->out), (X))

CVOID__PROTO_N(qsaver__put_number, qsaver_t *q, tagged_t term) {
  CVOID__CALL_N(number_to_string, term, 10);
  buffered_out__put_string(&q->out, Atom_Buffer);
}

/* Global emit */

void global_table_free(qsaver_t *q) {
  hashtab_free(q->functor_table);
  hashtab_free(q->tagged_table);
}

void global_table_new(qsaver_t *q) {
  q->functor_table = HASHTAB_NEW(64);
  q->tagged_table = HASHTAB_NEW(64);
  RESET_Q(q);
}

/* some emit functions */

CFUN__PROTO_N(qsaver__emit_tagged, qreg_t, qsaver_t *q, tagged_t term) {
  qreg_t output_reg;
  HASHTAB_LOOKUP(q->tagged_table, term, node, {
    CFUN__PROCEED(node->value.raw);
  }, {
    ALLOC_Q(q, output_reg);
    node->value.raw = output_reg;
    if (TaggedIsATM(term)) {
      if (term == atom_nil) {
	/* TODO: distinguish this case? disable it and see the difference in size */
	CHECK_FLUSH_OUTPUT(&q->out, C_EST + 1 * I_EST);
	PUTC(q, QI_LOAD_NIL);
	buffered_out__put_reg(&q->out, output_reg);
      } else {
	CHECK_FLUSH_OUTPUT(&q->out, C_EST + 2 * I_EST);
	PUTC(q, QI_LOAD_ATOM);
	buffered_out__put_reg(&q->out, output_reg);
	buffered_out__put_string(&q->out, TaggedToAtom(term)->name);
      }
    } else { /* is num */
      /* TODO: do not cache small integers in the table? */
      /* todo[ts]: may be wrong if numbits changes in target machine! */
      intval_t x;
      x = GetSmall(term);
      CHECK_FLUSH_OUTPUT(&q->out, C_EST + 2 * I_EST);
      PUTC(q, QI_LOAD_SMALL);
      buffered_out__put_reg(&q->out, output_reg);
      buffered_out__putintval(&q->out, x);
    }
    CFUN__PROCEED(output_reg);
  });
}

CFUN__PROTO_N(qsaver__emit_functor, qreg_t, qsaver_t *q, tagged_t functor) {
  qreg_t output_reg;
  qreg_t name_reg;
  HASHTAB_LOOKUP(q->functor_table, functor, node, {
    CFUN__PROCEED(node->value.raw);
  }, {
    ALLOC_Q(q, output_reg);
    node->value.raw = output_reg;
    name_reg = CFUN__EVAL_N(qsaver__emit_tagged, q, FUNCTOR_NAME(functor));
    CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST);
    PUTC(q, QI_LOAD_FUNCTOR);
    buffered_out__put_reg(&q->out, output_reg);
    buffered_out__put_reg(&q->out, name_reg);
    buffered_out__putintmach(&q->out, Arity(functor));
    CFUN__PROCEED(output_reg);
  });
}

CFUN__PROTO_N(qsaver__emit_var, qreg_t, qsaver_t *q, tagged_t term) {
  qreg_t output_reg;
  HASHTAB_LOOKUP(q->var_table, term, node, {
    CFUN__PROCEED(node->value.raw);
  }, {
    ALLOC_R(q, output_reg);
    node->value.raw = output_reg;
    CHECK_FLUSH_OUTPUT(&q->out, C_EST + I_EST);
    PUTC(q, QI_LOAD_VARIABLE);
    buffered_out__put_reg(&q->out, output_reg); 
    CFUN__PROCEED(output_reg);
  });
}

/* WARNING: do not pass complex expressions as macro arguments if you
   don't want to get unexpected results! (use always variables) */

#define EMIT_PUSH(F, Q, X) { \
  qreg_t reg; \
  qreg_t *val; \
  reg = CFUN__EVAL_N((F), (Q), (X)); \
  DYNSTACK_PUSH(qreg_t, &(Q)->regstack, val); \
  *val = reg; \
}

CFUN__PROTO_N(qsaver__emit_term, qreg_t, qsaver_t *q, tagged_t term) {
  qreg_t output_reg;

  SwOnAnyTag(term, functor, { /* VAR */
    output_reg = CFUN__EVAL_N(qsaver__emit_var, q, term);
    CFUN__PROCEED(output_reg);
  }, { /* NUM */
    goto cons;
  }, { /* ATM */
  cons:
    output_reg = CFUN__EVAL_N(qsaver__emit_tagged, q, term);
    CFUN__PROCEED(output_reg);
  }, { /* LST */
    goto emit_complex;
  }, { /* STR(blob(float)) */
    ALLOC_R(q, output_reg);
    CHECK_FLUSH_OUTPUT(&q->out, C_EST + I_EST);
    PUTC(q, QI_LOAD_NUMBER_F);
    buffered_out__put_reg(&q->out, output_reg);
    CVOID__CALL_N(qsaver__put_number, q, term);
    CFUN__PROCEED(output_reg);
  }, { /* STR(blob(bignum)) */
    ALLOC_R(q, output_reg);
    CHECK_FLUSH_OUTPUT(&q->out, C_EST + I_EST);
    PUTC(q, QI_LOAD_NUMBER_L);
    buffered_out__put_reg(&q->out, output_reg);
    CVOID__CALL_N(qsaver__put_number, q, term);
    CFUN__PROCEED(output_reg);
  }, { /* STR(struct) */ 
    goto emit_complex;
  });

  /* Emit a list or structure */
 emit_complex:
  {
    intmach_t depth, last;
    intmach_t top;
    intmach_t i, j;
    intmach_t arity;
    tagged_t x, t1;

    /* Get top of stack */
    top = DYNSTACK_GET_TOP(qreg_t, &q->regstack);

    /* First pass: emit arguments and save pointers to them */
    /* note: the last argument is emitted recursively while it is a
       list or a structure, storing the depth */
    x = term;
    depth = 0;

  again:
    SwOnAnyTagB(x, functor, { /* HVA */
      goto single;
    }, { /* SVA */
      goto single;
    }, { /* CVA */
      goto single;
    }, { /* NUM */
      goto single;
    }, { /* ATM */
      goto single;
    }, { /* LST */
      DerefCar(t1, x);
      EMIT_PUSH(qsaver__emit_term, q, t1);
      /* continue with last argument */ 
      DerefCdr(x, x);
      depth++;
      goto again;
    }, { /* STR(blob) */
      goto single;
    }, { /* STR(struct) */
      EMIT_PUSH(qsaver__emit_functor, q, functor);
      arity = Arity(functor);
      for (j = 1; j < arity; j++) {
	DerefArg(t1, x, j);
	EMIT_PUSH(qsaver__emit_term, q, t1);
      }
      /* continue with last argument */ 
      DerefArg(x, x, arity);
      depth++;
      goto again;
    });

  single:
    EMIT_PUSH(qsaver__emit_term, q, x);
    goto second_pass;

    /* Second pass: use the argument pointers to build the main term */
  second_pass:

    /* obtain the depth of the processed last structure or list */
    last = depth - 1;
    depth = 0;
    i = top;
    x = term;
    if (last == depth) {
      /* x: STR LST, never blob */
      if (TaggedIsSTR(x)) {
	/* put header */
	ALLOC_R(q, output_reg);
	CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST);
	PUTC(q, QI_LOAD_TUPLE_REG);
	buffered_out__put_reg(&q->out, output_reg);
	goto str_args;
      } else {
	/* put header */
	ALLOC_R(q, output_reg);
	CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST);
	PUTC(q, QI_LOAD_LIST_REG);
	buffered_out__put_reg(&q->out, output_reg);
	goto lst_args;
      }
    } else {
      /* x: STR LST, never blob */
      if (TaggedIsSTR(x)) {
	/* put header */
	ALLOC_R(q, output_reg);
	CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST);
	PUTC(q, QI_LOAD_TUPLE_TAIL_REG);
	buffered_out__put_reg(&q->out, output_reg);
	goto str_args_tail;
      } else {
	/* put header */
	ALLOC_R(q, output_reg);
	CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST);
	PUTC(q, QI_LOAD_LIST_TAIL_REG);
	buffered_out__put_reg(&q->out, output_reg);
	goto lst_args_tail;
      }

      /* all but the last argument, continue with last one */ 
    str_args_tail:
      /* x: STR */
      buffered_out__put_reg(&q->out, *DYNSTACK_ELEMENT(qreg_t, &q->regstack, i)); /* the functor */
      i++;
      arity = Arity(TaggedToHeadfunctor(x));
      for (j = 1; j < arity; j++) {
	CHECK_FLUSH_OUTPUT(&q->out, I_EST);
	buffered_out__put_reg(&q->out, *DYNSTACK_ELEMENT(qreg_t, &q->regstack, i));
	i++;
      }
      DerefArg(x, x, arity);
      goto tail_term;
    lst_args_tail:
      /* x: LST */
      CHECK_FLUSH_OUTPUT(&q->out, I_EST);
      buffered_out__put_reg(&q->out, *DYNSTACK_ELEMENT(qreg_t, &q->regstack, i));
      i++;
      DerefCdr(x, x);
      goto tail_term;

    tail_term:
      depth++;
      if (depth < last) {
	/* Continue with tail argument */
	/* x: STR LST, never blob */
	if (TaggedIsSTR(x)) {
	  /* put header */
	  CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST); /* TODO: check sizes */
	  PUTC(q, QI_LOAD_TUPLE_TAIL_HEAP);
	  goto str_args_tail;
	} else {
	  /* put header */
	  CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST); /* TODO: check sizes */
	  PUTC(q, QI_LOAD_LIST_TAIL_HEAP);
	  goto lst_args_tail;
	}
      } else { /* depth == last */
	/* x: STR LST, never blob */
	if (TaggedIsSTR(x)) {
	  /* put header */
	  CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST); /* TODO: check sizes */
	  PUTC(q, QI_LOAD_TUPLE_HEAP);
	  goto str_args;
	} else {
	  /* put header */
	  CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST); /* TODO: check sizes */
	  PUTC(q, QI_LOAD_LIST_HEAP);
	  goto lst_args;
	}
      }
    }

    /* put all the arguments */ 
  str_args:
    /* x: STR */
    buffered_out__put_reg(&q->out, *DYNSTACK_ELEMENT(qreg_t, &q->regstack, i)); /* the functor */
    i++;
    arity = Arity(TaggedToHeadfunctor(x));
    for (j = 1; j <= arity; j++) {
      CHECK_FLUSH_OUTPUT(&q->out, I_EST);
      buffered_out__put_reg(&q->out, *DYNSTACK_ELEMENT(qreg_t, &q->regstack, i));
      i++;
    }
    goto end;
  lst_args:
    /* x: LST */
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    buffered_out__put_reg(&q->out, *DYNSTACK_ELEMENT(qreg_t, &q->regstack, i));
    i++;
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    buffered_out__put_reg(&q->out, *DYNSTACK_ELEMENT(qreg_t, &q->regstack, i));
    i++;
    goto end;

  end:
    /* Unwind stack */
    DYNSTACK_SET_TOP(qreg_t, &q->regstack, top);
    /* Return pointer to main term */
    CFUN__PROCEED(output_reg);
  }
}

CVOID__PROTO_N(qsaver__emit_reloc_tables, qsaver_t *q) {
  symtbl_t j;
  intmach_t i;
  intmach_t reg;

  for (j = 0; j < SYMTBL__COUNT; j++) {
    for (i = 0; i < DYNSTACK_GET_TOP(reloc_entry_t, &(&q->reloc_table[j])->st); i++) {
      reg = CFUN__EVAL_N((*symtbl__emit[j]), q,
			DYNSTACK_ELEMENT(reloc_entry_t, &(&q->reloc_table[j])->st, i)->key);
      CHECK_FLUSH_OUTPUT(&q->out, C_EST + 2 * I_EST);
      PUTC(q, symtbl__qi[j]);
      buffered_out__put_reg(&q->out, reg);
      buffered_out__putintmach(&q->out, DYNSTACK_ELEMENT(reloc_entry_t, &(&q->reloc_table[j])->st, i)->val);
    }
  }
}

CVOID__PROTO_N(qsaver__emit_unboxedsmall, qsaver_t *q, ftype_typeid_t ftype_id, intval_t token) {
  intmach_t smethod;
  smethod = absmach__ftype__smethod(q->absmach, ftype_id);

  switch(smethod) {
  case QS_SMALL:
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    /* todo[ts]: may be wrong if size numbits changes in target machine */
    buffered_out__putintval(&q->out, token);
    q->offset += absmach__ftype__size(q->absmach, ftype_id);
    break;
  default:
    /* TODO: throw an exception? */
    fprintf(stderr, "{panic: unboxedsmall cannot be serialized using ftype %ld}\n", (long)ftype_id);
    exit(-1);
    break;
  }
}

CVOID__PROTO_N(qsaver__emit_basic, qsaver_t *q, ftype_typeid_t ftype_id, tagged_t token) {
  intmach_t smethod;
  relocoffset_t old_offset;
  tagged_t t1;
  tagged_t t2;
  smethod = absmach__ftype__smethod(q->absmach, ftype_id);

  switch(smethod) {
  case QS_SMALL:
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    /* todo[ts]: may be wrong if numbits changes in target machine */
    if (!TaggedIsSmall(token)) {
      fprintf(stderr, "bug: writing a small number that exceeds current absmach capacity\n");
      exit(-1);
    }
    buffered_out__putintval(&q->out, GetSmall(token));
    q->offset += absmach__ftype__size(q->absmach, ftype_id);
    break;
  case QS_INTEGER:
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    buffered_out__putintmach(&q->out, TaggedToIntmach(token));
    q->offset += absmach__ftype__size(q->absmach, ftype_id);
    break;
  case QS_POFFSET:
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    buffered_out__putintmach(&q->out, TaggedToIntmach(token));
    q->offset += absmach__ftype__size(q->absmach, ftype_id);
    break;
  case QS_FUNCTOR:
    {
      tagged_t spec;
      spec = token;
      if (!TaggedIsSTR(spec) || (TaggedToHeadfunctor(spec) != functor_slash)) goto error;
      DerefArg(t1,spec,1);
      DerefArg(t2,spec,2);
    }
    old_offset = reloc_table__set(&q->reloc_table[SYMTBL__FUNCTOR], SetArity(t1, GetSmall(t2)), q->offset);
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    buffered_out__putintmach(&q->out, old_offset);
    q->offset += absmach__ftype__size(q->absmach, ftype_id);
    break;
  case QS_TAGGED:
    old_offset = reloc_table__set(&q->reloc_table[SYMTBL__TAGGED], token, q->offset);
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    buffered_out__putintmach(&q->out, old_offset);
    q->offset += absmach__ftype__size(q->absmach, ftype_id);
    break;
  case QS_EMUL_ENTRY:
    {
      tagged_t spec;
      spec = token;
      if (!TaggedIsSTR(spec) || (TaggedToHeadfunctor(spec) != functor_slash)) goto error;
      DerefArg(t1,spec,1);
      DerefArg(t2,spec,2);
    }
    if (!TaggedIsATM(t1)) goto error;
    old_offset = reloc_table__set(&q->reloc_table[SYMTBL__EMUL_ENTRY], SetArity(t1, GetSmall(t2)), q->offset);
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    buffered_out__putintmach(&q->out, old_offset);
    q->offset += absmach__ftype__size(q->absmach, ftype_id);
    break;
  case QS_BUILTIN_ENTRY:
    {
      tagged_t spec;
      spec = token;
      if (!TaggedIsSTR(spec) || (TaggedToHeadfunctor(spec) != functor_slash)) goto error;
      DerefArg(t1,spec,1);
      DerefArg(t2,spec,2);
    }
    if (!TaggedIsATM(t1)) goto error;
    old_offset = reloc_table__set(&q->reloc_table[SYMTBL__BUILTIN_ENTRY], SetArity(t1, GetSmall(t2)), q->offset);
    CHECK_FLUSH_OUTPUT(&q->out, I_EST);
    buffered_out__putintmach(&q->out, old_offset);
    q->offset += absmach__ftype__size(q->absmach, ftype_id);
    break;
  default:
    /* TODO: throw an exception? */
    fprintf(stderr, "{panic: bad serializer for ftype_id=%ld}\n", (long)ftype_id);
    exit(-1);
    break;
  }
  return;
 error:
  /* TODO: throw an exception? */
  fprintf(stderr, "{panic: bad bytecode description}\n");
  exit(-1);
}

CVOID__PROTO_N(qsaver__emit_blob, qsaver_t *q, ftype_typeid_t ftype_id, tagged_t token) {
  tagged_t t1;
  tagged_t t2;

  DerefArg(t1, token, 1);
  DerefArg(t2, token, 2);
  CVOID__CALL_N(qsaver__put_number, q, t1);
  q->offset += GetSmall(t2);
  return;
}

CVOID__PROTO_N(qsaver__emit_bytecode, qsaver_t *q, tagged_t tokens) {
  tagged_t token;
  ftype_typeid_t ftype_id;
  ftype_typeid_t fmt_root[] = {q->absmach->ftype_id_i};

  FMT_LOOP(q->absmach, fmt_root, ftype_id, i, op, {
    /* GET_OP */
    if (tokens == atom_nil) break;
    DerefCar(token, tokens);
    DerefCdr(tokens, tokens);
    if (!TaggedIsSmall(token)) goto corrupted;
    op = GetSmall(token);
  }, {
    /* EMIT_OP */
    CVOID__CALL_N(qsaver__emit_unboxedsmall, q, ftype_id, op);
  }, {
    /* GET_ARRAY_I */
    DerefCar(token, tokens);
    DerefCdr(tokens, tokens);
    if (!TaggedIsSmall(token)) goto corrupted;
    i = GetSmall(token);
  }, {
    /* EMIT_ARRAY_I */
    CVOID__CALL_N(qsaver__emit_unboxedsmall, q, ftype_id, i);
  }, {
    /* EMIT_BLOB */
    DerefCar(token, tokens);
    DerefCdr(tokens, tokens);
    CVOID__CALL_N(qsaver__emit_blob, q, ftype_id, token);
  }, {
    /* EMIT_BASIC */
    DerefCar(token, tokens);
    DerefCdr(tokens, tokens);
    CVOID__CALL_N(qsaver__emit_basic, q, ftype_id, token);
  });

  CHECK_FLUSH_OUTPUT(&q->out, I_EST);
  buffered_out__putintmach(&q->out, FINISH_OP);
  return;
 corrupted:
  /* TODO: throw an exception? */
  fprintf(stderr, "{panic: token stream does not match instruction format}\n");
  exit(-1);
}

/* TODO: THIS is an upper bound! how many HVA are shared? */
CFUN__PROTO_N(qsaver__term_size, intmach_t, qsaver_t *q, tagged_t term) {
  intmach_t size;
  size = 0;
 again:
  SwOnAnyTagB(term, functor, { /* HVA */
    /* TODO: this is because we create an extra heap cell using
       LoadHVA ... can I optimize it? */
    size += q->absmach->tagged_size;
    goto finish;
  }, { /* SVA */
    /* TODO: this is because we create an extra heap cell using
       LoadHVA ... can I optimize it? */
    size += q->absmach->tagged_size;
    goto finish;
  }, { /* CVA */
    /* TODO: this is wrong... don't we look inside? */
    fprintf(stderr, "bug: CVA not supported in qsaver__term_size\n");
    goto finish;
  }, { /* NUM */
    goto finish;
  }, { /* ATM */
    goto finish;
  }, { /* LST */
    tagged_t car;
    tagged_t cdr;
    DerefCar(car, term);
    DerefCdr(cdr, term);
    size += 2 * q->absmach->tagged_size + CFUN__EVAL_N(qsaver__term_size, q, car);
    term = cdr;
    goto again;
  }, { /* STR(blob) */
    /* todo[ts]: align w.r.t. next absmach blob_unit_t size (not
       tagged_size) */
    /* todo[ts]: add functor_size instead of tagged_size? (right now
       it cannot differ) */
    size +=
      ABSMACH__ALIGN_SIZE(q->absmach, BlobFunctorSizeAligned(functor)) +
      2 * q->absmach->tagged_size;
    goto finish;
  }, { /* STR(struct) */
    tagged_t arg;
    intmach_t i;
    intmach_t arity;
    arity = Arity(functor);
    size += q->absmach->tagged_size;
    for (i = 1; i < arity; i++) {
      DerefArg(arg, term, i);
      size += q->absmach->tagged_size + CFUN__EVAL_N(qsaver__term_size, q, arg);
    }
    DerefArg(arg, term, arity);
    size += q->absmach->tagged_size;
    term = arg;
    goto again;
  });
 finish:
  CFUN__PROCEED(size);
}

void local_table_free(qsaver_t *q) {
  hashtab_free(q->var_table);

  reloc_table__free(&q->reloc_table[SYMTBL__FUNCTOR]);
  reloc_table__free(&q->reloc_table[SYMTBL__EMUL_ENTRY]);
  reloc_table__free(&q->reloc_table[SYMTBL__BUILTIN_ENTRY]);
  reloc_table__free(&q->reloc_table[SYMTBL__TAGGED]);

  DYNSTACK_FREE(qreg_t, &q->regstack);
}

void local_table_new(qsaver_t *q) {
  q->var_table = HASHTAB_NEW(16);

  reloc_table__init(&q->reloc_table[SYMTBL__FUNCTOR], 16);
  reloc_table__init(&q->reloc_table[SYMTBL__EMUL_ENTRY], 16);
  reloc_table__init(&q->reloc_table[SYMTBL__BUILTIN_ENTRY], 16);
  reloc_table__init(&q->reloc_table[SYMTBL__TAGGED], 16);

  DYNSTACK_INIT(qreg_t, &q->regstack, 1024);

  RESET_R(q);
}

void local_table_clear(qsaver_t *q) {
  hashtab_clear(q->var_table);

  reloc_table__clear(&q->reloc_table[SYMTBL__FUNCTOR]);
  reloc_table__clear(&q->reloc_table[SYMTBL__EMUL_ENTRY]);
  reloc_table__clear(&q->reloc_table[SYMTBL__BUILTIN_ENTRY]);
  reloc_table__clear(&q->reloc_table[SYMTBL__TAGGED]);

  RESET_R(q);
}

/* write code */

CBOOL__PROTO(qwrite_begin) {
  ERR__FUNCTOR("ql_inout:$qwrite_begin", 2);
  intmach_t i;
  stream_node_t *s;
  FILE *out;
  qsaver_t *q;

  DEREF(X(0), X(0));
  CBOOL__TEST(TaggedIsSmall(X(0)));
  
  DEREF(X(1), X(1));
  s = stream_to_ptr_check(X(1), 'w', &i);
  if (!s) BUILTIN_ERROR(i,X(1),2);
  out = s->streamfile;

  q = qlcurrs = CHECKALLOC(qsaver_t);
  if (X(0) == MakeSmall(1)) {
    q->absmach = absnext;
  } else {
    q->absmach = &abscurr;
  }
  q->out.output_file = out;

  global_table_new(q);
  local_table_new(q);
  BUFFERED_OUT__INIT(&(q->out));

  CBOOL__PROCEED;
}

CBOOL__PROTO(qwrite_end) {
  qsaver_t *q = qlcurrs;
  flush_buffered_output(&q->out);
  global_table_free(q);
  local_table_free(q);
  CHECKDEALLOC0(qsaver_t, q);
  CBOOL__PROCEED;
}

#define QWRITE_FINISH(Q) \
  CHECK_FLUSH_OUTPUT(&((Q)->out), C_EST + I_EST); \
  PUTC((Q), QI_RETURN); \
  buffered_out__put_reg(&((Q)->out), term_reg); \
  local_table_clear((Q));

CBOOL__PROTO(qwrite) {
  intmach_t size;
  intmach_t term_reg;
  tagged_t term;
  qsaver_t *q = qlcurrs;

  DEREF(term, X(0));

  size = q->absmach->q_pad1 + CFUN__EVAL_N(qsaver__term_size, q, term);
  /* ensure space */
  if (size > q->absmach->q_pad2) {
    CHECK_FLUSH_OUTPUT(&q->out, C_EST + I_EST);
    PUTC(q, QI_ENSURE_SPACE);
    buffered_out__putintmach(&q->out, size);
  }
  term_reg = CFUN__EVAL_N(qsaver__emit_term, q, term);

  QWRITE_FINISH(q);
  CBOOL__PROCEED;
}

CBOOL__PROTO(qwrite_b) { /* writes a bytecode clause (b/2) */
  intmach_t size;
  qreg_t term_reg;
  tagged_t term;
  qsaver_t *q = qlcurrs;

  DEREF(term, X(0));
  /* TODO: add more tests or preconditions? */
  // CBOOL__TEST(TaggedIsSTR(term));
  // functor_add_bytecode_clause = deffunctor("b", 2);
  // CBOOL__TEST(TaggedToHeadfunctor(term) == functor_add_bytecode_clause);
  CBOOL__TEST(Arity(TaggedToHeadfunctor(term)) == 2);

  {
    /* write a special term which contains bytecode */
    tagged_t bytecode_size;
    tagged_t tokens;
    tagged_t data;
    tagged_t bytecode_str;
    qreg_t bytecode_reg;
    qreg_t functor_reg;
    qreg_t data_reg;

    /* get parameters */
    DerefArg(bytecode_str, term, 1);
    /* pre: TaggedIsLST(bytecode_str) */ 
    DerefCar(bytecode_size, bytecode_str);
    DerefCdr(tokens, bytecode_str);
    DerefArg(data, term, 2);

    bytecode_reg = 0;
    CHECK_FLUSH_OUTPUT(&q->out, C_EST + 3 * I_EST);
    PUTC(q, QI_LOAD_DBNODE);
    buffered_out__put_reg(&q->out, bytecode_reg);
    buffered_out__putintmach(&q->out, TaggedToIntmach(bytecode_size));
    q->offset = 0;
    CVOID__CALL_N(qsaver__emit_bytecode, q, tokens);
    size = (q->absmach->q_pad1 + CFUN__EVAL_N(qsaver__term_size, q, data));
    /* ensure space */
    if (size > q->absmach->q_pad2) {
      CHECK_FLUSH_OUTPUT(&q->out, C_EST + I_EST);
      PUTC(q, QI_ENSURE_SPACE);
      buffered_out__putintmach(&q->out, size);
    }
    CVOID__CALL_N(qsaver__emit_reloc_tables, q);

    ALLOC_R(q, term_reg);
    functor_reg = CFUN__EVAL_N(qsaver__emit_functor, q, TaggedToHeadfunctor(term));
    data_reg = CFUN__EVAL_N(qsaver__emit_term, q, data); 
    CHECK_FLUSH_OUTPUT(&q->out, C_EST + 4 * I_EST);
    PUTC(q, QI_LOAD_TUPLE_REG);
    buffered_out__put_reg(&q->out, term_reg);
    buffered_out__put_reg(&q->out, functor_reg);
    buffered_out__put_reg(&q->out, bytecode_reg);
    buffered_out__put_reg(&q->out, data_reg); 
  }
  QWRITE_FINISH(q);
  CBOOL__PROCEED;
}
