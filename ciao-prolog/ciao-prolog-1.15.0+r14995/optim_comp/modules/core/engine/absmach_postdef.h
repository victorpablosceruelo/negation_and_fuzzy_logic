#if defined(Sparc) || defined(Sparc64m32)
#define FORCE_ALIGNED_ACCESS 1
#endif

#if defined(ABSMACH_OPT__threaded_bytecode_rel16)||defined(ABSMACH_OPT__threaded_bytecode)
extern char *emulabelarray[] __asm__("emulabelarray");
#if defined(ABSMACH_OPT__threaded_bytecode_rel16)
void wam__2(worker_t *w, char *A0);
#define opIdEnc(X) (emulabelarray[(X)] - (char *)wam__2)
#else
#define opIdEnc(X) ((FTYPE_ctype(f_o))emulabelarray[(X)])
#endif
#define OPCODEenc(X) opIdEnc(OPCODE(X))
#else
#define opIdEnc(X) (X)
#define OPCODEenc(X) OPCODE(X)
#endif

#define tagged__atm_offset tagged__qval_offset 
#define tagged__atm_size tagged__qval_size
/* TODO: tagged__atm_size is tagged__val_size-1 ? */

#define X2_CHN DYNX(X2_CHN)
#define ClockSlot DYNX(ClockSlot)
#define X5_CHN DYNX(X5_CHN)
#define RootArg DYNX(RootArg)
#define InvocationAttr DYNX(InvocationAttr)
#define PrevDynChpt DYNX(PrevDynChpt)
#define DynamicPreserved DYNX(DynamicPreserved)

#define FTYPEDEF_BASIC FTYPEDEF(basic)
#define FTYPEDEF_STR FTYPEDEF(str)
#define FTYPEDEF_ARRAY FTYPEDEF(array)
#define FTYPEDEF_BLOB FTYPEDEF(blob)

#define QS_INTEGER QS(integer)
#define QS_POFFSET QS(poffset)
#define QS_FUNCTOR QS(functor)
#define QS_TAGGED QS(tagged)
#define QS_EMUL_ENTRY QS(emul_entry)
#define QS_BUILTIN_ENTRY QS(builtin_entry)
#define QS_SMALL QS(small)

#define QL_UINT16 QL(uint16)
#define QL_UINT32 QL(uint32)
#define QL_UINT64 QL(uint64)
#define QL_BASEPTR QL(baseptr)

#define STRICT_ATOM TYPE_ERRORS(atom)
#define ATOMIC TYPE_ERRORS(atomic)
#define BYTE TYPE_ERRORS(byte)
#define COMPOUND TYPE_ERRORS(compound)
#define EVALUABLE TYPE_ERRORS(evaluable)
#define IN_BYTE TYPE_ERRORS(in_byte)
#define INTEGER TYPE_ERRORS(integer)
#define LIST TYPE_ERRORS(list)
#define NUMBER TYPE_ERRORS(number)
#define PREDICATE_INDICATOR TYPE_ERRORS(predicate_indicator)
#define VARIABLE TYPE_ERRORS(variable)
#define CALLABLE TYPE_ERRORS(callable)

#define IO_MODE DOMAIN_ERRORS(io_mode)
#define NOT_EMPTY_LIST DOMAIN_ERRORS(not_empty_list)
#define NOT_LESS_THAN_ZERO DOMAIN_ERRORS(not_less_than_zero)
#define OPERATOR_PRIORITY DOMAIN_ERRORS(operator_priority)
#define PROLOG_FLAG DOMAIN_ERRORS(prolog_flag)
#define READ_OPTION DOMAIN_ERRORS(read_option)
#define FLAG_VALUE DOMAIN_ERRORS(flag_value)
#define CLOSE_OPTION DOMAIN_ERRORS(close_option)
#define STREAM_OPTION DOMAIN_ERRORS(stream_option)
#define STREAM_OR_ALIAS DOMAIN_ERRORS(stream_or_alias)
#define STREAM_POSITION DOMAIN_ERRORS(stream_position)
#define STREAM_PROPERTY DOMAIN_ERRORS(stream_property)
#define WRITE_OPTION DOMAIN_ERRORS(write_option)
#define OPERATOR_SPECIFIER DOMAIN_ERRORS(operator_specifier)

#define PROCEDURE EXISTENCE_ERRORS(procedure)
#define STREAM EXISTENCE_ERRORS(stream)

#define ACCESS PERMISSION_TYPES(access)
#define CREATE PERMISSION_TYPES(create)
#define INPUT PERMISSION_TYPES(input)
#define MODIFY PERMISSION_TYPES(modify)
#define OPEN PERMISSION_TYPES(open)
#define OUTPUT PERMISSION_TYPES(output)
#define REPOSITION PERMISSION_TYPES(reposition)

#define BINARY_STREAM PERMISSION_OBJECTS(binary_stream)
#define TEXT_STREAM PERMISSION_OBJECTS(text_stream)
#define FLAG PERMISSION_OBJECTS(flag)
#define OPERATOR PERMISSION_OBJECTS(operator)
#define PAST_END_OF_STREAM PERMISSION_OBJECTS(past_end_of_stream)
#define PRIVATE_PROCEDURE PERMISSION_OBJECTS(private_procedure)
#define STATIC_PROCEDURE PERMISSION_OBJECTS(static_procedure)

#define CHARACTER_CODE_LIST DOMAIN_ERRORS(character_code_list)
#define IN_CHARACTER_CODE REPRESENTATION_ERRORS(in_character_code)
#define MAX_ARITY REPRESENTATION_ERRORS(max_arity)
#define MAX_INTEGER REPRESENTATION_ERRORS(max_integer)
#define MIN_INTEGER REPRESENTATION_ERRORS(min_integer)
#define CHARACTER_CODE REPRESENTATION_ERRORS(character_code)

#define FLOAT_OVERFLOW EVALUATION_ERRORS(float_overflow)
#define INT_OVERFLOW EVALUATION_ERRORS(int_overflow)
#define E_UNDEFINED EVALUATION_ERRORS(e_undefined)
#define E_UNDERFLOW EVALUATION_ERRORS(e_underflow)
#define ZERO_DIVISOR EVALUATION_ERRORS(zero_divisor)

#define ENTER_COMPACTCODE PREDTYPE(compactcode)
#define ENTER_COMPACTCODE_INDEXED PREDTYPE(compactcode_indexed)
#define ENTER_PROFILEDCODE PREDTYPE(profiledcode)
#define ENTER_PROFILEDCODE_INDEXED PREDTYPE(profiledcode_indexed)
#define ENTER_UNDEFINED PREDTYPE(undefined)
#define ENTER_CBOOL PREDTYPE(cbool)
#define ENTER_CINSNP PREDTYPE(cinsnp)
#define ENTER_CVOID PREDTYPE(cvoid)
#define ENTER_INTERPRETED PREDTYPE(interpreted)

#define NO_ACTION GDACTION(no_action)
#define SHARES_STRUCTURE GDACTION(shares_structure)
#define HAS_CONTINUATION GDACTION(has_continuation)
#define KEEP_STACKS GDACTION(keep_stacks)
#define BACKTRACKING GDACTION(backtracking)
#define CREATE_THREAD GDACTION(create_thread)
#define CREATE_WAM GDACTION(create_wam)
#define NEEDS_FREEING GDACTION(needs_freeing)

/* ------------------------------------------------------------------------- */
/* Runtime type info for unboxed data */
/* TODO: use a single absmach_postdef.h file? */
/* TODO: move to absmach definition */

#define FINISH_OP 32767

/* max depth of format stack */
#define FMTSTK 4
/* types of formats: one for arrays, other for structures */
#define FMT_STR 1
#define FMT_ARRAY 2

#define FMT_PUSH_STR(ARGS, ARITY) ({ \
  fmt_s++; \
  fmt[fmt_s].type = FMT_STR; \
  fmt[fmt_s].value = (intmach_t)(ARGS); \
  fmt[fmt_s].n = (ARITY); \
  fmt[fmt_s].i = 0; \
})

#define FMT_PUSH_ARRAY(ELEM, LEN) ({ \
  fmt_s++; \
  fmt[fmt_s].type = FMT_ARRAY; \
  fmt[fmt_s].value = (intmach_t)(ELEM); \
  fmt[fmt_s].n = (LEN); \
  fmt[fmt_s].i = 0; \
})

#define FMT_POP() ({ \
  fmt_s--; \
})

#define FMT_ADVANCE(ID) ({ \
  switch (fmt[fmt_s].type) { \
  case FMT_STR: \
    (ID) = ((ftype_typeid_t *)fmt[fmt_s].value)[fmt[fmt_s].i]; \
    break; \
  case FMT_ARRAY: \
    (ID) = ((ftype_typeid_t)fmt[fmt_s].value); \
    break; \
  default: \
    goto corrupted; \
  } \
  fmt[fmt_s].i++; \
})

/* TODO: do not ALWAYS use static FTYPE here */
/* TODO: store information for f_Y, f_Z and f_g in tables */
#define FMT_LOOP(ABSMACHDEF, FMT_ROOT, ID, ARRAY_I, OP, GET_OP, EMIT_OP, GET_ARRAY_I, EMIT_ARRAY_I, EMIT_BLOB, EMIT_BASIC) ({ \
  intmach_t fmt_s; /* stack level */ \
  fmt_t fmt[FMTSTK]; \
  fmt_s = -1; \
  FMT_PUSH_STR((FMT_ROOT), 1); \
  while (1) { \
  again: \
    if (fmt_s == -1) { \
      /* level 0, get next instruction */ \
      intval_t op; \
      GET_OP; \
      FMT_PUSH_STR(FTYPE_str__args((ABSMACHDEF)->ins_info, op), FTYPE_str__arity((ABSMACHDEF)->ins_info, op)); \
      (ID) = (ABSMACHDEF)->ftype_id_o; \
      EMIT_OP; \
      goto emit; \
    } else { \
      if (fmt[fmt_s].i >= fmt[fmt_s].n) { \
	/* pop one level */ \
	FMT_POP(); \
	goto again; \
      } else { \
	FMT_ADVANCE((ID)); \
      } \
      switch (FTYPE_type((ABSMACHDEF)->ftype_info, (ID))) { \
      case FTYPEDEF_BASIC: { \
          EMIT_BASIC; \
	  goto emit; \
        } \
      case FTYPEDEF_BLOB: { \
          EMIT_BLOB; \
	  goto emit; \
        } \
      case FTYPEDEF_STR: { \
	  /* push one level */ \
	  FMT_PUSH_STR(FTYPE_str__args((ABSMACHDEF)->ftype_info, (ID)), FTYPE_str__arity((ABSMACHDEF)->ftype_info, (ID))); \
	  goto again; \
        } \
      case FTYPEDEF_ARRAY: { \
	  intval_t ARRAY_I; \
	  GET_ARRAY_I; \
	  /* push one level */ \
	  FMT_PUSH_ARRAY(FTYPE_array__argtype((ABSMACHDEF)->ftype_info, (ID)), ARRAY_I); \
	  ftype_id = FTYPE_array__itype((ABSMACHDEF)->ftype_info, (ID)); \
	  EMIT_ARRAY_I; \
	  goto emit; \
        } \
      } \
    } \
  emit: \
    {} \
  } \
})

#define FTYPE_BASIC(SIZE, SMETHOD, LMETHOD) (ftype_base_t *)(&(ftype_basic_t){.type = FTYPEDEF_BASIC, .size = (SIZE), .smethod = (SMETHOD), .lmethod = (LMETHOD)})
#define FTYPE_STR(ARITY, ARGS) (ftype_base_t *)(&(ftype_str_t){.type = FTYPEDEF_STR, .arity = (ARITY), .args = (ftype_typeid_t *)(&((ftype_typeid_t[])ARGS))})
#define FTYPE_STR0() (ftype_base_t *)(&(ftype_str_t){.type = FTYPEDEF_STR, .arity = 0, .args = NULL})
#define FTYPE_ARRAY(ITYPE, ARGTYPE) (ftype_base_t *)(&(ftype_array_t){.type = FTYPEDEF_ARRAY, .itype = (ITYPE), .argtype = (ARGTYPE)})
#define FTYPE_BLOB() (ftype_base_t *)(&(ftype_blob_t){.type = FTYPEDEF_BLOB})

#define BRACES(...) {__VA_ARGS__}

#define FTYPE_type(F,ID) (((ftype_base_t *)(F)[(intmach_t)(ID)])->type)
#define FTYPE_str__args(F,ID) (((ftype_str_t *)(F)[(intmach_t)(ID)])->args)
#define FTYPE_str__arity(F,ID) (((ftype_str_t *)(F)[(intmach_t)(ID)])->arity)
#define FTYPE_array__itype(F,ID) (((ftype_array_t *)(F)[(intmach_t)(ID)])->itype)
#define FTYPE_array__argtype(F,ID) (((ftype_array_t *)(F)[(intmach_t)(ID)])->argtype)
#define FTYPE_basic__size(F,ID) (((ftype_basic_t *)(F)[(intmach_t)(ID)])->size)
#define FTYPE_basic__smethod(F,ID) (((ftype_basic_t *)(F)[(intmach_t)(ID)])->smethod)
#define FTYPE_basic__lmethod(F,ID) (((ftype_basic_t *)(F)[(intmach_t)(ID)])->lmethod)

/* ------------------------------------------------------------------------- */
/* runtime absmach definition */

/* Align size to absmach->size_align */
#define ABSMACH__ALIGN_SIZE(ABSMACH, SIZE) (((((SIZE)+(ABSMACH)->size_align-1))/(ABSMACH)->size_align)*(ABSMACH)->size_align)

#define absmach__ftype__size(ABS, A) FTYPE_basic__size((ABS)->ftype_info, (A))
#define absmach__ftype__smethod(ABS, A) FTYPE_basic__smethod((ABS)->ftype_info, (A))

/* ------------------------------------------------------------------------- */
/* Debug */

#if defined(USE_RTCHECKS)||defined(DEBUG_TRACE)
#define USE_DEBUG_INSCOUNT 1
#endif

#if defined(USE_DEBUG_INSCOUNT)
extern intmach_t debug_inscount;
bool_t dump_cond();
void init_debug_inscount();
#define INSCOUNT_NEXT() debug_inscount++
#endif

/* ------------------------------------------------------------------------- */
/* Debug trace */

#if defined(DEBUG_TRACE)

/* TODO: move tracing outside the debug code? add tracing levels? */
#define DEBUG__TRACE(COND, ...) ({ \
  if ((COND)) { TRACE_PRINTF(__VA_ARGS__); } \
})
/* debug_trace options */
extern bool_t debug_predtrace;
extern intmach_t debug_c;
extern bool_t debug_gc;
extern bool_t debug_threads;
extern bool_t debug_choicepoints;
extern bool_t debug_concchoicepoints;
extern bool_t debug_mem;
extern bool_t debug_conc;
extern bool_t debug_setarg;
extern bool_t debug_atomgc;

bool_t debug_trace__get_opt(const char *arg);

#else
#define DEBUG__TRACE(COND, ...)
#endif

/* ------------------------------------------------------------------------- */
/* Runtime checks (debugging) */

#if defined(USE_RTCHECKS)
CBOOL__PROTO_N(proofread, char *text, intmach_t arity, bool_t force);
void dump_tagged(tagged_t t);
CVOID__PROTO_N(dump_call, char *s, definition_t *func);

#define RTCHECK(X) X
#else
#define RTCHECK(X) {}
#endif 

/* TODO: give good definitions */
#define RTERROR() {}
#define RTERRORBOOL() FALSE

//#define USE_RTBLOCKCHECK 1
/* TODO: make block checking optional, improve and make it work for
   both linear and binary versions of alloc */
#if defined(USE_RTBLOCKCHECK)
void rtblockcheck(char *f, intmach_t l);
#define RTBLOCKCHECK rtblockcheck(__FUNCTION__, __LINE__)
#else
#define RTBLOCKCHECK
#endif

/* ------------------------------------------------------------------------- */
/* Offsets */

#define CharOffset(X,O)	((tagged_t *)((char *)(X) + (O)))

/* ------------------------------------------------------------------------- */
/* bignum */

#define BignumGetInt32(H) (*((int32_t *)(H)))
#if defined(ALIGN_BLOB_64)
#define BignumPushInt32(H, I) ({ \
  HeapPushT(int32_t, (H), (I)); \
  HeapPushT(int32_t, (H), 0); \
})
#else
#define BignumPushInt32(H, I) ({ \
  HeapPushT(int32_t, (H), (I)); \
})
#endif
#if BIGENDIAN || (FORCE_ALIGNED_ACCESS && !defined(ABSMACH_OPT__pointer64))
/* (In big endian architectures the bignum representation of int64_t
   has words swapped) */
/* (When FORCE_ALIGNED_ACCESS is set, int64_t must be written to the
   heap as two int32_t halves) */
#define ReadInt64(H) ((int64_t)((*(uint32_t *)(H)) | ((uint64_t)(*((uint32_t *)(H)+1))<<32)))
#define BignumGetInt64(H) ReadInt64((H))
#define BignumPushInt64(H, I) ({ \
  HeapPushT(int32_t, (H), ((uint64_t)(I)&0xffffffff)); \
  HeapPushT(int32_t, (H), ((uint64_t)(I)>>32)); \
})
#define Put32InUnion64(X) ((uint64_t)((uint32_t)(X))<<32)
#else
/* In little endian architectures the int64_t can be read/written directly */
#define BignumGetInt64(H) (*((int64_t *)(H)))
#define BignumPushInt64(H, I) ({ \
  HeapPushT(int64_t, (H), (I)); \
})
#define Put32InUnion64(X) ((uint64_t)((uint32_t)(X)))
#endif

#if FORCE_ALIGNED_ACCESS && !defined(ABSMACH_OPT__pointer64)
#define HeapPushFlt64(H, I) ({ \
  union { \
    uint32_t i[2]; \
    flt64_t f; \
  } flt_as_int; \
  flt_as_int.f = (I); \
  HeapPushT(uint32_t, (H), flt_as_int.i[0]); \
  HeapPushT(uint32_t, (H), flt_as_int.i[1]); \
})
#else
#define HeapPushFlt64(H, I) ({ \
  HeapPushT(flt64_t, (H), (I)); \
})
#endif

#if FORCE_ALIGNED_ACCESS && !defined(ABSMACH_OPT__pointer64)
#define UnboxFlt64(H,F) ({ \
  union { \
    uint32_t i[2]; \
    flt64_t f; \
  } flt_as_int; \
  uint32_t *ptr; \
  ptr = (uint32_t *)(H); \
  flt_as_int.i[0] = ptr[0]; \
  flt_as_int.i[1] = ptr[1]; \
  F = flt_as_int.f; \
})
#else
#define UnboxFlt64(H,F) ({ \
  F = (*((flt64_t *)(H))); \
})
#endif

#define TaggedToBignum(X) ((bignum_t *)TagpPtr(STR,(X)))

bool_t bn_positive(bignum_t *x);
intmach_t bn_add(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_incr(bignum_t *x, bignum_t *z, bignum_t *zmax);
intmach_t bn_plus(bignum_t *x, bignum_t *z, bignum_t *zmax);
intmach_t bn_subtract(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_decr(bignum_t *x, bignum_t *z, bignum_t *zmax);
intmach_t bn_minus(bignum_t *x, bignum_t *z, bignum_t *zmax);
intmach_t bn_and(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_or(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_xor(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_not(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_lshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax);
intmach_t bn_rshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax);
intmach_t bn_compare(bignum_t *x, bignum_t *y);
intmach_t bn_multiply(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_quotient_remainder_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_quotient_remainder_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
intmach_t bn_from_float(flt64_t f, bignum_t *z, bignum_t *zmax);
flt64_t bn_to_float(bignum_t *bn);
intmach_t bn_from_string(char *x, bignum_t *z, bignum_t *zmax, intmach_t base);
CVOID__PROTO_N(bn_to_string, bignum_t *x, intmach_t base);
intmach_t bn_length(bignum_t *x);

/* ------------------------------------------------------------------------- */
/* Exceptions */
/* TODO: worker? */

/* usage: goto, continue, break is forbidden inside CODE! */
#define EXCEPTION__CATCH(CODE, HANDLER) ({ \
  JMP_BUF catch_exception__handler; \
  JMP_BUF *catch_exception__old_handler; \
  catch_exception__old_handler = w->misc->errhandler; \
  w->misc->errhandler = &catch_exception__handler; \
  if (SETJMP(catch_exception__handler)) { \
    w->misc->errhandler = catch_exception__old_handler; \
    HANDLER; \
  } else { \
    CODE; \
  } \
})

#define EXCEPTION__THROW LONGJMP(*w->misc->errhandler, 1)

/* ------------------------------------------------------------------------- */
/* emulator */

/* Faults */

#include <stdlib.h>

#if defined(USE_DEBUG_INSCOUNT)
/* do not use stream code in panic messages -- jf */
#define TRACE_ABORT() ({ \
  extern intmach_t debug_inscount; \
  TRACE_PRINTF("[time = %ld] {aborted}\n", (long)debug_inscount); \
})
#else
#define TRACE_ABORT() ({})
#endif

#define PANIC_FAULT(Y) ({ \
  TRACE_ABORT(); \
  fprintf(stderr, "{panic: %s}\n", Y); \
  abort(); \
})
                          
#define SERIOUS_FAULT(Y) ({ \
  fprintf(stderr, "{error: %s}\n", Y); \
  LONGJMP(abort_env, WAM_ABORT); \
})

/* TODO: check that those 3 macros are used in cbool preds */
#define MAJOR_FAULT(Y) ({ \
  CVOID__CALL_N(failc, Y); \
  CBOOL__FAIL; \
})

#define USAGE_FAULT(Y) ({ \
  CVOID__CALL_N(failc, Y); \
  CBOOL__FAIL; \
})

#define MINOR_FAULT(Y) ({ \
  CBOOL__FAIL; \
})

/* ------------------------------------------------------------------------- */
/* Bit mask operations */

/* Prepare a mask like: */
/*    <-bits-><--offset--> */
/* 00011111111000000000000 */
#define MakeMask(Type, Bits, Offset) ((((Type)1<<(Bits))-1)<<(Offset))

/* Deposit Source into Mask:ed portion of Dest */
#define Deposit(Source,Mask,Dest) (((Source)&(Mask))|((Dest)& ~(Mask)))

/* ------------------------------------------------------------------------- */
/* Alignment operations */

#define ALIGN_TO(A,X) ((((X)-1) & -(A))+(A))

/* ------------------------------------------------------------------------- */
/* Small pointers (smallptr) and memory management routines */

/* A Smallptr is a pointer that some upper and lower bits set to zero
   (that can be used to store tags) */
/* Smallptr is an integer that represents an addressable pointer */
/* A pointer is addressable iff PtrToSmallptr(SmallptrToPtr(P)) == P */

#if defined(ABSMACH_OPT__pointer64)
#define SMALLPTR_ALLBITS 64
#else
#define SMALLPTR_ALLBITS 32
#endif

/* memory alignment for alloc routines */
/* note: pointers returned by alloc routines are aligned to that value */
/* note: bits set to zero by pointer alignment are used to store tags */
/* note: align to larger data type */
#define OWNMALLOC_ALIGN sizeof(tagged_t)

#if SMALLPTR_UPPERBITS+SMALLPTR_LOWERBITS == 0
#define SMALLPTR_MASK (~((intp_t)0))
#else
#define SMALLPTR_MASK MakeMask(intp_t, SMALLPTR_ALLBITS-SMALLPTR_UPPERBITS-SMALLPTR_LOWERBITS, SMALLPTR_LOWERBITS)
#endif

#if SMALLPTR_UPPERBITS+SMALLPTR_LOWERBITS == 0
#define ENSURE_ADDRESSABLE(P, SIZE) TRUE
#else
#define ENSURE_ADDRESSABLE(P, SIZE) \
  (((intp_t)(P) >= (intp_t)SMALLPTR_BASE) && \
  (((intp_t)(P) - (intp_t)SMALLPTR_BASE) < SMALLPTR_MASK) && \
   (((intp_t)(P) - (intp_t)SMALLPTR_BASE + (SIZE)) < SMALLPTR_MASK))
#endif

/* pre: ENSURE_ADDRESSABLE(P, 0) */
#if SMALLPTR_BASE != 0
#if defined(ABSMACH_OPT__tagp)
/* (one asm instruction cheaper when other adds) */
/* TODO: it is slower!? */
#define PtrToSmallptr(P) ((intp_t)(P) - SMALLPTR_BASE)
#else
#define PtrToSmallptr(P) ((intp_t)(P) & SMALLPTR_MASK)
#endif
/* Like PtrToSmallptr but uses substraction */
#define PtrToSmallptr2(P) ((intp_t)(P) - SMALLPTR_BASE)
#else
#define PtrToSmallptr(P) ((intp_t)(P))
#define PtrToSmallptr2(P) ((intp_t)(P))
#endif

#if SMALLPTR_BASE != 0
#define SmallptrToPtr(T,P) ((T *)((P) + SMALLPTR_BASE))
#else
#define SmallptrToPtr(T,P) ((T *)(P))
#endif

/* allocation of small pointers */

extern intmach_t total_mem_count;
extern intmach_t mem_prog_count;

#define INC_MEM_PROG(size) ({ \
  DEBUG__TRACE(debug_mem, \
               "Program memory increased by %ld bytes\n", \
               (long)size); \
  /* TRACE_PRINTF("%ld (%ld)[i] at %s (%s:%d)\n", (long)mem_prog_count, (long)size, __FUNCTION__, __FILE__, __LINE__);*/ \
  mem_prog_count = mem_prog_count + (size); \
})
#define DEC_MEM_PROG(size) ({ \
  DEBUG__TRACE(debug_mem, \
               "Program memory decreased by %ld bytes\n", \
               (long)size); \
  /* TRACE_PRINTF("%ld (%ld)[d] at %s (%s:%d)\n", (long)mem_prog_count, -(long)size, __FUNCTION__, __FILE__, __LINE__);*/ \
  mem_prog_count = mem_prog_count - (size); \
})

char *checkalloc(intmach_t size);
char *checkrealloc(char *ptr, intmach_t oldsize, intmach_t size);
void checkdealloc(char *ptr, intmach_t size);

void init_alloc();

/* alloc macros (including support for variable-length structures) */

#define CHECKALLOC(T) ((T *)checkalloc(sizeof(T)))
#define CHECKALLOC_ARRAY(T, I) ((T *)checkalloc(sizeof(T) * (I)))

#define SET_CHECKALLOC_TAILED(DST, T, I) ({ \
  (DST) = ((T *)checkalloc(TAILED_SIZE_TO_BYTES(T, I))); \
  SET_TAILED_SIZE(T, (DST), (I)); \
})
#define TAILED_TYPE(T) T##__TAIL
#define TAILED_SIZE_TO_BYTES(T, I) (sizeof(T) + sizeof(TAILED_TYPE(T)) * (I))
#define TAILED_BYTES_TO_SIZE(T, B) (((B) - sizeof(T)) / sizeof(TAILED_TYPE(T)))

#define SIZE_TAILED(T, PTR) T##__SIZE_TAILED((PTR))
#define SET_TAILED_SIZE(T, PTR, N) T##__SET_TAILED_SIZE((PTR), (N))

#define CHECKREALLOC0_ARRAY(T, PTR, OLDSIZE, NEWSIZE) ((T *)checkrealloc((char *)(PTR), sizeof(T) * (OLDSIZE), sizeof(T) * (NEWSIZE)))
#define SET_CHECKREALLOC0_TAILED(DST, T, PTR, I) ({ \
  (DST) = ((T *)checkrealloc((char *)(PTR), TAILED_SIZE_TO_BYTES(T, (SIZE_TAILED(T, (PTR)))), TAILED_SIZE_TO_BYTES(T, I))); \
  SET_TAILED_SIZE(T, (DST), (I)); \
})

#define CHECKDEALLOC0(T, PTR) checkdealloc((char *)(PTR), sizeof(T))
#define CHECKDEALLOC0_ARRAY(T, PTR, OLDSIZE) checkdealloc((char *)(PTR), sizeof(T) * (OLDSIZE))
#define CHECKDEALLOC0_TAILED(T, PTR) checkdealloc((char *)(PTR), TAILED_SIZE_TO_BYTES(T, (SIZE_TAILED(T, (PTR)))))

/* ------------------------------------------------------------------------- */
/* Term data definitions and macros */
/* the macros here involve casting,tagging detagging and the like
   term to pointer conversion must know where object are in virtual memory */
/* (some macros) requires: a heap and a trail */

#if defined(tagged__tag_offset)
/* todo[ts]: distinguish overflow into tag and overflow into qtag!
   (use tests with different names) */
#if (tagged__tag_offset == tagged__num_offset+tagged__num_size) || (tagged__qtag_offset == tagged__num_offset+tagged__num_size)
#define VAL_OVERFLOWS_INTO_TAG_OR_QTAG 1
#endif
#endif

#if defined(tagged__tag_is_split)
#define TAGMASK	(MakeMask(tagged_t, tagged__tag_size1, tagged__tag_offset1)|MakeMask(tagged_t, tagged__tag_size2, tagged__tag_offset2))
#else
#define TAGMASK	MakeMask(tagged_t, tagged__tag_size, tagged__tag_offset)
#endif
#define QTAGMASK MakeMask(tagged_t, tagged__qtag_size, tagged__qtag_offset)

#define PTRMASK MakeMask(tagged_t, tagged__ptr_size, tagged__ptr_offset)

#if defined(EXTGC)
#define EXTGC_MARKMASK ((tagged_t)extgc__gc_marked_size<<extgc__gc_marked_offset)
#define EXTGC_REVERSEDMASK ((tagged_t)extgc__gc_reversed_size<<extgc__gc_reversed_offset)
#define EXTGC_ANYMASK (EXTGC_MARKMASK|EXTGC_REVERSEDMASK)
#else
#define GC_MARKMASK ((tagged_t)tagged__gc_marked_size<<tagged__gc_marked_offset)
#define GC_REVERSEDMASK ((tagged_t)tagged__gc_reversed_size<<tagged__gc_reversed_offset)
#define GC_ANYMASK (GC_MARKMASK|GC_REVERSEDMASK)
#endif

/* shift required to move the small pointer bits to the tagged value
   bits */
#define SHIFTPTR (tagged__ptr_offset-SMALLPTR_LOWERBITS)
#if SHIFTPTR == 0
#define PtroffsetToVal(X) ((intval_t)(X))
#define SmallptrToVal(X) ((uintval_t)(X))
#define ValToSmallptr(X) ((intp_t)(X))
#elif SHIFTPTR >= 0
#define PtroffsetToVal(X) ((intval_t)(X)<<SHIFTPTR)
#define SmallptrToVal(X) (((uintval_t)(X))<<SHIFTPTR)
#define ValToSmallptr(X) ((intp_t)((X)>>SHIFTPTR))
#else
#define PtroffsetToVal(X) ((intval_t)(X)>>(-SHIFTPTR))
#define SmallptrToVal(X) (((uintval_t)(X))>>(-SHIFTPTR))
#define ValToSmallptr(X) ((intp_t)((X)<<(-SHIFTPTR)))
#endif

#if defined(ABSMACH_OPT__tagp)
#define PtrToVal(X) (PtroffsetToVal(-SMALLPTR_BASE)+SmallptrToVal((X)))
#define PtrToVal2(X) PtrToVal(X)
#else
#define PtrToVal(X) SmallptrToVal(PtrToSmallptr((X)))
#define PtrToVal2(X) SmallptrToVal(PtrToSmallptr2((X)))
#endif

#if defined(tagged__tag_is_split)
#define Tag1t(T) (((tagged_t)(T)<<tagged__tag_offset1))
#define Tag2t(T) (((tagged_t)(T)<<tagged__tag_offset2))
#define Tagt(T) (Tag1t((T)>>tagged__tag_size2) | Tag2t((T)&(MakeMask(tagged_t, tagged__tag_size2, 0))))
#else
#define Tagt(T) (((tagged_t)(T)<<tagged__tag_offset))
#endif

/* Creates a tagged from a tag and pointer */  
/* (specialized version for P pointers) */
/* note: P must be addressable (and cannot be null since it is not) */
#define Tagp(T,P) (Tagt((T))+PtrToVal((P)))
#define PointerToTerm(X) (Tagt((NUM))+PtrToVal2((X)))

/* todo[ts]: add a macro that uses tagged__qval_offset, for atoms */
#define Tagn(T,P) (Tagt((T))+(((tagged_t)(P)<<tagged__num_offset)))

/* Term to pointer: addressable pointers <-> small integers */
/* TODO: Zero is not 0!! the lowest addressable pointer cannot be
   encoded (it is reserved) */

/* term representation for the null pointer */
#define TermNull (Tagt((NUM)))

#define GetValQ(T) (((intval_t)((T)&MakeMask(tagged_t, tagged__qval_size, tagged__qval_offset)))>>tagged__qval_offset)

#define TaggedToPointer(T) SmallptrToPtr(tagged_t, ValToSmallptr((tagged_t)(T)&PTRMASK))
/* specialized version when T is known (and QTAGMASK, if used, is zero) */
#define TagpPtr(T,X) SmallptrToPtr(tagged_t, ValToSmallptr(X)-ValToSmallptr(Tagt((T))))
#define TermToPointer(T, X) SmallptrToPtr(T, ValToSmallptr(X)-ValToSmallptr(Tagt((NUM))))

/* todo[ts]: when sizeof(tagged_t) == 8 && sizeof(intval_t) == 4,
   TagOf((X)) == (T) may be more efficient */
#if defined(tagged__tag_is_split)
/* TODO: this code works even for not split tags (and it was the old
   definition)... is it more efficient in any tag scheme? */
#define HasTag(X,T) (((X) & TAGMASK) == Tagt((T)))
#else
/* TODO: this code works even for not split tags (and it was the old
   definition)... is it more efficient in any tag scheme? */
//#define HasTag(X,T) (((X) & TAGMASK) == Tagt((T)))
#define HasTag(X,T) (TagOf((X)) == (T))
#endif

#if defined(tagged__tag_is_split)
inline static uinttag_t TagOf(tagged_t P) {
  return (((uinttag_t)(((P)>>tagged__tag_offset1)&MakeMask(tagged_t, tagged__tag_size1, 0))<<tagged__tag_size2) +
	  ((uinttag_t)((P)&MakeMask(tagged_t, tagged__tag_size2, 0))));
}
#elif (tagged__tag_size+tagged__tag_offset == tagged__size) || defined(ABSMACH_OPT__halfwordtags)
/* upper bits before the tag are 0, or set to 0, masking is not necessary */
#define TagOf(P) ((uinttag_t)(((P)>>tagged__tag_offset)))
#else
#define TAGOFMASK MakeMask(uinttag_t, tagged__tag_size, 0)
#define TagOf(P) ((uinttag_t)(((P)>>tagged__tag_offset)&TAGOFMASK))
#endif

/* Get a tagged ensuring that it does not have gc marks */
#if defined(EXTGC)
/* gc bits are not in the word */
#define GC_UNMARKED(X) (X)
#define GC_UNMARKED_M(X) (X)
#else
#define GC_UNMARKED(X) ((X)&(~(GC_ANYMASK)))
#define GC_UNMARKED_M(X) ((X)&(~(GC_MARKMASK)))
#endif

#define TG_cache 1
#if defined(EXTGC)
//#define PROF_AG 1
/* TODO: enable PROF_AG to profile ptr to gcptr translations */
/* TODO: try to speed up the ptr_to_gcptr operation */
#if defined(PROF_AG)
CFUN__PROTO_N(ptr_to_gcptr, extgc_t *, tagged_t *ptr, intmach_t line, intmach_t n, intmach_t sect);
#define ptr_to_gcptr_any(N, SECT, PTR) CFUN__EVAL_N(ptr_to_gcptr, PTR, __LINE__, N, SECT)
#define sect_any 0
#define sect_heap 1
#define sect_frame 2
#define sect_trail 3
#define sect_choice 3
#else
CFUN__PROTO_N(ptr_to_gcptr, extgc_t *, tagged_t *ptr);
#define ptr_to_gcptr_sect_any(PTR) CFUN__EVAL_N(ptr_to_gcptr, PTR)
#endif
#define ptr_to_gcptr_sect_heap(PTR) PTR_TO_GCPTR(PTR, Heap_Start, Heap_End)
#define ptr_to_gcptr_sect_frame(PTR) PTR_TO_GCPTR(PTR, Stack_Start, Stack_End)
#define ptr_to_gcptr_sect_trail(PTR) PTR_TO_GCPTR(PTR, Trail_Start, Trail_End)
#define ptr_to_gcptr_sect_choice(PTR) PTR_TO_GCPTR(PTR, Trail_Start, Trail_End)
//#define PTR_TO_GCPTR(PTR, START, END) ((tagged_t *)(((char *)(PTR)) + ((char *)(END) - (char *)(START))))
#define PTR_TO_GCPTR(PTR, START, END) \
  ((extgc_t *) \
    ((char *)(END) + \
     (((char *)(PTR) - (char *)(START)) / (intmach_t)(sizeof(tagged_t)/sizeof(extgc_t)))))
#define REL_TO_GCREL(REL) \
  ((REL) / (intmach_t)(sizeof(tagged_t)/sizeof(extgc_t)))

#if defined(PROF_AG)
#define TGavag__resolve_ag(N, S, X) TG_ag(X) = ptr_to_gcptr_any(N, S, TG_av(X))
#else
#define TGavag__resolve_ag(N, S, X) TG_ag(X) = ptr_to_gcptr_##S(TG_av(X))
#endif
#define TGav__decl(X) tagged_t *X##av
#define TGavag__decl(X) tagged_t *X##av; extgc_t *X##ag = NULL
#define TGavagg__decl(X) tagged_t *X##av; extgc_t *X##ag = NULL; TGag__decl_cache(X)
#define TGavagv__decl(X) tagged_t *X##av; extgc_t *X##ag = NULL; TGav__decl_cache(X)
#define TGavagvg__decl(X) tagged_t *X##av; extgc_t *X##ag = NULL; TGavag__decl_cache(X)
#define TGavv__decl(X) tagged_t *X##av; TGav__decl_cache(X)
#define TGav__inc(X,I) TG_av(X) += (I)
#define TGavag__inc(X,I) ({ \
  intmach_t tg_inc_i = (I); \
  TG_av(X) += tg_inc_i; \
  TG_ag(X) += tg_inc_i; \
})
#define TGav__add(From,I,To) TG_av(To) = TG_av(From) + (I)
#define TGavag__add(From,I,To) ({ \
  intmach_t tg_inc_i = (I); \
  TG_av(To) = TG_av(From) + tg_inc_i; \
  TG_ag(To) = TG_ag(From) + tg_inc_i; \
})
#define TGav__charinc(X,I) TG_av(X) = CharOffset(TG_av(X), (I))
#define TGavag__charinc(X,I) ({ \
  intmach_t tg_inc_i = (I); \
  TG_av(X) = CharOffset(TG_av(X), tg_inc_i); \
  TG_ag(X) = (extgc_t *)CharOffset(TG_ag(X), REL_TO_GCREL(tg_inc_i));	\
})
#define TGag__copy(X,I) TG_ag(X) = TG_ag(I)
#define TGavag__copy(X,I) TG_av(X) = TG_av(I); TG_ag(X) = TG_ag(I)
#if defined(TG_cache)
#define TGav__decl_cache(X) tagged_t X##v
#define TGag__decl_cache(X) extgc_t X##g
#define TGavag__decl_cache(X) tagged_t X##v; extgc_t X##g
#define TG_v(X) X##v
#define TG_g(X) X##g
/* load g (when v and g are outdated) */
#define TG_load_g(X) ({ \
   TG_g(X) = *TG_ag(X); \
})
/* load g (when v is up to date) */
#define TG_load_g0(X) ({ \
   TG_g(X) = *TG_ag(X); \
})
/* load v (when v and g are outdated) */
#define TG_load_v(X) ({ \
   TG_v(X) = *TG_av(X); \
})
/* load v (when g is up to date) */
#define TG_load_v0(X) ({ \
   TG_v(X) = *TG_av(X); \
})
#else
#define TGav__decl_cache(X)
#define TGag__decl_cache(X)
#define TGavag__decl_cache(X)
#define TG_v(X) (*TG_av(X))
//#define TG_g(X) (({extgc_t *a = TG_ag(X); static int ok=0; static int bad=0; TGavag__resolve_ag(0, sect_any, X); if (a != TG_ag(X)) { bad++; fprintf(stderr, "wrong old:%p new:%p at %d, ok:%d, bad:%d\n", a, TG_ag(X), __LINE__, ok, bad); } else { ok++; } }), *TG_ag(X))
#define TG_g(X) (*TG_ag(X))
#define TG_load_g(X) ({ \
})
#define TG_load_g0(X) ({ \
})
#define TG_load_v0(X) ({ \
})
#define TG_load_v(X) ({ \
})
#endif
#else
#define TGav__decl(X) tagged_t *X##av
#define TGavag__decl(X) tagged_t *X##av
#define TGavagg__decl(X) tagged_t *X##av; TGav__decl_cache(X)
#define TGavagv__decl(X) tagged_t *X##av; TGav__decl_cache(X)
#define TGavagvg__decl(X) tagged_t *X##av; TGav__decl_cache(X)
#define TGavv__decl(X) tagged_t *X##av; TGav__decl_cache(X)
#define TGav__inc(X,I) TG_av(X) += (I)
#define TGav__add(From,I,To) TG_av(To) = TG_av(From) + (I)
#define TGav__charinc(X,I) TG_av(X) = CharOffset(TG_av(X), (I))
#define TGavag__inc(X,I) TGav__inc(X,I)
#define TGavag__add(From,I,To) TGav__add(From,I,To)
#define TGavag__charinc(X,I) TGav__charinc(X,I)
#define TGavag__resolve_ag(N, S, X)
#define TGag__copy(X,I) TG_av(X) = TG_av(I)
#define TGavag__copy(X,I) TG_av(X) = TG_av(I)
#if defined(TG_cache)
#define TGav__decl_cache(X) tagged_t X##v
#define TG_v(X) X##v
#define TG_g(X) X##v
/* load g (when v and g are outdated) */
#define TG_load_g(X) ({ \
   TG_g(X) = *TG_av(X); \
})
/* load g (when v is up to date) */
#define TG_load_g0(X) ({ \
})
/* load v (when v and g are outdated) */
#define TG_load_v(X) ({ \
   TG_v(X) = *TG_av(X); \
})
/* load v (when g is up to date) */
#define TG_load_v0(X) ({ \
})
#else
#define TGav__decl_cache(X)
#define TG_v(X) (*TG_av(X))
#define TG_g(X) (*TG_av(X))
#define TG_load_g(X) ({ \
})
#define TG_load_g0(X) ({ \
})
#define TG_load_v(X) ({ \
})
#define TG_load_v0(X) ({ \
})
#endif
#endif
#define TG_av(X) X##av
#define TGav__set(X,I) TG_av(X) = (I)
#define TGavag__set__resolve_ag(N,S,X,I) TG_av(X) = (I); TGavag__resolve_ag(N,S,X)
#define TG_ag(X) X##ag

#define TGav__push(P,X,I) ({ \
  tagged_t tp; \
  tp = (X); \
  tagged_t *pp = TG_av(P); \
  *pp = tp; \
  pp+=I; \
  TG_av(P) = pp; \
})

#define TG_Put(V,X) ({ \
  *TG_av(X) = (V); \
})

#if defined(EXTGC)
/* TODO: try to optimize PutPtr and PutPtr_UnsetR? */
#define TG_PutPtr(p,dest) ({ \
  tagged_t val = PtrToVal((p)); \
  TG_Put(Deposit(val,PTRMASK,TG_v(dest)), dest); \
})
#define TG_PutPtr_SetR(p,dest) ({ \
  TG_PutPtr(p, dest); \
  TG_SetR(dest); \
})
#define TG_PutPtr_UnsetR(p,dest) ({ \
  TG_PutPtr(p, dest); \
  TG_UnsetR(dest); \
})
#define TG_MoveValue_UnsetR(p,dest) ({ \
  TG_Put(Deposit((p),PTRMASK,TG_v(dest)), dest); \
  TG_UnsetR(dest); \
})
#define TG_MoveValue_MoveR(p,dest) ({ \
  TG_Put(Deposit(TG_v(p),PTRMASK,TG_v(dest)), dest); \
  gc_MoveR(p, dest); \
})
#define TG_IsM(x) (TG_g(x)&EXTGC_MARKMASK)
#define TG_IsR(x) (TG_g(x)&EXTGC_REVERSEDMASK)
#define TG_IsROrM(x) (TG_g(x)&EXTGC_ANYMASK)
#define TG_SetM(x) ((*TG_ag(x))|= EXTGC_MARKMASK)
#define TG_SetR(x) ((*TG_ag(x))|= EXTGC_REVERSEDMASK)
#define TG_UnsetM(x) ((*TG_ag(x))&=(~EXTGC_MARKMASK))
#define TG_UnsetR(x) ((*TG_ag(x))&=(~EXTGC_REVERSEDMASK))
/* TODO: optimize */
#define gc_MoveR(p, x) ({ \
  extgc_t *gcptr_x = TG_ag(x); \
  extgc_t *gcptr_p = TG_ag(p); \
  *(gcptr_x) = Deposit(*(gcptr_p),EXTGC_REVERSEDMASK,*(gcptr_x)); \
})
#define CopyGcBits(src, dst) ({ \
  *TG_ag(dst) = *TG_ag(src); \
})
#define TG_SetAll_SetM(T, X) ({ \
  TG_Put(T, X); \
  (*TG_ag(X)) |= EXTGC_MARKMASK; \
})
#define TG_SetAll_UnsetM(T, X) ({ \
  TG_Put(T, X); \
  (*TG_ag(X)) |= EXTGC_MARKMASK; \
})
/* pre: src == GC_UNMARKED_M(src) */
/* move src, which does not have the M gc mark into *TG_av(dest),
   ensuring that the M gc mark is off */
#define TG_MoveUNMARKED_M_UnsetM(src,dest) ({ \
  TG_Put(src, dest); \
  TG_UnsetM(dest); \
})
#else
#define TG_IsM(x) (TG_g(x)&GC_MARKMASK)
#define TG_IsR(x) (TG_g(x)&GC_REVERSEDMASK)
#define TG_IsROrM(x) (TG_g(x)&GC_ANYMASK)
#define TG_SetM(x)  ((*TG_av(x))|= GC_MARKMASK)
#define TG_SetR(x)  ((*TG_av(x))|= GC_REVERSEDMASK)
#define TG_UnsetM(x) ((*TG_av(x))&=(~GC_MARKMASK))
#define TG_UnsetR(x) ((*TG_av(x))&=(~GC_REVERSEDMASK))
/* TODO: try to optimize PutPtr and PutPtr_UnsetR? */
#define TG_PutPtr(p,dest) ({ \
  tagged_t val = PtrToVal((p)); \
  TG_Put(Deposit(val,PTRMASK,TG_v(dest)), dest); \
})
#define TG_PutPtr_SetR(p,dest) ({ \
  tagged_t val = PtrToVal((p)); \
  TG_Put(Deposit(val,PTRMASK,TG_v(dest))|GC_REVERSEDMASK, dest); \
})
#define TG_PutPtr_UnsetR(p,dest) ({ \
  tagged_t val = PtrToVal((p)); \
  TG_Put(Deposit(val,PTRMASK|GC_REVERSEDMASK,TG_v(dest)), dest); \
})
#define TG_MoveValue_UnsetR(p,dest) ({ \
  TG_Put((Deposit((p),PTRMASK,TG_v(dest)))&(~GC_REVERSEDMASK), dest); \
})
#define TG_MoveValue_MoveR(p,dest) ({ \
  TG_Put(Deposit(TG_v(p),PTRMASK|GC_REVERSEDMASK,TG_v(dest)), dest); \
})

/* do nothing since gc bits are inside the tagged word */
#define CopyGcBits(src,dst) ({ \
})
#define TG_SetAll_SetM(T, X) ({ \
  TG_Put((T)|GC_MARKMASK, X); \
})
/* pre: src == GC_UNMARKED_M(src) */
/* move src, which does not have the M gc mark into *TG_av(dest),
   ensuring that the M gc mark is off */
#define TG_MoveUNMARKED_M_UnsetM(src,dest) ({ \
  TG_Put(src, dest); \
})
#endif

/* ----- */
/* TagNested is a special atom temporally used only during term
   compilation.
   
   Free variables are unified with a TagNested. It contains:
   - an index, that identifies both the assigned X register
   - a mark
*/
#define IsATM_TagNested(F) TaggedATMIsATMQ((F))
#if defined(ABSMACH_OPT__qtag)
#define IsTagNested(F) ((F) & QTAGMASK)
#else
#define IsTagNested(F) TaggedIsATMQ((F))
#endif

#define NestedGetMark(V) ((V) & NESTEDMARK__MASK)
/* Pre: NestedGetMark(V) == NESTEDMARK__SINGLEVAR */
#define NestedSetMark__USEDVARCVA(V) ((V) | NESTEDMARK__USEDVARCVA)
/* Pre: NestedGetMark(V) == NESTEDMARK__SINGLEVAR */
#define NestedSetMark__USEDVAR(X) ((X) | NESTEDMARK__USEDVAR)
#define NestedSetMark__VAL(V) ((V) | NESTEDMARK__VAL)

#define NESTEDMARK__SINGLEVAR ((intval_t)0<<tagged__nestedmark_offset)
#define NESTEDMARK__USEDVARCVA  ((intval_t)2<<tagged__nestedmark_offset)
#define NESTEDMARK__USEDVAR ((intval_t)1<<tagged__nestedmark_offset)
#define NESTEDMARK__VAL ((intval_t)3<<tagged__nestedmark_offset)
#define NESTEDMARK__MASK ((intval_t)3<<tagged__nestedmark_offset)

/* Get the value of a TagNested */
#define NestedValue(T) (((intval_t)((T)&MakeMask(tagged_t, tagged__nestedval_size, tagged__nestedval_offset)))>>tagged__nestedval_offset)
/* Add I to the value of a TagNested */
#define NestedAdd(X,I) ((X)+((I)<<tagged__nestedval_offset))
/* Special atom with value X, and mark NESTEDMARK__SINGLEVAR */
#define TagNested(X) (Tagn(ATM,(X))|QTAGMASK)

/* ----- */

/* todo[ts]: move castings to where each def is used */
#define HVA ((tagged_t)TAG(hva))
#define CVA ((tagged_t)TAG(cva))
#define SVA ((tagged_t)TAG(sva))
#define UBV ((tagged_t)TAG(ubv))
#define NUM ((tagged_t)TAG(num))
#define ATM ((tagged_t)TAG(atm))
#define LST ((tagged_t)TAG(lst))
#define STR ((tagged_t)TAG(str))

#if defined(ABSMACH_OPT__qtag)
#define TagNUMQ(I) (Tagn(NUM,(I)) + QTAGMASK)
#define TagATMQ(I) (Tagn(ATM,(I)) + QTAGMASK)
#else
#define TagATMQ(I) (Tagn(ATM,(I)) + QTAGMASK)
#endif

/* Tests for pairs of tags */
#if defined(ABSMACH_OPT__tagtestbit) && defined(ABSMACH_OPT__highbittags)
#define TaggedSameTag(U,V) (TagH3_Is0((U)^(V)))
#else
#define TaggedSameTag(U,V) (TagOf_Is0((U)^(V)))
#endif

/* TODO: those asserts are required for optimized compilation of
   TaggedIsSmall and TaggedIsATMQ, make it general and/or move to
   ptoc__impcomp */
#if defined(ABSMACH_OPT__tagtestbit) && defined(ABSMACH_OPT__highbittags) && (tagged__qtag_offset + 1 == tagged__tag_offset)
/* 11 = 5(ATM) * 2 + 1 */
#if TAG(atm) != 5
#error "ATM is not 5"
#endif
#if defined(ABSMACH_OPT__qtag)
#if TAG(num) != 4
#error "NUM is not 4"
#endif
#endif
#endif

/* --------------------------------------------------------------------------- */

/* Definitions for relocation of pointers and taggeds */
#define GetRelocatedPointer(Type, Pointer, Offset) \
  ((Type *)((char *)(Pointer) + (Offset)))
#define RelocatePointer(Type, Pointer, Offset) ({ \
  (Pointer) = GetRelocatedPointer(Type, Pointer, Offset); \
})
/* reloc the pointer of a tagged_t */
/* todo[ts]: not valid when FACTOR is negative, optimize the good
   solution. Maybe the solution is to define PtroffsetToVal, which
   uses signed numbers. Performance of the unoptimized case does not
   seem to be affected, anyway. */
//#define RelocateTagged(X, FACTOR) { *(TG_av(X)) += PtroffsetToVal(FACTOR); }
#define RelocateTagged(X, FACTOR) TG_PutPtr(GetRelocatedPointer(tagged_t, TaggedToPointer(TG_v(X)), (FACTOR)), X)

/* ERRORTAG is a tagged_t pointer guaranteed to be different from all
   tagged_t objects */
/* todo[ts]: check 0 is never used as a synonym of ERRORTAG (so that
   HVA can change) */
#define ERRORTAG Tagn(HVA,0)

#define TaggedLow Tagn(NUM,0)
#define ZMask ((tagged_t)1<<(tagged__num_offset+tagged__num_size-1))
#define TaggedZero (TaggedLow+ZMask)
#define SmallIntMax ((intval_t)(((uintval_t)1<<(tagged__num_size-1))-1))
#define SmallIntMin (((intval_t)(-1)<<(tagged__num_size-1)))
#define TaggedIntMax MakeSmall(SmallIntMax)
#define HighInt	(SmallIntMax+1)

/* These should be used with caution. */
#define MakeSmall(X)	(((tagged_t)((intval_t)(X)<<tagged__num_offset))+TaggedZero)
#define GetSmall(X)	((intval_t)(((X)>>tagged__num_offset)-(TaggedZero>>tagged__num_offset)))

#define SmallAdd(X,A) ((X)+((A)<<tagged__num_offset))
#define SmallSub(X,A) ((X)-((A)<<tagged__num_offset))

#if tagged__num_size < 32
/* The range of small integers is [-HighInt, Highint) */
#define Int32IsSmall(X) ((int32_t)(X) >= -HighInt && (int32_t)(X) < HighInt)
#else
/* All int32_t are small */
#define Int32IsSmall(X) TRUE
#endif

#if tagged__num_size == 32
#define Int64IsSmall(X) Int64IsInt32((X))
#else
#define Int64IsSmall(X) ((int64_t)(X) >= -HighInt && (int64_t)(X) < HighInt)
#endif

#define Int64IsInt32(X) (((X)>>32) == ((X)>>31))

#if defined(ABSMACH_OPT__qtag)
/* TODO: move to absmach_def */
#define ConsTaggedIsNUM(F) (((F)&((tagged_t)1<<tagged__tag_offset)) == 0)
#define ConsTaggedIsATM(F) (((F)&((tagged_t)1<<tagged__tag_offset)) != 0)
#endif

#if defined(ABSMACH_OPT__qtag)
/* Blobs are represented by structures whose functor has the subtag
   bit set to 1.
   The subtag bit is reserved in all tagged words.

   Floats and bignums are stored as blobs, whose
   functors have the subtag bit set to 1 (NUM tag for floats, ATM tag
   for bignums).
*/
/* Special functors STR(blob(bignum)) and STR(blob(float)) */
/* Pre: a functor; Post: a functor for STR(blob(bignum)) or STR(blob(float)) */
#define FunctorIsBlob(F) ((F) & QTAGMASK)
/* Pre: any tagged; Post: a functor for STR(blob(bignum)) or STR(blob(float)) */
#define BlobHF(F) ((F) & QTAGMASK)
/* Pre: a functor for STR(blob); Post: a functor for STR(blob(float)) */
/* TODO: use a test with Pre NUM ATM and post NUM */
#define BlobHF_IsFloat(F) ConsTaggedIsNUM((F))
/* Pre: a functor for STR(blob); Post: a functor for STR(blob(bignum)) */
/* TODO: use a test with Pre NUM ATM and post ATM */
#define BlobHF_IsBignum(F) ConsTaggedIsATM((F))
/* Pre: a functor; Post: a functor for STR(blob(float)) */
#define FunctorIsFloat(X) (FunctorIsBlob((X)) && BlobHF_IsFloat((X)))
/* Pre: a functor; Post: a functor for STR(blob(bignum)) */
#define FunctorIsBignum(X) (FunctorIsBlob((X)) && BlobHF_IsBignum((X)))
#define BlobFunctorBignum(l) ((functor_t)TagATMQ((l)))
#define BlobFunctorFlt64 ((functor_t)TagNUMQ(sizeof(flt64_t)/sizeof(bignum_t)))
#define FunctorBignumValue(f) ((f-BlobFunctorBignum(0))>>tagged__atm_offset)
/* length in bytes of unboxed data */
#define BlobFunctorSize(X) (GetValQ((X)) * sizeof(bignum_t))
#define FloatFunctorSize(X) (GetValQ((X)) * sizeof(bignum_t))
#define BignumFunctorSize(X) (GetValQ((X)) * sizeof(bignum_t))
#else
/* Blobs are represented by structures whose functor has the subtag
   bit set to 1.
   The subtag bit is reserved in only in ATM taggeds.

   Floats and bignums are stored as blobs.

   The special functor 1 is reserved for floats, the special functor
   BIGINTBASE+N is reserved for a bignum of size N.
*/
#define BIGINTBASE 128
//#define BIGINTBASE (QTAGMASK>>(tagged__atm_offset+1))
/* Special functors STR(blob(bignum)) and STR(blob(float)) */
/* Pre: a functor; Post: a functor for STR(blob(bignum)) or STR(blob(float)) */
#define FunctorIsBlob(F) ((F) & QTAGMASK)
/* Pre: a functor; Post: a functor for STR(blob(bignum)) or STR(blob(float)) */
#define BlobHF(F) (TaggedIsATMQ((F)))
/* Pre: a functor for STR(blob); Post: a functor for STR(blob(float)) */
#define BlobHF_IsFloat(F) FunctorIsFloat((F))
/* Pre: a functor for STR(blob); Post: a functor for STR(blob(bignum)) */
/* TODO: do not use negation (allow more blob types) */
#define BlobHF_IsBignum(F) ((F) != BlobFunctorFlt64)
/* Pre: a functor; Post: a functor for STR(blob(float)) */
#define FunctorIsFloat(X) ((X) == BlobFunctorFlt64)
/* Pre: any tagged; Post: a functor for STR(blob(bignum)) */
#define FunctorIsBignum(X) (FunctorIsBlob((X)) && BlobHF_IsBignum((X)))
#define BlobFunctorBignum(l) ((functor_t)(TagATMQ((l)+BIGINTBASE)))
#define BlobFunctorFlt64 ((functor_t)(TagATMQ(1)))
#define FunctorBignumValue(f) ((f-BlobFunctorBignum(0))>>tagged__atm_offset)
/* length in bytes of unboxed data */
#define BlobFunctorSize(X) ((X) == BlobFunctorFlt64 ? FloatFunctorSize((X)) : BignumFunctorSize((X)))
#define FloatFunctorSize(X) sizeof(flt64_t)
#define BignumFunctorSize(X) ((GetValQ((X)) - BIGINTBASE) * sizeof(bignum_t))
#endif

/* size of blob, aligned to ensure correct alignment of tagged words
   in memory */
#if defined(ALIGN_BLOB_64)
#define BlobFunctorSizeAligned(X) ALIGN_TO(sizeof(int64_t), BlobFunctorSize((X)))
#define FloatFunctorSizeAligned(X) ALIGN_TO(sizeof(int64_t), FloatFunctorSize((X)))
#define BignumFunctorSizeAligned(X) ALIGN_TO(sizeof(int64_t), BignumFunctorSize((X)))
#else
/* no alignment is necessary (when disabled or when blob granularity
   matchs tagged word size */
#define BlobFunctorSizeAligned(X) BlobFunctorSize((X))
#define FloatFunctorSizeAligned(X) FloatFunctorSize((X))
#define BignumFunctorSizeAligned(X) BignumFunctorSize((X))
#endif

#define MakeBlob(Ptr) CFUN__EVAL_N(make_blob, (tagged_t *)(Ptr))

/* Box/unbox of integers */
#define BlobFunctorFix32 BlobFunctorBignum(sizeof(int32_t)/sizeof(bignum_t))
#define BlobFunctorFix64 BlobFunctorBignum(sizeof(int64_t)/sizeof(bignum_t))

/* Create a blob without mirror cell (i.e. for bignums in the C stack) */
#define Int32ToBlobnm(H, I) ({ \
  HeapPush(H, BlobFunctorFix32); \
  BignumPushInt32(H, (I)); \
})
#define Int64ToBlobnm(H, I) ({ \
  HeapPush(H, BlobFunctorFix64); \
  BignumPushInt64(H, (I)); \
})

#if tagged__num_size >= 32
/* todo[ts]: define and use
   CondInt32IsSmall(x,ExprIfSmall,ExprIfNotSmall) */
/* int32_t is always small */
#define Int32ToTagged(X) MakeSmall((X))
#define Int32ToTaggedCheck(X) MakeSmall((X))
#else
#define Int32ToTagged(X) (Int32IsSmall(X) ? MakeSmall((X)) : CFUN__EVAL_N(int32_to_blob_nocheck,(X)))
#define Int32ToTaggedCheck(X) (Int32IsSmall(X) ? MakeSmall((X)) : CFUN__EVAL_N(int32_to_blob_check,(X)))
#endif

#define Int64ToTagged(X) (Int64IsSmall(X) ? MakeSmall((X)) : CFUN__EVAL_N(int64_to_blob_nocheck,(X)))
#define Int64ToTaggedCheck(X) (Int64IsSmall(X) ? MakeSmall((X)) : CFUN__EVAL_N(int64_to_blob_check,(X)))

/* creates a blob from a integer large enough to store IntmaxPlus1
   and IntminMinus1 */
#if tagged__num_size < 32
/* int32_t is large enough to store IntmaxPlus1 or IntminMinus1 */
#define HigherIntToTaggedCheck(X) CFUN__EVAL_N(int32_to_blob_check, (X))
#else
#define HigherIntToTaggedCheck(X) CFUN__EVAL_N(int64_to_blob_check, (X))
#endif

#if tagged__num_size == 32
/* A casting to int64_t is necessary because intval_t is not large
   enough */
#define IntmaxPlus1 ((int64_t)SmallIntMax+1)
#define IntminMinus1 ((int64_t)SmallIntMin-1)
#else
#define IntmaxPlus1 (SmallIntMax+1)
#define IntminMinus1 (SmallIntMin-1)
#endif

#define IntmaxPlus1ToTagged() HigherIntToTaggedCheck(IntmaxPlus1)
#define IntminMinus1ToTagged() HigherIntToTaggedCheck(IntminMinus1)

#if defined(ABSMACH_OPT__pointer64)
/* intmach_t = int64_t */
#define IntmachToTagged(X) Int64ToTagged((X))
#define BlobToIntmach(X) blob_to_int64((X))
#define BlobFunctorFixIntmach BlobFunctorFix64
#else
/* intmach_t = int32_t */
#define IntmachToTagged(X) Int32ToTagged((X))
#define BlobToIntmach(X) blob_to_int32((X))
#define BlobFunctorFixIntmach BlobFunctorFix32
#endif

#if tagged__num_size > 32
/* intval_t = int64_t */
#define IntvalToTagged(X) Int64ToTagged((X))
#define IntvalToTaggedCheck(X) Int64ToTaggedCheck((X))
#define IntvalToBlobnm(H, X) Int64ToBlobnm((H), (X))
#define BlobToIntval(X) blob_to_int64((X))
#else
/* intval_t = int32_t */
#define IntvalToTagged(X) Int32ToTagged((X))
#define IntvalToTaggedCheck(X) Int32ToTaggedCheck((X))
#define IntvalToBlobnm(H, X) Int32ToBlobnm((H), (X))
#define BlobToIntval(X) blob_to_int32((X))
#endif

#define TaggedToIntmach(X) (TaggedIsSmall(X) ? (intmach_t)GetSmall(X) : BlobToIntmach(X))
#define TaggedToIntval(X) (TaggedIsSmall(X) ? (intval_t)GetSmall(X) : BlobToIntval(X))

#define BoxFloat(X) CFUN__EVAL_N(flt64_to_blob_nocheck, X)
#define BoxFloatCheck(X) CFUN__EVAL_N(flt64_to_blob_check, X)
#define TaggedToFloat(X) (TaggedIsSmall(X) ? (flt64_t)GetSmall(X) : blob_to_flt64(X))

#if SMALLPTR_BASE != 0
/* NULL is not addressable */
#define TermToPointerOrNull(T, X) ((X)==TermNull ? (T *)0 : TermToPointer(T, (X)))
#define PointerOrNullToTerm(X) ((X)==0 ? TermNull : PointerToTerm((X)))
#else
/* NULL is addressable */
#define TermToPointerOrNull(T, X) TermToPointer(T, (X))
#define PointerOrNullToTerm(X) PointerToTerm((X))
#endif

#define ARITYLIMIT (1<<ARITYSIZE) 

#define ATMTAB_ATOM(X) (atmtab[(X)]->value.atomp)
#define TaggedToAtom(X) ATMTAB_ATOM(AtomIndex((X)))

/* TODO: identify which of the following are public or private definitions */
/* TODO: change names */
#if defined(ABSMACH_OPT__functor_table)
#if !defined(ABSMACH_OPT__qtag)
#error "ABSMACH_OPT__functor_table is incompatible without ABSMACH_OPT__qtag since it encodes pointers as values of the ATM tagged. That is not possible without ABSMACH_OPT__qtag, since ATM taggeds have one bit reserved for the QTAGMASK. With ABSMACH_OPT__qtag, QTAGMASK bit is reserved for all taggeds."
#endif
/* TODO: study if this can work without overheads... */
/* note: functor key is not a tagged */
#define ARITYSIZE 10
#define ARITYOFFSET (tagged__size-ARITYSIZE)
#define INDEXMASK (((tagged_t)1<<ARITYOFFSET)-1) 
#define MAX_ATOM_INDEX INDEXMASK
#define FUNCTOR_KEY(ATOM, A) ((hashtab_key_t)((ATOM) & INDEXMASK) | ((tagged_t)(A)<<ARITYOFFSET))
#define TaggedToFunc(X)    ((definition_t *)TaggedToPointer((X)))
#define Arity(X)	FuncArity(TaggedToFunc((X)))
#define AtomIndex(T) (TaggedToFunc((T))->atom)
#define FUNCTOR_NAME(X) FuncName(TaggedToFunc((X)))
#define FuncName(Func) FuncFunctor((Func)->self0)
#define FuncArity(Func) ((Func)->arity)
#define FuncFunctor(Func) Tagp(ATM, (Func))
#define GetString(X)	(TaggedToFunc((X))->string)
tagged_t SetArity(tagged_t x, intmach_t a);
#define GET_ATOM(X) deffunctor((X), 0)
tagged_t deffunctor(char *name, intmach_t arity);
#define GET_DEF(FUNCTOR) TaggedToFunc((FUNCTOR))
#define LOOKUP_DEF(FUNCTOR) TaggedToFunc((FUNCTOR))
#else
/* Arity is encoded inside the functor */
#define ARITYSIZE 8
#define INDEXSIZE (tagged__atm_size-ARITYSIZE)
#define ARITYOFFSET (tagged__atm_offset+INDEXSIZE) /* 20 */
#define INDEXMASK MakeMask(tagged_t, INDEXSIZE, tagged__atm_offset) /* 000F FFFF */
#define MAX_ATOM_INDEX (INDEXMASK>>tagged__atm_offset)
#define Arity(X) (((intval_t)((X) & MakeMask(tagged_t, ARITYSIZE, ARITYOFFSET)))>>ARITYOFFSET)
#define SetArity(X,A) ((tagged_t)(((X) & ~MakeMask(tagged_t, ARITYSIZE, ARITYOFFSET)) | ((tagged_t)(A)<<ARITYOFFSET)))
#define GetIndexPart(T)	(((T)&INDEXMASK)>>tagged__atm_offset)
#define AtomIndex(T)	GetIndexPart((T))
#define MakeAtom(X)	Tagn(ATM,(X))
#define GET_ATOM(X)	MakeAtom(init_atom_check((X)))
#define deffunctor(NAME, ARITY) SetArity(GET_ATOM((NAME)),(ARITY))
#define FUNCTOR_NAME(X) SetArity((X), 0)
#define FuncName(Func) (SetArity((Func)->functor, 0))
#define FuncArity(Func) (Arity((Func)->functor))
#define FuncFunctor(Func) ((Func)->functor)
#define GetString(X)	(TaggedToAtom((X))->name)
definition_t *insert_definition(tagged_t functor, bool_t insertp);
#define GET_DEF(FUNCTOR) insert_definition((FUNCTOR), FALSE)
#define LOOKUP_DEF(FUNCTOR) insert_definition((FUNCTOR), TRUE)
#endif
#if defined(ABSMACH_OPT__functor_table)
definition_t *insert_definition0_ft(intmach_t atom, intmach_t arity, bool_t insertp);
definition_t *new_functor_ft(intmach_t atom, intmach_t arity);
#else
definition_t *insert_definition0(tagged_t functor, bool_t insertp);
definition_t *new_functor(tagged_t functor);
#endif

#if defined(ABSMACH_OPT__atom_len)
#define GetAtomLen(X)   (TaggedToAtom((X))->atom_len)
#else
#define GetAtomLen(X)   (strlen(GetString((X))))
#endif

/* Tagged memory areas */
#if defined(EXTGC)
/* todo[ts]: allocate external gc areas just for GC? */
/* todo[ts]: use memmove? */
#include <string.h>

/* Size plus extra size for extgc_t elements */
/* note: size must be multiple of sizeof(tagged_t) */
#define EXTSIZE(SIZE) ((SIZE)+(SIZE)/(sizeof(tagged_t)/sizeof(extgc_t)))
/* reserve extra (hidden) space for GC bits */
inline static tagged_t *ALLOC_AREA(intmach_t size) {
  char *ptr;
  extgc_t *gcptr;
  ptr = (char *)CHECKALLOC_ARRAY(char, EXTSIZE(size));
  gcptr = (extgc_t *)(ptr + size);
  memset(gcptr, 0, size/sizeof(tagged_t));
  return (tagged_t *)ptr;
}
inline static tagged_t *REALLOC_AREA(tagged_t *start, intmach_t oldcount, intmach_t newcount) {
  char *ptr;
  extgc_t *gcptr;
  ptr = (char *)CHECKREALLOC0_ARRAY(char, (char *)start,
				    EXTSIZE(oldcount), EXTSIZE(newcount));
  gcptr = (extgc_t *)(ptr + newcount);
  memset(gcptr, 0, newcount/sizeof(tagged_t));
  return (tagged_t *)ptr;
}
inline static void CLEAN_AREA(tagged_t *start, intmach_t size, tagged_t *begin, tagged_t *end) {
  char *ptr;
  extgc_t *gcptr;
  ptr = (char *)start;
  gcptr = (extgc_t *)(ptr + size);
  memset(gcptr+(begin-start), 0, end-begin);
}
#else
#define ALLOC_AREA(I) ((tagged_t *)CHECKALLOC_ARRAY(char, (I)))
#define REALLOC_AREA(START, OLDCOUNT, NEWCOUNT) ((tagged_t *)CHECKREALLOC0_ARRAY(char, (char *)(START), (OLDCOUNT), (NEWCOUNT)))
#define CLEAN_AREA(START, SIZE, BEGIN, END)
#endif

/* Access operations for complex tagged data */

/* finding the principal functor of a structure */
#define TaggedToHeadfunctor(X) (*TagpPtr(STR,X))
/* finding the arguments of a structure, first argument is 1 */
#define TaggedToArg(X,N) CharOffset(TagpPtr(STR,X),(N)*sizeof(tagged_t))
/* finding the car & cdr of a list. */
#define TaggedToCar(X) TagpPtr(LST, X)
#define TaggedToCdr(X) CharOffset(TagpPtr(LST, X),1*sizeof(tagged_t))
/* finding the constraints of a CVA. */
#define TaggedToGoal(X) CharOffset(TagpPtr(CVA, X),1*sizeof(tagged_t))
#define TaggedToDef(X) CharOffset(TagpPtr(CVA, X),2*sizeof(tagged_t))

/* Optimized switch on tag macros */

#define SwStruct(F, V, CODE_STRBlob, CODE_STRStruct) ({ \
  tagged_t F = TaggedToHeadfunctor((V)); \
  if (FunctorIsBlob(F)) { \
    CODE_STRBlob; \
  } else { \
    CODE_STRStruct; \
  } \
})

#define SwGoalTerm(F, X, CODE_STRBlob, CODE_STRStruct, CODE_LST, CODE_ATM, CODE_Other) ({ \
  Sw_STR_LST_ATM_Other((X), { /* STR */ \
    SwStruct(F, X, CODE_STRBlob, CODE_STRStruct); \
  }, CODE_LST, CODE_ATM, CODE_Other); \
})
/* Pre: NUM ATM LST STR(blob) STR(struct) */
#define SwNonvar(V, F, CODE_NUM, CODE_ATM, CODE_LST, CODE_STRBlob, CODE_STRStruct) ({ \
  switch (TagOf((V))) {  \
  case NUM: \
    CODE_NUM; \
    break; \
  case ATM: \
    CODE_ATM; \
    break; \
  case LST: \
    CODE_LST; \
    break; \
  case STR: \
    SwStruct(F, V, CODE_STRBlob, CODE_STRStruct); \
    break; \
  } \
})
/* Pre: (anything) */
#define SwEval(Reg, HeadFunctor, CODE_NUM, CODE_LST, CODE_STRBlob, CODE_STRStruct, CODE_Other) ({ \
  switch (TagOf(Reg)) { \
  case NUM: \
    CODE_NUM; \
    break; \
  case LST: \
    CODE_LST; \
    break; \
  case STR: \
    SwStruct(HeadFunctor, Reg, CODE_STRBlob, CODE_STRStruct); \
    break; \
  default: \
    CODE_Other; \
    break; \
  } \
})
/* Pre: STR(blob) */
#define SwBlob(Reg, HeadFunctor, CODE_STRFloat, CODE_STRBignum) ({ \
  tagged_t HeadFunctor; \
  HeadFunctor = TaggedToHeadfunctor(Reg); \
  if (FunctorIsFloat(HeadFunctor)) { \
    CODE_STRFloat; \
  } else { \
    CODE_STRBignum; \
  } \
}) 

/* Deref and switch on tag macros */

#define DerefSw_HVAorCVAorSVA_Other(Reg,CODE_HVAorCVAorSVA,CODE_Other) ({ \
  __label__ labelend; \
  if (IsVar(Reg)) { \
    for(;;) { \
      tagged_t Aux; \
      Aux = *TaggedToPointer(Reg); \
      if (Reg == Aux) { \
	CODE_HVAorCVAorSVA; \
        goto labelend; \
      } \
      Reg = Aux; \
      if (!IsVar(Reg)) break; \
    } \
  } \
  CODE_Other; \
labelend: {} \
})

/* Pre: HVA or CVA */
#define DerefSwHVAorCVA_HVA_CVA(Reg, CODE_HVA, CODE_CVA) ({ \
  for(;;) { \
    tagged_t Aux; \
    Aux = *TaggedToPointer(Reg); \
    if (Reg == Aux) { \
      break; \
    } \
    Reg = Aux; \
  } \
  Sw_HVA_CVA(Reg, CODE_HVA, CODE_CVA); \
})

/* Pre: HVA or CVA or SVA */
#define DerefSwHVAorCVAorSVA_HVA_CVA_SVA(Reg, CODE_HVA, CODE_CVA, CODE_SVA) ({ \
  for(;;) { \
    tagged_t Aux; \
    Aux = *TaggedToPointer(Reg); \
    if (Reg == Aux) { \
      break; \
    } \
    Reg = Aux; \
  } \
  SwHVAorCVAorSVA_HVA_CVA_SVA(Reg, CODE_HVA, CODE_CVA, CODE_SVA); \
})

/* Pre: HVA CVA NUM ATM LST STR (in any dereference step) */
#define HeapDerefSw_HVAorCVA_Other(T1, CODE_HVAorCVA, CODE_Other) \
  DerefSw_HVAorCVAorSVA_Other(T1, CODE_HVAorCVA, CODE_Other)

#define DerefVar(Reg) DerefSw_HVAorCVAorSVA_Other((Reg), {}, {})

#define DEREF(Xderef,X) ({ \
  tagged_t m_i; \
  m_i = (X); \
  DerefVar(m_i); \
  Xderef = m_i; \
})

/* Pre: HVA CVA SVA NUM ATM LST STR */
#if defined(ABSMACH_OPT__joinconts)
#define DerefSw_HVA_CVA_SVA_Other(Reg,HVACode,CVACode,SVACode,OtherCode) ({ \
  for (;;) { \
    Sw_HVA_CVA_SVA_Other(Reg, { \
      tagged_t aux; \
      aux = *TagpPtr(HVA,Reg); \
      if (Reg!=aux) { Reg=aux; continue; } else { HVACode; } \
    },{ \
      tagged_t aux; \
      aux = *TagpPtr(CVA,Reg); \
      if (Reg!=aux) { Reg=aux; continue; } else { CVACode; } \
    },{ \
      tagged_t aux; \
      aux = *TagpPtr(SVA,Reg); \
      if (Reg!=aux) { Reg=aux; continue; } else { SVACode; } \
    },{ \
      OtherCode; \
    }); \
    break; \
  } \
})
#else
#define DerefSw_HVA_CVA_SVA_Other(Reg,HVACode,CVACode,SVACode,OtherCode) ({ \
  DerefVar(Reg); \
  Sw_HVA_CVA_SVA_Other(Reg, { \
    { HVACode; } \
  },{ \
    { CVACode; } \
  },{ \
    { SVACode; } \
  },{ \
    { OtherCode; } \
  }); \
})
#endif

/* Pre: HVA CVA NUM ATM LST STR */
#if defined(ABSMACH_OPT__joinconts)
#define HeapDerefSw_HVA_CVA_Other(Reg,HVACode,CVACode,OtherCode) ({ \
  for (;;) { \
    Sw_HVA_CVA_Other(Reg, { \
      tagged_t aux; \
      aux = *TagpPtr(HVA,Reg); \
      if (Reg!=aux) { Reg=aux; continue; } else { HVACode; } \
    },{ \
      tagged_t aux; \
      aux = *TagpPtr(CVA,Reg); \
      if (Reg!=aux) { Reg=aux; continue; } else { CVACode; } \
    },{ \
      OtherCode; \
    }); \
    break; \
  } \
})
#else
#define HeapDerefSw_HVA_CVA_Other(Reg,HVACode,CVACode,OtherCode) ({ \
  DerefVar(Reg); \
  Sw_HVA_CVA_Other(Reg, { \
    { HVACode; } \
  },{ \
    { CVACode; } \
  },{ \
    { OtherCode; } \
  }); \
})
#endif

/* Pre: HVA CVA NUM ATM LST STR */
#define SwitchOnHeapVarB(Reg, HeadFunctor, CODE_HVA, CODE_CVA, CODE_NUM, CODE_ATM, CODE_LST, CODE_STRBlob, CODE_STRStruct) ({ \
  HeapDerefSw_HVAorCVA_Other(Reg, { /* HVA CVA */ \
    Sw_HVA_CVA(Reg, CODE_HVA, CODE_CVA); \
  }, { \
    /* NUM ATM LST STR */ \
    Sw_NUM_ATM_LST_STR(Reg, CODE_NUM, CODE_ATM, CODE_LST, { /* STR */ \
      SwStruct(HeadFunctor, Reg, CODE_STRBlob, CODE_STRStruct); \
    }); \
  }); \
})

#define DerefSw_any(Reg, HeadFunctor, CODE_HVA, CODE_SVA, CODE_CVA, CODE_NUM, CODE_ATM, CODE_LST, CODE_STRFloat, CODE_STRBignum, CODE_STRStruct) ({ \
  DEREF(Reg, Reg); \
  Sw_HVA_SVA_CVA_NUM_ATM_LST_STR(Reg, CODE_HVA, CODE_SVA, CODE_CVA, CODE_NUM, CODE_ATM, CODE_LST, { \
    SwStruct(HeadFunctor, Reg, { \
      if (FunctorIsFloat(HeadFunctor)) { \
        CODE_STRFloat; \
        break; \
      } else { \
        CODE_STRBignum; \
        break; \
      } \
    }, { \
      CODE_STRStruct; \
      break; \
    }); \
  }); \
})

#define DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(Reg, CODE_HVAorCVAorSVA, CODE_NUMorATM, CODE_LST, CODE_STR) ({ \
  DerefSw_HVAorCVAorSVA_Other(Reg, { /* HVA CVA SVA */ \
    CODE_HVAorCVAorSVA; \
  }, { /* Other */ \
    Sw_NUMorATM_LST_STR(Reg, CODE_NUMorATM, CODE_LST, CODE_STR); \
  }); \
})

#define HeapDerefSw_HVA_CVA_NUMorATM_LST_STR(Reg, CODE_HVA, CODE_CVA, CODE_NUMorATM, CODE_LST, CODE_STR) ({ \
  HeapDerefSw_HVA_CVA_Other(Reg, { /* HVA */ \
    CODE_HVA; \
  },{ /* CVA */ \
    CODE_CVA; \
  },{ \
    /* NUM ATM LST STR */ \
    Sw_NUMorATM_LST_STR(Reg, CODE_NUMorATM, CODE_LST, CODE_STR); \
  }); \
})

/* Pre: anything, Post: NUM ATM STR(blob) */
inline static bool_t IsAtomic(tagged_t X) {
  Sw_NUMorATM_STR_Other(X, { /* NUM or ATM */
    return TRUE;
  }, { /* STR */
    if (FunctorIsBlob(TaggedToHeadfunctor(X))) {
      return TRUE;
    } else {
      return FALSE;
    }
  }, { /* Other */
    return FALSE;
  });
  return FALSE; /* TODO: never reaches that code */
}
/* Pre: NUM ATM LST STR, Post: NUM ATM STR(blob) */
inline static bool_t IsNonvarAtomic(tagged_t U) {
  Sw_NUMorATM_LST_STR(U, { /* NUM or ATM */
    return TRUE;
  }, { /* LST */
    return FALSE;
  }, { /* STR */
    if (FunctorIsBlob(TaggedToHeadfunctor(U))) {
      return TRUE;
    } else {
      return FALSE;
    }
  });
  return FALSE; /* TODO: never reaches that code */
}
/* Pre: anything, Post: STR(blob(float)) */
inline static bool_t IsFloat(tagged_t U) {
  if (TaggedIsSTR(U)) {
    if (FunctorIsFloat(TaggedToHeadfunctor(U))) {
      return TRUE;
    } else {
      return FALSE;
    }
  } else {
    return FALSE;
  }
}
/* Pre: anything, Post: NUM STR(blob(bignum)) */
inline static bool_t IsInteger(tagged_t U) {
  Sw_NUM_STR_Other(U, { /* NUM */
    return TRUE;
  }, { /* STR */
    if (FunctorIsBignum(TaggedToHeadfunctor(U))) {
      return TRUE;
    } else {
      return FALSE;
    }
  }, { /* Other */
    return FALSE;
  });
  return FALSE; /* TODO: never reaches that code */
}
/* todo[ts]: to include more blob types, check that it is a BlobHF_IsFloat or a BlobHF_IsBignum */
/* Pre: anything, Post: NUM STR(blob) */
inline static bool_t IsNumber(tagged_t U) {
  Sw_NUM_STR_Other(U, { /* NUM */
    return TRUE;
  }, { /* STR */
    if (FunctorIsBlob(TaggedToHeadfunctor(U))) {
      return TRUE;
    } else {
      return FALSE;
    }
  }, { /* Other */
    return FALSE;
  });
  return FALSE; /* TODO: never reaches that code */
}

#define DerefSw_CVA_Other(X, CODE_CVA, CODE_Other) ({ \
  __label__ derefsw_cva; \
  __label__ derefsw_other; \
  __label__ derefsw_end; \
  DerefSw_HVAorCVAorSVA_Other((X),{ \
    Sw_CVA_HVAorSVA((X), { goto derefsw_cva; }, { goto derefsw_other; }); \
  }, { goto derefsw_other; }); \
 derefsw_cva: \
  CODE_CVA; \
  goto derefsw_end; \
 derefsw_other: \
  CODE_Other; \
  goto derefsw_end; \
 derefsw_end: {} \
})

/* Pre: NUM ATM STR LST */
#define SwTagC(Reg, HeadFunctor, CODE_NUM, CODE_ATM, CODE_LST, CODE_STRFloat, CODE_STRBignum, CODE_STRStruct) ({ \
  Sw_NUM_ATM_LST_STR(Reg, CODE_NUM, CODE_ATM, CODE_LST, { /* STR */ \
    SwStruct(HeadFunctor, Reg, { \
      if (FunctorIsFloat(HeadFunctor)) { /* STR(blob(float)) */ \
        CODE_STRFloat; \
      } else { /* STR(blob(bignum)) */ \
        CODE_STRBignum; \
      } \
    }, { /* STR(struct) */ \
      CODE_STRStruct; \
    }); \
  }); \
})

#define SwOnAnyTag(Reg, HeadFunctor, CODE_VAR, CODE_NUM, CODE_ATM, CODE_LST, CODE_STRFloat, CODE_STRBignum, CODE_STRStruct) ({ \
  switch (TagOf(Reg)) { \
  case SVA: \
  case HVA: \
  case CVA: \
    CODE_VAR; \
    break; \
  case NUM: \
    CODE_NUM; \
    break; \
  case ATM: \
    CODE_ATM; \
    break; \
  case LST: \
    CODE_LST; \
    break; \
  default: /*case STR:*/ \
    { \
      SwStruct(HeadFunctor, Reg, { \
	if (FunctorIsFloat(HeadFunctor)) { \
	  CODE_STRFloat; \
	} else { \
	  CODE_STRBignum; \
	} \
      }, { \
	CODE_STRStruct; \
      }); \
      break; \
    } \
  } \
})

#define SwOnAnyTagB(Reg, HeadFunctor, CODE_HVA, CODE_SVA, CODE_CVA, CODE_NUM, CODE_ATM, CODE_LST, CODE_STRBlob, CODE_STRStruct) ({ \
  switch (TagOf(Reg)) { \
  case HVA: \
    CODE_HVA; \
    break; \
  case SVA: \
    CODE_SVA; \
    break; \
  case CVA: \
    CODE_CVA; \
    break; \
  case NUM: \
    CODE_NUM; \
    break; \
  case ATM: \
    CODE_ATM; \
    break; \
  case LST: \
    CODE_LST; \
    break; \
  case STR: \
    { \
      SwStruct(HeadFunctor, Reg, { \
        CODE_STRBlob; \
      }, { \
	CODE_STRStruct; \
      }); \
      break; \
    } \
  } \
})

/* Pre: (anything) */
#define SwOnTagS(Reg, HeadFunctor, CODE_LST, CODE_STRBlob, CODE_STRStruct, CODE_Other) ({ \
  Sw_LST_STR_Other(Reg, CODE_LST, { /* STR */ \
    SwStruct(HeadFunctor, Reg, { \
      CODE_STRBlob; \
    }, { \
      CODE_STRStruct; \
    }); \
  }, CODE_Other); \
})

/* dereference until the variable is not a SVA + switch on SVA or Other */
#define DerefUpToSVA_Sw_SVA_Other(Reg,SVACode,OtherCode) ({ \
  __label__ swsva_other; \
  __label__ swsva_sva; \
  __label__ swsva_end; \
  if (TaggedIsSVA(Reg)) { \
    for(;;) { \
      tagged_t Aux; \
      Aux = *TagpPtr(SVA,Reg); \
      if (Aux == Reg) { \
        goto swsva_sva; \
      } \
      Reg = Aux; \
      if (!TaggedIsSVA(Reg)) { \
        goto swsva_other; \
      } \
    } \
  } else { \
    goto swsva_other; \
  } \
 swsva_sva: \
  SVACode; \
  goto swsva_end; \
 swsva_other: \
  OtherCode; \
  goto swsva_end; \
 swsva_end: {} \
})

/* Binding of variables */

/* Pre: TagOf(Q) == TagOf(R) && (TagOf(Q) == HVA || TagOf(R) == CVA) */
#define YoungerHeapVar(Q,R) HeapYoungerTagged(Q,R)
/* Pre: TagOf(Q) == TagOf(R) && TagOf(Q) == SVA */
#define YoungerStackVar(Q,R) StackYoungerTagged(Q,R)

#define CondHVA(X) (!OffHeaptopTagged(X,w->global_uncond))
#define CondCVA(X) (!OffHeaptopTagged(Tagp(HVA, TagpPtr(CVA, X)),w->global_uncond))
#define CondSVA(X) (!OffStacktopTagged(X,w->local_uncond))

#define DerefCar(Xderef,Ptr) DEREF(Xderef,*TaggedToCar(Ptr));
#define DerefCdr(Xderef,Ptr) DEREF(Xderef,*TaggedToCdr(Ptr));
#define DerefArg(Xderef,Ptr,I) DEREF(Xderef,*TaggedToArg(Ptr,I));

#define UnsafeVar(Frame,X) (!YoungerStackVar(Tagp(SVA, FrameNewTop(Frame, 0)), X))
/* TODO: slower, why? */
//#define UnsafeVar(Frame,X) (!YoungerStackVar((tagged_t)FrameNewTop(Frame, 0), TagpPtr(SVA, X)))

CBOOL__PROTO_N(cunify, tagged_t x1, tagged_t x2);
CBOOL__PROTO_N(cunify_args, intmach_t arity, tagged_t *pt1, tagged_t *pt2);

#define CBOOL__UNIFY(X, Y) CBOOL__CALL_N(cunify, (X), (Y))
#define CBOOL__LASTUNIFY(X, Y) CBOOL__LASTCALL_N(cunify, (X), (Y))

/* finding the arguments of a structure or list */
/* (assume that there are no errors) */
/* TODO: make it similar to TaggedToArg... take into account precondition? */
#define ComplexToArg(PTR, X, N) ({ \
  Sw_LST_STR_Other((X), { /* STR */ \
    if ((N)==1) (PTR) = TaggedToCar((X)); \
    else if ((N)==2) (PTR) = TaggedToCdr((X)); \
    else (PTR) = NULL; \
  }, { /* STR */ \
    (PTR) = TaggedToArg((X),(N)); \
  }, { \
    (PTR) = NULL; \
  }); \
})

/* Pre: STR(structure) LST; Post: pt1 points to the arguments, i is the arity */
#define DecompComplex(u, pt1, i) ({ \
  Sw_LST_STR(u, { /* LST */ \
    pt1 = TagpPtr(LST,u); u = functor_lst; i = 2; \
  }, { /* STR */ \
    pt1 = TagpPtr(STR,u); u = *pt1++; i = Arity(u); \
  }); \
})

/* ------------------------------------------------------------------------- */
/* Heap */

/* heap grows in positive direction */
/* w is an implicit argument */

#define Heap_Start          w->heap_start
#define Heap_End            w->heap_end    
#define Heap_Warn_Soft      w->heap_warn_soft

#define OnHeap(t)               (HeapYounger(Heap_End,t) && !HeapYounger(Heap_Start,t))
#define OffHeaptop(t,H)         (!HeapYounger(H,t))
#define OffHeaptopTagged(t,H)   (!HeapYoungerTagged(H,t))
#define HeapYounger(X,Y)	((tagged_t *)(X)>(tagged_t *)(Y))
#define HeapYoungerTagged(X,Y)	((tagged_t)(X)>(tagged_t)(Y))
#define HeapCharDifference(X,Y)	((char *)(Y) - (char *)(X))
#define HeapCharOffset(X,O)	((tagged_t *)((char *)(X) + (O)))

#define Heap_Warn(PAD) HeapCharOffset(Heap_End,-(PAD))

/* H: heap top */
#define HeapCharAvailable(H) HeapCharDifference((H),Heap_End)

/* H: heap top */
#define HeapCharUsed(H) HeapCharDifference(Heap_Start, (H))

#define HeapCharSize HeapCharAvailable(Heap_Start)

/* min. amount of heap at proceed */
#define CONTPAD (128*sizeof(tagged_t))
/* min. amount of heap at call */
// TODO: MAXATOM is dynamic??!! why MAXATOM here??
//#define CALLPAD (2*(MAXATOM) + CONTPAD) 
#define CALLPAD (2*(STATICMAXATOM)*sizeof(tagged_t) + CONTPAD) 

#define HeapPushT(T, H, V) ({ \
  *((T *)(H)) = (V); \
  (H) = HeapCharOffset((H), sizeof(T)); \
})
#if 0
/* TODO: necessary?? temporal variable __t used to conform strict-aliasing rules? */
#define HeapPushT(T, H, V) ({			\
      T *__t = (T *)(H);			\
      *__t = (V);				\
      (H) = HeapCharOffset((H), sizeof(T));	\
    })
#endif

/* ------------------------------------------------------------------------- */
/* Frame */

/* Frame stack grows in positive direction */
/* w is an implicit argument */

#define Stack_Start         w->stack_start
#define Stack_End           w->stack_end
#define Stack_Warn          ((tagged_t *)StackCharOffset(Stack_End,-STACKPAD))

#define OnStack(t) (StackYounger(Stack_End,t) && !StackYounger(Stack_Start,t))
#define OffStacktop(t,H)         (!StackYounger(H,t))
#define OffStacktopTagged(t,H)   (!StackYoungerTagged(H,t))
#define StackYounger(X,Y)	((tagged_t *)(X)>(tagged_t *)(Y))
#define StackYoungerTagged(X,Y)	((tagged_t)(X)>(tagged_t)(Y))
#define StackCharDifference(X,Y) ((char *)(Y) - (char *)(X))
#define StackCharOffset(X,O)	((frame_t *)((char *)(X) + (O)))
#define StackDir 1

#define StackCharAvailable(Top) StackCharDifference((Top), Stack_End)
#define StackCharUsed(Top) StackCharDifference(Stack_Start, (Top))
#define StackCharSize StackCharAvailable(Stack_Start)

/* min. amount of stack at allocate */
#define STACKPAD ((2*ARITYLIMIT + 16) * sizeof(tagged_t))

/* ------------------------------------------------------------------------- */
/* Choice points */

#define ChoiceptMarkPure(B) ((B)->flags |= 1)
#define ChoiceptTestPure(B) ((B)->flags & 1)
#define ChoiceptMarkStatic(B) ((B)->flags |= 2)
#define ChoiceptTestStatic(B) ((B)->flags & 2)
#define ChoiceptMarkNoCVA(B) ((B)->flags |= 4)
#define ChoiceptTestNoCVA(B) ((B)->flags & 4)

/* Choicestack grows in negative direction */
/* w is an implicit argument */

#define Choice_End          w->choice_end
#define Choice_Start        w->choice_start

#define OnChoice(t) (ChoiceYounger((t),Choice_Start) && !ChoiceYounger((t),Choice_End))
#define OffChoicetop(t,B)	ChoiceYounger(t,(B))
#define ChoiceYounger(X,Y)	((tagged_t *)(X)<(tagged_t *)(Y))
#define ChoiceCharOffset(X,O)	((choice_t *)((char *)(X) - (O)))
#define ChoiceCharDifference(X,Y) ((char *)(X) - (char *)(Y))
#define ChoiceArity(B) ((B)->next_alt->arity)
#define ChoiceCont(B)           ChoiceCont0((B), ChoiceArity((B)))
#define ChoiceCont0(B,A)        ChoiceCharOffset((B),-ChoiceSize((A)))
#define ChoiceNext0(B,A)        ChoiceCharOffset((B),ChoiceSize((A)))

/* todo[ts]: implement RelativePtrVal */
/* note: the choice is stored as a relative offset so that the choice
   stack can be relocated without readjusting any boxed choice address
   (that would be impossible without special tagging since it is
   stored as a small integer) */
#define ChoiceFromTagged(Y) ChoiceCharOffset(Choice_Start,GetSmall(Y)<<SMALLPTR_LOWERBITS)
#define ChoiceToTagged(Y) MakeSmall(ChoiceCharDifference(Choice_Start,Y)>>SMALLPTR_LOWERBITS)

#define ChoiceSize(ARITY) (sizeof(choice_t) + (ARITY)*sizeof(tagged_t))

#define ChoiceCharAvailable(B) ChoiceCharDifference((B), G->trail_top)

/* Do DO for each variable CHOICE->x[I] in the choice point */
#define ChoiceForeachX(CHOICE, I, DO) ({ \
  (I) = ChoiceArity((CHOICE)); \
  while (((I)--) > 0) { \
    DO; \
  } \
})

/* min. amount of trail/choice at try */
#define CHOICEPAD ((2*ARITYLIMIT)*VALUETRAIL__UNIT)

/* ------------------------------------------------------------------------- */
/* Trail */

/* trail grows in positive direction */
/* w is an implicit argument */

#define Trail_Start         w->trail_start
#define Trail_End           w->trail_end

#define OnTrail(t)  (TrailYounger(Trail_End,t) && !TrailYounger(Trail_Start,t))
#define TrailYounger(X,Y)	((tagged_t *)(X)>(tagged_t *)(Y))
#define TrailCharDifference(X,Y) ((char *)(Y) - (char *)(X))
#define TrailCharOffset(X,O)	((tagged_t *)((char *)(X) + (O)))
#define TrailDec(P)		(--(P))
#define TrailGetTop(P)		(P[-1])
#define TrailDir 1
#define TrailPush(P,X) ({ \
  tagged_t tp; \
  tp = (X); \
  tagged_t *pp = (P); \
  *pp = tp; \
  pp+=TrailDir; \
  (P) = pp; \
})

/* ------------------------------------------------------------------------- */
/* Value trail */

/* note: shares memory with the choice stack */
/* note: only usable in some built-ins, see unify and compare to check
   usage */
/* note: it is mandatory to check if enough space is available in the
   value trail before using it */

/* initial value_trail size: leave room for an extra choicept */
#define InitialValueTrail (-ChoiceSize(0))

/* initialize value_trail (do it one time for each worker) */
#define VALUETRAIL__INIT ({ \
  w->value_trail = InitialValueTrail; \
})

/* align for tagged_t member of the value trail unit */
/* todo[ts]: define as a structure so that the padding is set automatically */
#if FORCE_ALIGNED_ACCESS && defined(ABSMACH_OPT__tagged64) && !defined(ABSMACH_OPT__pointer64)
#define VALUETRAIL__UNITPAD sizeof(uint32_t)
#else
#define VALUETRAIL__UNITPAD 0
#endif

/* (private) trail a value in the value trail */
#define VALUETRAIL__TRAIL(X) ({ \
  char *b = (char *)w->choice; \
  intmach_t i = w->value_trail; \
  i -= sizeof(tagged_t); \
  *((tagged_t *)(b + i)) = *((X)); \
  i -= sizeof(tagged_t *) + VALUETRAIL__UNITPAD; \
  *((tagged_t **)(b + i)) = (X); \
  w->value_trail = i; \
})

/* set trail X and *X = Value */
#define VALUETRAIL__SET(X, Value) ({ \
  VALUETRAIL__TRAIL((X)); \
  *(X) = Value; \
})

/* undo all the updates done using VALUETRAIL__SET */
#define VALUETRAIL__UNDO() ({ \
  intmach_t valuetrail__i; \
  tagged_t *valuetrail__pt1; \
  char *valuetrail__pt2; \
  valuetrail__i = w->value_trail; \
  if (valuetrail__i < InitialValueTrail) { \
    valuetrail__pt2 = (char *)w->choice; \
    do { \
      valuetrail__pt1 = *(tagged_t **)(valuetrail__pt2 + valuetrail__i); \
      valuetrail__i += sizeof(tagged_t *) + VALUETRAIL__UNITPAD; \
      *valuetrail__pt1 = *(tagged_t *)(valuetrail__pt2 + valuetrail__i); \
      valuetrail__i += sizeof(tagged_t); \
    } while (valuetrail__i < InitialValueTrail); \
    w->value_trail = InitialValueTrail; \
  } \
})

#define VALUETRAIL__UNIT ((sizeof(tagged_t *) + sizeof(tagged_t) + VALUETRAIL__UNITPAD))

/* note: AMOUNT size in bytes, consider VALUETRAIL__UNIT bytes per entry */
#define VALUETRAIL__TEST_OVERFLOW(AMOUNT) ({ \
  intmach_t amount; \
  amount = (AMOUNT)-w->value_trail; \
  TEST_CHOICE_OVERFLOW(w->choice, amount); \
})

/* ------------------------------------------------------------------------- */
/* Liveinfo */

#define LIVEINFO__SIZE (FTYPE_size(f_l) + FTYPE_size(f_i))
#define LIVEINFO__HEAP(P) BCOp((P), FTYPE_ctype(f_l), 0)
#define LIVEINFO__ARITY(P) BCOp((P), FTYPE_ctype(f_i), FTYPE_size(f_l))
#define LIVEINFO__INIT(P, HEAP, ARITY) ({ \
      char *ptr = (P); /* conform to strict-aliasing rules */	\
      LIVEINFO__HEAP(ptr) = (HEAP);				\
      LIVEINFO__ARITY(ptr) = (ARITY);				\
    })
typedef char liveinfo_t[LIVEINFO__SIZE];

/* ------------------------------------------------------------------------- */
/* Instances */

/* Clauses of compiled predicates are stored as a linked list of
   records. The forward link of the last clause contains the total number
   of clauses. */

/* All invocations looking at an instance of an concurrent predicate
   will actually have a pointer to a pointer to the instance.  Every
   clause has a pointer to a queue of handles to itself.  Erasing a
   pointed to instance will change the pointer to the instance
   itself */



/* Terms recorded under a key or clauses of an interpreted predicate
   are stored as a doubly linked list of records.  The forward link is
   terminated by NULL; the backward link wraps around.  Each instance
   points back to the beginning of the list.  The rank field defines a
   total order on a list.  Two timestamps are associated with each
   instance to support proper semantics for dynamic code
   updates. (MCL, with help from the manual).  There are also two
   pointers (one for unindexed and other for indexed accesses) to
   queues which maintain the list of calls looking at each
   instance. */

#if defined(USE_THREADS)
#if !defined(ABSMACH_OPT__atom_locks)
#error "threads require atom_locks"
#endif
#endif

/* Information about atoms */

/* For a given goal and an emulated predicate, alternatives that might match
   the goal are stored as a linked list of records */

/* try-retry-trust repr. as a linked list. */

#define TRY_NODE_IS_NULL(T) (((T)==NULL) || ((T)==fail_alt))
#define TRY_NODE_SET_DET(T, REF) ({ \
  (T)->altop = OPCODEenc(restore_all_no_alt); \
  (T)->code = (char *)BCoff((REF)->emulcode, BCOp((REF)->emulcode, FTYPE_ctype(f_i), 0)); \
  (T)->next = NULL; \
})

/* ------------------------------------------------------------------------- */
/* Dynamic stacks of integers */

#define DYNSTACK_TYPE(T) dynstack__##T
#define DYNSTACK_OP(T, OP) dynstack__##OP##__##T
#define DYNSTACK_TEMPLATE(T) \
typedef struct _dynstack__##T { \
  T *base; \
  intmach_t top; \
  intmach_t limit; \
} DYNSTACK_TYPE(T); \
void DYNSTACK_OP(T, init) (DYNSTACK_TYPE(T) *stack, intmach_t size) { \
  stack->base = CHECKALLOC_ARRAY(T, size); \
  stack->top = 0; \
  stack->limit = size; \
} \
void DYNSTACK_OP(T, free) (DYNSTACK_TYPE(T) *stack) { \
  CHECKDEALLOC0_ARRAY(T, stack->base, stack->limit); \
} \
void DYNSTACK_OP(T, expand) (DYNSTACK_TYPE(T) *stack) { \
  stack->base = CHECKREALLOC0_ARRAY(T, stack->base, stack->limit, 2 * stack->limit); \
  stack->limit *= 2; \
} 
#define DYNSTACK_ENSURE(T, STACK, AMOUNT) \
  { if ((STACK)->top + (AMOUNT) >= (STACK)->limit) DYNSTACK_OP(T, expand) ((STACK)); }
#define DYNSTACK_ELEMENT(T, STACK, I) (&(STACK)->base[(I)])
#define DYNSTACK_GET_TOP(T, STACK) ((STACK)->top) 
#define DYNSTACK_SET_TOP(T, STACK, I) { (STACK)->top = (I); }
#define DYNSTACK_PUSH(T, STACK, REF) \
  { DYNSTACK_ENSURE(T, STACK, 1); (REF) = &(STACK)->base[(STACK)->top++]; }
#define DYNSTACK_FREE(T, STACK) DYNSTACK_OP(T, free) ((STACK))
#define DYNSTACK_INIT(T, STACK, SIZE) DYNSTACK_OP(T, init) ((STACK), (SIZE))

/* ------------------------------------------------------------------------- */
/* Hash tables (hashtab_t) */

#define HASHTAB_SIZE(X) (((X)->mask/sizeof(hashtab_node_t))+1) 
#define HASHTAB_SIZE2MASK(X) (((X)-1)*sizeof(hashtab_node_t))

typedef tagged_t hashtab_key_t;

/* get the hash code of a tagged_t as a intmach_t */
#if defined(ABSMACH_OPT__tagged64) && !defined(ABSMACH_OPT__pointer64)
/* sum bits in first and second word of a tagged_t to obtain a intmach_t */
#define HASHTAGGED(X) ((intmach_t)((X)>>32)+(intmach_t)(X))
#else
/* a tagged_t and a intmach_t have the same size */
#define HASHTAGGED(X) (X)
#endif

#define HASHNODE(TAB, OFFSET) \
  ((hashtab_node_t *)(((char *)&(TAB)->node[0]) + (OFFSET)))

/* TODO: use these macros in other parts ... */
#define HASHTAB_LOOKUP(SW, KEY, NODE, FOUND_CODE, NEW_CODE) ({ \
  __label__ htl__again; \
  hashtab_node_t *NODE; \
  intmach_t htl__i; \
  uintmach_t htl__mask; \
  uintmach_t htl__t0; \
htl__again: \
  htl__mask = (SW)->mask; \
  for (htl__i = 0, htl__t0 = HASHTAGGED((KEY)) & htl__mask; \
       ; \
       htl__i += sizeof(hashtab_node_t), \
       htl__t0 = (htl__t0 + htl__i) & htl__mask) { \
    NODE = HASHNODE((SW), htl__t0); \
    if (NODE->key == (KEY)) { \
      FOUND_CODE \
      break; \
    } else if (NODE->key == 0) { \
      if (((SW)->count + 1) * 2 > HASHTAB_SIZE((SW))) { \
        HASHTAB_EXPAND(&(SW)); \
        goto htl__again; \
      } \
      (SW)->count++; \
      NODE->key = (KEY); \
      NEW_CODE \
      break; \
    } \
  } \
})

#define HASHTAB_ITERATE(SW, NODE, CODE) ({ \
  hashtab_node_t *NODE; \
  intmach_t hti__i; \
  intmach_t hti__size; \
  hti__size = HASHTAB_SIZE((SW)) * sizeof(hashtab_node_t); \
  for (hti__i = 0; \
       hti__i < hti__size; \
       hti__i += sizeof(hashtab_node_t)) { \
    NODE = HASHNODE((SW), hti__i); \
    if (NODE->key != 0) { \
      CODE \
    } \
  } \
})

/* TODO: define parametric hash tables (for different types of keys and values)  */
#define HASHTAB_NEW(SIZE) hashtab_new((SIZE), (tagged_t)ERRORTAG)
#if defined(ABSMACH_OPT__tagged64) && !defined(ABSMACH_OPT__pointer64)
#define PUT_POINTER_IN_UNION_TAGGED(X) (tagged_t)(Put32InUnion64((uint32_t)(X)))
#else
#define PUT_POINTER_IN_UNION_TAGGED(X) (tagged_t)(X)
#endif
#define HASHTAB_NEW_DEF(SIZE, OTHERWISE) hashtab_new((SIZE), PUT_POINTER_IN_UNION_TAGGED(OTHERWISE))
hashtab_t *hashtab_new(intmach_t size, tagged_t otherwise);
hashtab_node_t *hashtab_get(hashtab_t *sw, hashtab_key_t key);
#define HASHTAB_EXPAND(PSW) hashtab_expand((PSW), (tagged_t)ERRORTAG)
#define HASHTAB_EXPAND_DEF(PSW, OTHERWISE) hashtab_expand((PSW), PUT_POINTER_IN_UNION_TAGGED(OTHERWISE))
void hashtab_expand(hashtab_t **psw, tagged_t otherwise);
void hashtab_clear(hashtab_t *sw);
hashtab_node_t *hashtab_lookup(hashtab_t **swp, hashtab_key_t k);
void hashtab_free(hashtab_t *sw);

/* ------------------------------------------------------------------------- */
/* Indexing table are used in indexing on first argument in calls and in
   looking up predicates.  They are operated as hash tables with quadratic
   overflow handling.  Hash table access is performed by using some of the
   low order bits of the key as array index, and then searching for a hit or
   for a zero key indicating that the key is absent from the table. 
   
   MCL: changed to make room for erased atoms: now a key == 1 indicates
   that the entry has been erased (but the search chain continues).  New 
   atoms are placed in the first free entry.

   NOTE (MCL): data structure different from that in Sicstus 1.8!!
*/

void incore_puthash(hashtab_t **psw,
		    intmach_t effar,
		    emul_info_t *current,
		    hashtab_key_t k);
void incore_insert(try_node_t **t0,
		   intmach_t effar,
		   emul_info_t *ref);
try_node_t *incore_copy(try_node_t *from);

void update_predtyp(definition_t *d);
void set_predtyp(definition_t *d, intmach_t type);
void set_predtyp_indexed(definition_t *d);

/* Classified somewhere else */
extern hashtab_node_t **atmtab;
extern hashtab_t *prolog_atoms;

void free_emulinfo(emul_info_t *cl);
void leave_to_gc(intmach_t type, char *info); /* TODO: move this code from emulator.c to registers.c and change the names? */
intmach_t copy_blob(tagged_t *src, tagged_t *dst);
void relocate_clocks(instance_t *inst, instance_clock_t *clocks);
void expunge_instance(instance_t *i);
void relocate_gcdef_clocks(instance_clock_t *clocks);
void move_queue(instance_handle_t **srcq, 
                instance_handle_t **destq,
                instance_t *destinst);
void jump_to_next_instance(instance_t *x2_p_insp,
                           instance_t *x5_p_insp,
                           instance_t **ipp,
                           instance_t **x2,
                           instance_t **x5);
bool_t prolog_init_radix(void);
void add_definition(hashtab_node_t *node, tagged_t key, definition_t *def);

#define TaggedToInstance(X) TermToPointerOrNull(instance_t, X)
#define TaggedToInstHandle(X) TermToPointerOrNull(instance_handle_t, X)
#define TaggedToInstancePtr(X) TermToPointerOrNull(instance_t *, X)
#define TaggedToRoot(X) TermToPointer(int_info_t, X)
#define TaggedToEmul(X) TermToPointer(emul_info_t, X)
#define TaggedToFunctor(X) TermToPointer(definition_t, X)

extern hashtab_t *modules_location; /* Shared */
extern hashtab_t *predicates_location; /* Shared */
#if defined(ABSMACH_OPT__oo_extensions)
extern hashtab_t *objfunctor_table; /* Shared */
#endif

/* Database locks */
extern SLOCK prolog_predicates_l;

/* Wait until new worker Id is generated */
extern SLOCK worker_id_pool_l;
extern SLOCK atom_id_l;
extern SLOCK wam_list_l;

#if defined(DEBUG_TRACE)
extern SLOCK    ops_counter_l;
#endif

/* Non-shared? --- set by each worker to decide whether re-initialize or exit after a failure */
extern bool_t in_abort_context;

extern char symbolchar[];               /* Shared --- type of each symbol */

extern char *installibdir;

/* All atom & functor definitions are shared */

extern tagged_t functor_lst;
extern tagged_t functor_slash;
extern tagged_t functor_Dref;
extern tagged_t functor_Dstream;
extern tagged_t functor_Dsetarg;
extern tagged_t functor_active;
extern tagged_t functor_pending;
extern tagged_t functor_failed;
extern tagged_t functor_available;

extern definition_t *address_true;
extern definition_t *address_fail;
extern definition_t *address_call;
extern definition_t *address_metacall;
extern definition_t *address_undefined_goal;
extern definition_t *address_help; 
extern definition_t *address_restart; 
extern definition_t *address_trace;
extern definition_t *address_getct;
extern definition_t *address_getct1;
extern definition_t *address_get;
extern definition_t *address_get2;
extern definition_t *address_get1;
extern definition_t *address_get12;
extern definition_t *address_peek;
extern definition_t *address_peek2;
extern definition_t *address_skip;
extern definition_t *address_skip2;
extern definition_t *address_skip_line;
extern definition_t *address_skip_line1;
extern definition_t *address_error;

extern tagged_t current_prompt;
extern tagged_t current_unknown;
extern tagged_t current_ferror_flag;
extern tagged_t current_quiet_flag;
extern tagged_t current_radix;

#if defined(ABSMACH_OPT__atom_len)
atom_t *new_atom_check(unsigned char *str, uintmach_t str_len, uintmach_t index);
#else
atom_t *new_atom_check(unsigned char *str, uintmach_t index);
#endif
CVOID__PROTO_N(init_number, char *string, tagged_t *p);

void glb_init_each_time();
void init_symbolchar();
CVOID__PROTO(init_once);
void init_locks();
void init_streams();
void init_float_to_str();
CBOOL__PROTO(init_dynlink);

#define STATICMAXATOM 1024     /* Avoid very long atoms inside the engine */

#define MAXATOM Atom_Buffer_Length

/* Local space to generate atoms and other general string operations */
#define Atom_Buffer          w->atom_buffer
#define Atom_Buffer_Length   w->atom_buffer_length

#define EXPAND_ATOM_BUFFER(new_max_atom_length) ({ \
  Atom_Buffer = CHECKREALLOC0_ARRAY(char, Atom_Buffer, Atom_Buffer_Length, new_max_atom_length); \
  Atom_Buffer_Length = new_max_atom_length; \
})

/* get a pointer to the atom buffer of at least LEN size, expand it
   before if required  */

#define GET_ATOM_BUFFER(S, LEN) ({ \
  if ((LEN) > Atom_Buffer_Length) { \
    EXPAND_ATOM_BUFFER((LEN)); \
  } \
  (S) = Atom_Buffer; \
})

/* double atom buffer size if required */
#define ENSURE_ATOM_BUFFER(LEN, CODE) ({ \
  if ((LEN) >= Atom_Buffer_Length) { \
    EXPAND_ATOM_BUFFER((LEN)*2); \
    CODE \
  } \
})

/* ------------------------------------------------------------------------- */
/* worker */
 
/* There are some registers which whould be private to each worker.  Those
   which are not critical for speed are subindirected in their own blocks.
   Others I think can affect performance appear just at the worker level.
   The reason for this difference is that changing the size of the struct
   worker causes a real mess in the whole compiler, and a bootstrapping
   procedure is needed.  Thus, if new no critical registers are needed, they
   can be allocated inside of one of these blocks, and this does not change
   the size of the worker_t itself. */

#define G (&w->g)

/* Access macros for the principal WAM registers, bytecode offsets, etc. */

#define Xb(I) (*CharOffset(w,(I))) /* I as bytecode operand */
#define X(I) (G->x[(I)]) /* I as zero-based */

#if defined(ABSMACH_OPT__oo_extensions)
/* TODO: I am not sure about this... */
#define MTHIS X(1)
#define MX0 X(0)
#define MX1 X(2)
#define MX2 X(3)
#define MX3 X(4)
#define MX4 X(5)
#define MX5 X(6)
#define MX6 X(7)
#define MX7 X(8)
#endif

/* initial choicepoint */
#define InitialChoice ChoiceNext0(Choice_Start, 1)

/* for tracing: */
#define X_OffsetToIndex(O) (((O)-sizeof(worker_t))/sizeof(tagged_t))
#define Y_OffsetToIndex(O) (((O)-sizeof(frame_t))/sizeof(tagged_t))
#define FrameSizeToCount(O) (((O)-sizeof(frame_t))/sizeof(tagged_t))

/* These are related to the I/O pointers */

#define Input_Stream_Ptr w->streams->input_stream_ptr
#define Output_Stream_Ptr w->streams->output_stream_ptr

/* Root of global logical variables (a structure) */

#define GLOBAL_VARS_ROOT w->global_vars_root

/* Enable/disable debugging */

#define Current_Debugger_Mode w->debugger_mode

/* TODO: document */

#define Next_Worker(w) (w->misc->next_worker)

#define Stop_This_Goal(w) (w->misc->stop_this_goal)

/* The local (per-thread) definitions for garbage collection */

#define Gc_Total_Grey    (w->misc->gc_total_grey)
#define Gcgrey           (w->misc->gcgrey)
#define Total_Found      (w->misc->total_found)    
#define Cvas_Found       (w->misc->cvas_found)     
#define Gc_Aux_Choice    (w->misc->gc_aux_choice)    
#define Gc_Choice_Start  (w->misc->gc_choice_start)
#define Gc_Heap_Start    (w->misc->gc_heap_start)
#define Gc_Stack_Start   (w->misc->gc_stack_start)

/* Topmost choicepoint for calls to concurrent facts. */

#define TopConcChpt     w->top_conc_chpt
#define TopConcChptFun() w->top_conc_chpt

/* Throwing exceptions */

#define ERR__FUNCTOR(NAME, ARITY) \
  static char *const err__name = NAME; static const intmach_t err__arity = ARITY;

#define ErrArgNo w->misc->errargno
#define ErrCode w->misc->errcode
#define ErrFuncName w->misc->errfuncname
#define ErrFuncArity w->misc->errfuncarity
#define Culprit w->misc->culprit

#define BUILTIN_ERROR(Code,Culpr,ArgNo) ({ \
  ErrCode = Code; \
  ErrFuncName = err__name; \
  ErrFuncArity = err__arity; \
  ErrArgNo = ArgNo; Culprit = Culpr; \
  EXCEPTION__THROW; \
})

#define ERROR_IN_ARG(Arg,ArgNo,ReqType) ({ \
  BUILTIN_ERROR(IsVar(Arg) ? INSTANTIATION_ERROR : TYPE_ERROR(ReqType), \
                Arg, ArgNo); \
})

/* in B: latest choice point, out A: top of the stack */
#if defined(ABSMACH_OPT__lazy_localtop)
#define ValidLocalTop() (G->local_top != NULL)
#endif

/* Obtain the frame size stored in a next_insn */
#define FrameSize(L) BCOp((L), FTYPE_ctype(f_e), -FTYPE_size(f_e))

/* Frame size in bytes, given the arity */
#define FrameSize0(ARITY) (sizeof(frame_t) + ((ARITY) * sizeof(tagged_t)))

#define FrameNewTop(E, ARITY) ((frame_t *)StackCharOffset((E), FrameSize0((ARITY))))

/* ------------------------------------------------------------------------- */
/* Events */

//public
#define TestEventOrHeapWarnOverflow(H) OffHeaptop((H),Heap_Warn_Soft)
#define TestEvent() (Heap_Warn_Soft <= Heap_Start)
#define UnsetEvent() { Heap_Warn_Soft = Heap_Warn(CALLPAD); }
#define TestCIntEvent() ( Int_Heap_Warn == Heap_Start )
#define SetCIntEvent() ({ \
  SetEvent(); \
  Int_Heap_Warn = Heap_Start; \
})
#define UnsetCIntEvent() { Int_Heap_Warn = Heap_Warn(CALLPAD); }
/* Equivalent to: if (TestCIntEvent()) SetWakeCount(0); else UnsetEvent(); */ 
#define ResetWakeCount() ({ \
  Heap_Warn_Soft = Int_Heap_Warn; \
})
#define IncWakeCount() ({ \
  SetEvent(); \
  Heap_Warn_Soft = HeapCharOffset(Heap_Warn_Soft,-1); \
})
#define WakeCount() (TestEvent() ? HeapCharDifference(Heap_Warn_Soft,Heap_Start) : 0)
/* make WakeCount()==X (pre: TestEvent()) */
#define SetWakeCount(X) ({ \
  Heap_Warn_Soft = HeapCharOffset(Heap_Start,-(X)); \
})
//private
#define SetEvent() ({ \
  if (!TestEvent()) { \
    SetWakeCount(0); \
  } \
}) 
#define Int_Heap_Warn (w->int_heap_warn)

/* Prolog debugger support */
/* TODO: move to options... */
#define USE_PROLOG_DEBUGGER 1
#if defined(USE_PROLOG_DEBUGGER)
/* TODO: Hmm, one per wam? */
extern bool_t debug_mode;
extern intmach_t debug_status;
#endif

/* ------------------------------------------------------------------------- */
/* Goal desc */

/* The goal descriptors are held together in a doubly linked circular
   list; there is a pointer to the list, which points always to a free
   goal descriptor (if there is any in the list).  All the free goal
   descriptors can be found following the forward link of this initial
   pointer.  */

worker_t *free_wam();
worker_t *create_wam_storage();
CVOID__PROTO(create_wam_areas);
CVOID__PROTO(release_wam);

CBOOL__PROTO(stack_shift_usage);
CBOOL__PROTO(termheap_usage);
CBOOL__PROTO(envstack_usage);
CBOOL__PROTO(choice_usage);
CBOOL__PROTO(trail_usage);

CVOID__PROTO(reinitialize_stacks);

void init_symbol_tables();

definition_t *query_predicate(char *name, intmach_t arity);
void set_defbits(char *name, intmach_t arity, intmach_t bits);
#define register_cbool(NAME, ARITY, PROC) register_cbool__2((NAME), (ARITY), (void *)(PROC))
definition_t *register_cbool__2(char *name, intmach_t arity, void *procedure);
#define register_cinsnp(NAME, ARITY, PROC) register_cinsnp__2((NAME), (ARITY), (void *)(PROC))
definition_t *register_cinsnp__2(char *name, intmach_t arity, void *procedure); 
#define register_cvoid(NAME, ARITY, PROC) register_cvoid__2((NAME), (ARITY), (void *)(PROC))
definition_t *register_cvoid__2(char *name, intmach_t arity, void *procedure); 
void unregister_cbool(char *name, intmach_t arity);
void unregister_cinsnp(char *name, intmach_t arity);
void unregister_cvoid(char *name, intmach_t arity);
#define register_builtin(ATOM, ARITY, PROC) register_builtin__2(ATOM, ARITY, (void *)PROC)
void register_builtin__2(char *atom, intmach_t arity, void *proc);
#define register_ctagged(ATOM, ARITY, PROC) register_ctagged__2(ATOM, ARITY, (void *)PROC)
void register_ctagged__2(char *atom, intmach_t arity, void *proc);

/* tagged->intmach hashtab (for compilation to C) */
hashtab_t *register_sw_on_tagged(tagged_t *taggeds, intmach_t length);
intmach_t tagged_hashtab_get(hashtab_t *sw, tagged_t k);

CVOID__PROTO(init_streams_each_time);
CVOID__PROTO(local_init_each_time);

extern char **prolog_argv;
extern int prolog_argc;

extern statistics_t stats;                        /* Shared, I guess */

/* Emulator */

void reinit_list(goal_descriptor_t *goal);
void init_goal_desc_list();
/*int kill_thread(goal_descriptor_t *goal_to_kill);*/
void allow_thread_cancellation();
void disallow_thread_cancellation();
CFUN__PROTO(attach_me_to_goal_desc_list, goal_descriptor_t *);
CVOID__PROTO_N(associate_wam_goal, goal_descriptor_t *goal_desc);
CVOID__PROTO(print_task_status);
void make_goal_desc_free(goal_descriptor_t *goal);
void release_goal_desc(goal_descriptor_t *goal_desc);
goal_descriptor_t *init_first_gd_entry(void);
goal_descriptor_t *gimme_a_new_gd(void);
goal_descriptor_t *look_for_a_free_goal_desc(void);
worker_t *get_my_worker();
void enqueue_thread(THREAD_T thread);
void unlink_wam(goal_descriptor_t *goal);

CBOOL__PROTO(prolog_eng_kill);
CBOOL__PROTO(prolog_eng_killothers);
CBOOL__PROTO(prolog_eng_wait);
CBOOL__PROTO(prolog_eng_self);
CBOOL__PROTO(prolog_eng_status);
CBOOL__PROTO(prolog_eng_status1);
CBOOL__PROTO(prolog_eng_release);
CBOOL__PROTO(prolog_eng_call);
CBOOL__PROTO(prolog_eng_backtrack);
CBOOL__PROTO(prolog_eng_cut);

CBOOL__PROTO_N(bu1_detach_attribute, tagged_t x);
CBOOL__PROTO_N(bu2_attach_attribute, tagged_t var, tagged_t constr);
CBOOL__PROTO_N(bu2_update_attribute, tagged_t x, tagged_t constr);
CVOID__PROTO(collect_one_pending_unification);
CVOID__PROTO_N(collect_pending_unifications, intmach_t wake_count);

extern intmach_t num_of_predicates;

CBOOL__PROTO(prompt);
CBOOL__PROTO(unknown);
CBOOL__PROTO(metachoice);
CBOOL__PROTO(metacut);
CBOOL__PROTO(retry_cut);
CBOOL__PROTO(setarg);
CBOOL__PROTO(undo);
CBOOL__PROTO(frozen);
CBOOL__PROTO(defrost);
CBOOL__PROTO(debugger_state);
CBOOL__PROTO(debugger_mode);
CBOOL__PROTO(compiling);
CBOOL__PROTO(ferror_flag);
CBOOL__PROTO(quiet_flag);
CBOOL__PROTO(spypoint);
CBOOL__PROTO(prolog_radix);
CBOOL__PROTO(constraint_list);
CFUN__PROTO_N(find_constraints, intmach_t, tagged_t *limit);
CBOOL__PROTO(prolog_eq);
CBOOL__PROTO(blob_data);

#define CONTCODE(Arity) BCoff(contcode, (Arity) * (FTYPE_size(f_e) + FTYPE_size(f_o)))
extern bcp_t contcode;

extern bcp_t bootcode;
extern bcp_t startgoalcode;
extern bcp_t failcode;
extern bcp_t exitcode;
extern try_node_t *termcode;
extern try_node_t *fail_alt;
extern instance_clock_t def_clock, use_clock;
extern hashtab_t *switch_on_builtin;
extern hashtab_t *switch_on_function;
extern definition_t *int_address;
extern char *emulator_version;
extern char incremental_symbol_table_path[];

CVOID__PROTO(init_some_bytecode);

void updateChoicepoints(intmach_t decrement);
CVOID__PROTO_N(trail_push_check, tagged_t x);

/* SERIOUS_FAULT - a fault that should not occur- indicating a corruption
                  such as following the STR tag not coming to a FNT tag
		  this kind of fault may not need to be testing in final
		  version but must in testing cause a total abort
   USAGE_FAULT   - a fault in the usage(incorrect parameters) of a 
                  builtin predicate - an error message is written.
   MINOR_FAULT   - a fault that should result in a error message being
                  written somewhere, but the builtin predicate just
		  fails and is not aborted
*/

/* Exit code from wam() when aborting */
#define WAM_ABORT -32768 /* see exceptions.pl */
#define WAM_INTERRUPTED -32767

extern JMP_BUF abort_env;

CFUN__PROTO_N(call_firstgoal, intmach_t, tagged_t goal, goal_descriptor_t *goal_desc);

/* --------------------------------------------------------------------------- */

#define NEVAL(Reg, Exit) ({ \
  if (!IsNumber(Reg)) { \
    Reg = CFUN__EVAL_N(evaluate, Reg); \
    if (!IsNumber(Reg)) { \
      Exit; \
    } \
  } \
})

stream_node_t *insert_new_stream(stream_node_t *new_stream);

flt64_t blob_to_flt64(tagged_t t);
int32_t blob_to_int32(tagged_t t);
int64_t blob_to_int64(tagged_t t);
CFUN__PROTO(bn_finish, tagged_t);
CFUN__PROTO_N(bn_call1, tagged_t, intmach_t (*f) (/* ??? */), tagged_t x);
CFUN__PROTO_N(bn_call2, tagged_t, intmach_t (*f) (/* ??? */), tagged_t x, tagged_t y);
CFUN__PROTO_N(bn_from_float_check, tagged_t, flt64_t f);
CFUN__PROTO_N(flt64_to_blob_nocheck, tagged_t, flt64_t i);
CFUN__PROTO_N(flt64_to_blob_check, tagged_t, flt64_t i);
#if !defined(ABSMACH_OPT__tagged32)
CFUN__PROTO_N(int32_to_blob_nocheck, tagged_t, int32_t i);
CFUN__PROTO_N(int32_to_blob_check, tagged_t, int32_t i);
#endif
CFUN__PROTO_N(int64_to_blob_nocheck, tagged_t, int64_t i);
CFUN__PROTO_N(int64_to_blob_check, tagged_t, int64_t i);
CFUN__PROTO_N(make_blob, tagged_t, tagged_t *ptr);
bool_t compare_blob(tagged_t *a, tagged_t *b);
CFUN__PROTO_N(make_structure, tagged_t, tagged_t functor);
intmach_t init_atom_check(char *str);
definition_t *find_definition(tagged_t term, tagged_t **argl, bool_t insertp);
definition_t *resolve_definition(tagged_t complex);
CVOID__PROTO_N(failc, char *);

#if defined(ABSMACH_OPT__p_in_reg)
char *preg asm("bx");
#define P preg
#else
#define P p
#endif
#define H pt2
#define S pt2
#define E frame

#define LoadH (H = G->heap_top)
#define StoreH (G->heap_top = H)

#define FAIL_INSNP ((bcp_t)(&G->next_alt->altop))
#define SUCCESS_INSNP (G->next_insn)
extern bcp_t proceed_code;
#define DEF_INSNP(FUNC) ((bcp_t)(&(FUNC)->enterop))

#define DEBUG_CREATE_CHOICE ({ \
  DEBUG__TRACE(debug_choicepoints, \
               "created choicepoint %p\n", w->choice); \
})

#define FAIL_DEBUG \
  RTCHECK({if ((TopConcChpt < w->choice) && \
           (TopConcChpt < w->previous_choice)) \
           TRACE_PRINTF("********** what happened here?\n");})

#define FAIL_HOOK() ({ \
  DEBUG__TRACE(debug_choicepoints, \
               "fail: choice = %lx, previous_choice = %lx, conc. choice = %lx\n", \
               (long)w->choice, (long)w->previous_choice, (long)TopConcChpt); \
  FAIL_DEBUG; \
})

#define DEBUG_DEEP_BACKTRACKING()  \
  DEBUG__TRACE(debug_choicepoints, \
               "deep backtracking, choice = %lx\n", (long)w->choice)

#define DEBUG_CLEANUP(TopCChpt, TopChoice) \
  DEBUG__TRACE(debug_concchoicepoints, \
               "cut: removing chains (%lx to %lx)\n", \
               (long)TopCChpt, (long)TopChoice)
#define DEBUG_CUT(NewChoice) \
  DEBUG__TRACE(debug_choicepoints, \
               "cut: new chpt = %lx\n", (long)NewChoice)

void remove_link_chains(choice_t **topdynamic, choice_t *chpttoclear);

#if defined(USE_THREADS)
#define ConcChptCleanUp(TopCChpt, TopChoice) ({ \
  if (ChoiceYounger(TopCChpt, TopChoice)) { \
     DEBUG_CLEANUP(TopCChpt, TopChoice); \
     remove_link_chains(&TopCChpt, TopChoice); \
  } \
})
#else
#define ConcChptCleanUp(TopCChpt, TopChoice)
#endif

/* ------------------------------------------------------------------------- */

/* copy the trail segment between [src..choicepoint->trail_top] to
   [dest..], removing 0's and updating the choicepoint new
   trail_top */
#if defined(EXTGC)
#define COMPRESS_TRAIL(CP, SRC, DEST) ({ \
  tagged_t *limit; \
  limit = (CP)->trail_top; \
  while (TrailYounger(limit,TG_av(SRC))) { \
    tagged_t ref; \
    ref = *TG_av(SRC); \
    if (ref != (tagged_t)0) { \
      TG_Put(ref, DEST); \
      CopyGcBits(SRC,DEST); \
      TGavag__inc(DEST, TrailDir); \
    } \
    TGavag__inc(SRC, TrailDir); \
  } \
  (CP)->trail_top = TG_av(DEST); \
})
#else
#define COMPRESS_TRAIL(CP, SRC, DEST) ({ \
  tagged_t *limit; \
  limit = (CP)->trail_top; \
  while (TrailYounger(limit,TG_av(SRC))) { \
    tagged_t ref; \
    ref = *TG_av(SRC); \
    TGav__inc(SRC, TrailDir); \
    if (ref != (tagged_t)0) { \
      TGav__push(DEST,ref,TrailDir); \
    } \
  } \
  (CP)->trail_top = TG_av(DEST); \
})
#endif

#define CompressTrailNoGC(tr0) ({ \
  TGav__decl(h); \
  TGav__decl(tr); \
  TGav__set(tr, tr0); \
  TGav__set(h, TG_av(tr)); \
  COMPRESS_TRAIL_NOGC(G, tr, h); \
})
#define COMPRESS_TRAIL_NOGC(CP, SRC, DEST) ({ \
  tagged_t *limit; \
  limit = (CP)->trail_top; \
  while (TrailYounger(limit,TG_av(SRC))) { \
    tagged_t ref; \
    ref = *TG_av(SRC); \
    TGav__inc(SRC, TrailDir); \
    if (ref != (tagged_t)0) { \
      TGav__push(DEST,ref,TrailDir); \
    } \
  } \
  (CP)->trail_top = TG_av(DEST); \
})

/* TODO: why (AMOUNT)*2?? debug */
#define TEST_HEAP_OVERFLOW(H, AMOUNT, ARITY) ({ \
  if (HeapCharAvailable((H)) < (AMOUNT)) { \
    G->heap_top = (H); \
    CVOID__CALL_N(explicit_heap_overflow, (AMOUNT)*2, (ARITY)); \
    (H) = G->heap_top; \
  } \
})

#define IsDeep() (G->flags == 0)

#define IsShallowTry() (G->flags == 1)

#define SetDeep() { G->flags = 0; }
#define SetShallowTry() { G->flags = 1; }
#define SetShallowRetry() { G->flags = 2; }

#define TEST_CHOICE_OVERFLOW(B, AMOUNT) ({ \
  if (ChoiceCharAvailable((B)) < (AMOUNT)) { \
    CVOID__CALL_N(choice_overflow, (AMOUNT), TRUE); \
  } \
})

CINSNP__PROTO(code_call1);

/* Stores Def+X[...] in a term */
/* note: This must not clobber  t2, X[*] */
#define EMUL_TO_TERM(DEF, TERM) ({ \
  if (FuncArity(DEF)==0) { \
    (TERM) = FuncName(DEF); \
  } else { \
    intmach_t arity; intmach_t i; \
    LoadH; \
    (TERM) = Tagp(STR,H); \
    HeapPush(H,FuncFunctor(DEF)); \
    arity = FuncArity(DEF); \
    for(i=0; i<arity; i++) \
      { tagged_t t1; t1=X(i); Unify_local_value(t1,H); } \
    StoreH; \
  } \
})

CBOOL__PROTO_N(cunify_args_aux, intmach_t arity, tagged_t *pt1, tagged_t *pt2, tagged_t *x1, tagged_t *x2);
CBOOL__PROTO_N(cunify_aux, tagged_t x1, tagged_t x2);

typedef enum {BLOCK, NO_BLOCK} BlockingType;
CINSNP__PROTO_N(current_instance, int_info_t *root, BlockingType block);

CFUN__PROTO_N(active_instance, instance_t *, instance_t *i, intmach_t time, bool_t normal);

CBOOL__PROTO(make_bytecode_object);
CFUN__PROTO_N(active_instance, instance_t *, instance_t *i, intmach_t itime, bool_t normal);
CVOID__PROTO(clock_overflow);
CFUN__PROTO_N(active_instance_conc, instance_t *, instance_t *i, int_info_t *pred_root);

CBOOL__PROTO(empty_gcdef_bin);
CBOOL__PROTO(prolog_abolish);
CBOOL__PROTO_N(abolish, definition_t *f);              
CBOOL__PROTO(define_predicate);
CBOOL__PROTO(erase_clause);
CBOOL__PROTO(set_property);

CFUN__PROTO_N(evaluate, tagged_t, tagged_t v);

CBOOL__PROTO(prolog_atom_codes);
CBOOL__PROTO(prolog_atom_length);
CBOOL__PROTO(prolog_sub_atom);
CBOOL__PROTO(prolog_atom_concat);
CBOOL__PROTO(prolog_copy_term);
CFUN__PROTO_N(cross_copy_term, tagged_t, tagged_t remote_term);
CBOOL__PROTO(prolog_name);
CBOOL__PROTO(prolog_number_codes);
CVOID__PROTO_N(number_to_string, tagged_t term, intmach_t base);
CBOOL__PROTO_N(string_to_number, unsigned char *AtBuf, intmach_t base, tagged_t *strnum, intmach_t arity);

CBOOL__PROTO(program_usage);
CBOOL__PROTO(internal_symbol_usage);
CBOOL__PROTO(total_usage);
CBOOL__PROTO(statistics);

CVOID__PROTO(pop_choicept);
CVOID__PROTO_N(push_choicept, try_node_t *alt);
CBOOL__PROTO(nd_atom_concat);
CBOOL__PROTO(current_atom);
CBOOL__PROTO(nd_current_atom);
CBOOL__PROTO(current_clauses);
CBOOL__PROTO(current_stream);
CBOOL__PROTO(nd_current_stream);
CBOOL__PROTO(prolog_repeat);
CBOOL__PROTO(nd_repeat);
CBOOL__PROTO(current_predicate);
CBOOL__PROTO(nd_current_predicate);
CBOOL__PROTO(predicate_property);
CBOOL__PROTO(nd_predicate_property);
CBOOL__PROTO(first_instance);
CINSNP__PROTO(next_instance);

void wam(goal_descriptor_t *);

THREAD_RES_T startgoal(THREAD_ARG wo);
THREAD_RES_T make_backtracking(THREAD_ARG wo);

CFUN__PROTO_N(def_retry_cbool, try_node_t *, cbool_t proc, intmach_t arity);
CFUN__PROTO_N(def_retry_cinsnp, try_node_t *, cinsnp_t proc, intmach_t arity);
CFUN__PROTO_N(def_exec_cinsnp, bcp_t, cinsnp_t proc);
CFUN__PROTO_N(def_success_cinsnp, bcp_t, cinsnp_t proc, intmach_t frame_live_size);

/* ------------------------------------------------------------------------- */
/* gc */

extern bool_t current_gcmode;
#define GCTRACE__OFF 0
#define GCTRACE__TERSE 1
#define GCTRACE__VERBOSE 2
extern intmach_t current_gctrace;
extern intmach_t current_gcmargin;
#define GCMARGIN_CHARS ((intmach_t)(current_gcmargin*1024*(sizeof(tagged_t)/4)))

void init_gc();

CBOOL__PROTO(gc_usage);
CBOOL__PROTO(gc_mode);
CBOOL__PROTO(gc_trace);
CBOOL__PROTO(gc_margin);

CVOID__PROTO_N(choice_overflow, intmach_t pad, bool_t remove_trail_uncond);
CVOID__PROTO(stack_overflow);
CVOID__PROTO_N(heap_overflow, intmach_t pad);
CVOID__PROTO_N(collect_goals_from_trail, intmach_t wake_count);
CVOID__PROTO_N(explicit_heap_overflow, intmach_t pad, intmach_t arity);
CFUN__PROTO_N(handle_event, definition_t *, definition_t *def);
CVOID__PROTO_N(v__handle_event, intmach_t arity);

/* ------------------------------------------------------------------------- */
/* interrupt */

CVOID__PROTO(control_c_normal);
void enable_conditions();

/* ------------------------------------------------------------------------- */
/* resources */

intmach_t resources__get_variable(char *name, intmach_t default_value);

/* ------------------------------------------------------------------------- */
/* profile */

#if defined(ABSMACH_OPT__profile_calls)
extern bool_t profile;
extern bool_t prof_include_time;

void dump_profile(void);
void add_to_profiling(definition_t *functor);

bool_t profile__get_opt(const char *arg);
#endif

/* ------------------------------------------------------------------------- */
/* float_tostr */

#if defined(USE_LONG_DOUBLE)
typedef long double ENG_LFLT;
#else
typedef flt64_t ENG_LFLT;
#endif

#define IEEE754_ABS_EXP (IEEE754_MASK_EXPONENT >> IEEE754_SHIFT_EXPONENT)       /* 2047 */
#define IEEE754_MIN_EXP (IEEE754_MASK_EXPONENT >> (IEEE754_SHIFT_EXPONENT + 1)) /* 1023 */
#define IEEE754_BIA_EXP (IEEE754_MIN_EXP + IEEE754_MANTISSA_LENGTH)             /* 1075 */
#define IEEE754_MAX_EXP (IEEE754_BIA_EXP - 1)                                   /* 1074 */
#define IEEE754_FIX_BIT (1LL << IEEE754_MANTISSA_LENGTH)        /* 0x0010000000000000LL */

#define IEEE854_ABS_EXP (IEEE854_MASK_EXPONENT >> IEEE854_SHIFT_EXPONENT)
#define IEEE854_MIN_EXP (IEEE854_MASK_EXPONENT >> (IEEE854_SHIFT_EXPONENT + 1))
#define IEEE854_BIA_EXP (IEEE854_MIN_EXP + IEEE_854_MANTISSA_LENGTH)
#define IEEE854_MAX_EXP (IEEE854_BIA_EXP - 1)
#define IEEE854_FIX_BIT (1LL << IEEE854_MANTISSA_LENGTH)

extern flt64_t invlog2[37];
extern ENG_LFLT *powtable[];
extern ENG_LFLT *invpowtable[];

/* #define EXP_CHAR  36 */
/* #define FRAC_SEP  36 */

/* floating point char */
#define FLOAT_POINT '.'

#define MAX_EXP   1074
#define MIN_EXP   1023
#define HIDD_BIT  0x0010000000000000LL
#define SHR_EXP   52
extern intmach_t char_digit[256];
extern char digits_upper[];
extern char digits_lower[];
extern char exponents_upper[];
extern char exponents_lower[];

void fillchardigit();
void fillpowtable(intmach_t base);
void freepowtable(intmach_t base);
void fillallpowtable();
void freeallpowtable();
char * float_to_string(char* buffer, intmach_t precision, char format, flt64_t x, intmach_t b);
ENG_LFLT powl_int(intmach_t base, intmach_t exp);

/* ------------------------------------------------------------------------- */
/* Streams */

#include <stdio.h>

extern intmach_t prolog_force_interactive;

#define TaggedToStream(X) TermToPointer(stream_node_t, X)

/* Streams pointing to "user" -- should be shared */

extern stream_node_t *stream_user_input;                   /* Shared */
extern stream_node_t *stream_user_output;                  /* Shared */
extern stream_node_t *stream_user_error;                   /* Shared */

/* Stream for tracing (statistics, predicate trace, etc.) */

#if defined(USE_DEBUG_INSCOUNT) || \
    defined(USE_RTCHECKS) || \
    defined(DEBUG_TRACE) || \
    defined(PROFILE_STATS) || \
    defined(PROFILE_EVENTFREQ) || \
    defined(PROFILE_BLOCKFREQ) || \
    defined(USE_GCSTATS)
#if !defined(USE_TRACE_OUTPUT)
#define USE_TRACE_OUTPUT 1
#endif
#endif

#if defined(USE_TRACE_OUTPUT)
extern stream_node_t *stream_trace; /* Shared */
extern FILE *stream_trace_file; /* Shared */
#define TraceFile stream_trace_file
#define TRACE_PRINTF(...) ({ \
  char m_buf[2048]; \
  snprintf(m_buf, 2048, __VA_ARGS__); \
  print_string(stream_trace, m_buf); \
  fflush(stream_trace_file); \
})
#endif

/* root of the stream pointers -- shared */

extern LOCK stream_list_l;
extern stream_node_t *root_stream_ptr;            /* Shared & locked */

#define INC_COUNTS(ch,stream) ({ \
  if (ch == (intmach_t)'\n')  \
    stream->last_nl_pos = stream->char_count += 1, stream->nl_count++; \
  else \
    stream->char_count++; \
})

#define DELRET -5
#define PEEK   -4
#define GET    -3
#define GET1   -2
#define SKIPLN -1

void print_syserror(char *s);
void writechar(intmach_t ch, intmach_t i, stream_node_t *s);
intmach_t readchar(stream_node_t *s, intmach_t type, definition_t *pred_address);
void print_string(stream_node_t *stream, char *p);
void init_streams();
stream_node_t *new_stream(tagged_t name, char *mode, FILE *file);
stream_node_t *stream_to_ptr(tagged_t t, intmach_t mode);
stream_node_t *stream_to_ptr_check(tagged_t t, intmach_t mode, intmach_t *errcode);
void update_std_streams(void);
void update_stream(stream_node_t *s, FILE *file);

CFUN__PROTO_N(ptr_to_stream_noalias, tagged_t, stream_node_t *n);
CFUN__PROTO_N(ptr_to_stream, tagged_t, stream_node_t *n);

/* ------------------------------------------------------------------------- */
/* Initialization */

CBOOL__PROTO_N(load_module_pack, FILE *qfile);

void engine_set_opts(char **optv, int optc);
void engine_init();
CVOID__PROTO(engine_finish);

void engine_exit(int exit_code);

void init_timing();
void init_ciaolib();

/* ------------------------------------------------------------------------- */
/* spec small arithmetic */
/* TODO: share with definition in arithmetic.c!!! */

#define INCLUDE_SPEC_SMALL_ARITH 1
#if defined(INCLUDE_SPEC_SMALL_ARITH)
#define TYPE_RTCHECK(Type, X) \
  RTCHECK({if (!Type((X))) TRACE_PRINTF("{violated type assertion at %s:%d}", __FILE__, __LINE__);})

#define INLINE static inline

INLINE CFUN__PROTO_N(arith_small__mod_2, tagged_t, tagged_t t, tagged_t u) {
  intmach_t rem, denom;
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  denom = (intmach_t)(u-TaggedZero);
  rem = (intmach_t)(t-TaggedZero)%denom;
  CFUN__PROCEED(((denom > 0 && rem < 0) || (denom < 0 && rem > 0) ?
		 rem+denom : rem ) + TaggedZero);
}
INLINE CFUN__PROTO_N(arith_small__or_2, tagged_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(((t^ZMask)|(u^ZMask))^ZMask);
}
INLINE CFUN__PROTO_N(arith_small__and_2, tagged_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(((t^ZMask)&(u^ZMask))^ZMask);
}
INLINE CFUN__PROTO_N(arith_small__xor_2, tagged_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(t^u^TaggedZero);
}
INLINE CFUN__PROTO_N(arith_small__rem_2, tagged_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED((intmach_t)(t-TaggedZero)%(intmach_t)(u-TaggedZero)+TaggedZero);
}
INLINE CFUN__PROTO_N(arith_small__idivide_2, tagged_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  /* TODO: hmmm output is not a small int?? */
  CFUN__PROCEED(IntvalToTaggedCheck((intval_t)(t-TaggedZero)/(intval_t)(u-TaggedZero)));
}
INLINE CFUN__PROTO_N(arith_small__fdivide_2, tagged_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  /* TODO: hmmm output IS NOT a small int */
  CFUN__LASTCALL_N(flt64_to_blob_check, TaggedToFloat(t)/TaggedToFloat(u));
}
INLINE CFUN__PROTO_N(arith_small__times_2, tagged_t, tagged_t t, tagged_t u) {
  intmach_t st;
  intmach_t su;
  intmach_t stu;
  tagged_t tu;

  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);

  st = GetSmall(t);
  su = (intmach_t)(u-TaggedZero);
  stu = st*su;
  tu = ((tagged_t)stu)+TaggedZero;
      
  if (su==0 || (stu/su==st && TaggedIsSmall(tu))) {
    CFUN__PROCEED(tu);
  }
  RTCHECK({fprintf(stderr, "hmm\n"); }); /* TODO: fix! */
}
INLINE CFUN__PROTO_N(arith_small__minus_2, tagged_t, tagged_t t, tagged_t u) {
  tagged_t t1;
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  t1 = t-(u-TaggedZero);
  TYPE_RTCHECK(TaggedIsSmall, t1);
  CFUN__PROCEED(t1);
}
INLINE CFUN__PROTO_N(arith_small__plus_2, tagged_t, tagged_t t, tagged_t u) {
  tagged_t t1;
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  t1 = t+(u-TaggedZero);
  TYPE_RTCHECK(TaggedIsSmall, t1);
  CFUN__PROCEED(t1);
}
INLINE CFUN__PROTO_N(arith_small__sign_1, tagged_t, tagged_t t) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  CFUN__PROCEED(((t==TaggedZero) ? TaggedZero :
		 (t < TaggedZero) ? SmallAdd(TaggedZero,-1) :
		 SmallAdd(TaggedZero,1)));
}
INLINE CFUN__PROTO_N(arith_small__abs_1, tagged_t, tagged_t t) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  RTCHECK({if (t==TaggedLow) { abort(); }});
  if (t < TaggedZero) {
    CFUN__PROCEED(TaggedZero-(t-TaggedZero));
  } else {
    CFUN__PROCEED(t);
  }
}
INLINE CFUN__PROTO_N(arith_small__not_1, tagged_t, tagged_t t) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  CFUN__PROCEED(t^(TaggedIntMax-TaggedLow));
}
INLINE CFUN__PROTO_N(arith_small__add1_1, tagged_t, tagged_t t) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  RTCHECK({if (t==TaggedIntMax) { fprintf(stderr, "hmm\n"); }});
  CFUN__PROCEED(SmallAdd(t,1));
}
INLINE CFUN__PROTO_N(arith_small__sub1_1, tagged_t, tagged_t t) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  RTCHECK({if (t==TaggedLow) { fprintf(stderr, "hmm\n"); }});
  CFUN__PROCEED(SmallAdd(t,-1));
}
INLINE CFUN__PROTO_N(arith_small__plus_1, tagged_t, tagged_t t) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  CFUN__PROCEED(t);
}
INLINE CFUN__PROTO_N(arith_small__minus_1, tagged_t, tagged_t t) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  RTCHECK({if (t==TaggedLow) { fprintf(stderr, "hmm\n"); }});
  CFUN__PROCEED(TaggedZero-(t-TaggedZero));
}
INLINE CFUN__PROTO_N(arith_small__numle_2, bool_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(t<=u);
}
INLINE CFUN__PROTO_N(arith_small__numgt_2, bool_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(t>u);
}
INLINE CFUN__PROTO_N(arith_small__numge_2, bool_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(t>=u);
}
INLINE CFUN__PROTO_N(arith_small__numlt_2, bool_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(t<u);
}
INLINE CFUN__PROTO_N(arith_small__numne_2, bool_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(t!=u);
}
INLINE CFUN__PROTO_N(arith_small__numeq_2, bool_t, tagged_t t, tagged_t u) {
  TYPE_RTCHECK(TaggedIsSmall, t);
  TYPE_RTCHECK(TaggedIsSmall, u);
  CFUN__PROCEED(t==u);
}
#endif

