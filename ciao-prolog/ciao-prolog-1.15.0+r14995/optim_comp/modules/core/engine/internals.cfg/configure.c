/* Obtain the value of system and architecture parameters that do not
   change from run to run, so that those values can be static data
   during native code compilation */

#if defined(__svr4__) || defined(Solaris) || defined(DARWIN)
# include <unistd.h>                                            /* sbrk () */
# include <stdlib.h>                                         /* malloc() */
#else                                                            /* SunOS */
# include <sys/types.h>
# include <malloc.h>
#endif
#include <stdio.h>
#include <unistd.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <math.h>

typedef unsigned long int tagged_t; /* 32 or 64 bit */

/* mask for upper X bits reserved */
#define PTRTAGMASK(X) (((((tagged_t)1)<<(X))-1)<<((sizeof(tagged_t)*8)-(X)))
/* addressable space when X bits are reserved */
#define ADDRESSABLE(X) (1<<((sizeof(tagged_t)*8)-(X)))

#define MAX_UPPER_BITS 5

/* ------------------------------------------------------------------------- */
/* configure__fpbits:
 *
 * ENG_FLT_SIGNIF <int>: number of significant digits in a float
 * ENG_FLT_ROUND <float>: a number to be used for rounding purposes
 *   when floats are printed
 *  
 * (so that display(5.347) exactly shows 5.347)
 */

/*
  safe_addition is intended to force A and B to be stored prior to doing the
  addition of A and B , for use in situations where optimizers might hold
  one of these in a register.

  There is a problem with gcc (3.1, at least): when using -O3, which turns
  inlining on, the number of digits in the (decimal) mantissa returned is
  20, due to the inlining of the safe_addition function, and the further
  optimization this brings about.  In order to avoid this in general, I have
  put the volatile keyword which tells the optimizer not to throw away
  references to the ret_val variable (since it may be read/written to by
  other threads :-), even if safe_addition is inlined.
*/

double safe_addition(volatile double *a, volatile double *b) {
  volatile double ret_val;
  ret_val = *a + *b;
  return ret_val;
} 

void find_fp_bits(int *t) {
  volatile static double one = 1.0;
  static int base = 2; 

  /* 'base' was originally found out from the implementation of the FPU/FP
     routines; it is tipically 2 in mos FP implementations.  I suppose,
     then, that we can choose whatever base is appropriate for us and use
     the loop below to determine the number of significant digits in
     (pseudo-) base 10. */
    
  volatile double a, c, tmp1;
  volatile int lt;

  lt = 0;
  a = c = one;
  while (c == one) {
    ++lt;
    a *= base;
    c = safe_addition(&a, &one);
    tmp1 = -a;
    c = safe_addition(&c, &tmp1);
  }
  *t = lt;
} 

void get_mask_descr(int size,
		    unsigned int *lx,
		    unsigned int *ly,
		    unsigned int *mask,
		    unsigned int *indx,
		    unsigned int *shft) {
  for(*indx=0; *indx<size; (*indx)++) {
    *mask = lx[*indx] ^ ly[*indx];
    if(*mask) {
      *shft = 0;
      while(((*mask >> *shft) & (unsigned int)1)==0) {
	(*shft)++;
      }
      return;
    }
  }
  *indx=0;
  *shft=0;
  *mask=0;
}

/* This function will calculate some values related to the internal
   representation of the double numbers according with the ieee754
   standard */

union dbl {
  double f;
  unsigned int i[sizeof(double)/sizeof(unsigned int)];
};
typedef union dbl dbl_t;

void configure_ieee754() {
  dbl_t x, y, z;
  unsigned int *lx, *ly, *lz;
  unsigned int mask;
  unsigned int index;
  unsigned int shift;
  int i;
  int size = sizeof(x) / sizeof(mask);

  lx = x.i;
  ly = y.i;
  lz = z.i;

  x.f = 1;
  y.f = -x.f;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE754_MASK_NEGATIVE  0x%08x\n", mask);
  printf("#define IEEE754_INDEX_NEGATIVE %d\n", index);
  printf("#define IEEE754_SHIFT_NEGATIVE %d\n", shift);

  x.f = 1;
  y.f = 2;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE754_MASK_EXPONENT  0x%08x\n", mask);
  printf("#define IEEE754_INDEX_EXPONENT %d\n", index);
  printf("#define IEEE754_SHIFT_EXPONENT %d\n", shift);

  x.f = 1;
  y.f = 1;
  z.f = 1;
  /* This have 20 bits */
  for (i=1; i<=20; i++) {
    y.f /= 2;
    x.f += y.f;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE754_MASK_MANTISSA0  0x%08x\n", mask);
  printf("#define IEEE754_INDEX_MANTISSA0 %d\n", index);
  printf("#define IEEE754_SHIFT_MANTISSA0 %d\n", shift);

  /* This have 32 bits */
  z.f = x.f;
  for (i=1; i<=32; i++) {
    y.f /= 2;
    x.f += y.f;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE754_MASK_MANTISSA1  0x%08x\n", mask);
  printf("#define IEEE754_INDEX_MANTISSA1 %d\n", index);
  printf("#define IEEE754_SHIFT_MANTISSA1 %d\n", shift);
  printf("#define IEEE754_MANTISSA_LENGTH 52\n\n");
}

union ldbl {
  long double f;
  unsigned int i[sizeof(long double)/sizeof(unsigned int)];
};
typedef union ldbl ldbl_t;

void configure_ieee854() {
  ldbl_t x, y, z;
  unsigned int *lx, *ly, *lz;
  unsigned int mask;
  unsigned int index;
  unsigned int shift;
  unsigned int split;
  unsigned int mantissa0_length;
  int i;
  int orig_size = sizeof(x) / sizeof(mask);
  int size;

  lx = x.i;
  ly = y.i;
  lz = z.i;

  if (orig_size == 2) {
    mantissa0_length = 20;
    size = orig_size;
  } else if (orig_size == 3) {
    printf("#define USE_LONG_DOUBLE 1\n");
    mantissa0_length = 31;
    size = orig_size;
  } else if (orig_size == 4) {
    printf("#define USE_LONG_DOUBLE 1\n");
    mantissa0_length = 31;
    size = orig_size;
    //    /* In some new PPC platforms the size of long double is 128 bits,
    //       that is unsupported */
    //    mantissa0_length=20;
    //    size = 2;
  }

  /* It is necessary to init to 0 to avoid garbage in the unused bits: */

  for (i = 0; i < size; i++) {
    lx[i] = 0;
    ly[i] = 0;
    lz[i] = 0;
  }

  /* printf("/\* x=0x%08X:0x%08X:0x%08X *\/\n", lx[0],lx[1],lx[2]); */
  /* printf("/\* y=0x%08X:0x%08X:0x%08X *\/\n", ly[0],ly[1],ly[2]); */
  /* printf("/\* z=0x%08X:0x%08X:0x%08X *\/\n", lz[0],lz[1],lz[2]); */

  x.f = 1;
  y.f = -x.f;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE854_MASK_NEGATIVE   0x%08x\n", mask);
  printf("#define IEEE854_INDEX_NEGATIVE  %d\n", index);
  printf("#define IEEE854_SHIFT_NEGATIVE  %d\n", shift);

  x.f = 1;
  y.f = 2;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE854_MASK_EXPONENT   0x%08x\n", mask);
  printf("#define IEEE854_INDEX_EXPONENT  %d\n", index);
  printf("#define IEEE854_SHIFT_EXPONENT  %d\n", shift);

  x.f = 1;
  y.f = 1;
  z.f = 1;

  /* This have mantissa0_length bits (plus a fixed bit if 31) */

  y.f /= 2;
  x.f += y.f;
  get_mask_descr(size, lx, lz, &mask, &index, &split);
  split++;
  for (i=1; i < split; i++) {
    y.f /= 2;
    x.f += y.f;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA0_0  0x%08x\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA0_0 %d\n", index);
  if (mantissa0_length < split) {
    fprintf(stderr, "{bug: configure.c: IEEE854_SPLIT_MANTISSA0_0 is negative!}\n");
    printf("#define IEEE854_SPLIT_MANTISSA0_0 %d\n", 0);
  } else {
    printf("#define IEEE854_SPLIT_MANTISSA0_0 %d\n", mantissa0_length - split);
  }

  z.f = x.f;
  for (i=split; i<mantissa0_length; i++) {
    y.f /= 2;
    x.f += y.f;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA0_1  0x%08x\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA0_1 %d\n", index);
  printf("#define IEEE854_SHIFT_MANTISSA0_1 %d\n", shift);

  lx[index] = lx[index] | (mask << shift);

  /* This have 32 bits */
  z.f = x.f;
  y.f /= 2;
  x.f += y.f;
  get_mask_descr(size, lx, lz, &mask, &index, &split);
  split++;
  for (i=1; i < split; i++) {
    y.f /= 2;
    x.f += y.f;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA1_0  0x%08x\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA1_0 %d\n", index);
  printf("#define IEEE854_SPLIT_MANTISSA1_0 %d\n", 32 - split);

  z.f = x.f;
  for(i=split; i < 32; i++) {
    y.f /= 2;
    x.f += y.f;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA1_1  0x%08x\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA1_1 %d\n", index);
  printf("#define IEEE854_SHIFT_MANTISSA1_1 %d\n", shift);

  printf("#define IEEE854_MANTISSA_LENGTH %d\n\n", mantissa0_length + 32);
}

void configure__fpbits() {
  int bits;
  double f;    
  int i, j;

  find_fp_bits(&bits);

  i = (bits*0.301029995663981); /* #significant digits, bits*log_10(2) */

  f = 0.5e-9;			/* rounding factor if above 18 */
  for (j=18; j>i; j--)
    f*=10.0;

  printf("#define ENG_FLT_SIGNIF %d\n", i);
  printf("#define ENG_FLT_ROUND %.*g\n\n", i, f);

  configure_ieee754();
  configure_ieee854();
}

/* ------------------------------------------------------------------------- */
/* configure__endianness: Obtain system endianness
 * 
 * BIGENDIAN: 1 if the system is big-endian, 0 if the system is little-endian
 */

void configure__endianness() {
  union {
    unsigned short as_short[2];
    unsigned int as_int;
  } u;
  u.as_int = 1;
  printf("#define BIGENDIAN %d\n", (int)u.as_short[1]);
}

/* ------------------------------------------------------------------------- */
/* Configure mmap */

#if defined(USE_MMAP)
#include "engine__own_mmap.h"
#define USE_OWN_MALLOC 1
#endif

#if defined(USE_MMAP)

/* Mininum amount of reserved bits for mmap (trying to reserve bigger
   memory regions crashes the process) */
#define MIN_MMAP_RESERVED_BITS 3

#define N_TRY_ADDR 9
unsigned int try_addr[N_TRY_ADDR] = { 
    0x60000000, 0x70000000, 0x80000000, 0x40000000, 0x90000000,
    0x30000000, 0xA0000000, 0x20000000, 0xB0000000};
void find_mmap_base(int reserved_bits, unsigned long int *mmap_base, unsigned long int *mmap_size) {
  tagged_t *pointer;
  int i = 0;

  /* Chunks of code identical to those in engine__own_malloc.c - keep
     them in sync! */

  for(i = 0; i <  N_TRY_ADDR; i++){
    pointer = (tagged_t *)try_addr[i];
    if (!own_fixed_mmap(pointer, ADDRESSABLE(reserved_bits)))
      break;
  }

  /* Give memory back */
  own_fixed_munmap((void *)pointer, ADDRESSABLE(reserved_bits)); 

  if (i < N_TRY_ADDR) {
    *mmap_base = (unsigned int)pointer;
    *mmap_size = ADDRESSABLE(reserved_bits);
  } else {
    fprintf(stderr, "{bug: could not find a fixed address for mmap}");
    *mmap_base = (unsigned int)0;
    *mmap_size = 0;
  }
}
#endif

/* ------------------------------------------------------------------------- */
/* configure__alloc: Memory management configuration
 *
 * MallocBase: 0'MMMM...0000000 where M are the top bits for pointers
 *   returned by malloc()
 * MIN_MEM_ALLOC: minimum amount of memory that makes malloc return
 *   pointers in that region
 *
 * Some systems (namely, LINUX) allocate memory in different parts of
 * the memory depending on how much we ask.  The result is that blocks
 * can be scattered so that the "unmutable four top bits" assumption
 * is broken (i.e., the mapped memory can't be fit into the pointer
 * part of a tagged word) and MallocBase is not useful at all.  We try
 * to find out dynamically if this ever happens, and at which point.
 * This will serve to calculate the minimum amount of memory to be
 * requested at a time from the system, and a (hopefully) correct
 * MallocBase.
 *  
 * If we, anyway, choose to build a system without snooping for a
 * good MallocBase, just use the first pointer the system returns.
 */

/* 
    Before switching to ld linker scripts, things to know (statically):

    * Do we have a working mmap() [which makes it necessary to use
    own_malloc()]?  If so, does it have the ANONYMOUS flag?  If it doesn't,
    does "/dev/zero" work?  We assume we can know this before compiling, and
    find out where does memory start.

    * Otherwise, do we need to grab a minimum amount of memory from malloc
    to make the region stable?

    * If we don't, then just take the first points malloc() gives us.
*/

#define MIN_MEM_BLOCK_CHARS 16384
#define MAX_MEM_BLOCK_CHARS (32*1024*1024)

#define ALIGN sizeof(tagged_t)                        /* Minimum block size */
#define CHARS_TO_TW(Chars) ((Chars)%ALIGN==0 ? (Chars)/ALIGN : (Chars)/ALIGN+1)

void find_malloc_base(tagged_t tagmask, tagged_t *malloc_base0, int *min_mem_alloc0) {
  /* Use system malloc to allocate blocks of memory */
  tagged_t malloc_base;
  int *chunk;
  int min_mem_alloc;
  int size;
  if (tagmask == 0) {
    /* No base or special min mem alloc value is required when tagmask is 0 */
    malloc_base = 0;
    min_mem_alloc = MIN_MEM_BLOCK_CHARS;
  } else {
    /* Obtain turn point (assumes that tagmask != 0) */ 
    size = ALIGN;
    while(1) {
      chunk = (int *)malloc(size);
      if (chunk == NULL) {
	/* tagmask is never 0, use (unsigned int)(CHARS_TO_TW(MIN_MEM_BLOCK_CHARS)) */
	malloc_base = (tagged_t)0;
	min_mem_alloc = MIN_MEM_BLOCK_CHARS;
	break;
      } else {
	if (((tagged_t)chunk & tagmask) == 0) {
	  /* not yet non-zero, continue */
	  size *= 2;
	  if (size > MAX_MEM_BLOCK_CHARS) {
	    /* not found, assume tag bits are always 0 */
	    malloc_base = (tagged_t)0;
	    min_mem_alloc = MIN_MEM_BLOCK_CHARS;
	    break;
	  } else {
	    free(chunk);
	  }
	} else {
	  /* Use that one, assume that there will be no more changes in
	     the upper bits */
	  malloc_base = (tagged_t)chunk & tagmask;
	  free(chunk);
	  min_mem_alloc = (size > MIN_MEM_BLOCK_CHARS ?
			   size : MIN_MEM_BLOCK_CHARS);
	  break;
	}
      }
    }
  }
  *malloc_base0 = malloc_base;
  *min_mem_alloc0 = min_mem_alloc;
}

void configure__alloc() {
  int bits;
#if defined(USE_OWN_MALLOC)
  for (bits = MAX_UPPER_BITS; bits >= 0; bits--) {
#if defined(USE_MMAP)
    if (bits >= MIN_MMAP_RESERVED_BITS) {
      /* Use mmap to allocate blocks of memory */
      unsigned long int mmap_base;
      unsigned long int mmap_size;
      find_mmap_base(bits, &mmap_base, &mmap_size);
      printf("#define MmapAllowed%d 1\n", bits);
      printf("#define MallocBase%d 0x%lx\n", bits, mmap_base);
      printf("#define MmapSize%d 0x%lx\n", bits, mmap_size);
    } else {
      printf("#define MmapAllowed%d 0\n", bits);
#endif      
      /* Use system malloc to allocate blocks of memory */
      tagged_t malloc_base;
      int min_mem_alloc;
      find_malloc_base(PTRTAGMASK(bits), &malloc_base, &min_mem_alloc);
      printf("#define MallocBase%d 0x%lx\n", bits, malloc_base);
      printf("#define MIN_MEM_ALLOC_%d %d\n", bits, min_mem_alloc);
#if defined(USE_MMAP)
    }
#endif
  }
  printf("#define USE_OWN_MALLOC 1\n");
#else /* !defined(USE_OWN_MALLOC) */
  /* Trust that the malloc implementation gives always pointers in the
     same region */
  tagged_t malloc_base;
  int *chunk;
  for (bits = MAX_UPPER_BITS; bits >= 0; bits--) {
    chunk = (int *)malloc(ALIGN);
    malloc_base = (tagged_t)chunk & PTRTAGMASK(bits);
    free(chunk);
    printf("#define MallocBase%d 0x%lx\n", bits, malloc_base);
  }
#endif
}

/* ------------------------------------------------------------------------- */
/* Call all configuration parts */

int main() {
  configure__endianness();
  configure__alloc();
  configure__fpbits();
  return 0;
}

