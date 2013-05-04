/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* Obtain the value of system and architecture parameters that do not
   change from run to run, so that those values can be static data
   during native code compilation */

#if defined(__svr4__) || defined(Solaris) || defined(DARWIN)
# include <stdlib.h>                                           /* malloc() */
#else                                                             /* SunOS */
# include <sys/types.h>
# include <malloc.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <math.h>

/* Import basic engine definitions */
#include "compat.h"
#include "termdefs.h"                          /* because of tagged_t (MCL) */
#include "own_malloc_defs.h"

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

ENG_FLT safe_addition(volatile ENG_FLT *a, volatile ENG_FLT *b) {
  volatile ENG_FLT ret_val;
  ret_val = *a + *b;
  return ret_val;
} 

/* Computing the accuracy of floats - Changed so that display(5.347) = 5.347 */

void find_fp_bits(ENG_INT *t) {
  volatile static ENG_FLT one = 1.0;
  static ENG_INT base = 2; 

  /* 'base' was originally found out from the implementation of the FPU/FP
     routines; it is tipically 2 in mos FP implementations.  I suppose,
     then, that we can choose whatever base is appropriate for us and use
     the loop below to determine the number of significant digits in
     (pseudo-) base 10. */
    
  volatile ENG_FLT a, c, tmp1;
  volatile ENG_INT lt;

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
		    unsigned long *lx,
		    unsigned long *ly,
		    unsigned long *mask,
		    unsigned int *indx,
		    unsigned int *shft) {
  for(*indx=0; *indx<size; (*indx)++) {
    *mask = lx[*indx] ^ ly[*indx];
    if(*mask) {
      *shft = 0;
      while(((*mask >> *shft) & (unsigned long)1)==0) {
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

void configure_ieee754() {
  double x, y, z;
  unsigned long *lx, *ly, *lz;
  unsigned long mask;
  unsigned int index;
  unsigned int shift;
  int i;
  int size = sizeof(x) / sizeof(mask);

  lx = (unsigned long *)&x;
  ly = (unsigned long *)&y;
  lz = (unsigned long *)&z;

  x = 1;
  y = -x;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE754_MASK_NEGATIVE   0x%08lX\n", mask);
  printf("#define IEEE754_INDEX_NEGATIVE  %d\n", index);
  printf("#define IEEE754_SHIFT_NEGATIVE  %d\n", shift);

  x = 1;
  y = 2;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE754_MASK_EXPONENT   0x%08lX\n", mask);
  printf("#define IEEE754_INDEX_EXPONENT  %d\n", index);
  printf("#define IEEE754_SHIFT_EXPONENT  %d\n", shift);

  x = 1;
  y = 1;
  z = 1;
  /* This have 20 bits */
  for (i=1; i<=20; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE754_MASK_MANTISSA0  0x%08lX\n", mask);
  printf("#define IEEE754_INDEX_MANTISSA0 %d\n", index);
  printf("#define IEEE754_SHIFT_MANTISSA0 %d\n", shift);

  /* This have 32 bits */
  z = x;
  for (i=1; i<=32; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE754_MASK_MANTISSA1  0x%08lX\n", mask);
  printf("#define IEEE754_INDEX_MANTISSA1 %d\n", index);
  printf("#define IEEE754_SHIFT_MANTISSA1 %d\n", shift);
  printf("#define IEEE754_MANTISSA_LENGTH 52\n\n");
}

#define USE_LONG_DOUBLE

void configure_ieee854_using_754()
{
  double x, y, z;
  unsigned int mantissa0_length=20;

#include "configure_float.h"

}

void configure_ieee854_using_854()
{
  long double x, y, z;
  unsigned int mantissa0_length=31;
  printf("#define USE_LONG_DOUBLE\n");
  
#include "configure_float.h"

}

void configure_ieee854() {
#if defined(USE_LONG_DOUBLE)
  long double x;
#else
  double x;
#endif
  unsigned long mask;
  int orig_size = sizeof(x) / sizeof(mask);

  if (orig_size==2) {
    configure_ieee854_using_754();
  }
#if defined(DARWIN) && defined(ppc)
  else if (orig_size==4)
  {
    printf("/* NOTE: in some new PPC platforms, the size of long double is 128 bits, */\n");
    printf("/* and the functions to handle that numbers has not been implemented yet */\n");
    printf("/* Using double instead long double. */\n");
    configure_ieee854_using_754();
  }
  else if (orig_size==3)
#else
    else
#endif
  {
    configure_ieee854_using_854();
  }
}
  
void configure__fpbits() {
  ENG_INT bits;
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
    tagged_t as_tagged;
  } u;
  u.as_tagged = 1;
  printf("#define BIGENDIAN %d\n", (int)u.as_short[1]);
}

/* ------------------------------------------------------------------------- */
/* configure__alloc: Memory management configuration
 *
 * MallocBase: 0xM0000000 where M are the top 4 bits for pointers
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

//         ( defined(LINUX) && defined(i86) ) 
//         ( defined(LINUX) && defined(Sparc) )       
//      || ( defined(LINUX) && defined(Sparc64) )     
//      || ( defined(LINUX) && defined(i86_64) )      
//      || ( defined(LINUX) && defined(mips) )        
//      || ( defined(LINUX) && defined(armv4l) )      
//      || ( defined(LINUX) && defined(armv5tel) )    
//      || ( defined(Win32) && defined(i86) )         
//      || ( defined(Darwin) && defined(ppc) )        
//      || ( defined(Solaris) && defined(i86) )       
//      || ( defined(Solaris) && defined(Sparc) )     
//      || ( defined(Solaris) && defined(Sparc64) )   
//      || ( defined(BSD) && defined(i86) )           



#if defined(HAS_MMAP)
#include "own_mmap.h"
# define USE_OWN_MALLOC
#endif

#define MIN_MEM_BLOCK_CHARS 16384

#define BIGTAGMASK 0xf0000000
#define ALIGN sizeof(tagged_t)                        /* Minimum block size */
#define CHARS_TO_TW(Chars) ((Chars)%ALIGN==0 ? (Chars)/ALIGN : (Chars)/ALIGN+1)
#define MIN_MEM_BLOCK (unsigned int)(CHARS_TO_TW(MIN_MEM_BLOCK_CHARS))
#define AddressableSpace 0x10000000


// ---------------------------------------------------------------------------

void write_mem_settings(tagged_t *base,
                        int space,
                        int min_block) {
//  if (use_mmap)       printf("#define USE_MMAP 1\n");
//  if (use_mmap &&
//      dev_zero)       printf("#define USE_DEV_ZERO 1\n");
  printf("#define MallocBase 0x%lx\n", (unsigned long int)base);
  if (space != 0)     printf("#define AddressableSpace 0x%x\n", space);
  if (min_block != 0) {
    printf("#if !defined(USE_OWN_MALLOC)\n#define USE_OWN_MALLOC\n#endif\n");
    printf("#define MIN_MEM_ALLOC 0x%x\n", min_block);
  }
}


// ---------------------------------------------------------------------------

#if defined(HAS_MMAP)

#define N_TRY_ADDR 9
unsigned int try_addr[N_TRY_ADDR] = { 
    0x60000000, 0x70000000, 0x80000000, 0x40000000, 0x90000000,
    0x30000000, 0xA0000000, 0x20000000, 0xB0000000};

tagged_t * configure_mmap(void)
{
  tagged_t * pointer;
  int i = 0;

  for(i = 0; i <  N_TRY_ADDR; i++){
    pointer = (tagged_t *) try_addr[i];
    if (!own_fixed_mmap(pointer, AddressableSpace))
      break;
  }

  own_fixed_munmap((void *)pointer, AddressableSpace);    // Give memory back

  if (i < N_TRY_ADDR) {
    write_mem_settings(pointer, AddressableSpace, AddressableSpace);
    return pointer;
  } else {
    fprintf(stderr, "After trying mmap: no success!");
    return NULL;
  }
}
#endif

// ---------------------------------------------------------------------------

// USE_OWN_MALLOC is always defined whenever HAS_MMAP is, but it may be defined
// in cases where HAS_MMAP is not.

#if defined(USE_OWN_MALLOC) /* malloc() to allocate large blocks of memory */ 
void try_malloc_chunks(void) {
  tagged_t malloc_base = (tagged_t)0;
  int *chunk;
  int min_mem_alloc = getpagesize();
  int size;

  size = ALIGN;
  while (size < AddressableSpace) {             /* Obtain turn point */ 
    chunk = (int *)malloc(size);
    if (chunk == NULL) { /* BIGTAGMASK is never 0, use MIN_MEM_BLOCK */
      malloc_base = (tagged_t)0;
      min_mem_alloc = MIN_MEM_BLOCK;
      break;
    } else {
      if (((tagged_t)chunk & BIGTAGMASK) == 0) {// not yet non-zero, continue 
        size *= 2;
        free(chunk);
      } else { /* Use that one, assume that there will be no more changes in
                  the upper bits */
        malloc_base = (tagged_t)chunk & BIGTAGMASK;
        free(chunk);
	int tagged_lots = CHARS_TO_TW(size);
	min_mem_alloc = (tagged_lots > MIN_MEM_BLOCK ?
			 tagged_lots : MIN_MEM_BLOCK);
        break;
      }
    }
  }
  write_mem_settings((tagged_t *)malloc_base, AddressableSpace, min_mem_alloc);
}
#endif

// ---------------------------------------------------------------------------

#if !defined(USE_OWN_MALLOC)
void try_malloc(void){
  tagged_t malloc_base;
  unsigned char *chunk;

  /* Trust that the malloc implementation gives pointers in the
     0xMmmmmmmm region, where 0xM0000000 is the base */

  chunk = malloc(MIN_MEM_BLOCK_CHARS);
  malloc_base = (tagged_t)chunk & BIGTAGMASK;
  free(chunk);
  write_mem_settings((tagged_t *)malloc_base, 0, 0);
}
#endif

// ---------------------------------------------------------------------------

void configure__alloc() {
#if defined(USE_OWN_MALLOC)
# if defined(HAS_MMAP)
  tagged_t * mmap_base;

  mmap_base = configure_mmap();  // Writes #defines if configuration successful

  if (mmap_base == NULL)
    try_malloc_chunks();
# else     // No mmap() available, or not working
  try_malloc_chunks();
# endif
#else  // !USE_OWN_MALLOC : Last desperate resort
  try_malloc();
#endif
}

/* ------------------------------------------------------------------------- */

/* SunOs does not include strsep  */
#if defined(SunOS4) || defined(Solaris) || defined(IRIX)
/*      $NetBSD: strsep.c,v 1.8 1998/10/13 20:32:09 kleink Exp $        */
/*-
 * Copyright (c) 1990, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Get next token from string *stringp, where tokens are possibly-empty
 * strings separated by characters from delim.  
 *
 * Writes NULs into the string at *stringp to end tokens.
 * delim need not remain constant from call to call.
 * On return, *stringp points past the last NUL written (if there might
 * be further tokens), or is NULL (if there are definitely no more tokens).
 *
 * If *stringp is NULL, strsep returns NULL.
 */
char *strsep(stringp, delim)
     char **stringp;
     const char *delim;
{
  char *s;
  const char *spanp;
  int c, sc;
  char *tok;
  
  if ((s = *stringp) == NULL) return (NULL);
  for (tok = s;;) {
    c = *s++;
    spanp = delim;
    do {
      if ((sc = *spanp++) == c) {
        if (c == 0) s = NULL;
        else s[-1] = 0;
        *stringp = s;
        return (tok);
      }
    } while (sc != 0);
  }
}
#endif

void generate_defines(char *cflags) {
  char *Dpointer;
  char *definition;
  char *macroname = NULL;
  char *definition_value = NULL;

  Dpointer = cflags;
  while (Dpointer && (Dpointer = strstr(Dpointer, "-D"))) {
    Dpointer += 2;
    if ((definition = strsep(&Dpointer, " "))) {
      definition_value = definition;
      macroname = strsep(&definition_value, "=");
    }
    if (definition_value)
      printf("#if !defined(%s)\n#define %s %s\n#endif\n\n", 
             macroname, macroname, definition_value);
    else
      printf("#if !defined(%s)\n#define %s\n#endif\n\n", 
             macroname, macroname);
  }
}

/* ------------------------------------------------------------------------- */
/* Call all configuration parts */

int startconfig(int argc, char **argv) {
  if (argc > 0) generate_defines(argv[1]);
  configure__endianness();
  configure__alloc();
  configure__fpbits();
  return 0;
}
