/* This file is part of the Online Negative Database (NDB), Copyright
   (C) 2004-2008 elena s ackley and the Regents of the University of
   New Mexico
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
/*
 * -------------------------------------------------------------------------
 * Filename      : rec.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for NDB Records
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Thu Apr 10 15:16:32 2008 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include <string.h>
#include "rec.h"
#include "perm.h"
#include "error.h"

#define true 1
#define false 0


/* = ceil(LEN / UNITSIZE) v.27 moved here */
int rec_calcnumunits(int lenarg)
{
  int numunits = lenarg/UNITSIZE;            /* ceil(LEN / UNITSIZE) */
  if(lenarg % UNITSIZE > 0 )
    numunits++;                 /* round up to nearest int */
  return numunits;
}


/* return the number of specified bits */
static int calc_size(Rec * rarg, int numunitsarg) 
{
  int i;
  int count = 0;
  for(i = numunitsarg - 1; i >= 0; i--) {
    int mi = i << 1;
    Rec msk = rarg[mi];
    
    /* replace with faster algorithm: e.g. a table of bits for each
       unsigned char value */
    int k = UNITSIZE;
    while(k--){
      count += ((msk >> k) & 1); 
      /* count += (msk & 1); msk >>= 1; */
    }
  }
  return count;
}


#if USE_RECSUBSUMED
static int RECSUBSUMED_CALLS = 0;
static int RECSUBSUMED_ITS = 0;
static int RECSUBSUMED[NUMUNITS];

void recsubsumed_stats(int numunitsarg)
{
  int i;
  for(i = 0; i < numunitsarg; i++)
    fprintf(stdout,"%d %d, ", i, RECSUBSUMED[i]);
  
  fprintf(stdout,"REC SUBSUMED: CALLS %d, ITERATIONS %d\n",
          RECSUBSUMED_CALLS,RECSUBSUMED_ITS);
}
#endif


/* return true if recX is subsumed by recY;
   a string x is subsumed by a string y, if for every specified bit ai
   in y the corresponding bit bi in x is also specified, and ai == bi;
   if (x ~= /y/) return 1 if such a string y is found, o.w. 0
*/
__inline__ int rec_subsumed_query(Rec * recXarg, Rec * recYarg, int numunitsarg) 
{
  /* ym & (~xm | (xv ^ yv))  if true, subsume failure, quit rtn f, o.w. t*/
  /* ym == 1  xm yv  xv  sub  ~sub  (~xm | (xv ^ yv)) ym & ~sub
     0 0   0      0   1        1               1 rtn f
     0 0   1      0   1        1               1 rtn f
     0 1   0      0   1        1               1 rtn f
     0 1   1      0   1        1               1 rtn f
     1 0   0      1   0        0               0  return true 
     1 0   1      0   1        1               1 rtn f
     1 1   0      0   1        1               1 rtn f 
     1 1   1      1   0        0               0  return true
  */
  int i;
#if USE_RECSUBSUMED
  RECSUBSUMED_CALLS++;
#endif
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    int vi = mi + 1;
    if(recYarg[mi] & (~recXarg[mi] | (recXarg[vi] ^ recYarg[vi]))) {
#if USE_RECSUBSUMED
      RECSUBSUMED_ITS += (i+1);
      RECSUBSUMED[i]++; 
#endif
      return 0;                        /* not subsumed */
    }
  }
#if USE_RECSUBSUMED
  RECSUBSUMED_ITS += numunitsarg;
  RECSUBSUMED[numunitsarg-1]++; 
#endif
  
  return 1;                           /* subsumed */
}


/* return true if ndbrecX is "matched" by patternrecY;
   a string x is "matched" by a string y, if for every specified bit ai
   in y the corresponding bit bi in x, if specified, is ai == bi, o.w. dontcare;
   if (x ~= /y/) return 1 if such a string x is found, o.w. 0 (changed name from
   rec_partial_query to correspond to Definition 1 in Relational Algebra paper
   v.29)
*/
__inline__ int rec_match_query(Rec * recXarg, Rec * recYarg, int numunitsarg) 
{
  /* (ym & xm & (xv ^ yv)) if true, no match quit rtn f, o.w. t*/
  /* ym == 1  xm yv  xv  match  ~match (ym & xm & (xv ^ yv)))
     0 0   0      1     0               0 return true
     0 0   1      1     0               0 return true
     0 1   0      1     0               0 return true
     0 1   1      1     0               0 return true
     1 0   0      1     0               0 return true
     1 0   1      0     1               1 rtn f 
     1 1   0      0     1               1 rtn f 
     1 1   1      1     0               0 return true
     ym == 0                1     0               0 return true
  */
  int i;
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    int vi = mi + 1;
    /* (ym & xm & (xv ^ yv)) no match */
    if( recYarg[mi] & recXarg[mi] & (recXarg[vi] ^ recYarg[vi])) {
      return 0;
    }
  }
  return 1;
}


/* return true if recX is subsumed by recY;
   a string x is "stingy" subsumed by a string y, if for ANY specified bit ai
   in y the corresponding bit bi in x is also specified, and ai == bi;
   if (x ~= /y?/) return 1 if such a string y is found, o.w. 0
*/
int rec_subsumed_query_stingy(Rec * recXarg, Rec * recYarg, int numunitsarg) 
{
  /* (ym & xm) & !(xv ^ yv)  if not zero, subsume successful return t, o.w. f */
  /* ym == 1  xm yv  xv  sub    (xm & ~(xv ^ yv)) ym & sub
     0 0   0      0        0               0 rtn f
     0 0   1      0        0               0 rtn f
     0 1   0      0        0               0 rtn f
     0 1   1      0        0               0 rtn f
     1 0   0      1        1               1  return true 
     1 0   1      0        0               0 rtn f
     1 1   0      0        0               0 rtn f 
     1 1   1      1        1               1  return true
  */
  int i;
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    int vi = mi + 1;
    if(recYarg[mi] & recXarg[mi] & ~(recYarg[vi] ^ recXarg[vi])) {
      return true;
    }
  }
  return false;
}


#define rec_hash(r,n) ((((r)[(n)<<1]) >> HALFSIZE) & HALFMASK)
#define rec_hashsize(r,n) ((r)[(n)<< 1])


/* return 1 if equal at every bit position, 0 if not the same */
__inline__ int rec_equal(Rec * rarg1, Rec * rarg2, int numunitsarg) 
{
  int i;
  
  if(rec_hashsize(rarg1,numunitsarg) != rec_hashsize(rarg2,numunitsarg))  
    return 0;                             /* definitely not equal */
  
  for(i = 0; i < numunitsarg ; i++) {      /* possibly equal */
    int mi = i << 1;  /* times 2 */
    int vi = mi + 1;
    if( rarg1[mi] != rarg2[mi] || rarg1[vi] != rarg2[vi] ) {
      return 0;
    }
  }
  return 1;
}


/* return first bit different or -1 if exactly alike */
int rec_firstbitdiff(Rec * r1arg, Rec * r2arg, int numunitsarg)
{
  int i;
  if(r1arg == NULL || r2arg == NULL)
    return 0;
  
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;  /* times 2 */
    if( r1arg[mi] != r2arg[mi] || r1arg[mi+1] != r2arg[mi+1] ) {
      int j;
      for(j = 0; j < UNITSIZE; j++) {
        if(lv(r1arg,mi,j) != lv(r2arg,mi,j)){
          return (i*UNITSIZE + j);
        }
      }
    }
  }
  return errneg1;
}


Rec * rec_create(int numunitsarg)
{
  Rec * rptr;
  assert(numunitsarg > 0);
  rptr = (Rec *) malloc(sizeof(Rec) * ((numunitsarg << 1) + 1) );
  if(rptr == NULL) {
    fprintf(stderr,"rec_create: failed to allocate rec\n");
    exit(err12);
  }
  rec_init(rptr, numunitsarg);
  return rptr;
}


/* initialize ALL to DON'T CARES, and hash to zero */
void rec_init(Rec * rarg, int numunitsarg) 
{
  int i = numunitsarg << 1;
  for( ; i >= 0; i--)
    rarg[i] = 0;
}


/* initialize ALL to random values v.20 ; up to caller to rec_init first */
void rec_random(Rec * rarg, int lenarg, int numunitsarg) 
{
  int i = lenarg - 1;
  for( ; i >= 0; i--)
    rec_setbit(rarg,i,coinflip(),numunitsarg);
}


/* rec_coalesce: set recz bits to the specified value in corresponding
   bits in recx or recy, o.w. remains dontcare v.27 (changed name from
   rec_merge to correspond to Definition 2 in Relational Algebra paper
   v.29) */
static void rec_coalesce_bitbybit(Rec * rxarg, Rec * ryarg, Rec * rzarg, int lenarg, int numunitsarg)
{
  int i = 0;
  while (i < lenarg) {
    int x = rec_getbit(rxarg,i);
    int y = rec_getbit(ryarg,i);
    if(x != DONTCARE) {
      assert(y == x || y == DONTCARE);
      rec_setbit(rzarg,i,x,numunitsarg);
    } else {
      assert(y == x || x == DONTCARE);
      rec_setbit(rzarg,i,y,numunitsarg);
    }
    i++;
  }
  return;
}


#define mask(r,i) ((r)[((i) << 1)])
#define val(r,i) ((r)[(((i) << 1) + 1)])

/* private */
/* set both the mask and value to zero for the specified bit */
static void storedontcare ( Rec * rarg, int miarg, int biarg) 
{
  int mi = miarg << 1;   /* times 2 */
  rarg[mi] = rarg[mi] & ~(ONE << biarg);
  rarg[mi+1] = rarg[mi+1] & ~(ONE << biarg);
}


/* private */
static void storeone ( Rec * rarg, int miarg, int biarg) 
{
  int mi = miarg << 1;   /* times 2 */
  Rec bi = (ONE << biarg);
  int t = 2;    /* first the mask, second time the val */
  while(t--) {
    rarg[mi] = rarg[mi] | bi;
    mi += 1;
  }
}


/* private */
static void storezero ( Rec * rarg, int miarg, int biarg) 
{
  int mi = miarg << 1;   /* times 2 */
  Rec bi = (ONE << biarg);
  rarg[mi] = rarg[mi] | bi;    /* mask */
  rarg[mi+1] = rarg[mi+1] & ~bi;   /* val */
}


/* return the hash of all the bits */
static int calc_hash(Rec * rarg, int numunitsarg) 
{
  int i = 0;
  int hash = 0;
  int bits = numunitsarg * UNITSIZE;
  
  while(i < bits) {
    if(! rec_isdc(rarg,i)) {
      hash += (i + 1);
    }
    i++;
  }
  return hash;
}

#define clear_hashsize(r,n) ((r)[(n)<< 1] = 0)
#define set_hashsize(r,h,s,n) ((r)[(n)<< 1] = ((((h) & HALFMASK) << HALFSIZE) | ((s)& HALFMASK)))


/* private */
/* hash value accumulates positive bit position for specified bits and
   negative if changing it to a dontcare */
static void rec_updatehashsize (Rec * rarg, int bitarg, int numunitsarg)
{
  if(rec_isdc(rarg,bitarg)){
    int size = rec_size(rarg,numunitsarg);
    int hash = rec_hash(rarg,numunitsarg);
    set_hashsize(rarg,hash+(bitarg+1),size+1,numunitsarg);
  }
}


/* private */
/* hash value accumulates positive bit position for specified bits and
   negative if changing it to a dontcare */
static __inline__ void rec_updatehashsizedc (Rec * rarg, int bitarg, int numunitsarg )
{
  if(!rec_isdc(rarg,bitarg)){
    int size = rec_size(rarg,numunitsarg);
    int hash = rec_hash(rarg,numunitsarg);
    set_hashsize(rarg,hash-(bitarg+1),size-1,numunitsarg);
  }
}


/* return true if recY has dontcares everywhere recX does; */
int rec_dontcare_match(Rec * recXarg, Rec * recYarg, int numunitsarg) 
{
  /* (ym & ~xm)  if true, failure, quit rtn f, o.w. t */
  int i;
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    if(recYarg[mi] & ~recXarg[mi])
      return 0;
  }
  return 1;
}


/* bitarg goes from 0 to (record length - 1), but notice we can't
   check that here because we don't know the length. up to the caller. */
void rec_setbit ( Rec * rarg, int bitarg, int valarg, int numunitsarg) 
{
  int mi = bitarg / UNITSIZE;
  int bi = bitarg % UNITSIZE;
  
  assert(mi < numunitsarg && mi >= 0);
  
  switch(valarg) {
  case 0:
    rec_updatehashsize(rarg,bitarg,numunitsarg);
    storezero(rarg,mi,bi);
    break;
  case 1:
    rec_updatehashsize(rarg,bitarg,numunitsarg);
    storeone(rarg,mi,bi);
    break;
  case DONTCARE:
    rec_updatehashsizedc(rarg,bitarg,numunitsarg);  /* before storing dc */
    storedontcare(rarg,mi,bi);
    break;
  default:
    fprintf(stderr,"rec_setbit: invalid value %d\n",valarg);
    exit(err16);
  }
  return;
}


void rec_flipbit ( Rec * rarg, int bitarg) 
{
  int mi = bitarg / UNITSIZE;
  int bi = bitarg % UNITSIZE;
  int valflip = loadvalue(rarg,mi,bi) ^ 1;
  
  switch(valflip) {
  case 0:
    storezero(rarg,mi,bi);
    break;
  case 1:
    storeone(rarg,mi,bi);
    break;
  case DONTCARE:
  default:
    fprintf(stderr,"rec_flipbit: invalid value %d\n",valflip);
    exit(err16);
  }
  return;
}


/* prints in leastsignificant bit order first */
static void printrec(Rec * rarg, int lenarg, FILE * fdarg) 
{
  int i = 0;
  while(i < lenarg) {
    if(rec_isdc(rarg,i)) {
      fprintf(fdarg,"*");
    } else {
      fprintf(fdarg,"%d",rec_getbit(rarg,i));
    }
    i++;
  }
  fprintf(fdarg,"\n");
}


void rec_print(Rec * rarg, int lenarg, FILE * fdarg) 
{
  printrec(rarg,lenarg,fdarg);
}


int rec_read_cnf(Rec * rarg, int numunitsarg, FILE * fdarg) 
{
  int maxidx = 0;
  while (1) {
    int idx = 7777777; /* v.32 initialize in case 'not a number' error */
    int got = fscanf(fdarg,"%d",&idx);
    if (got != 1) { 
      fprintf(stderr," rec_read_cnf: not a number on input (%d)\n",idx); 
      return 0; 
    }
    if (idx == 0) return maxidx;             /* end of record */
    if (idx < 0){
      rec_setbit(rarg,-idx-1,1,numunitsarg);
      if (-idx > maxidx) maxidx = -idx;
    } 
    if (idx > 0) {
      rec_setbit(rarg,idx-1,0,numunitsarg);
      if (idx > maxidx) maxidx = idx;
    }
  }
  return 0;
}


/* the database determines the record length, not the runtime */
/* handles the last negative record without the newline v.52.1 */
int rec_read(Rec * rarg, int numunitsarg, FILE * fdarg) 
{
  int numchars = 0;
  int c;
  
  while((c = fgetc(fdarg)) != '\n' && c != EOF ){
    if( c == '0' || c == '1') {
      rec_setbit(rarg,numchars,c-'0',numunitsarg);
    } else {
      if( c != '*') {
        fprintf(stderr,"Invalid char [%c] at position [%d]\n",c,numchars);
        return errneg1;
      }
    }
    numchars++;   /* includes full tri-alphabet */
  }

  if(c==EOF) ungetc(c,fdarg);  /* flag for next time v.52 */

  return numchars;
}


/* convert lenarg minus one bits in unsigned input to binary string -
   without reversing the order that ctobs does */
void itobs(unsigned int inputarg, char * strarg, int lenarg)
{
  int i = lenarg;
  
  strarg[i] = '\0';
  
  while(i-- > 0){
    int j = lenarg - i - 1;
    strarg[i] = '0' + ((inputarg >> j) & 1);
  }
} 


/* ctobs reverses the order of the bits, the same as rec_string2rec
   does for ascii, but not for binary */
void ctobs(unsigned char carg, char * strarg)
{
  int i;
  int char_t = 8;
  
  for(i = 0; i < char_t ; i++) {
    strarg[i] = '0' + ((carg >> i) & 1);
  }
  strarg[char_t] = '\0';
} 


/* converts binary string arg into rec format; returns number of
   specified chars (v.22) in record or -1 if "too long" error, bails if "too
   short"; ascii-to-binary is not addressed here at this time */
static int rec_binarystring2rec (Rec * rarg, int lenarg, char * sarg)
{
  int numunits = rec_calcnumunits(lenarg);
  int numchars = 0;
  int recsz = 0;
  int slen = strlen(sarg);
  
  while(numchars < slen) {
    int c = sarg[numchars];
    if(c != '\n'){
      if(c == EOF || c=='\0') {
        return errneg1;   /* eof */
      } 
      if(numchars > lenarg) { 
        fprintf(stderr,"Binary Length %d exceeds expected length %d [%c]\n",
                numchars,lenarg,c);
        return errneg1;
      }
      if(numchars == lenarg) { 
        return errneg1;
      }
      if( c == '*') {
        rec_setbit(rarg,numchars,DONTCARE,numunits);
      } else {
        if( c == '0' || c == '1') {
          rec_setbit(rarg,numchars,c-'0',numunits);
          recsz++;
        } else {
          fprintf(stderr,"Invalid char [%c] at position [%d]\n",c,numchars);
          return errneg1;
        }
      }
      numchars++;   /* includes full tri-alphabet */
    }
  }
  
  assert(numchars==lenarg);
  
#if 0
  /* pad with zeros */
  while (numchars < lenarg) {
    rec_setbit(rarg,numchars,0,numunits);
    numchars++;
  }
#endif
  return recsz;
}


/* sets CHAR_T bits in rec starting at atarg 
   reverses the bit order in rec to look like ascii value
   and work with perl scripts (Mar 23 2005);
   special 'don't care' symbol is handled (v.22); 
   return number of specified bits (v.22).
*/
static int rec_setchar(Rec * rarg, int lenarg, char carg, int atarg)
{ 
  int b;
  int numunits = rec_calcnumunits(lenarg);
  
  assert(atarg + CHAR_T - 1 < lenarg);
  
  if( carg == '*') {                        /* v.22 */
    for(b = 0; b < CHAR_T; b++) {
      rec_setbit(rarg,atarg + CHAR_T -1 -b,DONTCARE,numunits);
    }
    return 0;
  }
  
  for(b = 0; b < CHAR_T; b++) {
    int j = (carg >> b) & 1;
    /*    fprintf(stderr,"rec_setchar %c, bit %d, is %d\n",carg,b,j); */
    rec_setbit(rarg,atarg + CHAR_T -1 -b,j,numunits);
  } 
  return CHAR_T;
}


static char lc (char carg) 
{
  char c = carg;
  if( carg >= 'A' && carg <= 'Z' ) {          /* convert to all lower case */
    c = carg - 'A' + 'a';
  }
  return c;
}


static int valid_char( char c )
{
  /*  return (( c >= 'a' && c <= 'z' ) || ( c >= '0' && c <= '9' ) || (c == ' ') || (c == '-')); */
  return (c >=' ' && c <= '~');
}


/* returns number of specified bits, not the dontcares */
int rec_string2rec (Rec * rarg, int lenarg, char * sarg, int binarg)
{
  int len8;
  int numchars;
  int slen;
  int recsz;
  
  if( binarg ) {
    return rec_binarystring2rec(rarg,lenarg,sarg);
  }
  
  len8 = lenarg / 8;
  recsz = numchars = 0;
  slen = strlen(sarg);
  
  while(numchars < slen) {
    int c = sarg[numchars];
    if(c != '\n'){
      if(c == EOF) {
        return errneg1;   /* eof */
      }
      if(numchars > len8) {
        fprintf(stderr,"Ascii Length %d exceeds expected length %d\n",
                numchars,len8);
        return errneg1;
      }
      c = lc(c);
      
      if( valid_char(c) ) {
        recsz += rec_setchar(rarg,lenarg,c,numchars*CHAR_T);
      } else {
        fprintf(stderr,"Invalid char [%c] at position [%d]\n",c,numchars);
        exit(err16);
      }
      numchars++;   /* ascii chars */
    }
  }
  
  assert(numchars==len8);
#if 0
  /* pad with blanks */
  {
    while (numchars < len8) {
      recsz += rec_setchar(rarg,rtarg,' ',numchars*CHAR_T);
      numchars++;
    }
  }
#endif
  return recsz;
}


/* returns number of specified bits, not the dontcares (only works for
   32-bit length) must reverse order to be consistent with rec_print
   v.55; special check for mask added since 32-bit shift is undefined
   in the c-spec: http://www.vmunix.com/~gabor/c/draft.html#6.3.7
   (v.56.3) */ 
int rec_int2rec(Rec * rarg, int lenarg, unsigned int iarg, int numunitsarg)
{
  int i = lenarg;
  unsigned int revi = 0;
  assert(lenarg <= INTSIZE);
  while(i-- >= 0) {
    int v = ((iarg >> i) & 1);
    revi |= (v << (lenarg - i - 1));
  }
  rarg[1] = revi;
  rarg[0] = ((lenarg == INTSIZE) ? ~0 : ((1 << lenarg) - 1));  /* mask is all ones for lenarg bits */ 
  set_hashsize(rarg,calc_hash(rarg,numunitsarg),calc_size(rarg,numunitsarg),numunitsarg);
  return rec_size(rarg,numunitsarg);
}


/* bitwise-and (coalesce) for use by Neg Union, and -C (v.66.1): 1&*
   =1, 0&* = 0, *&* = *,plus the normal logical-and: 0&0=0, 1&1=1, and
   0&1=0 (though this last case is impossible when recX and recY
   match) */
void rec_coalesce(Rec * recXarg, Rec * recYarg, Rec * recRarg, int numunitsarg) 
{
  /* xm ym xv yv rm rv
     0   0  0  0  0  0
     0   1  0  0  1  0 
     0   1  0  1  1  1 
     1   0  0  0  1  0 
     1   0  1  0  1  1
     1   1  0  0  1  0
     1   1  0  1  1  0 (n/a when match done first)
     1   1  1  0  1  0 (n/a when match done first)
     1   1  1  1  1  1
  */
  /* if a then b else c == (a & b) | (~a & c) */
  /* rv = if (xm & ym) then (xv & yv) else (xv | yv) */ 
  /* (xm & ym) & (xv & yv) | ~(xm & ym) & (xv | yv)  */
  /* rm = xm | ym; */
  int i;
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    int vi = mi + 1;
    recRarg[vi] = ((recXarg[mi] & recYarg[mi]) & (recXarg[vi] & recYarg[vi])) | (~(recXarg[mi] & recYarg[mi]) & (recXarg[vi] | recYarg[vi])); 
    recRarg[mi] = (recXarg[mi] | recYarg[mi]);
  }

  set_hashsize(recRarg,calc_hash(recRarg,numunitsarg),calc_size(recRarg,numunitsarg),numunitsarg);

#if 0
 {
   Rec * rectest = rec_create(numunitsarg);
   int maxlen = numunitsarg * UNITSIZE;
   rec_coalesce_bitbybit(recXarg,recYarg,rectest,maxlen,numunitsarg);
   if(!rec_equal(rectest,recRarg,numunitsarg)) {
     fprintf(stderr,"rec_coalesce: bitbybit method is different than bitwise:\n");
     rec_print(rectest,maxlen,stderr);
     rec_print(recRarg,maxlen,stderr);
     rec_destroy(rectest);
     exit(err16);
   } else {
     fprintf(stderr,"rec_coalece: bitbybit method is same as bitwise:\n");
     rec_print(rectest,maxlen,stderr);
     rec_print(recRarg,maxlen,stderr);
     rec_destroy(rectest);
   }
 }
#endif
  
 return;
}


/* bitwise-or v.55: *|1 =1, *|0 = *, *|* = *  */
static int rec_or_bitbybit(Rec * rec1arg, Rec * rec2arg, Rec * rec3arg, int lenarg, int numunitsarg)
{
  int i;
  for(i=0; i < lenarg; i++) {
    int val1 = rec_getbit(rec1arg,i);
    int val2 = rec_getbit(rec2arg,i);
    if(val1!=DONTCARE && val2!=DONTCARE){
      rec_setbit(rec3arg,i,val1|val2,numunitsarg);
    } else {
      if (val1==DONTCARE && val2!=DONTCARE){
        if(val2==1)
          rec_setbit(rec3arg,i,1,numunitsarg);
      } else {
        if (val1!=DONTCARE && val2==DONTCARE){
          if(val1==1)
            rec_setbit(rec3arg,i,1,numunitsarg);
        }
      }
    }
  }
  return rec_size(rec3arg,numunitsarg);
}


/* bitwise-or v.56: *|1 =1, *|0 = *, *|* = *  */
void rec_or(Rec * recXarg, Rec * recYarg, Rec * recRarg, int numunitsarg) 
{
  /* xm ym xv yv rm rv
     0   0  0  0  0  0
     0   1  0  0  0  0 <-
     0   1  0  1  1  1
     1   0  0  0  0  0 <-
     1   0  1  0  1  1 * rv corrected from notes
     1   1  0  0  1  0
     1   1  0  1  1  1
     1   1  1  0  1  1
     1   1  1  1  1  1
  */
  /* if a then b else c == (a & b) | (~a & c) */
  /* rv = (xv | yv) ; rm = (xm & ym) | rv; */
  int i;
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    int vi = mi + 1;
    recRarg[vi] = recXarg[vi] | recYarg[vi];
    recRarg[mi] = (recXarg[mi] & recYarg[mi]) | recRarg[vi];
  }

  set_hashsize(recRarg,calc_hash(recRarg,numunitsarg),calc_size(recRarg,numunitsarg),numunitsarg);

#if 0
 {
   Rec * rectest = rec_create(numunitsarg);
   int maxlen = numunitsarg * UNITSIZE;
   rec_or_bitbybit(recXarg,recYarg,rectest,maxlen,numunitsarg);
   if(!rec_equal(rectest,recRarg,numunitsarg)) {
     fprintf(stderr,"rec_or: ERROR bitbybit method is different than bitwise:\n");
     rec_print(rectest,maxlen,stderr);
     rec_print(recRarg,maxlen,stderr);
     rec_destroy(rectest);
     exit(err16);
   } else {
     fprintf(stderr,"rec_or: bitbybit method is same as bitwise:\n");
     rec_print(rectest,maxlen,stderr);
     rec_print(recRarg,maxlen,stderr);
     rec_print(recXarg,maxlen,stderr);
     rec_print(recYarg,maxlen,stderr);
     rec_destroy(rectest);
   }
 }
#endif

 return;
}


/* bitwise-flip ($) for use by Ternary Set Diff (-x T) (v.72.2): input
   condition: recX and recY match, and recX is not subsumed by recY;
   * $ 0 = 1, * $ 1 = 0, * $ * = * when * is in recX only (*'s in recY do
   not flip recX value); result into recRarg.
*/
void rec_flip(Rec * recXarg, Rec * recYarg, Rec * recRarg, int numunitsarg) 
{
  /* xm ym xv yv rm rv
     0   0  0  0  0  0
     0   1  0  0  1  1 $ 
     0   1  0  1  1  0 $ 
     1   0  0  0  1  0 
     1   0  1  0  1  1
     1   1  0  0  1  0
     1   1  0  1  1  0 (n/a when match done first)
     1   1  1  0  1  0 (n/a when match done first)
     1   1  1  1  1  1
  */
  /* if a then b else c == (a & b) | (~a & c) */
  /* rv = if (~xm & ym) then (yv ^ 1) else (xv) */ 
  /* rv = (~xm & ym) & (yv ^ 1) | ~(~xm & ym) & (xv)  */
  /* rm = if (~xm)  then ym else xm
     rm = (~xm) & ym | xm & xm
     rm = (xm | ym) 
  */
  int i;
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    int vi = mi + 1;
    recRarg[vi] = ((~recXarg[mi] & recYarg[mi]) & (recYarg[vi]^1)) | (~(~recXarg[mi] & recYarg[mi]) & (recXarg[vi])); 
    recRarg[mi] = (recXarg[mi] | recYarg[mi]);
  }
  set_hashsize(recRarg,calc_hash(recRarg,numunitsarg),calc_size(recRarg,numunitsarg),numunitsarg);  
  return;
}


/* return hamming distance (i.e. rec size) after bitwise-xor v.61: *^x
   =1, x^* = 1, *^* = 0, 0^0 = 0, 0^1 = 1, 1^0 = 1, 1^1 = 0  */
int rec_hammingdistance(Rec * recXarg, Rec * recYarg, int numunitsarg) 
{
  /* xm ym xv yv hdmask
     0   0  - -   0
     0   1  - -   1
     1   0  - -   1
     1   1  0  0  0
     1   1  0  1  1
     1   1  1  0  1
     1   1  1  1  0
  */
  /* if a then b else c == (a & b) | (~a & c) */
  /* hdmask = (xm ^ ym) | ((xm & ym) & (xv ^ yv)); */
  Rec * hdrec = rec_create(numunitsarg);
  int i, sz;
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    int vi = mi + 1;
    hdrec[mi] = (recXarg[mi] ^ recYarg[mi]) | ((recXarg[mi] & recYarg[mi]) & (recXarg[vi] ^ recYarg[vi]));
  }
  sz = calc_size(hdrec,numunitsarg);
  rec_destroy(hdrec);
  return sz;
}


/* rec_relevant: return true (1) if rarg is relevant (based on
   bitwise-and); rarg is relevant iff there exists a bit set to one or
   dontcare (*) in the same position that the query rec contains a one
   value (v.71) */
int rec_relevant(Rec * qrarg, Rec * rarg, int numunitsarg) 
{
  /*  m  v  mq vq  mr vr
      0  0   1  0   0  0
      1  0   1  0   1  0
      1  1   1  0   1  0
      0  0   1  1   1  1   relevant!
      1  0   1  1   1  0
      1  1   1  1   1  1   relevant!
      x  x   0  0   0  0   n/a (and not relevant)
  */
  /* if (vq & ~(m ^ v)) then relevant */
  /* relevant only when the query value is one and the mask and value
     of rarg are the same (i.e. both are zero or both one) */ 
  int i;
  for(i = 0; i < numunitsarg ; i++) {
    int mi = i << 1;
    int vi = mi + 1;
    if( qrarg[vi] & ~(rarg[mi] ^ rarg[vi])) 
      return 1;                 /* quit, relevancy found */
  }
  return 0;
}


/* only intended for entire record copy -- note: it cannot be
   confident of the hash invarient in the case of rec_shrink */
void rec_copy(Rec * fromarg, Rec * toarg, int numunitsarg)
{
  memcpy(toarg,fromarg,sizeof(Rec) * ((numunitsarg << 1) + 1));
}


/* copy lenarg bits from "fromarg" rec to "toarg" rec (with
   numunitsarg) starting at bit startarg v.27 */
void rec_copy_bitbybit(Rec * fromarg, Rec * toarg, int lenarg, int startarg, int numunitsarg)
{
  int i;
  for(i=0; i < lenarg; i++) {
    int fromval = rec_getbit(fromarg,i);
    if(fromval!=DONTCARE){
      rec_setbit(toarg,i+startarg,fromval,numunitsarg);
    }
  }
}


/* flips all designated bits in fromarg to complement value in toarg;
   this only works when we're in binary mode v.23 */
void rec_copy_complement(Rec * fromarg, Rec * toarg, int lenarg, int numunitsarg)
{
  int i;
  for(i=0; i < lenarg; i++) {
    int fromval = rec_getbit(fromarg,i);
    if(fromval!=DONTCARE){
      rec_setbit(toarg,i,fromval^1,numunitsarg);
    }
  }
}


/* for the very first record read from file, after learning the true
   record length, this function shrinks the maxsize NUMUNITS to a
   record of the exact size, recalculates the hash, and frees the
   maxsize rec -- only used for deprecated ndb format */
Rec * rec_shrink(Rec * fromarg, int numunitsarg)
{
  int hash,size;
  int fromhash = rec_hash(fromarg,NUMUNITS);
  int fromsize = rec_size(fromarg,NUMUNITS);
  Rec * smallrec = rec_create(numunitsarg);
  rec_copy(fromarg,smallrec,numunitsarg); 
  hash = calc_hash(smallrec,numunitsarg);
  assert(fromhash == hash);
  size = calc_size(smallrec,numunitsarg);
  assert(fromsize == size);
  set_hashsize(smallrec,hash,size,numunitsarg);
  rec_destroy(fromarg);
  return smallrec;
}


int rec_checkhash(Rec * rarg, int numunitsarg)
{
  return (rec_hash(rarg,numunitsarg) == (calc_hash(rarg,numunitsarg) & HALFMASK));
}


void rec_destroy(Rec * rarg) 
{
  free(rarg);
}


/* TESTS BEGIN HERE */

/* check a single bit in y set to one then zero, against each bit in x
   set to one, in both directions */
int check_subsumed(void)
{
  int i;
  for(i = 0; i < MAXBITS; i++) {
    int j;
    Rec * y = rec_create(NUMUNITS);
    rec_setbit(y,i,1,NUMUNITS);
    for(j = 0; j < MAXBITS; j++) {
      Rec * x = rec_create(NUMUNITS);
      rec_setbit(x,j,1,NUMUNITS);
      if( i == j && !rec_subsumed_query(x,y,NUMUNITS)) {
        rec_destroy(x);
        rec_destroy(y);
        return 0;
      }
      if( i != j && rec_subsumed_query(y,x,NUMUNITS)) {
        rec_destroy(x);
        rec_destroy(y);
        return 0;
      }
      
      rec_setbit(x,j,0,NUMUNITS);
      if( rec_subsumed_query(x,y,NUMUNITS)) {
        rec_destroy(x);
        rec_destroy(y);
        return 0;
      }
      rec_destroy(x);
    }
    rec_destroy(y);
  }
  return 1;
}


/* check two consecutive bits with opposite values in y against
   incremental bits set to one in x, in both directions */
int check_subsumed2(void)
{
  int i;
  for(i = 0; i < MAXBITS - 1; i++) {
    int j;
    Rec * y = rec_create(NUMUNITS);
    Rec * x = rec_create(NUMUNITS);
    
    rec_setbit(y,i,1,NUMUNITS);
    rec_setbit(y,i+1,0,NUMUNITS);
    
    for(j = 0; j < MAXBITS; j++) {
      rec_setbit(x,j,1,NUMUNITS);
      if( rec_subsumed_query(x,y,NUMUNITS)) {
        rec_destroy(x);
        rec_destroy(y);
        return 0;
      }
      if( i != j && rec_subsumed_query(y,x,NUMUNITS)) {
        rec_destroy(x);
        rec_destroy(y);
        return 0;
      }
    }
    rec_destroy(x);
    rec_destroy(y);
  }
  return 1;
}


int check_copyhash(void)
{
  int i;
  Rec * y = rec_create(NUMUNITS);
  
  for(i = 0; i < MAXBITS; i++) {
    Rec * x = rec_create(NUMUNITS);
    Rec * z;
    Rec * w;
    Rec * u;
    Rec * v;
    rec_setbit(y,i,1,NUMUNITS);
    rec_copy(y,x,NUMUNITS);
    if(!rec_checkhash(x,NUMUNITS) || !rec_equal(y,x,NUMUNITS)){ 
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    rec_init(x,NUMUNITS);
    rec_copy_bitbybit(y,x,MAXBITS,0,NUMUNITS);
    if(!rec_checkhash(x,NUMUNITS) || !rec_equal(y,x,NUMUNITS)){ 
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    rec_init(x,NUMUNITS);
    rec_copy_bitbybit(y,x,MAXBITS/2,MAXBITS/2,NUMUNITS);
    if(!rec_checkhash(x,NUMUNITS) || rec_equal(y,x,NUMUNITS)){ 
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    /*  x second half same as first half of y here */
    u = rec_create(NUMUNITS);
    rec_coalesce_bitbybit(y,x,u,MAXBITS,NUMUNITS);
    z = rec_create(NUMUNITS);
    rec_coalesce(x,y,z,NUMUNITS);
    if(!rec_checkhash(z,NUMUNITS) || rec_equal(x,z,NUMUNITS) || (rec_equal(y,z,NUMUNITS) && i != MAXBITS-1 )){ 
      printf("bitbybit merger (%d),where second half of x is same as first half of y\n",i);
      /*       printf("checkhash: %d not equal %d\n",rec_hash(z,NUMUNITS), calc_hash(z,NUMUNITS)); */
      printf("rec x is: ");
      rec_print(x,MAXBITS,stdout);
      printf("rec y is: ");
      rec_print(y,MAXBITS,stdout);
      printf("rec z is: ");
      rec_print(z,MAXBITS,stdout);
      rec_destroy(x);
      rec_destroy(y);       /* last time around y is all ones and so is z */
      rec_destroy(z);
      rec_destroy(u);
      return 0;
    }
    if(!rec_equal(u,z,NUMUNITS)){ 
      printf("bitbybit merger (%d),where second half of x is same as first half of y: not equal to rec_coalesce_bitbybit result\n",i);
      rec_destroy(u);
      rec_destroy(y);
      rec_destroy(z);
      rec_destroy(x);
      return 0;
    }
    rec_destroy(u);

    /*  reverse merge order */
    v = rec_create(NUMUNITS);
    rec_coalesce_bitbybit(y,x,v,MAXBITS,NUMUNITS);
    w = rec_create(NUMUNITS);
    rec_coalesce(y,x,w,NUMUNITS);
    if(!rec_checkhash(w,NUMUNITS) || !rec_equal(z,w,NUMUNITS)){ 
      printf("bitbybit reverse merger (%d),where second half of x is same as first half of y\n",i);
      /* printf("checkhash: %d not equal %d\n",rec_hash(w,NUMUNITS), calc_hash(w,NUMUNITS)); */
      printf("rec x is: ");
      rec_print(x,MAXBITS,stdout);
      printf("rec y is: ");
      rec_print(y,MAXBITS,stdout);
      printf("rec z is: ");
      rec_print(z,MAXBITS,stdout);
      printf("rec w is: ");
      rec_print(w,MAXBITS,stdout);
      rec_destroy(x);
      rec_destroy(y);
      rec_destroy(w);
      rec_destroy(z);
      rec_destroy(v);
      return 0;
    }
    if(!rec_equal(v,w,NUMUNITS)){ 
      printf("bitbybit reverse merger (%d),where second half of x is same as first half of y: not equal to rec_coalesce_bitbybit result\n",i);
      rec_destroy(v);
      rec_destroy(w);
      rec_destroy(z);
      rec_destroy(y);
      rec_destroy(x);
      return 0;
    }
    rec_destroy(v);
    rec_destroy(w);
    rec_destroy(z);
    rec_destroy(x);
  }
  rec_destroy(y);
  return 1;
}


int check_equality(void)
{
  int i;
  for(i = 0; i < MAXBITS - 1; i++) {
    Rec * y = rec_create(NUMUNITS);
    Rec * x = rec_create(NUMUNITS);
    if(! rec_equal(x,y,NUMUNITS)) {        /* check zero bit records here */
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    rec_setbit(y,i,1,NUMUNITS);
    if(rec_equal(x,y,NUMUNITS)) {
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    rec_setbit(x,i,1,NUMUNITS);
    if(! rec_equal(x,y,NUMUNITS)) {        /* check one bit records here */
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    rec_setbit(y,i+1,0,NUMUNITS);  
    if( rec_equal(x,y,NUMUNITS)) {
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    rec_setbit(x,i+1,0,NUMUNITS);
    if( ! rec_equal(x,y,NUMUNITS)) {
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    if(rec_firstbitdiff(x,y,NUMUNITS) > 0) {    /* v.15 */
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    rec_setbit(x,i+1,1,NUMUNITS);
    if(rec_equal(x,y,NUMUNITS)) {
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    if(rec_firstbitdiff(x,y,NUMUNITS) != i+1) {       /* v.15 */
      rec_destroy(x);
      rec_destroy(y);
      return 0;
    }
    rec_destroy(x);
    rec_destroy(y);
  }
  return 1;
}


/* check a single bit in y set to one then zero, against each bit in x
   set to one, in both directions */
int check_subsumed_stingy(void)
{
  int i,j;
  Rec * h = rec_create(NUMUNITS);
  rec_random(h,MAXBITS,NUMUNITS);
  if(rec_size(h,NUMUNITS) < MAXBITS){
    fprintf(stderr,"failed rec_random test, rec size is %d not %ld\n",
            rec_size(h,NUMUNITS),(long) MAXBITS);
    rec_destroy(h);
    return 0;
  }

  rec_init(h,NUMUNITS);             /* re-init to all ones */
  for(i = 0; i < MAXBITS; i++) {
    rec_setbit(h,i,1,NUMUNITS);
  }

  for(j = 0; j < MAXBITS; j++) {
    Rec * r = rec_create(NUMUNITS);
    rec_setbit(r,j,1,NUMUNITS);
    if(!rec_subsumed_query_stingy(h,r,NUMUNITS)) {
      rec_destroy(r);
      rec_destroy(h);
      fprintf(stderr,"failed first stingy test, j = %d\n",j);
      return 0;
    }
    rec_setbit(r,j,0,NUMUNITS);
    if(rec_subsumed_query_stingy(h,r,NUMUNITS)) {
      rec_destroy(r);
      rec_destroy(h);
      fprintf(stderr,"failed second stingy test, j = %d\n",j);
      return 0;
    }
    rec_destroy(r);
  }

  /* test with accumulating bits, second one doesn't match but shouldn't matter */
  for(j = 0; j < MAXBITS-1; j++) {
    Rec * r = rec_create(NUMUNITS);
    rec_setbit(r,j,1,NUMUNITS);
    if(!rec_subsumed_query_stingy(h,r,NUMUNITS)) {
      rec_destroy(r);
      rec_destroy(h);
      fprintf(stderr,"failed third stingy test, j = %d\n",j);
      return 0;
    }
    rec_setbit(r,j+1,0,NUMUNITS);
    if(!rec_subsumed_query_stingy(h,r,NUMUNITS)) {
      rec_destroy(r);
      rec_destroy(h);
      fprintf(stderr,"failed fourth stingy test, j = %d\n",j);
      return 0;
    }
    rec_destroy(r);
  }
  rec_destroy(h);
  return 1;
}


void test_rec(void) 
{
  int i, j;
  Rec * bits1 = rec_create(NUMUNITS);
  fprintf(stderr,"starting test_rec..\n");

  /* test: set every bit in a new record is initialized to dontcare */
  for(i = 0; i < MAXBITS; i++) {
    rec_init(bits1,NUMUNITS);
    for(j = 0; j < MAXBITS; j++) {
      if(!rec_isdc(bits1,j)){
	exit(-4);
      }
    }
  }

  {
    /* test: set every bit to 1, and verify the size increments by 1 */
    rec_init(bits1,NUMUNITS);
    for(j = 0; j < MAXBITS; j++) {
      int sz;
      rec_setbit(bits1,j,1,NUMUNITS);
      sz = rec_size(bits1,NUMUNITS);
      if( sz != j+1){
	printf("rec size at %d is %d\n",j,sz);
	exit(5);
      }
    }
  }

  {
    /* test: set every other bit alternately 0 and 1, and clear to dontcare */
    int endcase = MAXBITS / 2 ;
    rec_init(bits1,NUMUNITS);

    for(j = 0; j < endcase; j++) {
      int sz;
      /*      printf("set bit %d to %d\n", j*2, j%2); */
      rec_setbit(bits1,j * 2,j % 2,NUMUNITS);   
      rec_setbit(bits1,j * 2, DONTCARE,NUMUNITS); 
      sz = rec_size(bits1,NUMUNITS);
      if( sz > 0){
	printf("rec size at %d is %d\n",j,sz);
	exit(6);
      }
    }
  }

  if(! check_subsumed() ) {
    printf("subsumed test 1 failed\n");
    exit(7);
  }

  if(! check_subsumed2() ) {
    printf("subsumed test 2 failed\n");
    exit(8);
  }

  if(! check_equality() ) {
    printf("equality test failed\n");
    exit(9);
  }

  if(! check_copyhash() ) {
    printf("copyhash test failed\n");
    exit(11);
  }

  if(! check_subsumed_stingy() ) {
    printf("subsumed test 3 stingy failed\n");
    exit(10);
  }

  rec_destroy(bits1);
  printf("test_rec completed\n");
}


/* test that records created with rec_int2rec are equal to those
   created with rec_string2rec v.55 */
void test_rec_int()
{
  int len = 3;
  int numunits = rec_calcnumunits(len);
  int endloop = 1 << len;
  unsigned int i = 0;
  Rec * reci = rec_create(numunits);
  Rec * recs = rec_create(numunits);

  fprintf(stdout,"starting test_rec_int..\n");

  while(i < endloop) {
    char qstr[4];
    itobs(i,qstr,len);
    rec_init(recs,numunits);
    rec_string2rec(recs,len,qstr,1);

    rec_init(reci,numunits);
    rec_int2rec(reci,len,i,numunits);

    if(!rec_equal(recs,reci,numunits)){
      fprintf(stderr,"test_rec_int: recs and reci (%d) not equal, sizes = %d,%d hash = %d,%d: \n",i,rec_size(recs,numunits), rec_size(reci,numunits), (int) rec_hash(recs,numunits), (int) rec_hash(reci,numunits));
      rec_print(recs,len,stderr);
      rec_print(reci,len,stderr);
      rec_destroy(reci);
      rec_destroy(recs);
      exit(7);
    }
    i++;
  }

  fprintf(stdout,"test 32-bit boundary case..\n");  /* v.56.3 */
  { 
    char qstr[33]; 
    len = 32;
    numunits = rec_calcnumunits(len);

    i = (1 << len) -1;
    if(i != ~0) {
      fprintf(stderr,"fyi: int_max, all ones NOT as shifted,%d..use ~0 instead\n",i);
      i = ~0;    /* int_max, all ones */
    }

    /* note: 32-bit shift tests "correctly" with -O compile time
       argument greater than one */

    itobs(i,qstr,len);
    rec_init(recs,numunits);
    rec_string2rec(recs,len,qstr,1);

    rec_init(reci,numunits);
    rec_int2rec(reci,len,i,numunits);

    printf("%d all ones: <%s> and <%d> <%u>\n",len,qstr,i,i);
    rec_print(recs,len,stdout);
    rec_print(reci,len,stdout);

    if(!rec_equal(recs,reci,numunits)){
      fprintf(stderr,"test_rec_int: recs and reci (%d) not equal, sizes = %d,%d hash = %d,%d: \n",i,rec_size(recs,numunits), rec_size(reci,numunits), (int) rec_hash(recs,numunits), (int) rec_hash(reci,numunits));
      rec_print(recs,len,stderr);
      rec_print(reci,len,stderr);
      rec_destroy(reci);
      rec_destroy(recs);
      exit(7);
    }
  } 
  rec_destroy(reci);
  rec_destroy(recs);
  printf("test_rec_int completed\n");    
}


void test_rec_or()
{
  int i;
  int len = 3;
  int numunits = rec_calcnumunits(len);
  int endloop = 1 << len;
  Rec * reci = rec_create(numunits);
  Rec * recs = rec_create(numunits);
  Rec * reco = rec_create(numunits);
  Rec * recobb = rec_create(numunits);

  fprintf(stderr,"starting test_rec_or..\n");

  fprintf(stdout,"test_rec_or: or same values..\n");
  i = 0;
  while(i < endloop) {
    char qstr[4];
    itobs(i,qstr,len);
    rec_init(recs,numunits);
    rec_string2rec(recs,len,qstr,1);

    rec_init(reci,numunits);
    rec_int2rec(reci,len,i,numunits);

    rec_init(reco,numunits);
    rec_init(recobb,numunits);

    rec_or(reci,recs,reco,numunits);
    rec_or_bitbybit(reci,recs,recobb,len,numunits);

    fprintf(stdout,"test_rec_or: %d | %d is :",i, i);
    rec_print(reco,len,stdout);

    if(!rec_equal(reco,recobb,numunits)){
      fprintf(stderr,"test_rec_or: reco and recobb (%d) not equal, sizes = %d,%d hash = %d,%d: \n",i,rec_size(reco,numunits), rec_size(recobb,numunits), (int) rec_hash(reco,numunits), (int) rec_hash(recobb,numunits));
      rec_print(reco,len,stderr);
      rec_print(recobb,len,stderr);
      rec_destroy(reci);
      rec_destroy(recs);
      rec_destroy(reco);      
      rec_destroy(recobb);
      exit(7);
    }
    i++;
  }

  fprintf(stdout,"test_rec_or: or complement values..\n");
  i = 0;
  while(i < endloop) {
    char qstr[4];
    itobs(i,qstr,len);
    rec_init(recs,numunits);
    rec_string2rec(recs,len,qstr,1);

    rec_init(reci,numunits);
    rec_int2rec(reci,len,endloop - i - 1,numunits);

    rec_init(reco,numunits);
    rec_init(recobb,numunits);

    rec_or(reci,recs,reco,numunits);
    rec_or_bitbybit(reci,recs,recobb,len,numunits);

    fprintf(stdout,"test_rec_or: %d | %d is :",i, endloop -i -1);
    rec_print(reco,len,stdout);

    if(!rec_equal(reco,recobb,numunits)){
      fprintf(stderr,"test_rec_or: reco and recobb (%d) not equal, sizes = %d,%d hash = %d,%d: \n",i,rec_size(reco,numunits), rec_size(recobb,numunits), (int) rec_hash(reco,numunits), (int) rec_hash(recobb,numunits));
      rec_print(reco,len,stderr);
      rec_print(recobb,len,stderr);
      rec_destroy(reci);
      rec_destroy(recs);
      rec_destroy(reco);      
      rec_destroy(recobb);
      exit(7);
    }
    i++;
  }

  rec_destroy(reci);
  rec_destroy(recs);
  rec_destroy(reco);      
  rec_destroy(recobb);
  printf("test_rec_or completed\n");    
}


void test_rec_hammingdistance()
{
  int i;
  int len = 3;
  int numunits = rec_calcnumunits(len);
  int endloop = 1 << len;
  Rec * reci = rec_create(numunits);
  Rec * recs = rec_create(numunits);
  int consecHD[7];      /* (1,2,1,3,1,2,1) */
  consecHD[0] = 1;
  consecHD[1] = 2;
  consecHD[2] = 1;
  consecHD[3] = 3;
  consecHD[4] = 1;
  consecHD[5] = 2;
  consecHD[6] = 1;

  fprintf(stdout,"starting test_rec_hammingdistance..\n");

  fprintf(stdout,"test_rec_hd: same values..\n");
  i = 0;
  while(i < endloop) {
    char qstr[4];
    int hd;
    itobs(i,qstr,len);
    rec_init(recs,numunits);
    rec_string2rec(recs,len,qstr,1);

    rec_init(reci,numunits);
    rec_int2rec(reci,len,i,numunits);

    hd = rec_hammingdistance(reci,recs,numunits);
    if( hd != 0){
      fprintf(stderr,"test_rec_hd: hamming distance of two same recs (%d) is %d, not 0\n",i,hd);
      exit(7);
    }
    i++;
  }

  fprintf(stdout,"test_rec_or: or complement values..\n");
  i = 0;
  while(i < endloop) {
    char qstr[4];
    int hd;
    itobs(i,qstr,len);
    rec_init(recs,numunits);
    rec_string2rec(recs,len,qstr,1);

    rec_init(reci,numunits);
    rec_int2rec(reci,len,endloop - i - 1,numunits);

    hd = rec_hammingdistance(reci,recs,numunits);
    if( hd != len){
      fprintf(stderr,"test_rec_hd: hamming distance of two complement recs (%d) is %d, not %d\n",i,hd,len);
      printrec(recs,len,stderr);
      printrec(reci,len,stderr);
      exit(8);
    }
    i++;
  }


  fprintf(stdout,"test_rec_or: or consecutive values..\n");
  i = 0;
  endloop--;
  while(i < endloop) {
    char qstr[4];
    int hd;
    itobs(i,qstr,len);
    rec_init(recs,numunits);
    rec_string2rec(recs,len,qstr,1);

    rec_init(reci,numunits);
    rec_int2rec(reci,len,i+1,numunits);
    
    hd = rec_hammingdistance(reci,recs,numunits);
    if( hd != consecHD[i]){
      fprintf(stderr,"test_rec_hd: hamming distance of two consecutive recs (%d) is %d, not %d\n",i,hd,consecHD[i]);
      exit(9);
    }
    i++;
  }

  rec_destroy(reci);
  rec_destroy(recs);
  printf("test_rec_hammingdistance completed\n");    
}
