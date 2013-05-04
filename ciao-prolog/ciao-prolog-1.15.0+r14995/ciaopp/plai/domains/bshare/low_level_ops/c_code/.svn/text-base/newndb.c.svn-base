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
 * Filename      : newndb.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for New NDB Algorithms
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Fri Mar 24 09:07:30 2006 
 * Updated       : Sun Mar  9 08:09:27 2008 
 *
 * Comments : based on the Fernando Esponda Empty NDB Algorithms; a
 * well-known random SAT formula for epsilon non-empty hard instances;
 * Haixia Jia's q-hidden algorithm for new singleton negative
 * databases; basic empty and full single negative record ndb.
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include "perm.h"
#include "freq.h"
#include "negdb.h"
#include "recset.h"
#include "newndb.h"
#include "error.h"

#if defined(ISCHARDQR)   || defined(NORANDSATLENLIMIT) 
#define Q 1                /* even chance (q=.5) to flip a bit */
#else
#define Q 1                /* as Q decreases, q increases (was golden
                            ratio 0.618, where they stay the same) .83
                            is the smallest Q (for maximum q=.546448)
                            that works for 100-bit lengths v.35 */
#endif

#define MAXTRIES 1000

int logbase2(int narg)
{
  int exp = 0;
  while((narg = narg >> 1) > 0) exp++;
  return exp;
}


/* *****************************************************
   Spec: Empty_NDB_Create(l)
   1. Pick log(l) bit positions at random
   2. for every possible assignment Vp of this positions{
   3.      select k1 randomly 1 <= k1 <= l
   4.      for j=1 to k1 {
   5.              select an additional n distinct positions
   6.              for every possible assignment Vq of these positions
   7.              Pick k2 bits at random from Vp middot Vq.
   8.              Create a entry for NDB with the k2
   chosen bits and fill the remaining l - k2
   positions with the don't care symbol.}}}
   ******************************************************* */
/* here are the differences between this function and the spec (per fe):
 * in the first step we select one bit position at random instead of
 * log(l). (or not at random if NPG_DETERMINISTIC v.61.4)
 *
 * in step 3, we use n, a user argument (-g) for extra bits to add
 * that defaults to zero: only when RECS2ADD is defined and n is nonzero will
 * determine_minimum_records2add select k1 at random, otherwise it
 * defaults to one.
 *
 * steps 4-8 describe negative_pattern_generate (npg); here, we call
 * npg when n is nonzero AND k2, the number of bits needed (bn) to achieve
 * the user specified number of minimum bits (-m) plus the extra bits (-g),
 * is nonzero.  
 */
int empty_ndb_create_onebit(Recset * rsarg, Runtime * rtarg)
{
  Rec * rec;
  int rbp, pi, n, numunits;
  Freq * bitfreqs;
  int len = recset_length(rsarg);

  if(len <= 0) {
    fprintf(stderr,"ERROR: New Empty Negative Database requires nonzero positive record length, not %d\n",len);
    return err5;
  }

  numunits = recset_numunits(rsarg);
  n = runtime_getN(rtarg);
  /* n=0;            try this!! */

#if ! (NPG_DETERMINISTIC)
  rbp = nextRand(len);        /* select one bit position at random */
#else
  rbp = 0;                    /* v.61.4 */
#endif

  /* initialize bit frequency to all ones -- even chance */
  bitfreqs = freq_create(numunits);
  freq_init_random(bitfreqs,len);
  recset_setbitfrequency(rsarg,bitfreqs);
  rec = rec_create(numunits);

  pi = 2;                         /* twice around the loop necessary */

  while(pi--) {
    rec_init(rec,numunits);
    rec_setbit(rec,rbp,pi,numunits);
    insert(rec,rsarg,rtarg,(!n?wo_npg:with_npg),n); 
  }
  recset_setupdateflag(rsarg);
  rec_destroy(rec);
  return 0;
}


/* *****************************************************
   Spec: Empty_NDB_Create(l)
   1. Pick log(l) bit positions at random
   2. for every possible assignment Vp of this positions{
   3.      select k1 randomly 1 <= k1 <= l
   4.      for j=1 to k1 {
   5.              select an additional n distinct positions
   6.              for every possible assignment Vq of these positions
   7.              Pick k2 bits at random from Vp middot Vq.
   8.              Create a entry for NDB with the k2
                   chosen bits and fill the remaining l - k2
                   positions with the don't care symbol.}}}
 ********************************************************/
/* here are the differences between this function and the spec (per fe):
 *
 * in step 3, we use n, a user argument (-g) for extra bits to add
 * that defaults to zero: only when RECS2ADD is defined and n is nonzero will
 * determine_minimum_records2add select k1 at random, otherwise it
 * defaults to one.
 *
 * randomization removed when compiled with NPG_DETERMINISTIC,
 * uses log(l) bit positions, minimum of one. (v.61.4).
 * 
 */
int empty_ndb_create(Recset * rsarg, Runtime * rtarg)
{
  Rec * rec;
  int rbp, pi, n, numunits;
  Freq * bitfreqs;
  Perm randPerm;
  int len = recset_length(rsarg);
  Negpattgen npg = with_npg;

#if NPG_NONE_NEWNDB0
  npg = wo_npg;                  /* v.63 */
#endif

  if(len <= 0) {
    fprintf(stderr,"ERROR: New Empty Negative Database requires nonzero positive record length, not %d\n",len);
    return err5;
  }

  numunits = recset_numunits(rsarg);
  n = runtime_getN(rtarg);          /* number of extra bits */

  perm_init(&randPerm,len);

  rbp = logbase2(len);

#if ! (NPG_DETERMINISTIC)
  fprintf(stdout,"empty_ndb_create: USING RANDOMIZATION\n");
  /* select number of bit positions at random, min of one */
  perm_randomize(&randPerm,len);

  /* pick a random number of log(1) bit positions */
  rbp = (rbp==0 ? 1 : nextRand(rbp) + 1 );  
#else
  /* select log len number of bit positions, min of one v.61.4 */
  rbp = (rbp==0 ? 1 : rbp );  
#endif

  fprintf(stderr,"selected %d bit positions for empty db\n",rbp); 

  /* if minbits (k2) is unspecified by user (-m or -k), set to random number of bits */  
  if(!runtime_getMinBits(rtarg)) {
    runtime_setMinBits(rtarg,rbp);
  }

  /* initialize bit frequency to all ones -- even chance for extra bits */
  bitfreqs = freq_create(numunits);
  freq_init_random(bitfreqs,len);
  recset_setbitfrequency(rsarg,bitfreqs);
  rec = rec_create(numunits);

  pi = 1 << rbp;                 /* 2^n times around the loop necessary */

  while(pi--) 
    {
      int i = rbp;
      int v = pi;
      rec_init(rec,numunits);
      /*      fprintf(stderr,"loop pi=%d, i=%d, v=%d\n",pi,i,v); */

      /* inner loop to build each record */
      while(i--) 
        {
          int idx = perm_get(&randPerm,i);
          /*          fprintf(stderr,"selected idx %d at random\n",idx); */
          rec_setbit(rec,idx,(v & 1),numunits);
          v >>= 1;
        }

      insert(rec,rsarg,rtarg,npg,n); 
    }
  recset_setupdateflag(rsarg);
  rec_destroy(rec);
  perm_final(&randPerm);
  return 0;
}


/* 
 * this is based on a well-known random SAT formula for hard instances
 * and a reimplementation of zero-hidden case in hmany.C by Haixia Jia (v.20); 
*/
int empty_ndb_create_randomSATformula(Recset * rsarg, Runtime * rtarg)
{
  Rec * rec;
  int numunits, numrecs, k;
  Freq * bitfreqs;                
  float r;      /* haixia says r >= 4.5 or 5 when k is 3, 2^k when k > 3 */
  int len = recset_length(rsarg);           /* must be around 100 */
  int count = 0;
  int maxtries, tries = 0;


#ifndef NORANDSATLENLIMIT
  if(len <= 0 || len < 96) {
    fprintf(stderr,"ERROR: New Empty Negative Database using Random SAT Formula requires nonzero positive record length of at least 96, not %d\n",len);
    return err5;
  }
#endif

  numunits = recset_numunits(rsarg);

  /* if minbits is unspecified by user (-m), set to 3-SAT */  
  if(!runtime_getMinBits(rtarg)) {
    runtime_setMinBits(rtarg,3);
  }

  k= runtime_getMinBits(rtarg);
  assert(k < (INTSIZE - 1));
  r = 1 << (k + 1);             /*      r = 2**(k+1) */  

#ifdef NORANDSATLENLIMIT
  r = (k==3 ? 4.5 : 1 << k);               /* 2^k */
#endif

#ifdef ISCHARDQR
  r = 5.5;
#endif

  numrecs = len * r;
  maxtries = MAXTRIES * numrecs;
  /*  fprintf(stderr,"%d number of records needed\n",numrecs); */

  /* initialize bit frequency to all ones--even chance. freq required
     for insert with_npg. freq required to get bitdistribution data */
  bitfreqs = freq_create(numunits);
  freq_init_random(bitfreqs,len);
  recset_setbitfrequency(rsarg,bitfreqs);

  rec = rec_create(numunits);

  while(count < numrecs && tries < maxtries){
    Perm randPerm;
    int i;

    /*    fprintf(stderr,"creating empty record numrec %d\n",numrecs); */
    rec_init(rec,numunits);

    /* pick a random number of k bit positions and set to 0 or 1
       at random */
    
    perm_init(&randPerm,len);
    perm_randomize(&randPerm,len);
    /*        fprintf(stderr,"creating empty record numrec %d --- loop count is %d\n",numrecs,count); */
    for(i = 0; i < k; i++){
      int idx = perm_get(&randPerm,i); 
      /*          fprintf(stderr,"%d. selected idx %d at random\n",i,idx); */
      rec_setbit(rec,idx,coinflip(),numunits);
    }
    
    count += insert(rec,rsarg,rtarg,wo_npg,0);
    perm_final(&randPerm);
    tries++;
  }

  recset_setupdateflag(rsarg);
  rec_destroy(rec);

  fprintf(stdout,"fyi: %d duplicates avoided (%d tries for %d records)\n", tries - numrecs, tries, numrecs);

  if(tries >= maxtries){
    fprintf(stderr,"ERROR: New Empty Negative Database using Random SAT Formula tried %d times, but couldn't find %d unique records (r is %f)\n",tries,numrecs,r);
    return err20;
  }

  return 0;
}

/* this is based on q-hidden SAT formula for hard instances described
   in "Generating Hard Satisfiable Formulas by Hiding Solutions
   Deceptively" (AAAI05) and a reimplementation of one-hidden case in
   hiddenq.C by Haixia Jia (v.21); return 0 for successful add; 1 for
   unsuccessful add; o.w. error; updated v.70 to do more than one hiddedn
   solution as hinted at in Haixia's dissertation */
int singleton_ndb_create(Recset * multiinputsetarg, Recset * rsarg, Runtime * rtarg)
{
  Rec * rec;
  int numunits, numrecs, k, numhidden;
  Freq * bitfreqs;
  int * val;
  float r;          /* haixia says r >= 4.5 or 5 when k is 3, 2^k when k > 3 */
  double q = (1 / (1.0 + Q));        /* constant for balanced hidden formula */
  int len = recset_length(rsarg);               /* must be around 100 */
  int count = 0;
  int maxtries, tries = 0;

  if(len != ( recset_length(multiinputsetarg) * (runtime_getBinMode(rtarg) ? 1 : CHAR_T))) {
    fprintf(stderr,"singleton: len is %d, multiinput len is %d\n",
            len, recset_length(multiinputsetarg));
  }

  assert(len == recset_length(multiinputsetarg));

  numhidden = recset_size(multiinputsetarg);
  assert(numhidden > 0);

#ifndef NORANDSATLENLIMIT
  if(len < 0 || len < 96) {
    fprintf(stderr,"New Singleton Negative Database using Haixia's q-hidden SAT Formula requires nonzero positive record length of at least 96, not %d\n",len);
    return err5;
  }
#endif

  fprintf(stderr,"q is %f\n",q); 
  numunits = recset_numunits(rsarg);

  /* if minbits is unspecified by user (-m or -k), set to 3-SAT */  
  if(!runtime_getMinBits(rtarg)) {
    /*    fprintf(stderr,"min bits is 3\n"); */
    runtime_setMinBits(rtarg,3);
  }

  k= runtime_getMinBits(rtarg);

  /* here, determining r is tricky business---too big or too small and
     the solution is too easy to solve. haixia says r >= 4.5 or 5 when
     k is 3, 2^k when k > 3. note, r times the length of the record
     gives us the number of records to create. duplicate records are
     caught as they are inserted, not here. */
  assert(k < (INTSIZE -1));
  r = 1 << (k + 1);                             /* default: r = 2**(k+1) */  

#ifdef NORANDSATLENLIMIT
  r = (k==3 ? 4.5 : 1 << k);    /* NOT ENOUGH records too many solutions */
#endif

#ifdef ISCHARDQR
  r = 5.5;         /* overrides NORANDSATLENLIMIT (good for reclen 1000) */
#endif

#if 0
  if(k > 6 && len >= 192){          /* long records need fewer recs v.25 */
    r = 1 << k;
  }
#endif

  numrecs = len * r;
  maxtries = MAXTRIES * numrecs;
  fprintf(stderr,"r is %f, %d number of records needed\n",r,numrecs);
  
  val = malloc(sizeof(int) * k);
  assert(val);

  /*  hidden = rec_create(numunits);
      rec_string2rec(hidden,len,strarg,runtime_getBinMode(rtarg)); */

  /* initialize bit frequency to all ones--even chance. freq required
     for insert with_npg. freq required to get bitdistribution data */
  bitfreqs = freq_create(numunits);
  freq_init_random(bitfreqs,len);
  recset_setbitfrequency(rsarg,bitfreqs);

  rec = rec_create(numunits);

  while(count < numrecs && tries < maxtries){
    Rec * hidden;         /* select at random each time through */
    Perm randPerm;
    int i;
    int allequal = 1;
    
    hidden = recset_getrec(multiinputsetarg,nextRand(numhidden)); 

    /* 
     *  Get all k values: "minus" for value 1; "plus" for value 0 with
     *  probability q; keep trying while all values are 0; stop when
     *  there's at least one 1. 1's flip the bit value. v.35
     */
    
    /*    fprintf(stderr,"get all k values: numrec is %d\n",numrecs); */

    while(allequal){
      for(i = 0; i < k; i++){
        double t = randd();              /* random number 0 to 1 */
                                  /* - with prob 1/(1+Q), o.w. + */
        if(t < q){
          val[i] = 1;   /* use complement: 1 with prob q, o.w. 0 */
        } else {
          val[i] = 0;
        }
      }

      for(i = 0; i < k; i++){
        allequal = allequal && (val[i] == 0);
        /* fprintf(stderr,"allequal is %d, val of %d is %d\n",allequal,i,val[i]); */
      }
    } /* end "allequal" while loop when k values are NOT all equal (to
         0), one bit must be flipped */
    
    /* fprintf(stderr,"\nNOT ALL EQUAL: creating singleton record numrec %d\n",numrecs); */
    rec_init(rec,numunits);
    
    /*
     * Pick a random number of k bit positions here
     */
    
    perm_init(&randPerm,len);
    perm_randomize(&randPerm,len);
    /*    fprintf(stderr,"creating record %d --- loop count is %d\n",numrecs,count); */

    /* 
     * Flip hval when random value is 1, and use in neg record. v.35
     */

    for(i = 0; i < k; i++){
      int idx = perm_get(&randPerm,i); 
      int hval = rec_getbit(hidden,idx);
      int newval = (hval ^ val[i]);         /* v.35 val of 1 flips hval */
      assert(hval>=0);     /* absolutely no dontcares */
      /*      fprintf(stderr,"%d. selected idx %d at random--",i,idx); 
              fprintf(stderr,"hidden val is %d. val is %d. newval is %d\n",
              hval,
              val[i],
              newval
              ); 
      */       
      rec_setbit(rec,idx,newval,numunits);
    }

    /* Insure rec does NOT match any of the multiinput recs, that is,
       all recs in multiinputsetarg do not match rec v.70 */
    if(!recset_query_match(multiinputsetarg,rec)){

      /* same as recset_addrec with insert making a copy of rec for rsarg;
         always 0 extrabits so as not to upset the numrec count v.35 */

      count += insert(rec,rsarg,rtarg,wo_npg,0);
    } else {
      /*      fprintf(stderr,"singleton: new neg record ");
              rec_print(rec,len,stderr);
              fprintf(stderr,"matches an input solution, DON'T USE IT!\n ");
      */
    }

    perm_final(&randPerm);
    tries++;
  } /* end next rec while loop */
  
    /* house-keeping, the hidden multiinputset is destroyed by runtime
       final later */
  recset_setupdateflag(rsarg);
  rec_destroy(rec);
  free(val);
  
  fprintf(stdout,"fyi: %d duplicates avoided (%d tries for %d records)\n", tries - numrecs, tries, numrecs);

  if(tries >= maxtries){
    fprintf(stderr,"ERROR: New Singleton Negative Database tried %d times, but couldn't find %d unique records (r is %f)\n",tries, numrecs,r);
    return err20;
  }

  /* check all hidden solutions have been "added" successfully, that is,
     they are NOT in NDB. note, superfluous solutions may also exist. .v.70 */
  {
    int i = 0;
    int errs = 0;
    while(i < numhidden) {
      Rec * qhidden = recset_getrec(multiinputsetarg,i);

      if(recset_query(rsarg,qhidden)){
        fprintf(stdout,"<B>ERROR! Unsuccessful Singleton Add </B>");
        rec_print(qhidden,len,stdout);
        fprintf(stdout," <B>is in NDB (not in DB)</B>");
        PrintBR(rtarg);
        errs++;
      } else {
        PrintBR(rtarg);
        fprintf(stdout,"<B>SUCCESSFULLY Added</B> ");
        rec_print(qhidden,len,stdout);
        fprintf(stdout,"<B>is in DB (not in NDB)</B>");
        PrintBR(rtarg);
      }
      i++;
    }

    if(errs > 0) 
      return 1;
  }
  return 0; 
}


/* returns recset with one record, all dontcares */
int empty_ndb_create_basic(Recset * rsarg, int lenarg){
  Rec * rec;

  recset_setlength(rsarg,lenarg);
  rec = rec_create(recset_numunits(rsarg));
  recset_addrec(rec,rsarg);
  recset_setupdateflag(rsarg);
  return 0;
}


/* returns recset with no negative records -  powerset DB -
   mode 2 v.66.1 */
int full_ndb_create_basic(Recset * rsarg, int lenarg){
  recset_setlength(rsarg,lenarg);
  recset_setupdateflag(rsarg);
  return 0;
}
