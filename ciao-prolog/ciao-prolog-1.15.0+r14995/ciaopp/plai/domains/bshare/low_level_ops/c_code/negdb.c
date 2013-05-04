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
 * Filename      : negdb.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for NDB Update Algorithms
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Mon Apr 21 13:01:46 2008 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include "negdb.h"
#include "error.h"

/* insure exactly one negative_pattern_generate version is defined, or
   set the default to NPG_REMV_BIAS (v.19); change default to NPG_NONE
   v.60.1; restored default back to NPG_REMV_BIAS v.63 */

#if (!defined NPG_REMV_BIAS && !defined NPG_ADD_RAND && !defined NPG_REMV_RAND && !defined NPG_DETERMINISTIC)
#define NPG_REMV_BIAS 1
#warning using default NPG_REMV_BIAS Negative Pattern Generate
#elif (NPG_REMV_BIAS && NPG_REMV_RAND) || (NPG_REMV_BIAS && NPG_ADD_RAND) ||  (NPG_REMV_RAND && NPG_ADD_RAND) || (NPG_REMV_BIAS && NPG_DETERMINISTIC) || (NPG_REMV_RAND && NPG_DETERMINISTIC) || (NPG_ADD_RAND && NPG_DETERMINISTIC) 
#error in MAKEFILE: MULTIPLE NPG -DEFS FOR NEGATIVE_PATTERN_GENERATE
#endif 


#if NPG_DETERMINISTIC
/* update rarg by removing non-essential bits deterministically until the size
   (specified bits) of rarg is minbits or entire length has been
   tried.
*/
static void deterministic_negative_pattern_generate(Rec * rarg, Recset * rsetarg, Runtime * rtarg) 
{
  int v = 0;
  int reclen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  int minbits = runtime_getMinBits(rtarg);
  int recsize = rec_size(rarg,numunits);

  while(v < reclen && recsize > minbits) {
    int idx = v;

#if NPG_VERBOSE_DEBUG
    fprintf(stdout,"npg: v is %d, idx is %d, rec before is ",v,idx);
    rec_print(rarg,reclen,stdout);
#endif

    if(! rec_isdc(rarg,idx) ) {                    /* skip if already a dc */
      rec_flipbit(rarg,idx);                       
      if(recset_query(rsetarg,rarg)) {
	rec_setbit(rarg,idx,DONTCARE,numunits);
	recsize--;
      } else {
	rec_flipbit(rarg,idx);                    /* revert back */
      }
    }
    
#if NPG_VERBOSE_DEBUG
    fprintf(stdout,"npg: v is %d, idx is %d, rec after is ",v,idx);
    rec_print(rarg,reclen,stdout);
#endif

    v++;
  }
  return;
}
#endif


#if NPG_REMV_BIAS
/* update rarg by removing non-essential bits randomly until the size
   (specified bits) of rarg is minbits or entire length has been
   tried.
*/
static void yetanother_negative_pattern_generate(Rec * rarg, Recset * rsetarg, Runtime * rtarg) 
{
  Perm randPerm;
  int v = 0;
  int plen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  int minbits = runtime_getMinBits(rtarg);
  int recsize = rec_size(rarg,numunits);
  int mostfreqflipped = 0;
  Freq * bitfreq = recset_getbitfrequency(rsetarg);

  if (bitfreq == NULL){                   /* fixed v.53 */
    recset_update_bit_frequency(rsetarg);
    bitfreq = recset_getbitfrequency(rsetarg);
  }

  perm_init(&randPerm,plen);
  plen = perm_notdontcares(&randPerm,rarg,plen);   /* skip dontcares */

  /* here, 2 versions at getting the bit frequency to be more evenly
     distributed; perm_ordered is in decending order, while
     perm_weighted_roulette is likely in decending order */

  perm_weighted_roulette(&randPerm,bitfreq); 
  /*  perm_ordered(&randPerm,bitfreq); */

  /*  fprintf(stderr,"yanpg: %d recsize, %d minbits, %d plen\n",
      recsize, minbits, plen); */
  
  while(v < plen && recsize > minbits) {
    int idx = perm_get(&randPerm,v);
#if NPG_VERBOSE_DEBUG
    fprintf(stdout,"npg: v is %d, idx is %d, rec before is ",v,idx);
    rec_print(rarg,recset_length(rsetarg),stdout);
#endif

    rec_flipbit(rarg,idx);                     
    if(recset_query(rsetarg,rarg)) {
      rec_setbit(rarg,idx,DONTCARE,numunits);
      recsize--;
      if(!v) mostfreqflipped++;
    } else {
      rec_flipbit(rarg,idx);                    /* revert back */
    }

#if NPG_VERBOSE_DEBUG
    fprintf(stdout,"npg: v is %d, idx is %d, rec after is ",v,idx);
    rec_print(rarg,recset_length(rsetarg),stdout);
#endif

    v++;
  }

#if NPG_VERBOSE_DEBUG
  if(mostfreqflipped && runtime_getCommand(rtarg) != 'N' ) {
    fprintf(stdout,"npg: flipped most frequent");
    PrintBR(rtarg);
  }
#endif

  perm_final(&randPerm);
  /*    fprintf(stderr,"yanpg: %d recsize now\n",recsize); */
  return;
}
#endif


#if NPG_REMV_RAND
/* update rarg by removing non-essential bits randomly until the size
   (specified bits) of rarg is minbits or entire length has been
   tried.
*/
static void new_negative_pattern_generate(Rec * rarg, Recset * rsetarg, Runtime * rtarg) 
{
  Perm randPerm;
  int v = 0;
  int reclen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  int minbits = runtime_getMinBits(rtarg);
  int recsize = rec_size(rarg,numunits);

  perm_init(&randPerm,reclen);
  perm_randomize(&randPerm,reclen);
  
  while(v < reclen && recsize > minbits) {
    int idx = perm_get(&randPerm,v);

#if NPG_VERBOSE_DEBUG
    fprintf(stdout,"npg: v is %d, idx is %d, rec before is ",v,idx);
    rec_print(rarg,reclen,stdout);
#endif

    if(! rec_isdc(rarg,idx) ) {                    /* skip if already a dc */
      rec_flipbit(rarg,idx);                       
      if(recset_query(rsetarg,rarg)) {
	rec_setbit(rarg,idx,DONTCARE,numunits);
	recsize--;
      } else {
	rec_flipbit(rarg,idx);                    /* revert back */
      }
    }

#if NPG_VERBOSE_DEBUG
    fprintf(stdout,"npg: v is %d, idx is %d, rec after is ",v,idx);
    rec_print(rarg,reclen,stdout);
#endif

    v++;
  }
  perm_final(&randPerm);
  return;
}
#endif

static void c_key_negative(Rec * rarg, Perm * sivarg, Recset * rsetarg, Runtime * rtarg);

#if NPG_ADD_RAND
static void negative_pattern_generate(Rec * rarg, Recset * rsetarg, Runtime * rtarg) 
{
  Rec * recsav;     /* holds values of unnecessary bits indexes */
  Perm siv;         /* extra bits indexes --- on the stack */
  int t;
  int sz;
  int i;
  int numunits = recset_numunits(rsetarg);

  perm_init(&siv,recset_length(rsetarg));

  recsav = rec_create(numunits);         /* v.19 bug fix */
  rec_copy(rarg,recsav,numunits);       /* also useful if not enough bits. */

  /*  fprintf(stderr,"print rec before ckeyneg: \n");
      rec_print(rarg,recset_length(rsetarg),stderr);
  */

  c_key_negative(rarg,&siv,rsetarg,rtarg);  /* set up siv and rarg */

  /*  fprintf(stderr,"print rec after ckeyneg: \n");
      rec_print(rarg,recset_length(rsetarg),stderr);
  */

  t = runtime_getMinBits(rtarg) - rec_size(rarg,numunits);
  sz = perm_get_length(&siv);

  /*   fprintf(stderr,"NegPattGen: found t = %d, and sz = %d\n",t,sz); */

  if( t <= 0 || !sz ) {
    rec_destroy(recsav);
    perm_final(&siv);
    return;
  }

  if( t >= sz ) {
    rec_copy(recsav,rarg,numunits);
    rec_destroy(recsav);
    perm_final(&siv);
    return;
  }

  perm_randomize(&siv,t);
  for(i = 0; i < t; i++) {
    int idx = perm_get(&siv,i);
    rec_setbit(rarg,idx,rec_getbit(recsav,idx),numunits);
  }

  rec_destroy(recsav);
  perm_final(&siv);
  return;
}
#endif


/* update rarg and sivarg to contain the essential bit string of rarg
   as input, and the indexes of the non-essential bits in the perm
   (perm length is set accordingly).
*/
static void c_key_negative(Rec * rarg, Perm * sivarg, Recset * rsetarg, Runtime * rtarg) {
  Perm randPerm;
  int v = 0;
  int count = 0;
  int plen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  Freq * bitfreq = recset_getbitfrequency(rsetarg);
  
  if(bitfreq==NULL){
    recset_update_bit_frequency(rsetarg);
    bitfreq = recset_getbitfrequency(rsetarg);
  }

  perm_init(&randPerm,plen);
  plen = perm_notdontcares(&randPerm,rarg,plen);     /* skip dontcares */  
  perm_weighted_roulette(&randPerm,bitfreq); 
  
  while(v < plen) {                           /* reverse roulette order */
    int idx = perm_get(&randPerm,v);
    rec_flipbit(rarg,idx);            

    /* if rec exists that subsumes rarg w bit flipped, then set bit to
       dontcare; o.w. bit is 'essential', restore it to original value */           

    if(recset_query(rsetarg,rarg)) {  
      rec_setbit(rarg,idx,DONTCARE,numunits);
      perm_swap(sivarg,count,idx);      
      count++;
    } else {
      rec_flipbit(rarg,idx);                            /* revert back */
    }
    v++;
  }
  perm_set_length(sivarg,count);
  perm_final(&randPerm);
  return;
}


#if MANAGED_GROWTH
/* return 1 if rarg is redundant and rarg is destroyed here; o.w. 0 v.64 */
static int managed_growth_redundancy_check(Rec * rarg, Recset * rsetarg) 
{
  int chk = recset_query(rsetarg,rarg);
  if(chk) {
    rec_destroy(rarg);
  }
  return chk;
}


/* return number of recs deleted or -1 (not 0) if rarg is ready to add,
   o.w. 0 mean rec was redundant, and destroyed; used before
   recset_addrec by insert and append; MANAGED_GROWTH option designed
   by Eric Trias v.64 */
int managed_growth(Rec * rarg, Recset * rsetarg) 
{
   Recset cache;
   int deleted;
   int rtn = 0;
   recset_init(&cache,recset_length(rsetarg));
   recset_split_subsumed(rarg,rsetarg,&cache);
   deleted = recset_size(&cache);
   recset_final(&cache);                       /* delete subsumed records v.62 */
   /*   fprintf(stdout,"managed growth: deleted %d records before adding newrec\n",
        deleted); */
   if(deleted > 0 || ! managed_growth_redundancy_check(rarg,rsetarg)) {
     rtn = (deleted > 0 ? deleted : -1);
   }
   /*   fprintf(stdout,"managedgrowth: returns %d\n",rtn); */
   return rtn;
}

#endif


#if RECS2ADD
static int determine_minimum_records2add(int narg, int determined) 
{
  int j;
  if( narg <= 1) {
    j = 1;
  } else {
    if( determined ) {
      j = narg;
    } else {
      j = nextRand(narg);
      if(! j ) { j = 1; }   /* minimum one time */
    }
  }
  return j;
}
#endif


static void random_distinct_bits(Rec * rarg, Perm * newparg, int narg)
{
  int plen = perm_get_length(newparg);
  plen = perm_dontcares(newparg,rarg,plen);

  /*  fprintf(stderr,"rand_distinct_bits: size of newperm is %d, narg is %d\n",plen,narg); */

#if ! (NPG_DETERMINISTIC)
  fprintf(stdout,"rand_distinct_bit: USING RANDOMIZATION\n");
  if(narg < plen) {
    perm_randomize(newparg,narg);
  }
#else
  perm_set_length(newparg,narg);
#endif

  /*   perm_print(newparg,stderr); */
  return;
}


/* modified to call append() for managed_growth effect v.67 */
static int addallbitcombos(Recset * rsetarg, Rec * rarg, Perm * rdbpermarg, Runtime * rtarg, int npgflag)
{
  int pc;
  int i = 0;
  int count = 0;
  int rdblen = perm_get_length(rdbpermarg);
  int numunits = recset_numunits(rsetarg);
  Rec * rec = rec_create(numunits); 

  assert(rdblen < INTSIZE);             /* v.56.2 */
  pc = 1 << rdblen;                     /* 2^n possible bit combos */

  while(i < pc) {
    int j;
    int x = i;
    rec_copy(rarg,rec,numunits);

    for(j = 0; j < rdblen; j++) {
      assert(rec_isdc(rarg,perm_get(rdbpermarg,j)));
      rec_setbit(rec,perm_get(rdbpermarg,j),x&1,numunits);
      x = x >> 1;                      /* consider next bit for next position */
    }
    count += append(rec,rsetarg,rtarg,npgflag);
    i++;
  }       /* end while */

  rec_destroy(rec);
  return count;
}



/* refactored from insert - doesn't add bits to rec - might remove
   them v.59; returns number of records added, 0 if duplicate (or
   redundant, could be negative with managed growth v.64),o.w. 1
   */
int append(Rec * rarg, Recset * rsetarg, Runtime * rtarg, Negpattgen npgflag)
{
  int numunits = recset_numunits(rsetarg);
  int count = 0;
  Rec * rec = rec_create(numunits);       /* efficiency would be bad here */
  
  rec_copy(rarg,rec,numunits);
  
#if NPG_VERBOSE_DEBUG
    fprintf(stdout,"here the combo rec %d before npg ",i);
    rec_print(rec,recset_length(rsetarg),stdout);
#endif

  if(npgflag) {
#if NPG_REMV_BIAS
    yetanother_negative_pattern_generate(rec,rsetarg,rtarg);
#endif
#if NPG_REMV_RAND
    new_negative_pattern_generate(rec,rsetarg,rtarg);
#endif
#if NPG_ADD_RAND
    negative_pattern_generate(rec,rsetarg,rtarg);
#endif
#if NPG_DETERMINISTIC
    deterministic_negative_pattern_generate(rec,rsetarg,rtarg);
#endif
  }

  {
#if MANAGED_GROWTH
    int mg = managed_growth(rec,rsetarg);
    count -= (mg < 0 ? 0 : mg );  /* compensate only if mg is positive */
    if(mg != 0)                   /* v.64 */
#endif
      count += recset_addrec(rec,rsetarg);
  }
    return count;
}


/* insert (formerly make_rndb_rec) --- copies rec arg and adds one or
   more modified versions to the recset arg --- the caller is
   responsible for the memory mgt of the rec arg;
   negative_pattern_generate flag is 0 for empty ndb creations and 1
   for regular inserts; the narg is the number of records to add
   (formerly named STEP6) which can also be different depending upon
   whether or not this is for an empty db.  returns the number of
   records added */
int insert(Rec * rarg, Recset * rsetarg, Runtime * rtarg, Negpattgen npgflag, int narg)
{
  int numunits = recset_numunits(rsetarg);
  int l = rec_size(rarg,numunits);          /* num specfied bits */
  int reclen = recset_length(rsetarg); 
  int dc = reclen - l;                      /* num unspecified bits */
  int count = 0;
  int bn = runtime_getMinBits(rtarg) - l;
  bn += narg;
  if(bn > dc ) { bn = dc; }
  if( bn < 0 ) { bn = 0; } 

  /*  fprintf(stderr,"make_rndb_rec(insert): \n"); */
  /*  rec_print(rarg,reclen,stderr); */

  /* fprintf(stderr,"(insert)recsize,bn,n,minbits: %d, %d, %d, %d<BR>",l,bn,narg,runtime_getMinBits(rtarg)); */
 
  if( !dc || !bn) {
    
    count = append(rarg,rsetarg,rtarg,npgflag);

   } else {          /* dc and bn are at least one, if here */
     int j = 1;      /* one time through only minimally */
     int k = 0;

 #if RECS2ADD
     j = determine_minimum_records2add(narg,npgflag^1); 
 #endif

     /*    fprintf(stderr,"make_rndb_rec(insert): more than one rec, j is %d\n",j); */

     while( k++ < j ) {          /*  at least once through */
       Perm dcperm;
       perm_init(&dcperm,reclen);
       random_distinct_bits(rarg,&dcperm,bn);
       count += addallbitcombos(rsetarg,rarg,&dcperm,rtarg,npgflag);
       perm_final(&dcperm);
     }
   }
  /*  fprintf(stdout,"insert: returns count %d\n",count); */
   return count;
 }


/* formerly unmake_rndb_rec. creates a tight hole that removes this
   record, yarg, from the negative db and no others  */
int delete(Rec * yarg, Rec * xarg, Recset * rsetarg, Runtime * rtarg, Negpattgen npgarg)
{
  int l = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  int n = runtime_getN(rtarg);
  int count = 0;
  Rec * s = rec_create(numunits);
  
  while(l--) {
    
    /* for each unspecified bit of y that is specified in x */
    if(rec_isdc(yarg,l) && !rec_isdc(xarg,l) ){
      rec_init(s,numunits);
      
      /* create a new string yi using the specified bits of y and the
         complement of the ith bit of x.
      */
      
      rec_copy(yarg,s,numunits);
      rec_setbit(s,l,rec_getbit(xarg,l)^1,numunits);
      
#if USE_APPEND
      count += append(s,rsetarg,rtarg,npgarg);  /* tried wo_npg v.9; as arg in v.57.5 */
#else
      count += insert(s,rsetarg,rtarg,npgarg,n);  /* tried wo_npg v.9; as arg in v.57.5 */
#endif
    }
  }
  rec_destroy(s);
  return count;
}


/* like online_add_record, without the user interface stuff, doesn't
   change rec ownership; returns number of records added to recset;
   used by recset_complement. v.60 */
int offline_add_record(Rec * rarg, Recset * rsetarg, Runtime * rtarg, Negpattgen npgarg)
{
  Recset cache;
  int deleted;
  int count = 0;
  
  recset_init(&cache,recset_length(rsetarg));
  recset_split_match(rarg,rsetarg,&cache);  
  deleted = recset_size(&cache);
  
#if 0
  fprintf(stdout,"offline_add: deleted cache size is %d\n",deleted);  /*  v.62 */
#endif

  if(deleted) {
    int n = 0;
    
    while(n < deleted) {
      count += delete(recset_getrec(&cache,n),rarg,rsetarg,rtarg,npgarg);
      n++;
    }
  } 

  recset_final(&cache);    /* clean up */
  return count;
}


/* given a randomly select a string, karg; let Dk be all the strings
 that are subsumed BY karg; if the number of Dk is greater than Tau,
 remove them from NDB.  insert karg. returns 0 when done, 1 when not
 done, o.w. error code  */
static int check_for_cleanup(Rec * karg, Recset * ndbarg, Runtime * rtarg, CleanResults * crarg) 
{
  int deleted = 0;
  int added = 0;
  int passed = 0;
  int reclen = recset_length(ndbarg);
  int numunits  = recset_numunits(ndbarg);
  int n = runtime_getN(rtarg);
  int sss = recset_query_subsumed_size(ndbarg,karg);

  int tau = runtime_getTau(rtarg);
  if(! tau) {
    tau = runtime_getMinBits(rtarg) - rec_size(karg,numunits);
    tau = ( tau < 0 ? 0 : tau);
    tau += n;
    tau = 1 << tau;                           /* 2^(r-|key|+n) */
  }

  /*  fprintf(stdout,"cleanup: deleted cache size is %d, tau is %d\n",deleted,tau); */

  if( sss >= tau) {
    Recset cache;
    recset_init(&cache,reclen);
    recset_split_subsumed(karg,ndbarg,&cache);
    deleted = recset_size(&cache);

    if(sss != deleted){
      fprintf(stderr,"check_for_cleanup: deleted cache size is %d, sss is %d\n",
              deleted,sss);
      exit(err13);   /* could it be a return instead of exit ? */
    }
    
#if USE_APPEND
    added = append(karg,ndbarg,rtarg,with_npg);
#else
    added = insert(karg,ndbarg,rtarg,with_npg,n);
#endif
    recset_setupdateflag(ndbarg);
    passed = 1;
    recset_final(&cache);
  }
  cleanresults_set(crarg,added,deleted,passed);
  return !passed;       /* zero for done */
}


/* randomly select a string; let Dk be all the strings that have the
   same c_key, K; if the number of Dk is greater than Tau, remove them
   from NDB.  insert K. */
int cleanup(Recset * ndbarg, Runtime * rtarg, FILE * fdarg, int iterarg, CleanResults * cleanresultsarg) 
{
  Rec * randrecptr;
  int randidx;
  int sz;
  Perm siv;
  int ksize;
   int numunits = recset_numunits(ndbarg);
   Rec * K = rec_create(numunits);
   
   sz = recset_size(ndbarg);
   randidx = nextRand(sz);
   randrecptr = recset_getrec(ndbarg,randidx);   /* rec remains in recset */
   
   perm_init(&siv,recset_length(ndbarg));
   rec_copy(randrecptr,K,numunits);  /* don't modify the actual record just yet */
   
   ksize = rec_size(K,numunits);
   
   c_key_negative(K,&siv,ndbarg,rtarg);   /* K may be modified here */
   perm_final(&siv);

   if(rec_size(K,numunits) > ksize){      /* anomaly */
     fprintf(stdout,"cleanup key has %d bits > %d bits in its record",
             rec_size(K,numunits),ksize);
     PrintBR(rtarg);
   } 

   check_for_cleanup(K,ndbarg,rtarg,cleanresultsarg);
   
   if(fdarg) {
     cleanresults_print(cleanresultsarg,iterarg,fdarg);
   }
   
   rec_destroy(K);
   return cleanresults_getNet(cleanresultsarg);
 }


/* randomly select a string; let Dk be all the strings that have the
   same c_key, K; if the number of Dk is greater than Tau, remove them
   from NDB.  insert K. */
int new_cleanup(Recset * ndbarg, Runtime * rtarg, FILE * fdarg, int iterarg, CleanResults * cleanresultsarg) 
{
  Rec * randrec;
  int randidx;
  int sz;
   Perm randPerm;
   Rec * K;
   int v = 0;
   int reclen = recset_length(ndbarg);
   int numunits = recset_numunits(ndbarg);
   int notdone = 1;
   
   cleanresults_clear(cleanresultsarg);
   
   /* select a random record */
   sz = recset_size(ndbarg);
   randidx = nextRand(sz);
   randrec = recset_getrec(ndbarg,randidx);
   
   K = rec_create(numunits);
   rec_copy(randrec,K,numunits); /* don't modify the actual record just yet */
   
   perm_init(&randPerm,reclen);
   perm_randomize(&randPerm,reclen);

   while(v < reclen && notdone) {
     int idx = perm_get(&randPerm,v);
     if(! rec_isdc(K,idx) ) {                    /* skip if already a dc */
       rec_flipbit(K,idx);                       
       if(recset_query(ndbarg,K)) {
         rec_setbit(K,idx,DONTCARE,numunits);
         notdone = check_for_cleanup(K,ndbarg,rtarg,cleanresultsarg);
       } else {
         rec_flipbit(K,idx);                    /* revert back */
       }
     }
     v++;
   }
   
   if(fdarg) {
     cleanresults_print(cleanresultsarg,iterarg,fdarg);
   }
   /*  fprintf(stderr,"cleanup: net %d\n",cleanResults.net); */
   
   rec_destroy(K);
    perm_final(&randPerm);
   return cleanresults_getNet(cleanresultsarg);
}

#define MINCLEANUPS 100
#define MAXCLEANUPS 3000

/* do optional cleanups for -T percent of ndb size times; reset
   minbits in runtime only if user did not specified it; calculate
   number of times as a percent of recset size; use min and max
   cleanups; v.57 */
void cleanup_plus_option(Recset * rsetarg, Runtime * rtarg)
{
  int t = runtime_getCleanTests(rtarg);    /* treat as a percent */
  if(t > 0){         /* v.57 */
    int tpercent = (recset_size(rsetarg) * t) / 100;
    tpercent = (tpercent < MINCLEANUPS ? MINCLEANUPS : tpercent);
    tpercent = (tpercent > MAXCLEANUPS ? MAXCLEANUPS : tpercent);
    
    if(tpercent > 0) {
      runtime_setCleanTests(rtarg,tpercent);
      if (runtime_getMinBitsSpecifiedFlag(rtarg)){
        test_clean(rsetarg,rtarg);
      } else {  /* minbits not specified by user so use current recset minbits */
        int savminbits = runtime_getMinBits(rtarg);
        runtime_setMinBits(rtarg,recset_min_recordsize(rsetarg));
        test_clean(rsetarg,rtarg);
        runtime_setMinBits(rtarg,savminbits); /* restore original minbits v.57 */      
       }
      runtime_setCleanTests(rtarg,t);    /* restore percent */
    }
  }
  return;
}


/* pass as argument, min percent of ndb size to try cleanups v.57.3 */
void cleanup_plus_option_minimum(Recset * rsetarg, Runtime * rtarg, int minarg)
{
  int t = runtime_getCleanTests(rtarg);           /* treat as a percent */
  if (t == 0) runtime_setCleanTests(rtarg,minarg);   /* use minarg then */
  
  cleanup_plus_option(rsetarg,rtarg);          
  
  if (t == 0) runtime_setCleanTests(rtarg,0);   /* restore if necessary */
}


#define MAX_COMPRESS_TRIES 100

/* compress, goes through each record in the recset, removes it, calls npg,
   and "appends" the new record that possibly has stars. duplicates are dropped
   (this is how we decrease the size). repeat record length times. v.57.5 */
/* could have used append(), but more memory operations that way;
   quite if no improvement in ndbsize v.59 */
/* does nothing if minarg is zero v.59.12 */
/* as of v.62 this code broke since the underlying recset can change
   with recset_addrec; does nothing with MANAGED_GROWTH in v.63. fixed
   with managed_growth refactoring v.64 */
/* repeat a constant number of maximum times MAX_COMPRESS_TRIES v.72.6 */
int compress(Recset * rsetarg, Runtime * rtarg, int minarg)
{
  int j, savminbits;
  int lastsz, sz;

#if SKIP_COMPRESS
  return recset_size(rsetarg);     /* DOES NOTHING */
#endif
  
  if(minarg <= 0) {
    return recset_size(rsetarg);          /* v.59.12 */
  }

  savminbits = runtime_getMinBits(rtarg);
  runtime_setMinBits(rtarg,minarg);   

  sz = lastsz = recset_size(rsetarg);
  lastsz +=1;  /* just to kick start */
  j = 0;
  
  while(j++ < MAX_COMPRESS_TRIES && sz < lastsz){
    int i = sz;
    lastsz = sz;

    fprintf(stdout,"compress: j is %d, ndb size is now %d\n",j,i);

    while(i-- > 0) {
      Rec * rec = recset_getrec(rsetarg,i);
      recset_removerec(rsetarg,i);
#if NPG_REMV_BIAS
      yetanother_negative_pattern_generate(rec,rsetarg,rtarg);
#endif
#if NPG_REMV_RAND
      new_negative_pattern_generate(rec,rsetarg,rtarg);
#endif
#if NPG_ADD_RAND
      negative_pattern_generate(rec,rsetarg,rtarg);
#endif
#if NPG_DETERMINISTIC
      deterministic_negative_pattern_generate(rec,rsetarg,rtarg);
#endif 

#if MANAGED_GROWTH
      if(!managed_growth_redundancy_check(rec,rsetarg)) /* mini managed-growth v.64 */
#endif
        recset_addrec(rec,rsetarg);      /* destroys duplicate recs only */  
    }
    sz = recset_size(rsetarg);
  }
  runtime_setMinBits(rtarg,savminbits); /*restore min bits */
  return sz;
} 

