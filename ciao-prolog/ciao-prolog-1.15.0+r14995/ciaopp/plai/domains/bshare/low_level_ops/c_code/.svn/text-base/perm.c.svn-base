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
 * Filename      : perm.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for Permutations
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Wed Oct 10 16:54:14 2007 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include "perm.h"
#include "freq.h"
#include "error.h"

/* we can try to be slightly less bad about uniform distribution of
   created numbers.  This nextRand is related to the Java nextInt
   routine, except I dropped the power-of-2 optimization it has
   because 64 bit ints don't exist yet.
*/
int nextRand(int narg) {
  int bits, val;

  assert(narg>0);
  
  do {
    bits = random() & ~(1<<31);
    val = bits % narg;
  } while(bits - val + (narg-1) < 0);
  
  return val;
} 


/* returns 0 or 1 */
int coinflip()
{ 
  return nextRand(2);
}


/* returns a random double between 0 and 1 */
double randd()
{
  return (double)((random() / (1.0 *RAND_MAX))); 
}


void seed_random(int seedarg) 
{
  if (!seedarg)
    srandom(time(0) ^ getpid());
  else 
    srandom(seedarg);
}


int perm_seed_random(int narg) 
{
  int seed = narg;
  if (!seed){
    seed = time(0) ^ getpid();
  }
  srandom(seed);
  return seed;
}


/* return position of idxarg */
int __inline__ perm_get_reverse(Perm * parg, int idxarg) 
{
  int rtn;
  int plen = perm_get_length(parg);
  assert(idxarg < plen);
  rtn = parg->pir[idxarg];
  assert(parg->pi[rtn] < plen);
  return rtn;
}


/* if the position of the index in question falls within the official
   length of the perm then the index is in the perm and true is
   returned; o.w. false is return. could not use perm_get_reverse. v.27 */
int perm_index_presentp(Perm * parg, int idxarg, int lenarg)
{
  assert(idxarg >= 0);
  return parg->pir[idxarg] < lenarg;
}


static __inline__ void perm_set(Perm *parg, int idxarg, int valarg) 
{
  assert(valarg >= 0);
  parg->pi[idxarg] = valarg;
}


static __inline__ void perm_set_reverse(Perm *parg, int idxarg, int valarg) 
{
  assert(valarg >= 0);
  parg->pir[idxarg] = valarg;
}


void __inline__ perm_set_length(Perm *parg, int lenarg) 
{
  assert(lenarg >= 0);
  parg->len = lenarg;
}


void perm_swap(Perm * parg, int xarg, int yarg)
{
  int tmp = perm_get(parg,xarg);
  perm_set(parg,xarg,perm_get(parg,yarg));
  perm_set_reverse(parg,perm_get(parg,xarg),xarg);

  perm_set(parg,yarg,tmp);
  perm_set_reverse(parg,perm_get(parg,yarg),yarg);
}


Perm * perm_create(int lenarg)
{
  Perm * p;
  p = malloc(sizeof(Perm));
  if(p == NULL) {
    fprintf(stderr,"perm_create: failed to allocate perm\n");
    exit(err12);
  }
  perm_init(p,lenarg);
  return p; 
}


void perm_init (Perm * parg, int lenarg) 
{
  int i;
  assert(lenarg>=0);

  parg->pi = (unsigned int *) malloc(sizeof(int) * lenarg);
  if(parg->pi == NULL) {
    fprintf(stderr,"perm_create: failed to allocate perm pi\n");
    exit(err12);
  }
  
  parg->pir = (unsigned int *) malloc(sizeof(int) * lenarg);
  if(parg->pir == NULL) {
    fprintf(stderr,"perm_create: failed to allocate perm pir\n");
    exit(err12);
  }
  
  for(i = 0; i < lenarg; i++) {
    perm_set(parg,i,i);
    perm_set_reverse(parg,i,i);
  }
  /* set length here */
  perm_set_length(parg,lenarg);
}


/* populate perm arg with indexes with specified bits (not dontcares)
   in rec arg */
int perm_notdontcares (Perm * parg, Rec * rarg, int reclenarg) 
{
  int cnt = 0;
  int i;

  for(i = 0; i < reclenarg; i++) {
    if(!rec_isdc(rarg,i)) {
      perm_swap(parg,cnt,i);
      cnt++;
    }
  }
  perm_set_length(parg,cnt);

  /* sanity check */
  for(i = 0; i < cnt; i++) 
    assert(!rec_isdc(rarg,perm_get(parg,i))); 
  return cnt;
}


/* populates a new perm with indexes of all the rec's in rsetarg that
   have dontcares where rarg does.  TO BE TESTED */
int perm_recset_subset(Perm * parg, Rec * rarg, Recset * rsetarg)
{
  int numunits = recset_numunits(rsetarg);
  int i = recset_size(rsetarg);
  int cnt = 0;

  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    
    if ( rec_dontcare_match(rarg,recy,numunits)) {
      perm_swap(parg,cnt,i);
      cnt++;
    }
  }
  if(cnt > 0)
    perm_set_length(parg,cnt);
  return cnt;
}


/* populate perm arg with  indexes with dontcares in rec arg */
int perm_dontcares (Perm * parg, Rec * rarg, int reclenarg) 
{
  int cnt = 0;
  int i;
  
  for(i = 0; i < reclenarg; i++) {
    if(rec_isdc(rarg,i)) {
      perm_swap(parg,cnt,i);
      cnt++;
    }
  }
  perm_set_length(parg,cnt);

  /* sanity check */
  for(i = 0; i < cnt; i++) 
    assert(rec_isdc(rarg,perm_get(parg,i))); 
  return cnt;
}


/* same as shuffle, resets len of perm */
/* based on fisher-yates shuffle in place v.25 */
void perm_randomize (Perm * parg, int newlenarg) 
{
  int i;
  int endrec = perm_get_length(parg);

  assert(newlenarg <= endrec);

  i=0;
  while(i < newlenarg){
    int x = nextRand(endrec-i);  /* shrinking range */    
    x+=i;
    perm_swap(parg,x,i);
    i++;
  }

  /* set new length */
  perm_set_length(parg,newlenarg);
}


/* input perm of specified bit indexes of record and recset bit
 frequency; return perm in random order weighted by frequency, with
 greatest more likely to be first */
void perm_weighted_roulette (Perm * parg, Freq * farg) 
{
  int i,j;
  int start = 0;
  int plen = perm_get_length(parg);
  int sum = 0;

  assert(farg);

  /* sum frequencies of bit indexes in perm */
  for(i = 0; i < plen; i++) {    
    int fi = freq_get(farg,perm_get(parg,i)) + 1;
    assert(fi>0);
    sum += fi;
  }

#if PERM_VERBOSE_DEBUG
  fprintf(stdout,"weighted roulette: BEFORE perm index/freqs are: \n");
  for(i = 0; i < plen; i++) {
    int idx = perm_get(parg,i);
    fprintf(stdout,"%d %d,\n",
	    idx,
	    freq_get(farg,idx)
	    );
  }
#endif

  for(j = 0; j < plen; j++) {
    int r = nextRand(sum) + 1;      /* 1<=r<=sum */
    int x = 0;
    for(i = start; i < plen; i++) {
      int fi = freq_get(farg,perm_get(parg,i)) + 1;
      assert(fi>0);
      x += fi;
      if ( x >= r) {
	sum -= fi;
	perm_swap(parg,start,i);
	start++;
	break;
      }
    }
  }

#if PERM_VERBOSE_DEBUG
  fprintf(stdout,"weighted roulette: AFTER perm index/freqs are: \n");
  for(i = 0 ; i < plen; i++) {
    int idx = perm_get(parg,i);
    fprintf(stdout,"%d %d,\n",
	    idx,
	    freq_get(farg,idx)
	    );
  }
#endif
}


/* input perm of specified bit indexes of record and recset bit
 frequency; return perm in decending order frequency, with
 greatest coming first */
void perm_ordered (Perm * parg, Freq * farg) 
{
  int plen = perm_get_length(parg);
  int j;

#if PERM_VERBOSE_DEBUG
  fprintf(stdout,"ordered: BEFORE perm index/freqs are: \n");
  for(j = 0; j < plen; j++) {
    int idx = perm_get(parg,j);
    fprintf(stdout,"%d %d,\n",
	    idx,
	    freq_get(farg,idx)
	    );
  }
#endif
  j = 0;
  while (j < plen) {
    int  i = plen-1;
    while (i > j) {
      int fi = freq_get(farg,perm_get(parg,i));
      int fib = freq_get(farg,perm_get(parg,i-1));
      if(fi > fib) {
	perm_swap(parg,i,i-1);
      }
      i--;
    }
    j++;
  }

#if PERM_VERBOSE_DEBUG
  fprintf(stdout,"ordered: AFTER perm index/freqs are: \n");
  for(j = 0; j < plen; j++) {
    int idx = perm_get(parg,j);
    fprintf(stdout,"%d %d,\n",
	    idx,
	    freq_get(farg,idx)
	    );
  }
#endif
}


/* same as perm_range except in descending order v.27 */
static int perm_range_descending(Perm * parg, int startarg, int endarg, int binmodearg, int cntarg)
{
  int i = startarg;
  int cnt = cntarg;
  int  plen = perm_get_length(parg);

  while(i >= endarg) {
    if(!binmodearg){   /* convert range into 8 bits each */
      int j;
      int n = i * CHAR_T;
      if(n < plen && n >=0 && !perm_index_presentp(parg,n,cnt)) { 
        for (j=0; j < CHAR_T; j++){
          perm_swap(parg,cnt++,perm_get_reverse(parg,n+j));
        }
      } else 
        fprintf(stderr,"perm_range_descending: ignoring ascii index (%d) near position %d of input (out-of-range [0 to %d] or a duplicate)\n",n,cnt,plen);
    } else {
      if(i < plen && i >=0 && !perm_index_presentp(parg,i,cnt)) {
        perm_swap(parg,cnt++,perm_get_reverse(parg,i));
      } else 
        fprintf(stderr,"perm_range_descending: ignoring binary index (%d) near position %d of input (out-of-range or a duplicate)\n",i,cnt);
    }
    i--;
  }
  return cnt;
}


/* adds more indexes to the perm, starting with startarg, and ending
   with endarg; if not in binary mode, converts letter indexes to bit
   indexes. v.27 */
static int perm_range(Perm * parg, int startarg, int endarg, int binmodearg, int cntarg)
{
  int i, cnt, plen;

  /*  fprintf(stderr,"perm_range: start %d, end %d, cnt %d\n",startarg,endarg,cntarg); */

  if(startarg > endarg){
    return perm_range_descending(parg,startarg,endarg,binmodearg,cntarg);
  }

  i = startarg;
  cnt = cntarg;
  plen = perm_get_length(parg);

  while(i <= endarg) {
    if(!binmodearg){   /* convert range into 8 bits each */
      int j;
      int n = i * CHAR_T;
      if(n < plen && n >=0 && !perm_index_presentp(parg,n,cnt)) {
        for (j=0; j < CHAR_T; j++){
          perm_swap(parg,cnt++,perm_get_reverse(parg,n+j));
        }
      } else 
        fprintf(stderr,"perm_range: ignoring ascii index (%d) near position %d of input (out-of-range [0 to %d] or a duplicate)\n",n,cnt,plen);
    } else {
      if( i < plen && i >=0 && !perm_index_presentp(parg,i,cnt)) {
        perm_swap(parg,cnt++,perm_get_reverse(parg,i));
      } else 
        fprintf(stderr,"perm_range: ignoring binary index (%d) near position %d of input (out-of-range or a duplicate)\n",i,cnt);
    }
    i++;
  }
  return cnt;
}


/* make_num: returns the number formed by ascii characters of length
   in second argument; returns the number 0 or greater, or -1 if an
   error v.27 */
static int make_num(char * fmstart, int lenarg) {
  char * tostr;
  int rtnnum = -1;

  if(lenarg > 0 && fmstart != NULL) {
    tostr = malloc(sizeof(char) * (lenarg+1));
    assert(tostr!=NULL);
    memcpy(tostr,fmstart,lenarg+1);
    tostr[lenarg] = '\0';
    rtnnum = atoi(tostr);
    /*    fprintf(stderr,"make_num: got [%s], len [%d], returning [%d]\n",fmstart,lenarg,rtnnum); */
    free(tostr);
  }

  return rtnnum;
}


/* parse comma-delimited list argument, dashes for inclusive range,
   into perm indexes, account for 8 bits if binmode is false (that is,
   ascii letters); returns new perm length or -1 if error in make_num
   detected. exits immediately if unexpected character found. v.27 */
int perm_list2perm (Perm * parg, char * listarg, int binmodearg)
{
  int start, len, i;
  int listlen = 0;
  int num = -1;
  int rangeflag = 0;
  int rangestart = -1;
  int plen = 0;
  int cnt = 0;

  assert(listarg!=NULL);
  assert(parg!=NULL);

  plen = perm_get_length(parg); /* input as bits, converted if not binmode */
  plen = (!binmodearg ? plen / CHAR_T : plen);

  listlen = strlen(listarg);
  i = 0;
  len = 0;
  start = 0;

  while(i < listlen){
    char c = listarg[i];
    switch (c) {
    case ',':
      num = make_num(&listarg[start],len);
      if(num < 0 || num >= plen) {
        fprintf(stderr,"perm_list2perm: number in list [%d] is beyond acceptable range 0 to %d\n",
                num,plen-1);
        return -1;
      }
      if(!rangeflag){
        rangestart = num;
      } else {
        rangeflag = 0;
      }
      cnt = perm_range(parg,rangestart,num,binmodearg,cnt);
      len = 0;
      start = i+1;
      break;
    case '-':
      rangestart = make_num(&listarg[start],len);
      if(rangestart < 0 || rangestart >= plen) {
        fprintf(stderr,"perm_list2perm: number in list [%d] is beyond acceptable range 0 to %d\n",
                rangestart,plen-1);
        return -1;
      }
      rangeflag = 1;
      len = 0;
      start = i+1;
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      len++;
      break;
    case '\n':
    case ' ':
    default:   
      fprintf(stderr,"perm_list2perm: invalid character [%c] at position %d\n",c,i+1);
      exit(err11);
    };
    i++;
  }
  /* last one, allows an empty list */
  if(len > 0) {
    num = make_num(&listarg[start],len);
    if(num < 0 || num >= plen) {
      fprintf(stderr,"perm_list2perm: last number in list [%d] is beyond acceptable range 0 to %d\n",
              num,plen-1);
      return -1;  
    }
    if(!rangeflag){
      rangestart = num;
    }
    cnt = perm_range(parg,rangestart,num,binmodearg,cnt);
  }
  /*  fprintf(stderr,"perm_list2perm: setting permlength to %d\n",cnt); */
  perm_set_length(parg,cnt);
  return cnt;
}



void perm_print (Perm * parg, FILE * fdarg) 
{
  int i;
  int endrec = perm_get_length(parg);

  for(i = 0; i < endrec; i++) {
    fprintf(fdarg,"%d ", perm_get(parg,i));
  }
  fprintf(fdarg,"\n");

  for(i = 0; i < endrec; i++) {
    fprintf(fdarg,"%d ",perm_get_reverse(parg,i));
  }
  fprintf(fdarg,"\n\n");
}


/* print rec in cnf format: 1..len space-delimited indexes (negative for zero
   value, positive for ones, don't cares are skipped), line
   delimiter zero; required file header output by caller (v.22) */
static void perm_printreccnf(Rec * rarg, int lenarg, FILE * fdarg){
  Perm perm;
  int j = 0;

  perm_init(&perm,lenarg);        /* all the bit indexes in order */

  while(j < lenarg){
    int idx = perm_get(&perm,j);
    if(! rec_isdc(rarg,idx)) {
      if(rec_getbit(rarg,idx) == 1) {
        fprintf(fdarg,"-%d ",idx+1);
      } else {
        fprintf(fdarg,"%d ",idx+1);
      }
    }
    j++;
  } 
  fprintf(fdarg,"0\n");
  perm_final(&perm);
}


/* special case: for all don't care negative record, we want an UNSAT
   result (i.e. no positive solutions); print each position with both
   +/- on separate lines ending in 0 v.51 */
static void perm_printalldccnf(int lenarg, FILE * fdarg){
  int j = 0;
  while(j++ < lenarg){
    fprintf(fdarg,"-%d 0 \n",j);
    fprintf(fdarg,"+%d 0 \n",j);
  } 
}


/* create and print a rec with only the bits specified in the perm arg (v.22);
   the NegFileFormat argument determines the format used (v.30)  */
void perm_print_rec(Perm * parg, Rec * rarg, NegFileFormat nffarg, FILE * fdarg){
  Rec * prec;
  int plen = perm_get_length(parg);
  int newidx = 0;
  int numunits = rec_calcnumunits(plen);

  prec = rec_create(numunits);
  
  while(newidx < plen){
    int fullidx = perm_get(parg,newidx);
    rec_setbit(prec,newidx,rec_getbit(rarg,fullidx),numunits);
    newidx++;
  } 

  if (nffarg == cnfformat) {          /* v.30 */
    if(rec_size(prec,numunits) == 0)           /* all dontcares v.51 */
      perm_printalldccnf(plen,fdarg);
    else
      perm_printreccnf(prec,plen,fdarg);
  } else 
    rec_print(prec,plen,fdarg);

  rec_destroy(prec);
}


/* copy only the bits specified in the perm arg into the to-rec-arg
   with numunits, from the from-rec arg; numunits arg reflects the
   to-record length not the from rec length which could be different
   (v.30) */
void perm_copy_rec(Perm * parg, Rec * fmrarg, Rec * torarg, int numunitsarg){
  int plen = perm_get_length(parg);
  int newidx = 0;
  
  while(newidx < plen){
    int fullidx = perm_get(parg,newidx);
    rec_setbit(torarg,newidx,rec_getbit(fmrarg,fullidx),numunitsarg);
    newidx++;
  } 
}


void perm_final(Perm * parg)
{
  if(parg->pi != NULL) free(parg->pi);
  if(parg->pir != NULL) free(parg->pir);
  perm_set_length(parg,0);
}


void perm_destroy(Perm * parg)
{
  perm_final(parg);
  if(parg!= NULL) free(parg);
}


/* TESTS */

static int check_list_perm()
{
  Perm P;
  int plen, pget;
  int binmode = 1;
  char * list = "2,2,5-7,10,1,6"; 
  char * listr = "1,10,7-5,2,2,6";
  char * list1 = "0";
  char * listdash = "-";
  char * listcomma = ",";

  fprintf(stderr,"test_perm: in check_list_perm..\n");

  perm_init(&P,12);
  perm_list2perm(&P,list,binmode);
  plen = perm_get_length(&P);
  if (plen != 6) {
    fprintf(stderr,"check_list_perm: binmode perm length is %d, not 6\n",plen);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,2);
  if (pget != 6) {
    fprintf(stderr,"check_list_perm: binary perm midrange at position 2 is %d, not 6\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,0);
  if (pget != 2) {
    fprintf(stderr,"check_list_perm: binary perm at first position 0 is %d, not 2\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,plen-1);
  if (pget != 1) {
    fprintf(stderr,"check_list_perm: binary perm at last position %d is %d, not 1\n",
           plen-1,pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  fprintf(stderr,"check_list_perm: binary perm completed, should match [%s]:\n",list);
  perm_print(&P,stderr);
  perm_final(&P);

  /* decending order in binary */
  perm_init(&P,12);
  perm_list2perm(&P,listr,binmode);
  plen = perm_get_length(&P);
  if (plen != 6) {
    fprintf(stderr,"check_list_perm: binmode reverso perm length is %d, not 6\n",plen);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,3);
  if (pget != 6) {
    fprintf(stderr,"check_list_perm: binary reverso perm midrange at position 3 is %d, not 6\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,0);
  if (pget != 1) {
    fprintf(stderr,"check_list_perm: binary reverso perm at first position 0 is %d, not 1\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,plen-1);
  if (pget != 2) {
    fprintf(stderr,"check_list_perm: binary reverso perm at last position %d is %d, not 2\n",
           plen-1,pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  fprintf(stderr,"check_list_perm: binary reverso perm completed, should match [%s]:\n",listr);
  perm_print(&P,stderr);
  perm_final(&P);

  /* check ascii mode */
  binmode = 0;
  perm_init(&P,96);
  perm_list2perm(&P,list,binmode);
  plen = perm_get_length(&P);
  if (plen != 48) {
    fprintf(stderr,"check_list_perm: ascii perm length is %d, not 48\n",plen);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,18);
  if (pget != 50) {
    fprintf(stderr,"check_list_perm: ascii perm midrange at position 18 is %d, not 50\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,0);
  if (pget != 16) {
    fprintf(stderr,"check_list_perm: ascii perm at first position 0 is %d, not 16\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,plen-1);
  if (pget != 15) {
    fprintf(stderr,"check_list_perm: ascii perm at last position %d is %d, not 15\n",
           plen-1,pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }
  fprintf(stderr,"check_list_perm: ascii perm completed, should match [%s] converted to bit positions:\n",list);
  perm_print(&P,stderr);
  perm_final(&P);

  /* ascii reverso */
  perm_init(&P,96);
  perm_list2perm(&P,listr,binmode);
  plen = perm_get_length(&P);
  if (plen != 48) {
    fprintf(stderr,"check_list_perm: ascii reverso perm length is %d, not 48\n",plen);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,18);
  if (pget != 58) {
    fprintf(stderr,"check_list_perm: ascii reverso perm midrange at position 18 is %d, not 58\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,0);
  if (pget != 8) {
    fprintf(stderr,"check_list_perm: ascii reverso perm at first position 0 is %d, not 8\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,plen-1);
  if (pget != 23) {
    fprintf(stderr,"check_list_perm: ascii reverso perm at last position %d is %d, not 23\n",
           plen-1,pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }
  fprintf(stderr,"check_list_perm: ascii reverso perm completed, should match [%s] converted to bit positions:\n",listr);
  perm_print(&P,stderr);
  perm_final(&P);

  /* single index list */
  binmode = 1;
  perm_init(&P,12);
  perm_list2perm(&P,list1,binmode);
  plen = perm_get_length(&P);
  if (plen != 1) {
    fprintf(stderr,"check_list_perm: single index binary perm length is %d, not 1\n",plen);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,0);
  if (pget != 0) {
    fprintf(stderr,"check_list_perm: single index binary perm at first position is %d, not 0\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  perm_final(&P);

  /* single ascii index */
  binmode = 0;
  perm_init(&P,12);
  perm_list2perm(&P,list1,binmode);
  plen = perm_get_length(&P);
  if (plen != 8) {
    fprintf(stderr,"check_list_perm: single index ascii perm length is %d, not 8\n",plen);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }

  pget = perm_get(&P,2);
  if (pget != 2) {
    fprintf(stderr,"check_list_perm: single index ascii perm at position 2 is %d, not 2\n",
           pget);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }
  perm_final(&P);
  fprintf(stdout,"check_list_perm: single index binary and ascii perm complete\n");

  /* bogus single index list */
  binmode = 1;
  perm_init(&P,12);
  perm_list2perm(&P,listcomma,binmode);
  plen = perm_get_length(&P);
  if (plen != 12) {
    fprintf(stderr,"check_list_perm: bogus single comma perm length is %d, not 12\n",plen);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }
  perm_final(&P);

  perm_init(&P,12);
  perm_list2perm(&P,listdash,binmode);
  plen = perm_get_length(&P);
  if (plen != 12) {
    fprintf(stderr,"check_list_perm: bogus single dash perm length is %d, not 12\n",plen);
    perm_print(&P,stderr);
    perm_final(&P);
    return 0;
  }
  fprintf(stdout,"check_list_perm: bogus single dash comma  perm complete\n");

  perm_final(&P);
  return 1;
}


static int check_dontcare_perm()
{
  Perm P;
  Rec * rec;
  int len = 24;
  char * recstr = "*110*010**1*0*10*******1";  /* rec from rndb */

  fprintf(stderr,"test_perm: in check_dontcare_perm..\n");
  rec = rec_create(rec_calcnumunits(len));
  rec_string2rec(rec,len,recstr,1);

  perm_init(&P,len);

  perm_dontcares(&P,rec,len);

  if(perm_get_length(&P) != 13) {
    fprintf(stderr,"dontcare perm length wrong %d\n",perm_get_length(&P));
    perm_final(&P);
    return 0;
  }

  perm_final(&P);
  perm_init(&P,len);

  perm_notdontcares(&P,rec,len);

  if(perm_get_length(&P) != 11) {
    fprintf(stderr,"no dontcare perm length wrong %d\n",perm_get_length(&P));
    perm_final(&P);
    return 0;
  }

  rec_destroy(rec);
  perm_final(&P);
  return 1;
}


static int check_perm(Perm *parg) 
{
  int i;
  int endrec = perm_get_length(parg);

  if ( endrec > MAXBITS || endrec < 0 ) {
    return 0;
  }

  /* pir correctly reverses pi */
  for(i = 0; i < endrec; i++) {
    int rev = perm_get_reverse(parg,i);
    if ( rev < endrec && perm_get(parg,rev) != i) {
      return 0;
    }
  }

  /* all indexes different and within range */
  {
    Rec * bitsrec = rec_create(NUMUNITS);

    for(i = 0; i < endrec; i++) {
      int e = perm_get(parg,i);
      if ( ! rec_isdc(bitsrec,e)){
	printf("%d is not dc\n",i);
	return 0;
      } else {
	rec_setbit(bitsrec,e,1,NUMUNITS);
      }
      if ( e > MAXBITS || e < 0 ) {
	return 0;
      }
      e = perm_get_reverse(parg,i);
      if ( e > MAXBITS || e < 0 ) {
	return 0;
      }
    }
    rec_destroy(bitsrec);
  }
  return 1;
}


void test_perm ( ) 
{
  int i;

  fprintf(stderr,"starting test_perm..\n");

  /* entire range of full size perm */
  for(i = 0; i < MAXBITS; i++) {
    Perm P;
    perm_init(&P,i);
    perm_randomize(&P,i);
    if(!check_perm(&P)) {
      perm_print(&P,stderr);
      perm_final(&P);
      exit(-1);
    }
    perm_final(&P);
  }

  /* entire range of reduced size of perm */
  for(i = 0; i < MAXBITS; i++) {
    Perm P;
    perm_init(&P,i);
    perm_randomize(&P,i/2);
    if(!check_perm(&P)) {
      perm_print(&P,stderr);
      exit(2);
    }
    perm_final(&P);
  }

  /* how good is the randomization */
  {
    int rtest = 0;
    int avgpi = (MAXBITS - 0) / 2;
    int diff;
    int threshold = (MAXBITS / 20);  /* try 5% */

    for(i = 0; i < 1000; i++) {
      Perm P;
      perm_init(&P,MAXBITS);
      perm_randomize(&P,1);
      rtest += perm_get(&P,0);
      perm_final(&P);
    }
    rtest /= 1000;
    diff = avgpi - rtest;
    
    if (diff > threshold || diff < -threshold ) {
      printf("avg of 1000 random perm tests is %d, avg pindex is %d, threshold is %d---off by %d\n",
	     rtest,avgpi,threshold,diff);
    }
  }
  
  /* how good is the randomization, really v.25 */
  {
    int rtest[6];
    int failedcount = 0;
    int numtests = 500000;

    fprintf(stdout,"test_perm: really good randomization..\n");
    for(i = 0; i < 6; i++) rtest[i]=0;

    for(i = 0; i < numtests; i++) {
      Perm P;
      perm_init(&P,3);
      perm_randomize(&P,3);
   
      if(perm_get(&P,0)==0) {
        if(perm_get(&P,1)==1) {
          rtest[0]++;
        } else {
          rtest[1]++;
        }
      } else {
        if(perm_get(&P,0)==1) {
          if(perm_get(&P,1)==0) {
            rtest[2]++;
          } else {
            rtest[3]++;
          } 
        } else {
          if(perm_get(&P,1)==0) {
            rtest[4]++;
          } else {
            rtest[5]++;
          }
        }
      }
      perm_final(&P);
    }
    
    for(i = 0; i < 6; i++) {
      int chkr = (rtest[i]*100)/numtests;
      printf("test_perm: %d %d%% \n",i,chkr);
      if (chkr > 16 || chkr < 16) {
        failedcount++;
        printf("test_perm: combo %i failed with %d out of %d, %d %% should have been 16\n",
               i,rtest[i],numtests,chkr);
      }
    }

    if (failedcount > 0) {
      fprintf(stdout,"rtest: %d,%d,%d,%d,%d,%d\n",
              rtest[0],rtest[1],rtest[2],rtest[3],rtest[4],rtest[5]);
      exit(24);
    }
  }
      
  if(!check_dontcare_perm()) {
    exit(25);
  }

  if (!check_list_perm()) {
    exit(26);
  }

  fprintf(stderr,"test_perm completed.\n");
}

