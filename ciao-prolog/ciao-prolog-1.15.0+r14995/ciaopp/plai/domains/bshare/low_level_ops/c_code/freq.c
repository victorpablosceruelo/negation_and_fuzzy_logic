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
 * Filename      : freq.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for Bit Frequencies
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Fri Sep 14 08:07:02 2007 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include "freq.h"
#include "perm.h"
#include "error.h"


Freq * freq_create(int numintsarg)
{
  Freq * f;
  assert(numintsarg > 0);
  f = malloc(sizeof(Freq));
  if(f == NULL) {
    fprintf(stderr,"freq_create: failed to allocate freq\n");
    exit(err12);
  }
  f->bit = malloc(sizeof(int) * numintsarg * UNITSIZE);
  if(f->bit == NULL) {
    fprintf(stderr,"freq_create: failed to allocate freq bits array\n");
    exit(err12);
  }
  return f; 
}


__inline__ int freq_get(Freq * farg, int idxarg) 
{
  assert(idxarg < farg->len);
  return farg->bit[idxarg];
}


static void freq_set(Freq *farg, int idxarg, int valarg) 
{
  assert(valarg >= 0 && idxarg >=0 );
  farg->bit[idxarg] = valarg;
}


static __inline__ void freq_set_length(Freq *farg, int lenarg) 
{
  farg->len = lenarg;
}


static void freq_init_value (Freq * farg, int lenarg, int valarg) 
{
  int i;
  assert(lenarg >= 0);
  for(i = 0; i < lenarg; i++) {
    freq_set(farg,i,valarg);
  }
  /* set length here */
  freq_set_length(farg,lenarg);
}


void freq_init (Freq * farg, int lenarg) 
{
  freq_init_value(farg,lenarg,0);
}


void freq_init_random (Freq * farg, int lenarg) 
{
  freq_init_value(farg,lenarg,1);
}


void freq_init_copy (Freq * fmfarg, Freq * tofarg)
{
  int i;
  int flen = freq_get_length(fmfarg);
  assert(flen >= 0);
  for(i = 0; i < flen; i++) {
    freq_set(tofarg,i,freq_get(fmfarg,i));
  }
  /* set length here */
  freq_set_length(tofarg,flen);
}


/* return 1 if equal, 0 if not */
int freq_equal (Freq * f1arg, Freq * f2arg)
{
  int i;
  int flen1 = freq_get_length(f1arg);
  int flen2 = freq_get_length(f2arg);
  assert(flen1 >= 0);
  if(flen1 != flen2) return 0;
  for(i = 0; i < flen1; i++) {
    if(freq_get(f1arg,i) != freq_get(f2arg,i))
      return 0;
  }
  return 1;
}


int freq_total(Freq * farg) 
{
  int i;
  int total = 0;
  int endrec = freq_get_length(farg);
  for(i = 0; i < endrec; i++) {
    total += freq_get(farg,i);
  }
  return total;
}


/* returns the (smallest) index of the most frequent */
/* 0 1 2 3 4 idx  */
/* 0 1 8 1 0 freq */
/* returns idx 2  */
int freq_most(Freq * farg) 
{
  int i;
  int max = 0;
  int rtn = 0;
  int endrec = freq_get_length(farg);
  for(i = 0; i < endrec; i++) {
    int freq = freq_get(farg,i);
    if(freq > max){
      max = freq;
      rtn = i;
    }
  }
  return rtn;
}


/* compares the contents of two pointers into a Freq array; no change
   to Freq; no global needed; for call to qsort v.45, v.49 */
static int freq_compare(const void * f1arg, const void * f2arg)
{
  unsigned int ** f1 = (unsigned int **) f1arg;
  unsigned int ** f2 = (unsigned int **) f2arg;
  /*  fprintf(stderr,"freq_compare: comparing %d and %d\n",*(*f1), *(*f2)); */
  if ( *(*f1) < *(*f2) ) return -1;
  if ( *(*f1) == *(*f2) ) return  0;
  return 1;
}

#define freq_internalarray(f) (((f)->bit))

/* sort the frequencies of farg into uninitialized sortarrayarg; used
   freq_convert_to_index to get at the sorted freq  */
void freq_sort(Freq * farg, unsigned int ** sortarrayarg)
{
  int i;
  unsigned int * freqbaseaddr = freq_internalarray(farg);
  int flen = freq_get_length(farg);
  
  /* initialize array to pointers to bitfreq's array of ints */
  for(i = 0; i < flen; i++) sortarrayarg[i] = freqbaseaddr + i;

#if 0 
  fprintf(stderr,"freq_sort: BEFORE qsort, array looks like\n");
  for(i = 0; i < flen; i++) fprintf(stderr,"%d %p\n",i,sortarrayarg[i]);
#endif

  /* sort the frequencies without changing the freq */
  qsort(sortarrayarg,flen,sizeof(int*),freq_compare);

#if 0
  fprintf(stderr,"freq_sort: AFTER qsort, array looks like\n");
  for(i = 0; i < flen; i++) fprintf(stderr,"%d %p\n",i,sortarrayarg[i]);

  fprintf(stderr,"freq_sort: AFTER qsort, converted sorted array looks like\n");
  for(i = 0; i < flen; i++){
    int idx = freq_convert_to_index(farg,sortarrayarg[i]);
    fprintf(stderr,"%d %d %d\n",i,idx,freq_get(farg,idx));
  }
#endif
}


/* convert an freq array address into an bit index v.45 */
int freq_convert_to_index(Freq * farg, unsigned int * ptrarg)
{
  unsigned int * baseaddr = freq_internalarray(farg);
  return (int) (ptrarg - baseaddr);
}


/* returns largest non-zero freq index */
/* 0 1 2 3 4 idx  */
/* 0 1 8 1 0 freq */
/* returns idx 3  */
/* o.w. 0 if all zero */
int freq_max(Freq * farg) 
{
  int i;
  int endrec = freq_get_length(farg);
  for(i = endrec - 1; i >= 0; i--) {
    if(freq_get(farg,i) > 0)
      return i;
  }
  return 0;
}


/* returns smallest non-zero freq index */
/* 0 1 2 3 4 idx  */
/* 0 1 8 1 0 freq */
/* returns idx 1  */
int freq_min(Freq * farg) 
{
  int i;
  int endrec = freq_get_length(farg);
  for(i = 0; i < endrec; i++) {
    if(freq_get(farg,i) > 0)
      return i;
  }
  return 0;
}


/* return the greatest recsize index: the accumulated total
   is not more than percentage of totarg */
int freq_accumpercent(Freq * farg, int totarg, int percentarg)
{
  int idx = 0;
  int lastidx = freq_get_length(farg);
  int accumfreq = 0;

  /*  assert(totarg > 0); */

  if(!percentarg || totarg <= 0)
    return freq_min(farg);

  while(idx < lastidx){
    int p;
    accumfreq += freq_get(farg,idx);
    p = ((accumfreq/(totarg* 1.0)) * 100);
    if(p > percentarg){
      fprintf(stderr,"accumfreq: number of records with <= %d specified bits is %d%% (%d/%d) of NDB and greater than %d%% threshold--set minimum bits to %d\n",
              idx+1, p, accumfreq, totarg, percentarg, idx);
      break;
    }
    idx++;
  }
  return idx;
}


void freq_destroy(Freq * farg)
{
  assert(farg);
  free(farg->bit);
  free(farg);
}


void freq_print_stats (Freq * farg, FILE * fdarg, int dbsizearg) 
{
  int i;
  int endrec;
  if(dbsizearg <= 0) return;        /* return quietly v.19 */
  if(farg==NULL) return;            /* return quietly v.56 */
  endrec = freq_get_length(farg);
  for(i = 0; i < endrec; i++) {
    int freq = freq_get(farg,i);
    int avg = (freq / dbsizearg) * 10000;
    float avgpercent = avg / 100;
    fprintf(fdarg,"%d %d %f\n", i, freq, avgpercent);
  }
}


void freq_print (Freq * farg, FILE * fdarg) 
{
  int i;
  int endrec = freq_get_length(farg);
  for(i = 0; i < endrec; i++) 
    fprintf(fdarg,"%d %d\n", i,freq_get(farg,i));
}


void test_freq(void)
{ 
  Freq * freq;
  fprintf(stderr,"starting test_freq...\n");

  freq = freq_create(1);
  freq_init(freq,10);
  if( freq_total(freq) != 0) {
    exit(25);
  }

  freq_increment(freq,0);
  freq_increment(freq,0);
  freq_increment(freq,7);
  freq_increment(freq,7);
  freq_increment(freq,7);
  freq_increment(freq,9);

  if( freq_total(freq) != 6) {
    exit(26);
  }

  if(freq_get(freq,0) != 2 || freq_get(freq,7) != 3 || freq_get(freq,9) != 1) {
    exit(27);
  }

  if(freq_most(freq) != 7 || freq_max(freq) != 9 || freq_min(freq) != 0) {
    exit(28);
  }

  freq_print(freq,stdout);
  freq_print_stats(freq,stdout,1);
  freq_destroy(freq);
  fprintf(stdout,"complete freq test\n");
}


void test_freq_sort(int flenarg)
{ 
  Freq * freq;
  Freq * savefreq;
  int i;
  unsigned int ** sortarray;
  int numunits = rec_calcnumunits(flenarg);

  fprintf(stderr,"starting test_freq_sort...length %d\n",flenarg);

  freq = freq_create(numunits);
  freq_init(freq,flenarg);
  if( freq_total(freq) != 0) {
    exit(1);
  }
  
  i = flenarg << 1;

  while(i-- > 0){
    int r = nextRand(flenarg);
    freq_increment(freq,r);
  }

  if( freq_total(freq) != (flenarg << 1)) {
    freq_destroy(freq);
    exit(2);
  }

  sortarray = malloc(sizeof(unsigned int*) * (flenarg));
  if(sortarray==NULL){
    fprintf(stderr,"test_freq_sort: out of memory condition for sort array\n");
    freq_destroy(freq);
    exit(err12);
  }

  savefreq = freq_create(numunits);
  freq_init_copy(freq,savefreq);
  if( !freq_equal(freq,savefreq) ) {
    fprintf(stderr,"test_freq_sort: copy not the same as original freq\n");
    freq_destroy(freq);
    freq_destroy(savefreq);
    free(sortarray);
    exit(3);
  }

  fprintf(stderr,"test_freq_sort: BEFORE sort, freq looks like this:\n");
  freq_print(freq,stderr);

  freq_sort(freq,sortarray);       /* uninitialized ptr to an array of ptrs to ints */

  fprintf(stderr,"test_freq_sort: AFTER sort, freq looks like this:\n");
  freq_print(freq,stderr);
  fprintf(stderr,"test_freq_sort: AFTER sort, converted sortarry looks like this:\n");
  for(i=0 ; i < flenarg; i++){
    int idx = freq_convert_to_index(freq,sortarray[i]);
    fprintf(stderr,"%d %d %d\n",i,idx,freq_get(freq,idx));
  }

  for(i=flenarg - 1; i > 0; i--){
    int idx1 = freq_convert_to_index(freq,sortarray[i]);
    int idx2 = freq_convert_to_index(freq,sortarray[i-1]);
    int f1 = freq_get(freq,idx1);
    int f2 = freq_get(freq,idx2);
    if(f1 < f2){
      fprintf(stderr,"test_freq_sort: sorted freq not in order (at %d) idx1=%d, idx2=%d, f1=%d, f2=%d\n",i,idx1,idx2,f1,f2);
      free(sortarray);
      freq_destroy(freq);
      freq_destroy(savefreq);
      exit(4);
    }
  }
   
  if(!freq_equal(freq,savefreq)){
    fprintf(stderr,"test_freq_sort: sorted freq order changed\n");
    free(sortarray);
    freq_destroy(freq);
    freq_destroy(savefreq);
    exit(5);
  }

  free(sortarray);
  freq_destroy(freq);
  freq_destroy(savefreq);
  fprintf(stdout,"complete freq sort test\n");
}


