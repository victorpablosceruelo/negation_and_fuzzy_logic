/* This file is part of the Online Negative Database (NDB), Copyright
   (C) 2008 ackleyshack,LLC and Copyright (C) 2004-2008 elena s ackley
   and the Regents of the University of New Mexico.
 
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
 * Filename      : recset.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for a Set of NDB Records
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Fri May  9 10:28:43 2008 
 *
 * Comments : based on the Fernando Esponda NDB Algorithms; plus Eric
 *            Trias' complement.
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include "negdb.h"
#include "newndb.h"
#include "rstree.h"
#include "error.h"
#include "recset.h"

#if SANITY
static int recset_query_slow(Recset * rsetarg, Rec * rarg);
static int recset_query_size_slow(Recset * rsetarg, Rec * rarg);
static int recset_query_all_slow(Recset * rset1arg, Recset * rset2arg);
static int recset_query_match_slow(Recset * rsetarg, Rec * rarg);
static int recset_query_match_size_slow(Recset * rsetarg, Rec * rarg);
static int recset_query_match_all_slow(Recset * rset1arg, Recset * rset2arg);
static int recset_query_match_none_slow(Recset * rset1arg, Recset * rset2arg);
static int recset_query_subsumed_slow(Recset * rsetarg, Rec * rarg);
static int recset_query_subsumed_size_slow(Recset * rsetarg, Rec * rarg);
#endif

__inline__ Rec * recset_getrec(Recset * rsetarg, int idxarg) 
{
  if(idxarg >= rsetarg->numrecs) {
    fprintf(stderr,"recset_getrec: idx is (%d), numrecs is (%d), recset size is (%d)\n",idxarg,rsetarg->numrecs,recset_size(rsetarg));
    assert(idxarg < rsetarg->numrecs);
  }
  return rsetarg->collection[idxarg];
}


/* returns current position in the recset array, starts search at end;
returns -1 if not found  v.46 */
int recset_getrec_index(Recset * rsarg, Rec * rarg) {
  int rtn = -1;
  int i = recset_size(rsarg);
  int numunits = recset_numunits(rsarg);
  
  while(i-- > 0) {
    if(rec_equal(rarg,recset_getrec(rsarg,i),numunits)) {
      rtn = i;
      break;
    }
  }
  return rtn;
}

/* returns -1 if no record of specified size is found v.46 */
int recset_getrec_lastofsize_index(Recset * rsarg, int recszarg)
{
  int rtn = -1;
  int i = recset_size(rsarg);
  int numunits = recset_numunits(rsarg);
  
  while(i-- > 0) {
    if(rec_size(recset_getrec(rsarg,i),numunits) == recszarg) {
      rtn = i;
      break;
    }
  }
  return rtn;
}


/* creates freq if it doesn't exist v.45, no more v.48 */
Freq * recset_getbitfrequency(Recset * rsetarg)
{
  return rsetarg->bitfreq;
}


void recset_setbitfrequency(Recset *rsetarg, Freq * farg)
{
  rsetarg->bitfreq = farg;
}


static void recset_recbitfreq(Rec * rarg, Freq * farg, int reclenarg)
{
  int i;
  for(i = reclenarg - 1; i >= 0; i--) {
    if (! rec_isdc(rarg,i)){
      freq_increment(farg,i);
    }
  }
}


static void recset_recbitfreq_neg(Rec * rarg, Freq * farg, int reclenarg)
{
  int i;
  for(i = reclenarg - 1; i >= 0; i--) {
    if (! rec_isdc(rarg,i)){
      freq_decrement(farg,i);
    }
  }
}


void recset_update_bit_frequency(Recset * rsarg)
{
  int i;
  int lastrec = recset_size(rsarg);
  int reclen = recset_length(rsarg);
  Freq * bitfreqs = recset_getbitfrequency(rsarg);

  if (bitfreqs == NULL) {
    bitfreqs = freq_create(recset_numunits(rsarg));
    recset_setbitfrequency(rsarg,bitfreqs);
  }

  freq_init(bitfreqs,reclen);
    
  for(i = lastrec - 1; i >= 0; i--) {
    recset_recbitfreq(recset_getrec(rsarg,i),bitfreqs,reclen);    
  }
}


/* returns the lowest bit that appears the most */
static __inline__ int recset_getmaxbitfreq(Recset * rsarg)
{
  return freq_max(recset_getbitfrequency(rsarg));
}


static __inline__ void recset_printbitfreq(Recset * rsarg, FILE * fdarg)
{
  freq_print_stats(recset_getbitfrequency(rsarg),fdarg,recset_size(rsarg));
}


/* recset_update_recsize_freq creates freq if it doesn't exist v.45 */
Freq * recset_getrecsizefrequency(Recset * rsetarg)
{
  return rsetarg->recszfreq;
}

/* v.45 */
void recset_setrecsizefrequency(Recset *rsetarg, Freq * farg)
{
  rsetarg->recszfreq = farg;
}


static __inline__ void recset_recszfreq(Rec * rarg, Freq * farg, int numunitsarg)
{
  freq_increment(farg,rec_size(rarg,numunitsarg));
}


static __inline__ void recset_recszfreq_neg(Rec * rarg, Freq * farg, int numunitsarg)
{
  freq_decrement(farg,rec_size(rarg,numunitsarg));
}


/* populates freq with record sizes v.45 */
void recset_update_recsize_frequency(Recset * rsarg)
{
  int i;
  int lastrec = recset_size(rsarg);
  int numunits = recset_numunits(rsarg);
  Freq * recszfreq = recset_getrecsizefrequency(rsarg);

  if (recszfreq == NULL) {
    recszfreq = freq_create(recset_numunits(rsarg)+1);
    recset_setrecsizefrequency(rsarg,recszfreq);
  }

  freq_init(recszfreq,recset_length(rsarg)+1);

  for(i = lastrec - 1; i >= 0; i--) {
    recset_recszfreq(recset_getrec(rsarg,i),recszfreq,numunits);    
  }
}


/* returns the maximum number of specified bits per record v.45 */
int recset_max_recordsize(Recset * rsetarg)
{
  Freq * recszfreq = recset_getrecsizefrequency(rsetarg);
  if(recszfreq == NULL){
    recset_update_recsize_frequency(rsetarg);
    recszfreq = recset_getrecsizefrequency(rsetarg);
  }
  return freq_max(recszfreq);
}


/* returns the maximum number of specified bits per record v.45 */
int recset_most_recordsize(Recset * rsetarg)
{
  Freq * recszfreq = recset_getrecsizefrequency(rsetarg);
  if(recszfreq == NULL){
    recset_update_recsize_frequency(rsetarg);
    recszfreq = recset_getrecsizefrequency(rsetarg);
  }
  return freq_most(recszfreq);
}


/* returns the minimum number of specified bits per record v.45 */
int recset_min_recordsize(Recset * rsetarg)
{
  Freq * recszfreq = recset_getrecsizefrequency(rsetarg);
  if(recszfreq == NULL){
    recset_update_recsize_frequency(rsetarg);
    recszfreq = recset_getrecsizefrequency(rsetarg);
  }
  return freq_min(recszfreq);
}


/* returns the value that appears most for a specified bit position,
   -1 if it doesn't appear at all v.50 */
int recset_popular_bitposition_value(Recset * rsetarg, int bparg)
{
  int i;
  int sz = recset_size(rsetarg);
  int count1 = 0;
  int count0 = 0;

  assert(bparg < recset_length(rsetarg));
  for(i = 0; i < sz; i++){
    Rec * rec = recset_getrec(rsetarg,i);
    int val = rec_getbit(rec,bparg);
    if(val==1) count1++;
    if(val==0) count0++;
  }

  /*  fprintf(stderr,"recset_popular_bitposition_value: found %d 0's and %d 1's\n",count0,count1); */
  return (count1==count0 && count1==0 ? -1 : count1 > count0);
}


int recset_getupdateflag(Recset * rsetarg) 
{
  return rsetarg->updateflag;
}


void recset_setupdateflag(Recset * rsetarg) 
{
  rsetarg->updateflag = 1;
}


void recset_clearupdateflag(Recset * rsetarg) 
{
  rsetarg->updateflag = 0;
}


/* last arg might better be a parm */
static void recset_copy_collection(Rec ** fromarg , Rec ** toarg, int numarg)
{
  int i;
  for(i = numarg - 1; i >= 0; i-- ) {
    toarg[i] = fromarg[i];
  }
}


/* private for now */
static void recset_expand(Recset * rsetarg)
{
  int sz = rsetarg->capacity << 1;
  Rec ** newrset = (Rec **) malloc(sizeof(Rec*)*sz);
  if(newrset == NULL) {
    fprintf(stderr,"recset_expand: failed to allocate new recset\n");
    exit(err12);
  }
  recset_copy_collection(rsetarg->collection,newrset,rsetarg->numrecs);
  free(rsetarg->collection);
  rsetarg->collection = newrset;
  rsetarg->capacity = sz;
  
  /* removed call to recset_tree_clear when root not null; rstree
     points to rec's which aren't changed with the double and copy
     (this expand). just took a few versions to think that
     through. v.30 */
}


/* called directly when recset is on the stack, o.w. called by recset_create */
void recset_init(Recset * rsetarg, int lenarg) 
{
  int sz = 2;
  rsetarg->collection = (Rec **) malloc(sizeof(Rec*) * sz);
  if(rsetarg->collection == NULL) {
    fprintf(stderr,"recset_init: failed to allocate new recset collection\n");
    exit(err12);
  }
  rsetarg->numrecs = 0;
  rsetarg->capacity = sz;
  recset_clearupdateflag(rsetarg);
  rsetarg->recbitlen = lenarg;
  rsetarg->bitfreq = NULL;   /* lazy evaluation */
  rsetarg->recszfreq = NULL; /* lazy evaluation */
  rsetarg->root = NULL;      /* lazy evaluation */
}


Recset * recset_create()
{
  Recset * rsetptr = (Recset *) malloc(sizeof(Recset));
  if(rsetptr == NULL) {
    fprintf(stderr,"recset_create: failed to allocate recset\n");
    exit(err12);
  }
  recset_init(rsetptr,0);
  rsetptr->root = rstree_createnode(0);
  return rsetptr;
}


static int recset_existsrec(Recset * rsetarg, Rec * rarg);

void recset_sanitycheck_tree(Recset * rsetarg)
{
  int i;
  int treesize = 0;
  int lastrec = recset_size(rsetarg);
  int flag = 0;
  
  if(rsetarg->root != NULL)
    treesize = rstree_size(rsetarg->root);
  
  if(treesize != lastrec) {
    fprintf(stderr,"recset_sanitycheck_tree: tree,array sizes different %d,%d\n",treesize,lastrec);
    flag = 1;
    /*    exit(err17); v.45 testing */
  }
  
  for (i = 0; i < lastrec; i++) {
    if(! recset_existsrec(rsetarg,recset_getrec(rsetarg,i))) {
      fprintf(stderr,"recset_sanitycheck_tree: NDB record (%d) does not exist in tree\n",i);
      rec_print(recset_getrec(rsetarg,i),recset_length(rsetarg),stderr);
      flag++;
    }
  }
  
  if(flag > 0){
    fprintf(stderr,"recset_sanitycheck_tree: exiting with %d errors\n",flag);
    exit(err17);
  } else {
    fprintf(stderr,"recset_sanitycheck_tree: a-ok\n");
  }
  return;
}


void recset_tree_clear(Recset * rsetarg)
{
  rstree_destroy(rsetarg->root);
  rsetarg->root = NULL;
}


static void recset_tree_build(Recset * rsetarg)
{
  int i;
  int lastrec = recset_size(rsetarg); /* could be zero at build time */
  
  if(rsetarg->root != NULL)            /* v.45 */
    recset_tree_clear(rsetarg);
  
  rsetarg->root = rstree_createnode(0);
  
  for( i = 0; i < lastrec; i++)
    rstree_addrec(recset_getrec(rsetarg,i),rsetarg->root,rsetarg);   /* insert into tree */
  
#if SANITY
  fprintf(stderr,"recset_tree_build: calling sanity check\n");
  recset_sanitycheck_tree(rsetarg);
#endif
}


int recset_tree_size(Recset * rsetarg)
{
  assert(rsetarg->root);
  return rstree_size(rsetarg->root);
}


/* return true if arg rec already exists exactly in recset, o.w. false */
static int recset_existsrec(Recset * rsetarg, Rec * rarg) 
{
  if(rsetarg->root == NULL)
    recset_tree_build(rsetarg);
  return rstree_exists(rsetarg->root,rarg,rsetarg);
}


/* does not free the removed record */
void recset_removerec(Recset * rsetarg, int idxarg)
{
  Freq * bitfreq = recset_getbitfrequency(rsetarg);
  Freq * recszfreq = recset_getrecsizefrequency(rsetarg);
  int lastrecidx = recset_size(rsetarg) - 1; 
  int reclen = recset_length(rsetarg);
  Rec * rec = recset_getrec(rsetarg,idxarg);
  int treethere;
  
#if SANITY
  fprintf(stderr,"recset_removerec: before removing rec\n");
  rec_print(rec,recset_length(rsetarg),stderr);
  if(rsetarg->root !=NULL){
    fprintf(stderr,"recset_removerec: calling sanity check\n");
    recset_sanitycheck_tree(rsetarg);
  } else {
    fprintf(stderr,"recset_removerec: there is no tree\n");
  }
#endif
  
  /* update bit frequency */
  if(bitfreq != NULL)
    recset_recbitfreq_neg(rec, bitfreq, reclen);

  /* update recsize frequency v.45, only if not null v.48 */
  if(recszfreq != NULL)
    recset_recszfreq_neg(rec, recszfreq, recset_numunits(rsetarg));
  
  treethere = rstree_removerec(rsetarg,rec);
  
  if (idxarg < lastrecidx) {
    rsetarg->collection[idxarg] = rsetarg->collection[lastrecidx];
    rsetarg->collection[lastrecidx] = NULL;       /* v.45 */
  }
  rsetarg->numrecs--;
  assert(rsetarg->numrecs >= 0);
  
#if SANITY
  fprintf(stderr,"recset_removerec: after removing rec\n");
  rec_print(rec,recset_length(rsetarg),stderr);
  if(treethere){
    fprintf(stderr,"recset_removerec: calling sanity check\n");
    recset_sanitycheck_tree(rsetarg);
  }
#endif
}


/* removes and frees the first equal record; 
   up to caller to destroy rarg argument (v.34) */
int recset_deleterec(Rec * rarg, Recset * rsetarg)
{
  int rtn = 0;
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  
  while(i-- > 0) {
    Rec * rec = recset_getrec(rsetarg,i);
    if(rec_equal(rarg,rec,numunits)) {
      recset_removerec(rsetarg,i);
      rec_destroy(rec);
      rtn = 1;
      break;
    }
  }
  return rtn;
}


int recset_size(Recset * rsetarg)
{
  return rsetarg->numrecs;
}


void recset_setlength(Recset * rsetarg, int lenarg)
{
  rsetarg->recbitlen = lenarg;
}


/* v.27 number of bits per record */
int recset_length(Recset * rsetarg)
{
  return rsetarg->recbitlen;
}


/* v.27 numunits calculated from length */
int recset_numunits(Recset * rsetarg)
{
  int len = recset_length(rsetarg);
  assert(len > 0);
  return rec_calcnumunits(len);
}


/* v.27 replace runtime function - use recset_max_recordsize for recset specifics */
int recset_maxbits(Recset * rsetarg)
{
  return recset_numunits(rsetarg) * UNITSIZE;
}


int recset_ischanged(Recset * rsetarg) 
{
  return rsetarg->updateflag;
}


static void recset_freerecs(Recset * rsetarg)
{
  int i = 0;
  int lastrec = recset_size(rsetarg);
  
  while(i < lastrec) {
    rec_destroy(rsetarg->collection[i++]); 
  }
  
  free(rsetarg->collection);
  rsetarg->collection = NULL;
  rsetarg->numrecs = 0;
  rsetarg->capacity = 0;
  rsetarg->recbitlen = 0;   /* v.65.2 */

  recset_clearupdateflag(rsetarg);

  if(rsetarg->root != NULL) {
    recset_tree_clear(rsetarg);
  }
}


void recset_final(Recset * rsetarg)
{
  recset_freerecs(rsetarg);
  if(rsetarg->bitfreq != NULL) {
    freq_destroy(rsetarg->bitfreq);
    rsetarg->bitfreq = NULL;
  }
  if(rsetarg->recszfreq != NULL) {
    freq_destroy(rsetarg->recszfreq);
    rsetarg->recszfreq = NULL;
  }
}


void recset_destroy(Recset * rsetarg)
{
  if(rsetarg != NULL) {
    recset_final(rsetarg);
    free(rsetarg); 
  }
}


/* returns 1 if added, 0 if duplicate, and frees duplicate record
   (refactored managed_growth_reductions v.64)
*/
int recset_addrec(Rec * rarg, Recset * rsetarg)
{
  Freq * bitfreq;
  Freq * recszfreq;
  int dupflag = 0;
  
#if SANITY
  fprintf(stderr,"recset_addrec: adding record ");
  rec_print(rarg,recset_length(rsetarg),stderr);
  assert(rec_checkhash(rarg,recset_numunits(rsetarg)));
#endif
  
  if(rsetarg->root == NULL)
    recset_tree_build(rsetarg);

  dupflag = rstree_addrec(rarg,rsetarg->root,rsetarg);             /* put into tree */
 
  if(dupflag){
    rec_destroy(rarg);  /* duplicate */
    return 0;
  }
  
  if(rsetarg->numrecs >= rsetarg->capacity) {
    recset_expand(rsetarg);
  }
  
  rsetarg->collection[rsetarg->numrecs] = rarg;
  rsetarg->numrecs++;
  
#if SANITY
  assert(rsetarg->numrecs == recset_tree_size(rsetarg)); /* v.19.1 */
  if(!recset_existsrec(rsetarg,rarg)){                   /* v.45 */
    fprintf(stderr,"recset_addrec: tree does reflect existence of rec: ");
    rec_print(rarg,recset_length(rsetarg),stderr);
    exit(err17);
  }
#endif

  /* update bit frequency */
  bitfreq = recset_getbitfrequency(rsetarg);
  if(bitfreq != NULL)
    recset_recbitfreq(rarg, bitfreq, recset_length(rsetarg));

  /* update recsize frequency v.45 */
  recszfreq = recset_getrecsizefrequency(rsetarg);  /* before the add v.45 */
  if(recszfreq != NULL)
    recset_recszfreq(rarg,recszfreq, recset_numunits(rsetarg));
  
  return 1;
}


/* for the query that can have don't care symbols in its string (v.22); 
   updated for more efficient selects (v.32); or not (v.41) */
/* must have at least 2 specified bits per record for Project option
   to give correct results when no solutions are present,
   expand record size of ndbcopy if necessary v.53 */
static void recset_partial_query(Rec * rarg, Recset * ndbarg, Recset * partialsetarg, Runtime * rtarg)
{
  int projopt = runtime_getProjOption(rtarg);

  if(projopt || RELOP_EFFICIENCY_STEP ){
    Recset ndbcopy;

    nsql_thetaselect(rarg,ndbarg,partialsetarg,rtarg); /* result could have no recs */

    recset_init(&ndbcopy,recset_length(ndbarg));
    recset_copy(ndbarg,&ndbcopy);

    /* right after the copy, insure the ndb has at least two specified
       bits per record (replaces err21) v.53 */
    if(projopt && recset_min_recordsize(&ndbcopy) < SAT2) {
      if(runtime_getMinBits(rtarg) < SAT2) runtime_setMinBits(rtarg,SAT2);
      recset_expandrecsize(&ndbcopy,rtarg);
    }

#if 1
    /* delete all the records from ndbcopy that are subsumed by the
       records in partialsetarg; undocumented efficiency step v.32;
       UNNEEDED with v.59's addrec that eliminates redundant recs;
       restored with v.64 that refactored addrec redundancy checks
       v.65 */
    recset_delete_subsumed(&ndbcopy,partialsetarg); 
#endif

    recset_merge(&ndbcopy,partialsetarg); 
    recset_final(&ndbcopy);

  } else {
    
    /* per spec, just not efficient in terms of output size; here for
       testing purposes only v.34; or if RELOP_EFFICIENCY_STEP is
       turned off (v.41); or when MANAGED_GROWTH is used (v.60.1) */
    
    recset_copy(ndbarg,partialsetarg);
    nsql_thetaselect(rarg,ndbarg,partialsetarg,rtarg);
  }

  return;
}


/* returns false if there exists a record in the recset that does NOT
   subsume rarg; o.w. true (required by npg) */
int recset_query(Recset * rsetarg, Rec * rarg) 
{
  int rtn;
  if(rsetarg->root == NULL) {
    fprintf(stderr,"recset_query: must build tree first\n"); 
    recset_tree_build(rsetarg);
  }
  rtn = rstree_query(rsetarg->root,rarg,recset_numunits(rsetarg));
  
#if SANITY
  if (recset_query_slow(rsetarg,rarg) != rtn) { /* v.42 */
    fprintf(stderr,"recset_query: internal inconsistency (%d)\n",rtn);
    exit(7);
  }
  recset_tree_save(rsetarg,"rndb-tree.txt");
#endif

  return rtn;
}


/* returns TRUE if a ndb record exists that subsumes rarg (i.e. in
   NDB, not in DB); o.w. all records in rsetarg do NOT subsume rarg,
   false (in DB) */
/* added for testing purposes v.42 */
static int recset_query_slow(Recset * rsetarg, Rec * rarg)
{
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  
  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    
    if(rec_subsumed_query(rarg,recy,numunits)) {
      return 1;
    }
  }
  return 0;
}


/* returns count of recs in recset that subsume rarg */
int recset_query_size(Recset * rsetarg, Rec * rarg) 
{
  int count;
  if(rsetarg->root == NULL)
    recset_tree_build(rsetarg);
  count = rstree_query_size(rsetarg,rarg,recset_numunits(rsetarg));

#if SANITY
 {
   int slowcount = recset_query_size_slow(rsetarg,rarg);
   if ( slowcount != count) { /* v.52 */
     fprintf(stderr,"recset_query_size: internal inconsistency (%d,%d)\n",
             count,slowcount);
     exit(7);
   } else {
     fprintf(stderr,"recset_query_size: counts match (%d,%d)\n",
             count,slowcount);
   }
 }
#endif
 return count;
}

#if SANITY
/* returns count of records in recset that subsume rarg;
   added for testing purposes v.52 */
static int recset_query_size_slow(Recset * rsetarg, Rec * rarg)
{
  int count = 0;
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  
  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    if(rec_subsumed_query(rarg,recy,numunits)) {
      count++;
    }
  }
  return count;
}
#endif

/* returns false if there does not exist a record that rarg subsumes;
   o.w. true (there is at least one  record subsumed by rarg) */
int recset_query_subsumed(Recset * rsetarg, Rec * rarg) 
{
  int rtn;
  if(rsetarg->root == NULL) {
    fprintf(stderr,"recset_query_subsumed: must build tree first\n");
    recset_tree_build(rsetarg);
  }
  rtn = rstree_query_subsumed(rsetarg->root,rarg,recset_numunits(rsetarg));

#if SANITY
  if (recset_query_subsumed_slow(rsetarg,rarg) != rtn) { /* v.52 */
    fprintf(stderr,"recset_query_subsumed: internal inconsistency (%d)\n",rtn);
    exit(7);
  }
  recset_tree_save(rsetarg,"rndb-tree.txt");
#endif

  return rtn;
}


#if SANITY
/* returns TRUE if a ndb record exists that rarg subsumes; o.w. all
   records in rsetarg are NOT subsumed by rarg, false */
/* added for testing purposes v.52 */
static int recset_query_subsumed_slow(Recset * rsetarg, Rec * rarg)
{
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  
  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    
    if(rec_subsumed_query(recy,rarg,numunits)) {
      return 1;  /* there exists a record subsumed by rarg */
    }
  }
  return 0;      /* no records are subsumed by rarg */
}
#endif


/* returns count of recs in recset subsumed by rarg */
int recset_query_subsumed_size(Recset * rsetarg, Rec * rarg) 
{
  int count;
  if(rsetarg->root == NULL)
    recset_tree_build(rsetarg);

  count = rstree_query_subsumed_size(rsetarg,rarg,recset_numunits(rsetarg));

#if SANITY
 {
   int slowcount = recset_query_subsumed_size_slow(rsetarg,rarg);
  if ( slowcount != count) { /* v.52 */
    fprintf(stderr,"recset_query_subsumed_size: internal inconsistency (%d,%d)\n",
            count,slowcount);
    exit(7);
  } else {
    fprintf(stderr,"recset_query_subsumed_size: counts match (%d,%d)\n",
            count,slowcount);
  }
 }
#endif

 return count;
}


#if SANITY
/* returns count of records in recset that are subsumed by rarg;
   added for testing purposes v.52 */
static int recset_query_subsumed_size_slow(Recset * rsetarg, Rec * rarg)
{
  int count = 0;
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  
  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    if(rec_subsumed_query(recy,rarg,numunits)) {
      count++;
    }
  }
  return count;
}
#endif


/* return 0 if the two negative input recsets are equivalent (based on Eric
   Trias' algorithm) v.72; complements input first; mode 0 v.72.7 */
static int recset_equivalence_with_complements(Recset * rset1arg, Recset * rset2arg, Runtime * rtarg)
{
  int rtn = 1;
  Recset * compset1;
    
#if ! MANAGED_GROWTH
  runtime_setMinBits(rtarg,recset_length(rset1arg));        /* v.64 */
#endif

  compset1 = recset_create();
  recset_complement(rset1arg,compset1,rtarg); 

  /* compares complement of first recset looking for no records that
     "match" (i.e. any specified bits are the same) in the second
     ndb v.71 */

  if(recset_query_match_none(compset1,rset2arg)){
    Recset * compset2;

    compset2 = recset_create();
    recset_complement(rset2arg,compset2,rtarg);
    
    /* compares complement of second recset looking for no records
       that "match" (i.e. specified bits are the same) in the first
       ndb v.71 */

    if(recset_query_match_none(compset2,rset1arg)){
      rtn = 0;
      }
    recset_destroy(compset2);
  }
  recset_destroy(compset1);
  return rtn;
}


/* return 0 if the two input recsets are equivalent (based on Eric
   Trias' algorithm) v.66; compresses input first for reliable results
   v.69; mode 1 v.72.7 */
static int recset_equivalence_with_compress(Recset * rset1arg, Recset * rset2arg, Runtime * rtarg)
{
  int minbits1 = recset_min_recordsize(rset1arg);
  int minbits2 = recset_min_recordsize(rset2arg);

  if(minbits1 <= minbits2) {
    fprintf(stdout,"recset_equivalence: compress to minbits1 (%d)\n",minbits1);
    compress(rset1arg,rtarg,minbits1);
    compress(rset2arg,rtarg,minbits1);
  } else {
    fprintf(stdout,"recset_equivalence: compress to minbits2 (%d)\n",minbits2);
    compress(rset1arg,rtarg,minbits2);
    compress(rset2arg,rtarg,minbits2);
  }
  return !(recset_query_all(rset1arg,rset2arg) && recset_query_all(rset2arg,rset1arg)); 
}


/* return 0 if the two input recsets are identical (based on Eric
   Trias' algorithm); checks for same number of records, then one-way
   match v.72; mode 2 designed for fully-specified positive sets
   v.72.7 */
static int recset_identity_match(Recset * rset1arg, Recset * rset2arg, Runtime * rtarg)
{
  int rtn = 1;
  if(recset_size(rset1arg) == recset_size(rset2arg)) {
    if(recset_query_match_all(rset1arg,rset2arg))
      rtn = 0;
  }
  return rtn;
}


/* return TRUE if all the records in recset 1 with expanded *'s is
   subsumed in the second set; mode 3 designed for ternary positive
   and negative sets v.72.8 */
static int recset_query_expand_all(Recset * rset1arg, Recset * rset2arg, Runtime * rtarg)
{
  Rec * reccopy;
  int rtn = 1;
  int reclen = recset_length(rset1arg);
  int numunits = recset_numunits(rset1arg);
  int sz = recset_size(rset1arg);
  int i = 0;

  assert(reclen < INTSIZE); 

  reccopy = rec_create(numunits);

  while(i < sz && rtn) {
    Perm dcperm;
    int plen, pc;
    int b = 0;
    Rec * rec = recset_getrec(rset1arg,i);

    perm_init(&dcperm,reclen);
    plen = perm_dontcares(&dcperm,rec,reclen);

    pc = 1 << plen;                     /* 2^n possible bit combos */

    while( b < pc && rtn) {
      int j;
      int x = b;

      rec_copy(rec,reccopy,numunits);

      for(j = 0; j < plen; j++) {
        assert(rec_isdc(rec,perm_get(&dcperm,j)));
        rec_setbit(reccopy,perm_get(&dcperm,j),x&1,numunits);
        x = x >> 1;                      /* consider next bit for next position */
      }
      if(! recset_query(rset2arg,reccopy)) { 
#if 0
        fprintf(stderr,"recset_query_expand_all: at %d of %d of original rec %d:",b,pc,i);
        rec_print(rec,reclen,stderr);
        fprintf(stderr,"recset_query_expand_all: no record in rset2 subsumes expanded rec:");
        rec_print(reccopy,reclen,stderr);
#endif
        rtn = 0;                   /* done. no record in rset2 subsumes reccopy */
      }
      b++;
    }       /* end pc while */
    i++;
    perm_final(&dcperm);
  } /* end i while */

  rec_destroy(reccopy);
  return rtn;
}


/* return 0 if the two input recsets are equivalent (based on Eric
   Trias' algorithm); expand the *'s in each record and check for
   subsumption in the other set, both-ways; mode 3 designed for
   ternary positive and negative sets v.72.8 */
static int recset_equivalence_with_expansion(Recset * rset1arg, Recset * rset2arg, Runtime * rtarg)
{
  return !(recset_query_expand_all(rset1arg,rset2arg,rtarg) && recset_query_expand_all(rset2arg,rset1arg,rtarg));
}


/* return 0 if the two input recsets are equivalent (based on Eric
   Trias' algorithms) v.66; compresses input first for reliable
   results (mode 1) v.69; three modes: 0 requires complementing; 1
   requires compressing; 2 is checks for identity using match (v.72.7).
*/
int recset_equivalence(Recset * rset1arg, Recset * rset2arg, Runtime * rtarg)
{
  int mode = runtime_getCompareMode(rtarg);
  int rtn;
  switch (mode) {
  case 0:
    rtn = recset_equivalence_with_complements(rset1arg,rset2arg,rtarg);
    break;
  case 1:
    rtn = recset_equivalence_with_compress(rset1arg,rset2arg,rtarg); 
    break;
  case 2:
    rtn = recset_identity_match(rset1arg,rset2arg,rtarg);
    break;
  case 3:
    rtn = recset_equivalence_with_expansion(rset1arg,rset2arg,rtarg);
    break;
  default:
    rtn = 1;
  };
  return rtn;
}


/* returns true if all the recs in rset1arg are subsumed by rset2arg;
     ow. false if a record exists in rset1arg that does NOT match a
     rec in rset2arg; (used by online_compare) v.61.3 */
int recset_query_all(Recset * rset1arg, Recset * rset2arg) 
{
  int rtn = 1;
  int i = recset_size(rset1arg);

  assert(recset_length(rset1arg) == recset_length(rset2arg));

  while(i-- > 0) {
    Rec * recx = recset_getrec(rset1arg,i);

    rtn = recset_query(rset2arg,recx); 

   if(rtn == 0) break;
  }
  
#if SANITY
  if(recset_query_all_slow(rset1arg,rset2arg) != rtn) { /* v.61.3 */
    fprintf(stderr,"recset_query_all: internal inconsistency (%d)\n",rtn);
    exit(7);
  }
  recset_tree_save(rset1arg,"rndb-tree-1.txt");
  recset_tree_save(rset1arg,"rndb-tree-2.txt");
#endif
  
  return rtn;
}


#if SANITY
/* queries all the recs in rset1arg for a record in recset2arg that
   subsumes it. return false if there's not a record that subsumes
   them; o.w. true (used for compare) v.69
*/
static int recset_query_all_slow(Recset * rset1arg, Recset * rset2arg)
{
  int i = recset_size(rset1arg);
  int numunits = recset_numunits(rset1arg);

  assert(recset_length(rset1arg) == recset_length(rset2arg));
  
  while(i-- > 0) {
    int j = recset_size(rset2arg);
    Rec * recx = recset_getrec(rset1arg,i);
    int matchflag = 0;

    while(j-- > 0) {
      Rec * recy = recset_getrec(rset2arg,j);
    
      if(rec_subsumed_query(recx,recy,numunits) ) { /* there exists a record that subsumes recx */
        matchflag = 1;
        break;
      }
    }
    if(matchflag == 0) {       /* no match found */
      fprintf(stdout,"recset_query_all_slow: no match found for reci %d\n",i);
      rec_print(recx,recset_length(rset1arg),stdout);
      fprintf(stdout,"recset_query_all_slow: in recset 2:\n");
      recset_print(rset2arg,stdout);
      fprintf(stdout,"recset_query_all_slow: in recset 1:\n");
      recset_print(rset1arg,stdout);
     return 0;
    }
  }
  return 1;                    /* recset1 is subsumed by recset2 */
}
#endif


/* returns false if NO record exists that MATCHES rarg;
   o.w. true (used by pattern add); comment corrected v.70 */
int recset_query_match(Recset * rsetarg, Rec * rarg) 
{
  int rtn;
  if(rsetarg->root == NULL) {
    fprintf(stderr,"recset_query_match: must build tree first\n");
    recset_tree_build(rsetarg);
  }
  rtn = rstree_query_match(rsetarg->root,rarg,recset_numunits(rsetarg));
  
#if SANITY
  if (recset_query_match_slow(rsetarg,rarg) != rtn) { /* v.41 */
    fprintf(stderr,"recset_query_match: internal inconsistency (%d)\n",rtn);
    exit(7);
  }

  recset_tree_save(rsetarg,"rndb-tree.txt");
#endif
  return rtn;
}


/* returns true if a record exists that DOES match rarg;
   o.w. all records in rsetarg do NOT match rarg, false */
/* added for testing purposes v.41; comment corrected v.70 */
static int recset_query_match_slow(Recset * rsetarg, Rec * rarg)
{
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  
  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    
    if(rec_match_query(recy,rarg,numunits)) {
      fprintf(stderr,"MATCH FOUND: \n");
      rec_print(recy,recset_length(rsetarg),stderr);
      fprintf(stderr,"matches input: \n");
      rec_print(rarg,recset_length(rsetarg),stderr);
      return 1;
    }
  }
  return 0;
}


/* returns count of recs in recset that "match" rarg v.38 */
int recset_query_match_size(Recset * rsetarg, Rec * rarg) 
{
  int count;
  if(rsetarg->root == NULL)
    recset_tree_build(rsetarg);
  count = rstree_query_match_size(rsetarg,rarg,recset_numunits(rsetarg));

#if SANITY
 {
   int slowcount = recset_query_match_size_slow(rsetarg,rarg);
  if ( slowcount != count) { /* v.52 */
    fprintf(stderr,"recset_query_match_size: internal inconsistency (%d,%d)\n",
            count,slowcount);
    exit(7);
  } else {
    fprintf(stderr,"recset_query_match_size: counts match (%d,%d)\n",
            count,slowcount);
  }
 }
#endif
  return count;

}


#if SANITY
/* returns count of records in recset that are matched by rarg;
   added for testing purposes v.52 */
static int recset_query_match_size_slow(Recset * rsetarg, Rec * rarg)
{
  int count = 0;
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  
  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    if(rec_match_query(recy,rarg,numunits)) {
      count++;
    }
  }
  return count;
}
#endif


/* returns false if all the recs in rset1arg are NOT matched by rset2arg;
     ow. true if a record exists in rset2arg that does match each
     recs in rset1arg; (used by online_compare) v.61.3 */
int recset_query_match_all(Recset * rset1arg, Recset * rset2arg) 
{
  int rtn = 1;
  int i = recset_size(rset1arg);

  assert(recset_length(rset1arg) == recset_length(rset2arg));

  while(i-- > 0) {
    Rec * recx = recset_getrec(rset1arg,i);

    if(!(rtn = recset_query_match(rset2arg,recx)))  /* quit if NO rec matches */
      break;
  }
  
#if SANITY
  if(recset_query_match_all_slow(rset1arg,rset2arg) != rtn) { /* v.71 */
    fprintf(stderr,"recset_query_match_all: internal inconsistency (%d)\n",rtn);
    exit(7);
  }
  recset_tree_save(rset1arg,"rndb-tree-1.txt");
  recset_tree_save(rset1arg,"rndb-tree-2.txt");
#endif
  
  fprintf(stderr,"recset_query_match_all: return (%d)\n",rtn);
  return rtn;
}

#if SANITY
/* queries all the recs in rset1arg for a record in recset2arg that
   matches it. return false (0) if NO record matches them; o.w. true
   (used for compare) v.71
*/
static int recset_query_match_all_slow(Recset * rset1arg, Recset * rset2arg)
{
  int i = recset_size(rset1arg);
  int numunits = recset_numunits(rset1arg);

  assert(recset_length(rset1arg) == recset_length(rset2arg));
  
  while(i-- > 0) {
    int j = recset_size(rset2arg);
    Rec * recx = recset_getrec(rset1arg,i);
    int matchflag = 0;

    while(j-- > 0) {
      Rec * recy = recset_getrec(rset2arg,j);
    
      if(rec_match_query(recx,recy,numunits) ) { /* there exists a record that matches recx */
        matchflag = 1;
        break;
      }
    } /* end j while */
    
    if(matchflag == 0) { 
      fprintf(stdout,"recset_query_match_all_slow: match NOT found for reci %d\n",i);
      rec_print(recx,recset_length(rset1arg),stdout);
      return 0;     /* recset1 is NOT matched by recset2 */
    }
  }   /* end i while */
  return 1;         /* match found for each element of recset1 */                    
}
#endif


/* returns true if all the recs in rset1arg are NOT matched by rset2arg;
     ow. false if a record exists in rset2arg that does match a
     rec in rset1arg; (used by online_compare) v.61.3, v.72.6 */
int recset_query_match_none(Recset * rset1arg, Recset * rset2arg) 
{
  int rtn = 1;
  int i = recset_size(rset1arg);

  assert(recset_length(rset1arg) == recset_length(rset2arg));

  while(i-- > 0) {
    Rec * recx = recset_getrec(rset1arg,i);

    if(recset_query_match(rset2arg,recx))  /* quit if rec found that matches */
      rtn = 0;
      break;
  }
  
#if SANITY
  if(recset_query_match_none_slow(rset1arg,rset2arg) != rtn) { /* v.71 */
    fprintf(stderr,"recset_query_match_none: internal inconsistency (%d)\n",rtn);
    exit(7);
  }
  recset_tree_save(rset1arg,"rndb-tree-3.txt");
  recset_tree_save(rset1arg,"rndb-tree-4.txt");
#endif
  
  return rtn;
}

#if SANITY
/* queries all the recs in rset1arg for a record in recset2arg that
   matches it. return true (1) if NO record matches them all; o.w. false
   (used for compare) v.72.6
*/
static int recset_query_match_none_slow(Recset * rset1arg, Recset * rset2arg)
{
  int i = recset_size(rset1arg);
  int numunits = recset_numunits(rset1arg);

  assert(recset_length(rset1arg) == recset_length(rset2arg));
  
  while(i-- > 0) {
    int j = recset_size(rset2arg);
    Rec * recx = recset_getrec(rset1arg,i);
    int matchflag = 0;

    while(j-- > 0) {
      Rec * recy = recset_getrec(rset2arg,j);
    
      if(rec_match_query(recx,recy,numunits) ) { /* there exists a record that matches recx */
        matchflag = 1;
        break;
      }
    } /* end j while */
    
    if(matchflag == 1) { 
      fprintf(stdout,"recset_query_match_none_slow: match found for reci %d\n",i);
      rec_print(recx,recset_length(rset1arg),stdout);
      return 0;     /* recset1 has a rec matched by recset2 */
    }
  }   /* end i while */
  return 1;         /* no matches found */                    
}
#endif


/* return true if rec found in perm indexes of recset */
int recset_perm_query(Rec * rarg, Recset * rsetarg, Perm * parg, int numunitsarg) 
{
  int i;
  int lastrec = perm_get_length(parg);
  fprintf(stdout,"recset_perm_query: lastrec in perm is %d ",lastrec);
  
  for(i = 0;  i < lastrec; i++) {
    int j = perm_get(parg,i);
    fprintf(stdout,"recset_perm_query: j in perm at %d is %d ",i,j);
    if ( rec_subsumed_query(rarg,recset_getrec(rsetarg,j),numunitsarg)) {
      return 1;
    }
  }
  return 0;  /* not found */
}


/* for each rec in ndb2 delete the rec's in ndb1 that are
   "subsumed" by it. deleted recs are freed v.32; added measuring in v.36
*/
void recset_delete_subsumed(Recset * ndb1arg, Recset * ndb2arg)
{
  int i = recset_size(ndb2arg);
  int numunits = recset_numunits(ndb2arg);
#if defined(DISPLAY_RECCOUNT_DELETED) || defined(DISPLAY_RECCOUNT_DELETED_ALL)
  int total_reccount_old = recset_size(ndb1arg);
#endif
  int total_reccount_deleted = 0;
  
  while(i-- > 0) {
    Rec * recy = recset_getrec(ndb2arg,i);
    int j = recset_size(ndb1arg);
    int reccount = 0;
    
    while(j-- > 0) {
      Rec * recx = recset_getrec(ndb1arg,j);
      if(rec_subsumed_query(recx,recy,numunits) ) {
        recset_removerec(ndb1arg,j);
        rec_destroy(recx);
        reccount++;
        total_reccount_deleted++;
      } 
    }

#if defined(DISPLAY_RECCOUNT_DELETED_ALL)
    fprintf(stderr,"fyi (recset_delete_subsumed): record #(%d) saved us %d records\n",i,reccount);
#endif
  }
  
#if defined(DISPLAY_RECCOUNT_DELETED) || defined(DISPLAY_RECCOUNT_DELETED_ALL)
  if(total_reccount_old > 0){
    int new_total = recset_size(ndb1arg) + recset_size(ndb2arg);
    int old_total = total_reccount_old + recset_size(ndb2arg);
    int diff = old_total - new_total;
    float new_percent_of_orig = ((new_total/(total_reccount_old*1.0))*10000)/100;
    float percent_of_orig = ((old_total/(total_reccount_old*1.0))*10000)/100;
    float diff_percent_of_orig = ((diff/(total_reccount_old*1.0))*10000)/100;
    fprintf(stderr,"fyi (recset_delete_subsumed): new total of %d records --- %.2f%% of the original %d records\n",new_total, new_percent_of_orig,total_reccount_old);
    fprintf(stderr,"fyi (recset_delete_subsumed): ");
    if (new_total < old_total) {
      fprintf(stderr,"BETTER THAN ");
    } else {
      if (new_total > old_total) {
        fprintf(stderr,"WORSE THAN ");
      } else {
        fprintf(stderr,"SAME AS ");
      }
    }
    fprintf(stderr,"combined total of %d records (without any removed) --- %.2f%% of the original %d records\n",old_total, percent_of_orig, total_reccount_old);
    if(new_total != old_total)
      fprintf(stderr,"fyi (recset_delete_subsumed): a difference of %d record --- %.2f%% of the original %d records\n",diff, diff_percent_of_orig, total_reccount_old);
    fprintf(stderr,"\n");
  }
#endif
  
#if SANITY
  fprintf(stderr,"recset_delete_subsumed: calling sanity check\n");
  recset_sanitycheck_tree(ndb2arg);
#endif
}


#if 0
/* maintain consistent projected output in both ndb and cnf formats
   (cnf ignores all stars while ndb does not). returns 1 if a record
   with all don't care symbols was destroyed, o.w. 0. assumes no
   duplicates. v.34 (no longer used v.62) */
static int recset_delete_alldontcare_record(Recset * rsetarg)
{
  Rec * rec = rec_create(recset_numunits(rsetarg));
  int rtn = recset_deleterec(rec, rsetarg);
  rec_destroy(rec);
  return rtn;
}
#endif

/* transfers ownership of all the rec's in rsetarg that subsume rarg 
   to a new recset. iterates over all recs starting at the end to
   assist with removals.
*/
void recset_split(Rec * rarg, Recset * rsetarg, Recset * newrsetarg)
{
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  recset_setlength(newrsetarg,recset_length(rsetarg));   /* v.27 */
  
  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    
    if(rec_subsumed_query(rarg,recy,numunits)) {
      recset_removerec(rsetarg,i);       /* before the add v.45 */
      recset_addrec(recy,newrsetarg);    /* add destroys duplicates */      
    }
  }
  
#if SANITY
  fprintf(stderr,"recset_split: calling sanity check\n");
  recset_sanitycheck_tree(newrsetarg);
#endif
}


/* transfers ownership of all the rec's in rsetarg that are subsumed
   by rarg to a new recset. iterates over all recs starting at the end
   to assist with removals. used by cleanup.
*/
void recset_split_subsumed(Rec * rarg, Recset * rsetarg, Recset * newrsetarg)
{
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  recset_setlength(newrsetarg,recset_length(rsetarg));   /* v.27 */
  
  /* recset_tree_clear(rsetarg);   v.45 */
  
  while(i-- > 0) {
    Rec * recx = recset_getrec(rsetarg,i);
    
    if(rec_subsumed_query(recx,rarg,numunits) ) {
      recset_removerec(rsetarg,i);            /* before the add v.45 */
      recset_addrec(recx,newrsetarg);         /* destroys duplicate or redundant (v.59) rec */
    }
  }
#if SANITY
  fprintf(stderr,"recset_split_subsumed: calling sanity check\n");
  recset_sanitycheck_tree(newrsetarg);
#endif
}


/* transfers ownership of all the rec's in rsetarg that are matched
   by rarg to a new recset. iterates over all recs starting at the end
   to assist with removals. (used for near powersets v.38)
*/
void recset_split_match(Rec * rarg, Recset * rsetarg, Recset * newrsetarg)
{
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  recset_setlength(newrsetarg,recset_length(rsetarg));   /* v.27 */
  
  while(i-- > 0) {
    Rec * recx = recset_getrec(rsetarg,i);
    
    if(rec_match_query(recx,rarg,numunits) ) {
      recset_removerec(rsetarg,i);      /* before the add v.45 */
      recset_addrec(recx,newrsetarg);   /* destroys duplicates */

    } 
  }

#if SANITY
  fprintf(stderr,"recset_split_match: calling sanity check\n");
  recset_sanitycheck_tree(newrsetarg);
#endif
}


/* moves recs in from-recset to the to-recset (in reverse order to
   assist with removals); returns number of records added to the
   to-recset (less any duplicates) v.19.1 (like nsql_intersection,
   except without the third output recset, but in reverse order)
*/
int recset_merge(Recset * fromrsetarg, Recset * torsetarg)
{
  int i = recset_size(fromrsetarg);
  int count = 0;

  recset_tree_clear(fromrsetarg); 
  /*  recset_tree_clear(torsetarg);  v.45 uncomment */
  
  while(i-- > 0) {
    Rec * rec = recset_getrec(fromrsetarg,i);
    recset_removerec(fromrsetarg,i);   /* do remove first v.45 */
    count += recset_addrec(rec,torsetarg); /* destroys duplicate recs */
  }
  assert(!recset_size(fromrsetarg));
  return count;
}


void recset_select(Rec * rarg, Recset * ndbarg, Runtime * rtarg)
{
  Recset partialset;
  int len = recset_length(ndbarg);
  
  recset_init(&partialset,len);
  recset_partial_query(rarg,ndbarg,&partialset,rtarg);
  
  if(runtime_getProjOption(rtarg)){  /* only works for O_ne (v.31) */
    recset_save_secondary_projected(rarg,&partialset,rtarg); 
  } else {
    recset_save_secondary(&partialset,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0));
  }
  
  recset_final(&partialset);
  return;
}


/* join ndb1 and ndb2 based on join conditions 1 and 2 and put results in ndb3;
   based on "Towards an Algebra for Negative Databases" (2006) v.27 
   return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. 
*/
int recset_join(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg) 
{
  Perm permorder1, permorder2;
  int plen1, plen2;
  int reclen1, reclen2;
  int binmode = runtime_getBinMode(rtarg);
  int rtn;

  reclen1 = recset_length(ndb1arg);
  if(reclen1 <= 0) return err8;

  reclen2 = recset_length(ndb2arg);
  if(reclen2 <= 0) return err8;

  perm_init(&permorder1,reclen1);
  if(runtime_getOrder1(rtarg)==NULL)
    perm_set_length(&permorder1,0);
  else 
    if (perm_list2perm(&permorder1,runtime_getOrder1(rtarg),binmode) < 0) {
      fprintf(stderr,"ERROR (recset_join): join condition 1 [%s]\n",
              runtime_getOrder1(rtarg));
      perm_final(&permorder1);
      return err11;
    }

  perm_init(&permorder2,reclen2);
  if(runtime_getOrder2(rtarg)==NULL)
    perm_set_length(&permorder2,0);
  else
    if (perm_list2perm(&permorder2,runtime_getOrder2(rtarg),binmode) < 0) {
      fprintf(stderr,"ERROR (recset_join): join condition 2 [%s]\n",
              runtime_getOrder2(rtarg));
      perm_final(&permorder1);
      perm_final(&permorder2);
      return err11;
    }

  plen1 = perm_get_length(&permorder1);
  plen2 = perm_get_length(&permorder2);

  if ( plen1 != plen2 ){
    fprintf(stderr,"ERROR (recset_join): Join conditions are not the same length (%d,%d)\n",
            plen1,plen2);
    perm_final(&permorder1);
    perm_final(&permorder2);
    return err11;
  }

  if (plen1 > reclen2) {
    fprintf(stderr,"ERROR (recset_join): Join conditions are not less than records in NDB2 (%d,%d)\n",
            plen1,reclen2);
    perm_final(&permorder1);
    perm_final(&permorder2);
    return err11;
  }
  
  rtn = nsql_join(ndb1arg,ndb2arg,ndb3arg,&permorder1,&permorder2,rtarg);
  perm_final(&permorder1);
  perm_final(&permorder2);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/*    return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. */
int recset_crossproduct(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg)
{
  int rtn = nsql_crossproduct(ndb1arg,ndb2arg,ndb3arg,rtarg);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/*    return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. */
int recset_intersection(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg) 
{
  int rtn =  nsql_intersection(ndb1arg,ndb2arg,ndb3arg,rtarg);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/*    return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. */
int recset_union(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg)
{
  int rtn = nsql_union(ndb1arg,ndb2arg,ndb3arg,rtarg);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/* return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. v.65 */
int recset_negbinaryunionstar(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg) 
{
  int rtn = nsql_negbinaryunionstar(ndb1arg,ndb2arg,ndb3arg,rtarg);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/* return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. v.55 */
int recset_binaryunion(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg) 
{
  int rtn = nsql_binaryunion(ndb1arg,ndb2arg,ndb3arg,rtarg);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/* return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. v.56.3 */
int recset_difference(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg) 
{
  int rtn = nsql_difference(ndb1arg,ndb2arg,ndb3arg,rtarg);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/* return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. v.72.3 */
int recset_setdiff(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg) 
{
  int rtn = nsql_setdiff(ndb1arg,ndb2arg,ndb3arg,rtarg);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/* return 0 for normal; 1 if resulting ndb3 has zero records; o.w. err code. v.71 */
int recset_relevance(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg) 
{
  int rtn = nsql_relevance(ndb1arg,ndb2arg,ndb3arg,rtarg);
  if (rtn != 0) return rtn;
  return (recset_size(ndb3arg) <= 0);
}


/* deep copy; though record array order should not be assumed, on
   the principle-of-least-surprise, copy maintains order of records
   since none are being removed as of v.51 */
int recset_copy(Recset * fmrsetarg, Recset * torsetarg)
  {
  int sz = recset_size(fmrsetarg);
  int numunits = recset_numunits(fmrsetarg);
  int i = 0;
  int count = 0;

  recset_setlength(torsetarg,recset_length(fmrsetarg));

  while(i < sz) {
    Rec * recy = recset_getrec(fmrsetarg,i);
    Rec * recycopy = rec_create(numunits);
    rec_copy(recy,recycopy,numunits);
    count += recset_addrec(recycopy,torsetarg);
    i++;
  }
  return count;
}


/* deep copy records from the fmsetarg to the torsetarg with only the
   bit positions specified in the perm; this step removes duplicates
   v.30; don't add all *'s if nodcoptarg is set v.62 */
static int recset_perm_copy(Recset * fmrsetarg, Recset * torsetarg, Perm * parg, int nodcoptarg)
  {
  int sz = recset_size(fmrsetarg);
  int plen = perm_get_length(parg);
  int i = sz;
  int count = 0;
  int numunits;

  recset_setlength(torsetarg,plen);
  numunits = recset_numunits(torsetarg);

  while(i-- > 0) {
    Rec * recy = recset_getrec(fmrsetarg,i);
    Rec * recycopy = rec_create(numunits);

    perm_copy_rec(parg,recy,recycopy,numunits);
    if(!nodcoptarg || rec_size(recycopy,numunits) > 0)       /* don't add all *'s v.62 */
      count += recset_addrec(recycopy,torsetarg);
    else
      rec_destroy(recycopy);
  }
  return count;
}


/* appends new copies of all the rec's in rsetarg that are "matched"
   by rarg, then coalesced, into a new recset. iterates over all recs
   starting at the end. for use with -C and a complement ndb (v.66.1)
*/
int recset_partial_copy(Rec * rarg, Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg)
{
  int i = recset_size(rsetarg);
  int numunits = recset_numunits(rsetarg);
  Rec * reca = rec_create(numunits);
  int count = 0;

  while(i-- > 0) {
    Rec * recy = recset_getrec(rsetarg,i);
    if(rec_match_query(recy,rarg,numunits)) {   
      rec_init(reca,numunits);
      rec_coalesce(rarg,recy,reca,numunits);      
      count += append(reca,newrsetarg,rtarg,wo_npg);
    }
  }

#if SANITY
    recset_sanitycheck_tree(newrsetarg);
    fprintf(stdout,"recset_partial_copy: (count is %d)\n",count);
    recset_print(newrsetarg,stdout);
#endif

    rec_destroy(reca);
    return count;
}


/* this is GROSS with the tree being rebuilt for each insert; reverses
   order of the records to accomplish the traversal without a second
   copy of the recset; do NOT set updateflag since this is for
   internal use only v.57 */
void recset_expandrecsize(Recset * rsarg, Runtime * rtarg)
{
  int i = recset_size(rsarg) - 1;
  int minbits = runtime_getMinBits(rtarg);
  int numunits = recset_numunits(rsarg);
  int n = runtime_getN(rtarg);

  if(minbits > recset_length(rsarg)){
    fprintf(stderr,"recset_expandrecsize: minbits (%d) is greater than record length %d, nothing to do\n",
            minbits,recset_length(rsarg)); 
    return;
  }

  fprintf(stdout,"expanding record size to %d minimum bits..\n",minbits);
  PrintBR(rtarg);

  /*  fprintf(stderr,"this is GROSS with the tree being rebuilt for each insert\n"); */

  for(; i >= 0; i--) {
    Rec * recptr = recset_getrec(rsarg,i);
    int l = rec_size(recptr,numunits);
    if( l < minbits ) {
      /*      fprintf(stderr,"call from check_expand, remove rec idx %d, len is %d\n",i,l); */
      /*      recset_tree_clear(rsarg); */
      recset_removerec(rsarg,i);
      insert(recptr,rsarg,rtarg,wo_npg,n);
      rec_destroy(recptr);
    }
  }
  /* recset_setupdateflag(rsarg); do not set the updateflag v.57 */ 
}


typedef struct ndbhdr {
  char mode;
  int len;
} NDBheader;

static int parse_ndbheader(FILE * fdarg, NDBheader * hdrarg)
{
  char m;
  int len = 0;
  if(fscanf(fdarg,"%c%d\n",&m,&len) != 2){
    fprintf(stderr," error: bad ndb header\n");
    return err10;
  }
  if(m != 'a' && m!= 'b'){
    fprintf(stderr," error: bad mode in ndb header\n");
    return err10;
  }
  hdrarg->mode = m;
  hdrarg->len = len;
  return 0;
}


static int recset_build_ndb(Recset * rsarg, Runtime * rtarg, FILE * fdarg)
{
  NDBheader hdr;
  int numunits;
  int rtn = 0;
  int i = 0;
  int reclen = recset_length(rsarg);

  hdr.len = 0;
  hdr.mode = 'x';

  rtn = parse_ndbheader(fdarg,&hdr);
  /*   printf("parse_ndbheader returned %d, reclen is %d\n",hdr.len,reclen); */

  if(rtn != 0) return rtn;              /* signal to use old format */

  if(reclen != 0 && hdr.len != reclen) {  /* we know hdrlen is not zero here */
    fprintf(stderr,"recset_build_ndb: record length from file (%d) is not the same as use input (%d)\n",hdr.len,reclen);
    return err8;
  }

  if(hdr.len < 0 || hdr.len > MAXBITS){
    fprintf(stderr,"recset_build_ndb: record length from file [%d] is out of range 0 to %d\n",
            hdr.len,MAXBITS);
    return err8;
  }
  
  /* allow an ascii file to be read in binary mode v.52 */
  if(hdr.mode == 'a' && runtime_getBinMode(rtarg)) { 
    fprintf(stderr,"fyi: recset_build_ndb: ascii mode file will be processed in binary mode\n");
    /*    return err10; */
  }

  /* binary mode automatically set when file is binary */
  if(hdr.mode == 'b') runtime_setBinMode(rtarg);

  /* all clear, go ahead */
  recset_setlength(rsarg,hdr.len);
  numunits = recset_numunits(rsarg);


  while(1) {
    int len;
    Rec * rec = rec_create(numunits);

    len = rec_read(rec,numunits,fdarg);     /* chix & egg: rec length for numunits? */

    if(len == 0) {  /* EOF */
      rec_destroy(rec);
      break;
    }
    
    if(len != hdr.len){
      fprintf(stderr,"Read bad RNDB record (%d) length (%d), not (%d)\n",
              i,len,hdr.len);
      return err8;
    }
    
    i++;
    recset_addrec(rec,rsarg);
  }
  return 0;
}


static int recset_build_ndb_old(Recset * rsarg, FILE * fdarg)
{
  int i = 0;
  int maxlen = 0;
  int reclen = recset_length(rsarg);
  int numunits = (reclen == 0? NUMUNITS : recset_numunits(rsarg));

  while(1) {
    int len;
    Rec * rec = rec_create(numunits);

    len = rec_read(rec,numunits,fdarg);     /* chix & egg: rec length for numunits? */
    if(len < 0 || len > MAXBITS){
      fprintf(stderr,"Read bad RNDB record (%d) length %d\n",i,len);
      return err8;
    }

    if(len == 0) {  /* EOF */
      rec_destroy(rec);
      break;
    }
    
    if (!maxlen) {    /* set or sanity check the record length here */ 
      maxlen = len;
      if(reclen == 0){ 
        recset_setlength(rsarg,len);
        numunits = recset_numunits(rsarg);
        rec = rec_shrink(rec,numunits);
      } else {                            
        if(reclen != len){
          fprintf(stderr,"Length input is different than length read from ndb file\n");
          return err8;
        }
      }
    }

    if(len != maxlen){
      fprintf(stderr,"Read RNDB record (%d) with length %d, not expected length %d\n",
              i,len,maxlen);
      return err8;
    }

    i++;
    recset_addrec(rec,rsarg);
  }
  return 0;
}


typedef struct cnfhdr {
  int numVars;
  int numClauses;
} CNFheader;

/* return number of records (i.e. clauses), and length
(i.e. variables) from header p cnf #var #cl 
skip comments (lines that begin with a single c)
*/
static int parse_cnfheader(CNFheader * cnfhdrarg, FILE * fdarg)
{
  cnfhdrarg->numVars = 0;
  cnfhdrarg->numClauses = 0;

  while (1) {
    char code;
    int hdr = fscanf(fdarg,"%c",&code);
    if (hdr != 1) {
      fprintf(stderr," error: bad cnf header %d [code is %c]\n ",hdr,code); 
      return err10; 
    }
    if (code == 'c') {
      while (1) {
        int c = fgetc(fdarg);
        if (c < 0) return 0;    /* EOF */
        if (c == '\n') break;
      }
    } else {
      if (code == 'p') {
        int hdr = fscanf(fdarg," cnf %d %d",&(cnfhdrarg->numVars),&(cnfhdrarg->numClauses));
        if (hdr != 2) { 
          fprintf(stderr," error: bad cnf problem header (%d)\n ",hdr); 
          return err10; 
        }
        break;
      }
    }
  }
  return 0;
}


static int recset_build_cnf(Recset * rsarg, FILE * fdarg)
{
  int maxlen, numunits, numrecs, i, reccnt, reclen;
  CNFheader cnfhdr;
  int rtn = 0;

  rtn = parse_cnfheader(&cnfhdr,fdarg);
  if(rtn > 0) return rtn;

  maxlen = cnfhdr.numVars;
  numrecs = cnfhdr.numClauses;

  /*  fprintf(stdout,"recset_build_cnf: numrecs is %d\n",numrecs); */

  if(maxlen <= 0 || maxlen > MAXBITS){
    fprintf(stderr,"Read bad cnf RNDB record length %d\n",maxlen);
    return err8;
  }

  reclen = recset_length(rsarg);
  if(reclen == 0){ 
    recset_setlength(rsarg,maxlen);  
  } else {                            
    if(reclen != maxlen){
      fprintf(stderr,"Length input is different than length read from cnf header\n");
      return err8;
    }
  }

  /* sets numunits value for recs and freq */
  numunits = recset_numunits(rsarg);

  reccnt = 0;
  for(i=0; i < numrecs; i++) { 
    int len;
    Rec * rec = rec_create(numunits);

    len = rec_read_cnf(rec,numunits,fdarg);  /* highest index seen */

    if(len == 0) {         /* EOF */
      rec_destroy(rec);
      fprintf(stderr,"recset_build_cnf: eof after %d records\n",i);
      break;
    }

    if(len > maxlen){
      fprintf(stderr,"Read cnf RNDB record (%d) with index %d, not expected length %d\n",
              i,len,maxlen);
      return err8;
    }
    
    if (!recset_addrec(rec,rsarg)) {
      fprintf(stderr,"record %d is a duplicate\n",i);
    } else {
      reccnt++;
    }
  }
  fprintf(stdout,"recset_build_cnf: read %d records, added %d, header suggested %d clauses\n",i,reccnt,numrecs); 
  return 0;
}


int recset_build(Recset * rsarg, Runtime * rtarg, char * filenamearg, NegFileFormat ffarg)
{
  int maxkey, minkey, mostkey, minbits;
  int rtn = 0;
  Freq * recszfreq;
  FILE * fd = fopen(filenamearg,"r");
  
  if (fd==NULL) { 
    fprintf(stderr,"ERROR (recset_build): Unable to open file [%s].\n",filenamearg);
    return err9;                     /* v.35 */
  }

  if (ffarg == cnfformat)
    rtn = recset_build_cnf(rsarg,fd);
  else {
    rtn = recset_build_ndb(rsarg,rtarg,fd);
    if(rtn == err10){     /* try old ndb format */
      fclose(fd);
      fprintf(stderr,"recset_build: trying deprecated ndb format\n");
      fd = fopen(filenamearg,"r");
      if (fd==NULL) { 
        fprintf(stderr,"ERROR (recset_build): Unable to open file [%s].\n",filenamearg);
        return err9;                     /* v.35 */
      }
      rtn = recset_build_ndb_old(rsarg,fd);
    }
  }
  fclose(fd);
  
  if(rtn != 0) return rtn;

  if(recset_size(rsarg) < 0) {      /* allow no records v.39 */
    fprintf(stderr,"Bad RNDB size %d\n",recset_size(rsarg));
    return err6;
  }

  /* setup recsize frequency after record length is set */
  recset_update_recsize_frequency(rsarg);
  recszfreq = recset_getrecsizefrequency(rsarg);
  
  maxkey = freq_max(recszfreq);
  minkey = freq_min(recszfreq);
  mostkey = freq_most(recszfreq);

  /* use record size rather than key to avoid confusion v.35; output recset size v.59.5 */
  fprintf(stdout,"read %d records",recset_size(rsarg));
  PrintBR(rtarg);
  fprintf(stdout,"max record size found to be %d, min record size %d, and most frequent %d",
          maxkey,minkey,mostkey);
  PrintBR(rtarg);
  /*  freq_print(recszfreq,stderr); */

  minbits = runtime_getMinBits(rtarg);

  /* update when zero, regardless of -m or -k or neither. v.61.2 */

  if(minbits==0) {   
    minbits = freq_accumpercent(recszfreq,recset_size(rsarg),runtime_getPercent(rtarg)); 
                                                    /* v.14 minkey
                                                       returned if
                                                       percent
                                                       unspecified */
    runtime_setMinBits(rtarg,minbits);
    fprintf(stdout,"set record size to %d minimum bits\n",minbits);
    PrintBR(rtarg);
  } else {  /* make sure minbits is not greater than record length (known now) v.57.5 */
    int len = recset_length(rsarg);
    if(minbits > len) {
      fprintf(stdout,"warning: reset minimum record size to record length (%d), instead of (%d)\n",len, minbits);
      runtime_setMinBits(rtarg,len);
      minbits = len;
    }
  }

#if EXPAND_INPUT_NDB || ! MANAGED_GROWTH
  {
    int cmd = runtime_getCommand(rtarg);
    /* avoid expansion for queries (changed test for Q instead of lock in v.27) */
    /* also true for 'M' query v.45; completely disable (v.60); 
     enable flag, but not for unnegate (-u) (v.61.4)  */
    if(minkey < minbits && cmd != 'Q' && cmd != 'M' && cmd != 'U' ) {   
      recset_expandrecsize(rsarg,rtarg);
    }
  }
#endif  

  /* before expansion only if insert uses npg which it doesn't; better
     here for more accuracy after expand v.14 */
  recset_update_bit_frequency(rsarg); 

#if COUNT_NODES
  rstree_node_stats();
#endif
  return 0;
}


static int recset_checkbitfreq(Recset * rsarg, Freq * bitfreqarg)
{
  Freq * dbfreqs = recset_getbitfrequency(rsarg);
  int reclen = recset_length(rsarg);
  int rej = 0;
  int i;

  if (dbfreqs == NULL) 
    return 0;

  for(i = 0; i < reclen; i++){
    if(freq_get(dbfreqs,i) != freq_get(bitfreqarg,i)){
      fprintf(stderr,"checkbitfreq: bit %d does not compare %d,%d\n",
              i,freq_get(dbfreqs,i),freq_get(bitfreqarg,i));
      rej++;
    }
  }
  return rej;
}


/* created in v.27, and modified in v.30 to take the record length as
   first arg */
void recset_printndbheader(int lenarg, Runtime * rtarg, FILE * fdarg)
{
  fprintf(fdarg,"%c%d\n",
          runtime_convertBinMode(runtime_getBinMode(rtarg)),
          lenarg
          );
  return;
}


static void print_cnfheader(int lenarg, int ndbszarg, FILE * fdarg)
{
  fprintf(fdarg,"c cnf #vars #clauses\nc #vars is record length, #clauses is number of records\n");
  fprintf(fdarg,"c guideline density ratio is around 4.3: <%.2f>\n",ndbszarg/(lenarg * 1.0));
  fprintf(fdarg,"p cnf %d %d\n",lenarg,ndbszarg);
}


/* save in cnf format, not ndb (v.22); records printed in recset array order */
static void recset_cnf_save_and_checkbitfreq(Perm * dcpermarg, Recset * rsarg, Runtime * rtarg, char * filenamearg, Freq * bitfreqsarg)
{
  int sz;
  int len;
  int i = 0;
  int numunits = recset_numunits(rsarg);
  FILE * fd  = fopen(filenamearg,"w");       /* moved down v.35 */

  if( fd == NULL ) {
    fprintf(stderr,"file open error on recset cnf save and checkbitfreq\n"); 
    exit(err14);
  }

  sz = recset_size(rsarg);
  len = perm_get_length(dcpermarg);
  Print(rtarg,"Length of new record is %d\n",len);

  if(sz > 0 && recset_min_recordsize(rsarg) == 0) {
    int extra = (len << 1);
    fprintf(fd,"c substituted %d clauses for an ALL * record\n",extra);
    print_cnfheader(len,sz+extra-1,fd);
  } else {
    print_cnfheader(len,sz,fd);
  }

  while(i < sz) {
    Rec * rec = recset_getrec(rsarg,i);
    if (rec_size(rec,numunits) > 0)         /* v.31 cnf doesn't handle all dc's */
      perm_print_rec(dcpermarg,rec,cnfformat,fd);
    else
      fprintf(stderr,"recset_cnf_save_and_checkbit: skipped all * record (%d)\n",i);
    recset_recbitfreq(rec,bitfreqsarg,len);    
    i++;
  }
  fclose(fd);
  recset_clearupdateflag(rsarg);
  Print(rtarg,"Saved %d records in %s and checking bit frequencies\n",sz,filenamearg);
}


/* records printed in recset array order */
static void recset_ndb_save_and_checkbitfreq(Recset * rsarg, Runtime * rtarg, char * filenamearg, Freq * bitfreqsarg)
{
  int sz, len, i;
  FILE * fd;

  assert(filenamearg);
  fd = fopen(filenamearg,"w");
  
  if( fd == NULL ) {
    fprintf(stderr,"file open error on recset ndb save and checkbitfreq %s\n",filenamearg); 
    exit(err14);
  }

  sz = recset_size(rsarg);
  len = recset_length(rsarg);
  i = 0;

  recset_printndbheader(len,rtarg,fd);

  while(i < sz) {
    Rec * rec = recset_getrec(rsarg,i);
    rec_print(rec,len,fd);
    recset_recbitfreq(rec,bitfreqsarg,len);    
    i++;
  }

  fclose(fd);
  recset_clearupdateflag(rsarg);
  Print(rtarg,"Saved %d records in %s and checking bit frequencies\n",sz,filenamearg);
}


int recset_save_and_checkbitfreq(Recset * rsarg, Runtime * rtarg, char * filenamearg)
{
  int checkrtn;
  int len = recset_length(rsarg);
  Freq * bitfreqs = freq_create(recset_numunits(rsarg));
  
  freq_init(bitfreqs,len);
  

  if (runtime_getFormat(rtarg) == cnfformat) {
    Perm perm;
    perm_init(&perm,len);
    recset_cnf_save_and_checkbitfreq(&perm,rsarg,rtarg,filenamearg,bitfreqs);
    perm_final(&perm);
  } else {                                             /* ndb format */
    recset_ndb_save_and_checkbitfreq(rsarg,rtarg,filenamearg,bitfreqs);
  }
  
  checkrtn = recset_checkbitfreq(rsarg,bitfreqs);
  freq_destroy(bitfreqs);

  recset_save_secondary(rsarg,rtarg,0);       /* secondary output */

  return checkrtn;
}


static void recset_tree_print(Recset * rsarg, FILE * fdarg);

/* save recset in new ndb format; records printed in recset array
   order; or sorted tree order if compile time flag set (v.60.1) */
static void recset_ndb_save(Recset * rsarg, Runtime * rtarg, char * filenamearg)
{
  FILE * fd;
  int sz, len, i;

  if(filenamearg==NULL) return;

  fd = fopen(filenamearg,"w");
  if( fd == NULL ) {
    fprintf(stderr,"file open error on recset ndb save %s\n", filenamearg); 
    exit(err14);
  }

  sz = recset_size(rsarg);
  len = recset_length(rsarg);
  i = 0;
    
  recset_printndbheader(len,rtarg,fd);

#if SAVE_SORTED_NDB
  recset_tree_print(rsarg,fd);
#else
  while(i < sz) {
    Rec * rec = recset_getrec(rsarg,i);
    rec_print(rec,len,fd);
    i++;
  }
#endif

  fclose(fd);
  recset_clearupdateflag(rsarg);
}


/* save in cnf format, not ndb (v.22); records printed in recset array order */
static void recset_cnf_save(Perm * dcpermarg, Recset * rsarg, char * filenamearg)
{
  FILE * fd;
  int i, sz, len;

  if(filenamearg==NULL) return;

  fd = fopen(filenamearg,"w");
  if( fd == NULL ) {
    fprintf(stderr,"file open error on recset cnf save\n"); 
    exit(err14);
  }

  sz = recset_size(rsarg);
  len = perm_get_length(dcpermarg);

  if(sz > 0 && recset_min_recordsize(rsarg) == 0) {
    int extra = (len << 1);
    fprintf(fd,"c substituted %d clauses for an ALL * record\n",extra);
    print_cnfheader(len,sz+extra-1,fd);
  } else {
    print_cnfheader(len,sz,fd);
  }
  
  i = 0;
  while(i < sz) {
    Rec * rec = recset_getrec(rsarg,i);    
    perm_print_rec(dcpermarg,rec,cnfformat,fd);
    i++;
  }
  fclose(fd);
  recset_clearupdateflag(rsarg);
}


static void recset_print_bitdistribution(Recset * rsarg, Runtime * rtarg)
{
  FILE * fd;
  char * testdir;
  testdir = runtime_getTestDataDirectory(rtarg);

  if(testdir == NULL) {
    fd = fopen("./distrib7-RNDB-0.dat","w");
  }  else {
    char * fname = new_string_concat(testdir,"/distrib7-RNDB-0.dat");
    if( fname == NULL) {
      fprintf(stderr,"filename creation error for print bit distribution data\n"); 
      exit(err12);
    }
    fd = fopen(fname,"w");
    free(fname);
  }

  if( fd == NULL ) {
    fprintf(stderr,"file open error on print bit distribution\n"); 
    exit(err14);
  }

  fprintf(fd,"set terminal pbm small color\n");
  fprintf(fd,"set ylabel \"Number of Specified Appearances in Negative Records\\n(not the don't cares) \"\n");
  fprintf(fd,"set xlabel \"Bit Location in Negative Record\"\n");
  fprintf(fd,"set title \"Distribution of Specified Bits Across Negative Database %d-bit records\"\n",
	  recset_length(rsarg));
  fprintf(fd,"plot '-' using 1:2 title \"NDB\" with linespoints lt 3\n");
  fprintf(fd,"##bit count percent\n");
  recset_printbitfreq(rsarg,fd);
  fclose(fd);
}


void recset_save_secondary(Recset * rsarg, Runtime * rtarg, int cmprsarg)
{
  char *filename;

  if(cmprsarg>0){                             /* v.68 */
    compress(rsarg,rtarg,cmprsarg); 
    cleanup_plus_option(rsarg,rtarg);
  }

  filename = runtime_getNameOutputFile(rtarg);
  if(filename != NULL) {
    NegFileFormat off = runtime_getFormatOutputFile(rtarg);
    if ( off == cnfformat) {
      Perm perm;
      perm_init(&perm,recset_length(rsarg));
      recset_cnf_save(&perm,rsarg,filename);
      perm_final(&perm);
    } else {
      recset_ndb_save(rsarg,rtarg,filename);
    }
    Print(rtarg,"Saved %d records in <%s>, record length [%d] bits",
          recset_size(rsarg),filename,recset_length(rsarg));
  }
  return;
}


/* eliminate the specified record positions in the input from the
   output; create a temporary recset to avoid saving duplicates; note:
   this only works for NegTheta O_ne, but is allowed for testing
   purposes.  v.31 */ 
void recset_save_secondary_projected(Rec * rarg, Recset * rsarg, Runtime * rtarg)
{
  Recset projrset;
  Perm dcperm;

  if (runtime_getNegTheta(rtarg) != O_ne) {
    fprintf(stderr,"WARNING: Project option -P only works for -Q theta of \"eq\" at this time. Consider retrying your query without the -P option for proper output.\n");
  }

  perm_init(&dcperm,recset_length(rsarg));
  perm_dontcares(&dcperm,rarg,recset_length(rsarg));
  
  recset_init(&projrset,perm_get_length(&dcperm));
  recset_perm_copy(rsarg,&projrset,&dcperm,1);  /* 1=skips all dont care rec v.62 */

  recset_save_secondary(&projrset,rtarg,0);
  recset_final(&projrset);
  perm_final(&dcperm);
}


void recset_save(Recset * rsarg, Runtime * rtarg, char * filenamearg)
{
  NegFileFormat ff = runtime_getFormat(rtarg);
  if ( ff == cnfformat) {
    Perm perm;
    perm_init(&perm,recset_length(rsarg));
    recset_cnf_save(&perm,rsarg,filenamearg);
    perm_final(&perm);
  } else {                                         /* always new ndb format */
    recset_ndb_save(rsarg,rtarg,filenamearg);
  }

  Print(rtarg,"Saved %d records in %s",recset_size(rsarg),filenamearg);

#if COUNT_NODES 
  rstree_node_stats();
#endif
  
  /* also save updated bit distribution v.17 (when not null v.19) */
  recset_print_bitdistribution(rsarg,rtarg);

  recset_save_secondary(rsarg,rtarg,0);                /* secondary output */
}


void recset_tree_save(Recset * rsarg, char * filenamearg)
{
  if (rsarg->root != NULL)
    rstree_save(rsarg->root,recset_length(rsarg),filenamearg);
}


/* v.59.12 prints a sorted recset */
static void recset_tree_print(Recset * rsarg, FILE * fdarg)
{
  if (rsarg->root == NULL)
    recset_tree_build(rsarg);
  rstree_print(rsarg->root,recset_length(rsarg),fdarg);
}


/* quick and dirty output for testing purposes v.45; records printed
   in recset array order v.51 */
void recset_print(Recset * rsetarg, FILE * fdarg)
{
  int sz = recset_size(rsetarg);
  int reclen = recset_length(rsetarg);
  int i = 0;
  while(i < sz) {
    rec_print(recset_getrec(rsetarg,i),reclen,fdarg);
    i++;
  }
}


/* based on Davis-Putnam-Logemann-Loveland algorithm v.45 return 0 if
   recset is empty (has no positive solutions); o.w. 1 or error
   errneg2 if recset max recsize is too big; solution is returned in
   the second argument; recset arg is unchanged */
int recset_notemptyp_solution(Recset * rsetarg, Rec * solrecarg, EasyReduceMode reducemodearg, int ksatarg)
{
  int recsz, rtn, flag, reclen, numunits;
  Recset rsetcopy;

  /* 2 fencepost checks (no negative records, or all *'s negative record) v.51 */
  if(recset_size(rsetarg) == 0) 
    return 1;     /* no neg records means DB is completely full */

  if(recset_min_recordsize(rsetarg) == 0) /* all *'s means DB is empty */
    return 0;

  reclen = recset_length(rsetarg);
  numunits = recset_numunits(rsetarg);

  recset_init(&rsetcopy,reclen);
  recset_copy(rsetarg,&rsetcopy);

  /* reduce returns chase flag = 0 if bad decision; -1 if completely
     done; 1 for normal; or errneg2 if not 3SAT */
  flag = easy_reduce_sat(&rsetcopy,solrecarg,reducemodearg,ksatarg);
  
  recsz = rec_size(solrecarg,numunits);
  rtn = (flag ==-1 && recsz!=0  ? 1 : 0);  
  
#if 0
  fprintf(stderr,"recset_notemptyp_solution: reduced recset: \n");
  recset_print(&rsetcopy,stderr);
#endif 

  recset_final(&rsetcopy);
  return (flag == errneg2 ? flag : rtn);
}


/* based on Davis-Putnam-Logemann-Loveland algorithm v.45 return 0 if
   recset is empty (has no positive solutions); o.w. 1 or errneg2 if
   ndb is not 3SAT (2SAT preferred for polynomial time); recset arg is
   unchanged; (use recset_emptyp_solution directly to own the
   solutions) */
int recset_notemptyp(Recset * rsetarg, Runtime * rtarg)
{
  int rtn, numunits;
  Rec * solutionrec;

  numunits = recset_numunits(rsetarg);
  solutionrec = rec_create(numunits);

  rtn = recset_notemptyp_solution(rsetarg,solutionrec,runtime_getReduceMode(rtarg),EASYSAT);

#if 1
  if (rtn == errneg2) {
    fprintf(stdout,"recset_notemptyp: WARNING!! NDB needs a cleanup/morph before proceeding\n");
  } else {
    fprintf(stderr,"\nrecset_notemptyp: returning %d, found solutions: \n",rtn);
    rec_print(solutionrec,recset_length(rsetarg),stderr);
    fprintf(stderr,"\n");
  }
#endif

  rec_destroy(solutionrec);
  return rtn;
}


/* return total hamming_distance between two neighbors v.61 */
static int recset_hamming_distance_total_all(Recset * rsetarg, Perm * parg)
{
  int numunits = recset_numunits(rsetarg);
  int hdtotal = 0;
  int i = 0;
  int sz = recset_size(rsetarg);

  assert(sz == perm_get_length(parg));
  sz--;
  while(i++ < sz){
    hdtotal += rec_hammingdistance(recset_getrec(rsetarg,perm_get(parg,i)),recset_getrec(rsetarg,perm_get(parg,i-1)),numunits);
  }
  return hdtotal;
}


#if UNNEGATE_HAMMDIS_RANDOMHC || UNNEGATE_HAMMDIS_DETERMINISTIC
/* return total hamming_distance between two neighbors v.61 */
static int recset_hamming_distance_neighbors(int pidxarg, Recset * rsetarg, Perm * parg)
{
  int numunits = recset_numunits(rsetarg);
  int hdtotal;

  /* handle special boundary cases first at the ends of the perm */
  if(pidxarg == 0){
    return rec_hammingdistance(recset_getrec(rsetarg,perm_get(parg,0)),recset_getrec(rsetarg,perm_get(parg,1)),numunits);
  }

  if(pidxarg >= perm_get_length(parg) - 1){
    return rec_hammingdistance(recset_getrec(rsetarg,perm_get(parg,pidxarg)),recset_getrec(rsetarg,perm_get(parg,pidxarg-1)),numunits);
  }

  /* total up both neighbors here */
  hdtotal = rec_hammingdistance(recset_getrec(rsetarg,perm_get(parg,pidxarg)),recset_getrec(rsetarg,perm_get(parg,pidxarg-1)),numunits);
  hdtotal += rec_hammingdistance(recset_getrec(rsetarg,perm_get(parg,pidxarg)),recset_getrec(rsetarg,perm_get(parg,pidxarg+1)),numunits);
  return hdtotal;
}
#endif


#if UNNEGATE_HAMMDIS_RANDOMHC
/* input: is a perm, initialized to record length order; rsetarg is to
   be "ordered" in perm, but unchanged in array. uses random
   hill-climbing max_tries to order recs for complement v.61 */
static void recset_hillclimb_hammingdistance(Recset * rsetarg, Perm * parg)
{
  int n = recset_size(rsetarg);
  int MAX_TRIES = n * n;   /* might need some limit checks */
  int tries = 0;
  
  assert(parg!=NULL);
 
   if(n <= 1) {
      return;
   }
  
  while(tries < MAX_TRIES) {
    Perm prand;
    int hda, hdanew;
    int hdb, hdbnew;
    int a,b;

    /* get 2 distinct parg indexes at random */
    perm_init(&prand,n);
    perm_randomize(&prand,2);

    a = perm_get(&prand,0);
    b = perm_get(&prand,1);
    perm_final(&prand);

    hda = recset_hamming_distance_neighbors(a,rsetarg,parg);
    hdb = recset_hamming_distance_neighbors(b,rsetarg,parg);

    perm_swap(parg,a,b);

    hdanew = recset_hamming_distance_neighbors(a,rsetarg,parg);
    hdbnew = recset_hamming_distance_neighbors(b,rsetarg,parg);

    /* bigger HD, smaller complement? v.61.1 */
    if(hdanew + hdbnew <= hda + hdb) {      /* no improvement, switch back */
      perm_swap(parg,a,b);
    }
    
    tries++;
  }  /* end while loop */
  
  return;
}
#endif


#if UNNEGATE_HAMMDIS_DETERMINISTIC
/* input: is a perm, initialized to record length order; rsetarg is to
   be "ordered" in perm, but unchanged in array. use next-descent
   hill-climbing to order recs for complement v.61 */
static void recset_order_hammingdistance(Recset * rsetarg, Perm * parg)
{
  int n = recset_size(rsetarg);
  int m = n - 1;
  int i;
  
  assert(parg!=NULL);
 
   if(n <= 1) {
      return;
   }
  
   for(i=0 ; i < m; i++){
     int j;

     for(j = i+1; j < n ; j++) {
       int hda, hdanew;
       int hdb, hdbnew;

       hda = recset_hamming_distance_neighbors(i,rsetarg,parg);
       hdb = recset_hamming_distance_neighbors(j,rsetarg,parg);

       perm_swap(parg,i,j);

       hdanew = recset_hamming_distance_neighbors(i,rsetarg,parg);
       hdbnew = recset_hamming_distance_neighbors(j,rsetarg,parg);

       /* bigger HD, smaller complement? v.61.1 */
       if(hdanew + hdbnew <= hda + hdb) {      /* no improvement, switch back */
         perm_swap(parg,i,j);
       }
     }  /* end j for loop */
   }  /* end i for loop */
  
  return;
}
#endif


/* based on Eric Trias' idea to "add" each negative record to a new
   empty recset in order to reveal the positive solutions, possibly
   with dontcare's; it's exponential because delete is
   exponential. returns number of recs in new recset. v.56 compress to
   cmpbitsarg minimum bits when gt 0, o.w. no compression (for
   compare) v.57.5; remove compression here v.60; in increasing
   hamming distance order v.61 */
int recset_complement(Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg)
{
  Perm hdperm;
  int newhdtot;
  int i = recset_size(rsetarg);
  int count = 0;
  int lasthdtot = 0;
  int MAX_TRIES = 1000;
  Negpattgen npg = with_npg;           /* v.63, the new default is with npg */

#if NPG_NONE_UNNEGATE
  npg = wo_npg;                  
#endif

#if 0
  fprintf(stdout, "RECSET COMPLEMENT INPUT has %d recs, %d len\n",i,recset_length(rsetarg));
  /*  recset_print(rsetarg,stdout); */
#endif
  
  empty_ndb_create_basic(newrsetarg,recset_length(rsetarg));      /* init to a single all *'s record */

  perm_init(&hdperm,i);

  lasthdtot = recset_hamming_distance_total_all(rsetarg,&hdperm);
  fprintf(stdout,"RECSET INITIAL HAMMING TOTALS (%d)\n", lasthdtot);
  newhdtot = lasthdtot;  
  lasthdtot++;              /* get started */

  while(newhdtot < lasthdtot && count < MAX_TRIES) {
    lasthdtot = newhdtot;

#if UNNEGATE_HAMMDIS_DETERMINISTIC
    recset_order_hammingdistance(rsetarg,&hdperm);
#elif UNNEGATE_HAMMDIS_RANDOMHC
    recset_hillclimb_hammingdistance(rsetarg,&hdperm);
#endif

    newhdtot = recset_hamming_distance_total_all(rsetarg,&hdperm);
    count++;
    fprintf(stdout,"RECSET POST# %d HAMMING TOTALS (%d)\n", count,newhdtot);
  }

  count = 0;
  while(count < i) {                                             /* changed direction v.61 */
    Rec * recx = recset_getrec(rsetarg,perm_get(&hdperm,count));
#if 0
    fprintf(stdout,"recset_complement: adding rec %d: (index %d) [hd=%d] ",count, perm_get(&hdperm,count),rec_hammingdistance(recx,(count==0? recx: recset_getrec(rsetarg,perm_get(&hdperm,count-1))),recset_numunits(rsetarg)));
    rec_print(recx,recset_length(rsetarg),stdout);
#endif

    offline_add_record(recx,newrsetarg,rtarg,npg); /* doesn't change recx ownership -sweet */

#if 0
  fprintf(stdout, "recset_complement: after offline_add, recset size is %d\n",recset_size(newrsetarg));
#endif

    count++;
  } 
  
  count = recset_size(newrsetarg);
  
#if 0
  fprintf(stdout, "RECSET COMPLEMENT: %d recs\n",count);
  /* recset_print(newrsetarg,stdout); */
#endif

  perm_final(&hdperm);
  return count;
}


/* compares two recsets by looking for records that are exactly the
   same, and deleting them from the second recset; returns 0 if
   same. v.56 */
int recset_compare_destructively(Recset * rset1arg, Recset * rset2arg, Runtime * rtarg){
  int sz = recset_size(rset1arg);
  int i;

  if(sz != recset_size(rset2arg)) {
    return 1;       /* can't be the same */
  }

  i = 0;
  while(i < sz){
    Rec * recq = recset_getrec(rset1arg,i);
    if (recset_deleterec(recq,rset2arg) == 0) {  /* not found */
      break; 
    }
    i++;
  }  
  return recset_size(rset2arg);
}


void test_recset() 
{
  Recset * rsetptr;
  Rec * rec1;
  Rec * rec2;
  Rec * rec3;
  int n, numunits;
  int len = 3;
  int q;

  fprintf(stderr,"starting test_recset..\n");

  numunits = rec_calcnumunits(len);

  rsetptr = recset_create();
  recset_setlength(rsetptr,len);

  rec1 = rec_create(numunits);
  rec2 = rec_create(numunits);
  rec3 = rec_create(numunits);

  rec_setbit(rec1,1,1,numunits);
  if (recset_size(rsetptr) || rstree_size(rsetptr->root) || recset_query(rsetptr,rec1)){   /* query against an empty database */
    exit(10);
  }

  recset_addrec(rec1,rsetptr);
  q = recset_query(rsetptr,rec1);  /* query again */

  if (!recset_size(rsetptr) || ! rstree_size(rsetptr->root) || !q){   
    exit(11);
  }

  /* added v.42 */
  if(q != recset_query_slow(rsetptr,rec1)){
    fprintf(stderr,"test_recset: slow query does not match tree query\n");
    exit(9);
  }
     
  /* added v.42 */
  if(q != recset_query_match(rsetptr,rec1)){
    fprintf(stderr,"test_recset: match query does not match default query\n");
    exit(11);
  }

  n = recset_size(rsetptr);
  rec_setbit(rec2,1,1,numunits);
  if (!recset_query(rsetptr,rec2)){
    exit(12);
  }

  recset_addrec(rec2,rsetptr);  /* duplicate don't add, free */
  if(recset_size(rsetptr) != n) {
    fprintf(stderr,"test_recset: recset size is %d,%d not %d\n",
            recset_size(rsetptr),recset_tree_size(rsetptr),n);
    exit(13);
  }

  rec_setbit(rec3,2,1,numunits);      /* already existing */
  rec_setbit(rec3,2,0,numunits);
  recset_addrec(rec3,rsetptr);

  if(recset_size(rsetptr) == n) {
    fprintf(stderr,"test_recset: recset size is %d,%d not %d\n",
            recset_size(rsetptr),recset_tree_size(rsetptr),n);
    exit(15);
  }

  recset_destroy(rsetptr);
  fprintf(stderr,"completed recset test 1\n");
}


/* add rec1 to recset1; rec2 is the same as rec1; split from recset1
   all the records that "subsume" rec2 (all of them) to recset2; verify
   the size of the recset2 is the same as recset1 before the split,
   and that the size of recset1 after the split is zero. */
void test_recset_split() 
{
  int n;
  int len = 24;
  int numunits = rec_calcnumunits(len);
  Recset * rsetptr = recset_create();
  Recset * rsetptr2 = recset_create();
  Rec * rec1 = rec_create(numunits);
  Rec * rec2 = rec_create(numunits);


  fprintf(stderr,"starting test_recset_split..\n");

  recset_setlength(rsetptr,len);

  rec_setbit(rec1,1,1,numunits);
  if (recset_size(rsetptr) || recset_query(rsetptr,rec1)){   /* query against empty database */
    exit(10);
  }

  recset_addrec(rec1,rsetptr);
  if (!recset_size(rsetptr) || !recset_query(rsetptr,rec1)){   /* query again */
    exit(11);
  }

  n = recset_size(rsetptr);
  rec_setbit(rec2,1,1,numunits);
  if (!recset_query(rsetptr,rec2)){
    exit(12);
  }

  if(!rec_equal(rec1,rec2,numunits)) {
    exit(16);
  }

  recset_split_subsumed(rec2,rsetptr,rsetptr2);

  if(recset_size(rsetptr2) != n || rstree_size(rsetptr2->root) != n) {
    exit(-17);
  }

  if(recset_size(rsetptr) != 0 || rstree_size(rsetptr->root) != 0) {
    exit(-18);
  }

  recset_destroy(rsetptr);
  recset_destroy(rsetptr2);
  rec_destroy(rec2);

  fprintf(stderr,"completed recset split test\n");
}


/* add rec1 to recset1; rec2 is the same as rec1; split from recset1
   all the records that "match" rec2 (all of them) to recset2; verify
   the size of the recset2 is the same as recset1 before the split,
   and that the size of recset1 after the split is zero.  */
void test_recset_split_match() 
{
  int n;
  int len = 24;
  int numunits = rec_calcnumunits(len);
  Recset * rsetptr = recset_create();
  Recset * rsetptr2 = recset_create();
  Rec * rec1 = rec_create(numunits);
  Rec * rec2 = rec_create(numunits);


  fprintf(stderr,"starting test_recset_split_match..\n");

  recset_setlength(rsetptr,len);

  rec_setbit(rec1,1,1,numunits);
  
  if(recset_query_match(rsetptr,rec1) != recset_query_match_slow(rsetptr,rec1))
    exit(1);
  
  if (recset_size(rsetptr) || recset_query_match(rsetptr,rec1)){   /* query against empty database */
    exit(10);
  }

  recset_addrec(rec1,rsetptr);
  if (!recset_size(rsetptr) || !recset_query_match(rsetptr,rec1)){   /* query again */
    exit(11);
  }

  n = recset_size(rsetptr);
  rec_setbit(rec2,1,1,numunits);
  if (!recset_query_match(rsetptr,rec2)){
    exit(12);
  }

  if(!rec_equal(rec1,rec2,numunits)) {
    exit(16);
  }

  recset_split_match(rec2,rsetptr,rsetptr2);

  if(recset_size(rsetptr2) != n || rstree_size(rsetptr2->root) != n) {
    exit(-17);
  }

  if(recset_size(rsetptr) != 0 || rstree_size(rsetptr->root) != 0) {
    exit(-18);
  }

  recset_destroy(rsetptr);
  recset_destroy(rsetptr2);
  rec_destroy(rec2);

  fprintf(stderr,"completed recset split match test\n");
}
