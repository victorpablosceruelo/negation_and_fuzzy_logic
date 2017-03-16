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
 * Filename      : nsql.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Provides Negative SQL functions
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Sun May 14 15:16:41 2006 
 * Updated       : Mon Apr 14 12:08:04 2008 
 *
 * Comments      : based on the Fernando Esponda and Eric Trias 
 *                 Negative Database Algebra
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include "runtime.h"
#include "recset.h"
#include "negdb.h"
#include "perm.h"
#include "error.h"

/* NSQL_NPG defined as wo_npg since not specified in spec; seems
   faster than with_npg; NPG_NONE compile flag effects ALL NPG use,
   not just nsql functions v.60.1; NPG_NONE option removed and option
   to change NSQL_NPG added (default is still wo_npg) v.63 */

#if NPG_USE_NSQL_ALL
#define NSQL_NPG with_npg
#else
#define NSQL_NPG wo_npg
#endif

typedef enum grls { GREATR,LESSR } GRLS; 

static int nsql_thetaselect_eq(Rec * rarg, Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg);
static int nsql_thetaselect_ne(Rec * rarg, Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg);
static int nsql_thetaselect_other(Rec * rarg, Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg);


int nsql_thetaselect(Rec * rarg, Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg)
{
  int rtn = 0;
  int ntheta = runtime_getNegTheta(rtarg);
  switch (ntheta) {
  case O_ne:
    rtn = nsql_thetaselect_ne(rarg,rsetarg,newrsetarg,rtarg);
    break;
  case O_eq:
    rtn = nsql_thetaselect_eq(rarg,rsetarg,newrsetarg,rtarg);
    break;
  default:
    rtn = nsql_thetaselect_other(rarg,rsetarg,newrsetarg,rtarg);
  };
  
  if(rtn > 0) {
    compress(newrsetarg,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0));
  }
  
  return rtn;
}


/* add a copy of the complement of each specified bit of rarg to the
   new recset so that it maintains the original select criterion v.23,
   and one bit per record for queries with more than one bit to work v.24 */
/* changed to use append, removed Projoption distinction, reuse one
   rec since append handles the copy v.59 */
static int nsql_thetaselect_ne(Rec * rarg, Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg)
{
  Perm ndcperm;
  int i;
  int plen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  Rec * reccomplement = rec_create(numunits);

  perm_init(&ndcperm,plen);
  plen = perm_notdontcares(&ndcperm,rarg,plen);   /* flip one bit at a time v.23 */

  i = 0;
  while(i < plen){
    int idx = perm_get(&ndcperm,i);
    int val = rec_getbit(rarg,idx);
    rec_init(reccomplement,numunits);
    rec_setbit(reccomplement,idx,val^1,numunits);
    append(reccomplement,newrsetarg,rtarg,NSQL_NPG);
    i++;
  }
  rec_destroy(reccomplement);
  perm_final(&ndcperm);
  return plen;
}


static int nsql_thetaselect_eq(Rec * rarg, Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg) { 
  append(rarg,newrsetarg,rtarg,NSQL_NPG);
  return 1;
}


static int nsql_thetaselect_other(Rec * varg, Recset * rsetarg, Recset * newrsetarg, Runtime * rtarg) 
{ 
  Perm ndcperm;
  int i;
  int plen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  NegTheta ntheta = runtime_getNegTheta(rtarg);
  GRLS grls = ((ntheta == O_gt || ntheta == O_ge) ? GREATR : LESSR); 
  Rec * recx = rec_create(numunits);
#if USE_INSERT_NSQL_ALL
  int extrabits = runtime_getN(rtarg);
#endif

  perm_init(&ndcperm,plen);
  plen = perm_notdontcares(&ndcperm,varg,plen); /* assumed to be in increasing order */
  /*  fprintf(stderr,"nsql_theta_select_other: not don't cares perm size is %d\n",plen); */
  i = 0;
  while(i < plen){
    int idx = perm_get(&ndcperm,i);
    int val = rec_getbit(varg,idx);
    if(val == grls ) {     /* foreach bit i in v == grls, create a rec */
      int j;
      rec_init(recx,numunits);
      rec_setbit(recx,idx,val^1,numunits);        /* with i bit set to ~ */ 
      /*      fprintf(stdout,"nsql_theta_select_other: creating rec %d for index %d, new val %d\n",i,idx,val^1); */
      j=i-1;
      while(j >= 0) {               /* and all bits "left" (decreasing indices) */
        int jdx = perm_get(&ndcperm,j);
        int jval = rec_getbit(varg,jdx);
        if(jval == (grls^1)) {                /* that are ~, set to ~ */
          rec_setbit(recx,jdx,jval,numunits);
          /* fprintf(stdout,"nsql_theta_select_other: and left bit %d val %d \n",jdx,jval); */
        }
        j--;
      }
#if USE_INSERT_NSQL_ALL
      insert(recx,newrsetarg,rtarg,NSQL_NPG,extrabits); 
#else
      append(recx,newrsetarg,rtarg,NSQL_NPG); 
#endif
    }
    i++;
  }

  /* for O_le and O_ge add a copy of v to NDB2, disreagard for O_lt
     and O_gt confused backwards */
  if( ntheta == O_le || ntheta == O_ge ) {
#if USE_INSERT_NSQL_ALL
    insert(varg,newrsetarg,rtarg,NSQL_NPG,extrabits); 
#else
    append(varg,newrsetarg,rtarg,NSQL_NPG); 
#endif
  }
  rec_destroy(recx);
  perm_final(&ndcperm);
  return plen;
}



int nsql_join(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Perm * joincond1arg, Perm * joincond2arg, Runtime * rtarg)
{
  Rec * rec;
  int n, y1len, numunits1;
  int m, y2len, numunits2, nonj2len, numunits_nonj2len;
  int reclen3, numunits3;
  int j, k;
#if USE_INSERT_NSQL_ALL
  int extrabits = runtime_getN(rtarg);
#endif

  n = recset_length(ndb1arg);  
  numunits1 = recset_numunits(ndb1arg);
  y1len = perm_get_length(joincond1arg);

  m = recset_length(ndb2arg);  
  numunits2 = recset_numunits(ndb2arg);
  y2len = perm_get_length(joincond2arg);
  nonj2len = m - y2len;
  numunits_nonj2len = rec_calcnumunits(nonj2len);

  reclen3 = n + nonj2len;
  recset_setlength(ndb3arg,reclen3);
  numunits3 = recset_numunits(ndb3arg);

  /* STEP 1: for each x of NDB1: (a) create record z with x as its
     prefix and m-|Y2| *'s as its suffix; (b) append z to NDB3. */
  
  rec = rec_create(numunits3);
  j = recset_size(ndb1arg);
  while(j-- > 0) {
    Rec * recx = recset_getrec(ndb1arg,j);
    rec_init(rec,numunits3);
    rec_copy_bitbybit(recx,rec,n,0,numunits3); 
#if USE_INSERT_NSQL_ALL
    insert(rec,ndb3arg,rtarg,NSQL_NPG,extrabits);
#else
    append(rec,ndb3arg,rtarg,NSQL_NPG);
#endif
  }
  
  /* STEP 2: for every y of NDB2: (a) create w = n*'s and map unto it
     the values of join positions 2: for all i, set the value of w at
     the position indicated as the ith entry of joincond1, to the
     value of y at the position indicated as the ith entry of joincond2 */

  j = recset_size(ndb2arg);
  while(j-- > 0) {
    Rec * recy = recset_getrec(ndb2arg,j);
    int i = 0;

    rec_init(rec,numunits3);

    while(i < y1len){
      int pos1 = perm_get(joincond1arg,i);
      int pos2 = perm_get(joincond2arg,i);
      rec_setbit(rec,pos1,rec_getbit(recy,pos2),numunits3);
      i++;
    }

    /* (b) create rec z of length m - |Y2| by mapping unto it all the
       non-join positions of y: for all i, set the value of the ith
       position of z to the value of the position indicated as the ith
       entry of Um - Y2. */
    i = 0;
    k = 0;            /* the nonj2len part of reclen3 */
    while(i < m) {
      if(! perm_index_presentp(joincond2arg,i,y2len)) {
        rec_setbit(rec,n+k,rec_getbit(recy,i),numunits3);
        k++;
      }
      i++;
    }

    assert(k == nonj2len);
 
    /* (c) concatenate w with z (already done with n+k in b part
       above) and append to NDB3 */
#if USE_INSERT_NSQL_ALL
    insert(rec,ndb3arg,rtarg,NSQL_NPG,extrabits);
#else
    append(rec,ndb3arg,rtarg,NSQL_NPG);
#endif

  }
  rec_destroy(rec);
  return 0;
}


/* like a join with completely specified join conditions, except the
   same length records in both input recsets is also necessary v.27 */
/* note: might want to consider using append or insert instead of
   recset_copy to take advantage of managed growth and eliminate the
   need to do the recset_delete_subsumed step v.67 */
int nsql_intersection(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg)
{
  int n = recset_length(ndb1arg);  
  int m = recset_length(ndb2arg);  

  if(n != m){
    fprintf(stderr,"ERROR (nsql_intersection): Intersection requires same length records in input negative databases (%d,%d)\n",n,m);
    return err8;
  }

  recset_setlength(ndb3arg,m);

  /* STEP 1: for each x of NDB1: copy x into NDB3. */

  recset_copy(ndb1arg,ndb3arg);

  /* delete all the records from ndb3 that are subsumed by the records
     in ndb2arg; undocumented efficiency step v.36;
  */
#if RELOP_EFFICIENCY_STEP
  recset_delete_subsumed(ndb3arg,ndb2arg);
#endif  

  /* STEP 2: for every y of NDB2: copy y into NDB3; */
  recset_copy(ndb2arg,ndb3arg);

  return 0;
}


/* cartesian (or cross) product is like a join with empty join conditions v.27 */
int nsql_crossproduct(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg)
{
  int i,j;
  int numunits3;
  Rec * recz;
  int n = recset_length(ndb1arg);  
  int m = recset_length(ndb2arg);
#if USE_INSERT_NSQL_ALL
  int extrabits = runtime_getN(rtarg);
#endif

  recset_setlength(ndb3arg,n+m);
  numunits3 = recset_numunits(ndb3arg);
  recz = rec_create(numunits3);

  /* STEP 1: for each x of NDB1: (a) construct a record that has
     prefix x and suffix m * symbols; (b) append to NDB3 */
  
  j = recset_size(ndb1arg);
  while(j-- > 0) {
    Rec * recx = recset_getrec(ndb1arg,j);
    rec_init(recz,numunits3);
    rec_copy_bitbybit(recx,recz,n,0,numunits3);
#if USE_INSERT_NSQL_ALL
    insert(recz,ndb3arg,rtarg,NSQL_NPG,extrabits);
#else
    append(recz,ndb3arg,rtarg,NSQL_NPG);
#endif
  }

  /* STEP 2: for each y of NDB2: (a) construct a record that has as
     prefix n * sumbols and as its suffix y; (b) append to NDB3 */

  i = recset_size(ndb2arg);
  while(i-- > 0) {
    Rec * recy = recset_getrec(ndb2arg,i);
    rec_init(recz,numunits3);
    rec_copy_bitbybit(recy,recz,m,n,numunits3);
#if USE_INSERT_NSQL_ALL
    insert(recz,ndb3arg,rtarg,NSQL_NPG,extrabits);
#else
    append(recz,ndb3arg,rtarg,NSQL_NPG);
#endif
  }
  rec_destroy(recz);
  return 0;
}


/* union requires NDB1 and NDB2 be the same record length */
int nsql_union(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg)
{
  Rec * recz;
  int n,m, numunits;
  int j;
#if USE_INSERT_NSQL_ALL
  int extrabits = runtime_getN(rtarg);
#endif

  n = recset_length(ndb1arg);  
  m = recset_length(ndb2arg);  

  if(n != m){
    fprintf(stderr,"ERROR (nsql_union): Union requires same length records in input negative databases (%d,%d)\n",n,m);
    return err8;
  }

  recset_setlength(ndb3arg,n);
  numunits = recset_numunits(ndb1arg);

  /* For every x of NDB1: for every y of NDB2 such that x MATCHES y:

     i. create a rec z = x o y: Let Y and Y' be the bit positions of
     x and y, respectively, that have either a 0 or a 1 set.  Set
     z[Y]=x[Y], z[Y']=y[Y'], and for all other j positions , z[j]=*;

     ii. add z to NDB3. */
  recz = rec_create(numunits);
  j = recset_size(ndb1arg);
  while(j-- > 0) {
    Rec * recx = recset_getrec(ndb1arg,j);
    int i = recset_size(ndb2arg);
    while(i-- > 0) {
      Rec * recy = recset_getrec(ndb2arg,i);

      if(rec_match_query(recx,recy,numunits)) {
        rec_init(recz,numunits);
        rec_coalesce(recx,recy,recz,numunits);
#if USE_INSERT_NSQL_ALL
        insert(recz,ndb3arg,rtarg,NSQL_NPG,extrabits);
#else
        append(recz,ndb3arg,rtarg,NSQL_NPG);
#endif
      }
    }
  }
  rec_destroy(recz);
  return 0;
}


/* Based on Eric Trias' algorithm: (v.56)
 * Given NDB1 and NDB2 with length l,
 *  a. Complement NDB1, call it DB1.
 *  b. Complement NDB2, call it DB2.
 *  c. bitwise-or every rec of DB1 with every rec of DB2, append to BU
 *  d. bitwise-or every rec of BU with itself for closure, until it
 *     doesn't change, call it BUstar
 *  e. Complement BUstar
 */
int nsql_negbinaryunionstar(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg)
{
  int l, numunits;
  int eqv, count;
  int savminbits;
  Recset * DB2;
  Recset * DB1;
  Recset * BU;
  Recset * BUstar;

  l = recset_length(ndb1arg);
  assert(l == recset_length(ndb2arg));

  recset_setlength(ndb3arg,l);
  numunits = recset_numunits(ndb3arg);
  
  /* STEP 1: complement NDB1 and call it DB1 */
  DB1 = recset_create();
  recset_complement(ndb1arg,DB1,rtarg);

#if RELOP_MIDCLEANUP_OPTION && (! MANAGED_GROWTH || NPG_NONE_UNNEGATE)
  fprintf(stdout,"negBUstar: compress DB1 (%d) \n",recset_size(DB1));
  compress(DB1,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0)); /* v.59.7 */
  cleanup_plus_option(DB1,rtarg);               /* v.57 */
#endif 

  /* STEP 2: complement NDB2 and call it DB2 */
  DB2 = recset_create();
  recset_complement(ndb2arg,DB2,rtarg);

#if RELOP_MIDCLEANUP_OPTION && (! MANAGED_GROWTH || NPG_NONE_UNNEGATE)
  fprintf(stdout,"negBUstar: compress DB2 (%d) \n",recset_size(DB2)); /* v.66.2 */
  compress(DB2,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0)); /* v.59.7 */
  cleanup_plus_option(DB2,rtarg);               /* v.57 */
#endif 
  
  /* STEP 3: for every rec of DB1: bitwise-or with DB2 and append to BU */
  BU = recset_create();
  recset_setlength(BU,l);
  savminbits = runtime_getMinBits(rtarg);
  runtime_setMinBits(rtarg,1);

  nsql_binaryunion(DB1,DB2,BU,rtarg);

  recset_destroy(DB1);
  recset_destroy(DB2);

#if RELOP_MIDCLEANUP_OPTION && ! MANAGED_GROWTH
  fprintf(stdout,"negBUstar: compress BU (%d) \n",recset_size(BU));
  compress(BU,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0)); /* v.59.7 */
  cleanup_plus_option(BU,rtarg);               /* v.57 */
#endif 

  /* STEP 4: bitwise-or every rec of BU with itself, call it BUstar */
#if 1
  fprintf(stdout, "Before STAR UNION BU has %d recs, %d len\n",
          recset_size(BU),recset_length(BU));
  /* recset_print(BU,stdout); */
#endif
    
  runtime_setMinBits(rtarg,0);   /* no compression here v.72 */
  eqv = 0;
  count = 1;
  while(!eqv) {
    fprintf(stdout,"negBUstar: closure count %d\n",count++);
    BUstar = recset_create();
    recset_setlength(BUstar,l);
    eqv = nsql_binaryunion(BU,BU,BUstar,rtarg);  /* v.72 test for equivalance avoided */
    recset_destroy(BU);
    BU=BUstar;
#if 1
    fprintf(stdout, "STAR UNION now has %d recs, %d len\n",
            recset_size(BU),recset_length(BU));
    /* recset_print(BU,stdout); */
#endif
  }

  BUstar=BU;
  runtime_setMinBits(rtarg,savminbits);         /* restore minbits before complement v.72 */

#if RELOP_MIDCLEANUP_OPTION && ! MANAGED_GROWTH
  fprintf(stdout,"negBUstar: compress BUstar (%d)\n",recset_size(BUstar));
  compress(BUstar,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0)); /* v.59.12 */
  cleanup_plus_option(BUstar,rtarg);            /* v.59.12, v.65 typo bug */
#endif 
  
  /* STEP 5: Complement BUstar into NDB3 */
  recset_complement(BUstar,ndb3arg,rtarg);     /* compress when we return v.59.12 */
  runtime_setMinBits(rtarg,1);                 /* most compression v.72.5 */
  
  recset_destroy(BUstar);
  return 0;
}


/* Based on Eric Trias' algorithm: (v.65)
 * Given two recsets, cNDB1 and cNDB2 with length l (already complemented),
 *  bitwise-or every rec of cNDB1 with every rec of cNDB2, append to cndb3arg
 *
 * return 1 if result has same number of records as second input set; o.w. 0 (v.72) 
*/
int nsql_binaryunion(Recset * cndb1arg, Recset * cndb2arg, Recset * cndb3arg, Runtime * rtarg)
{
  int l, numunits;
  unsigned int j;
  Rec * recz;
#if USE_INSERT_NSQL_BU || USE_INSERT_NSQL_ALL
  int extrabits = runtime_getN(rtarg);
#endif
#if NPG_USE_NSQL_BU || NPG_USE_NSQL_ALL
  int npg = with_npg;
#else
  int npg = wo_npg;
#endif

  l = recset_length(cndb1arg);
  assert(l == recset_length(cndb2arg));

  recset_setlength(cndb3arg,l);
  numunits = recset_numunits(cndb3arg);
  
  /* for every rec of cNDB1: bitwise-or with cNDB2 and append to CNDB3 */
  recz = rec_create(numunits);

  j = recset_size(cndb1arg);

  while(j-- > 0) {
    Rec * recj = recset_getrec(cndb1arg,j);
    int i = recset_size(cndb2arg);

    /* for NSH TESTING to watch progress in log file */
#if 0
    fprintf(stdout,"nsql_bu: starting cndb1 rec %d, size is %d.\n",
            j,recset_size(cndb3arg));
    fflush(stdout);
    /*    recset_tree_save(cndb3arg,"cndb3.tmp"); */
#endif

    while(i-- > 0) {
      rec_init(recz,numunits); 
      rec_or(recj,recset_getrec(cndb2arg,i),recz,numunits);
      if(rec_size(recz,numunits) > 0){
#if USE_INSERT_NSQL_BU || USE_INSERT_NSQL_ALL
        insert(recz,cndb3arg,rtarg,npg,extrabits); /* with_npg works better */
#else
        append(recz,cndb3arg,rtarg,npg); /* with_npg works better */
#endif
      }
    }
  } /* end j-while loop */

  rec_destroy(recz); 
  return (recset_size(cndb2arg) == recset_size(cndb3arg));  /* v.72 */
}



/* Based on Eric Trias' algorithm: (v.56)
 * Given NDB1 and NDB2 with length l,
 *  a. Complement NDB1, call it DB1.
 *  b. negUnion DB1 and NDB2, to produce a set of records that is in DB1 but not in DB2;
 *  d. complement the result
 */
int nsql_difference(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, Runtime * rtarg)
{
  int l, numunits;
  Recset * DB1;
  Recset * diffset;
  
  l = recset_length(ndb1arg);
  assert(l == recset_length(ndb2arg));
  
  recset_setlength(ndb3arg,l);
  numunits = recset_numunits(ndb3arg);
  
  /* STEP 1: complement NDB1 and call it DB1 */
  DB1 = recset_create();
  recset_complement(ndb1arg,DB1,rtarg);
  
  /* STEP 2: negUnion DB1 and NDB2, and call it diffset */
  diffset = recset_create();
  nsql_union(DB1,ndb2arg,diffset,rtarg);
  
  recset_destroy(DB1);
  
#if RELOP_MIDCLEANUP_OPTION && ! MANAGED_GROWTH
  compress(diffset,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0)); /* v.59.7 */
  cleanup_plus_option(diffset,rtarg);                  /* v.57 */
#endif
  
  /* STEP 3: Complement diffset into NDB3 */
  recset_complement(diffset,ndb3arg,rtarg);
  recset_destroy(diffset);
  return 0;
}


/*
 * Given DB1 and DB2 with length l, and fully specified
 * return all records in DB1 not represented in DB2  (v.72.3)
 */
int nsql_setdiff(Recset * db1arg, Recset * db2arg, Recset * db3arg, Runtime * rtarg)
{
  int l, i, recsetsize1, numunits;

  l = recset_length(db1arg);
  assert(l == recset_length(db2arg));
  
  recset_setlength(db3arg,l);

  i = 0;
  recsetsize1 = recset_size(db1arg);
  numunits = recset_numunits(db1arg);

  while(i < recsetsize1) {
    Rec * reca = recset_getrec(db1arg,i);
    /*    assert(rec_size(reca,numunits) == l);        NO *'s ALLOWED */
    if(!recset_query_subsumed(db2arg,reca)) {   
      /* reca subsumes no rec in db2 (and NO * in db2 either) */
      append(reca,db3arg,rtarg,wo_npg);
    }
    i++;
  }
  return 0;
}


/* Based on NSH definition of relevance (v.71) Given two positive
 * recsets, DB1 and DB2 with length l and fully-specified, bitwise-and
 * every rec2 of DB2 with every rec1 of DB1, if result is non-zero,
 * append rec1 to db3arg; INCORRECT RESULTS with *'s - doesn't pass
 * four unit tests (v.71.2); useful for case without *'s in first
 * input set (v.72).
 */
int nsql_relevance(Recset * db1arg, Recset * db2arg, Recset * db3arg, Runtime * rtarg)
{
  int l, numunits;
  unsigned int j;
#if NPG_USE_NSQL_BU || NPG_USE_NSQL_ALL
  int npg = with_npg;
#else
  int npg = wo_npg;
#endif

  l = recset_length(db1arg);
  assert(l == recset_length(db2arg));

  recset_setlength(db3arg,l);
  numunits = recset_numunits(db3arg);
  
  /* for every rec of DB2: bitwise-and with DB1; if non-zero result,
     append rec1 to DB3 */

  j = recset_size(db2arg);

  while(j-- > 0) {
    Rec * recj = recset_getrec(db2arg,j);
    int i = recset_size(db1arg);

    /*    assert(rec_size(recj,numunits) == l);     NO *'s ALLOWED */
    while(i-- > 0) {
      Rec * reci = recset_getrec(db1arg,i);
      /*   assert(rec_size(reci,numunits) == l);     NO *'s ALLOWED */

      if(rec_relevant(recj,reci,numunits)){
        append(reci,db3arg,rtarg,npg);
      }
    }    /* end i-while loop */
  } /* end j-while loop */

  return 0;
}

