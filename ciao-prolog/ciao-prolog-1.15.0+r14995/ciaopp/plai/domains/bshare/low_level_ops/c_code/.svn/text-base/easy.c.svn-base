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
 * Filename      : easy.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for Easy-to-Reverse NDB functions
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Wed Mar 28 03:28:56 2007 
 * Updated       : Thu Nov 29 19:54:25 2007 
 *
 * Comments : based on the Davis-Putnam-Logemann-Loveland SAT
 * Algorithm and Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include "easy.h"
#include "recset.h"
#include "perm.h"
#include "freq.h"
#include "error.h"
#include "newndb.h"

#define EASYDEBUG 0

static void easy_chase(Recset * rsetarg, int bitposarg, int valarg, Rec * solrecarg, int * flagptrarg);
static void easy_reduce_random(Recset * rsetarg, Rec * solrecarg, int * flagptrarg, EasyReduceMode modearg);
static void easy_reduce_popular(Recset * rsetarg, Rec * solrecarg, int * flagptrarg, EasyReduceMode modearg);


/* rsetarg doesn't have xrec; keeps backup copies of recset and
   solution rec for backtracking purposes*/ 
static void easy_reduce_to_chase(Recset * rsetarg, int bparg, int valarg, Rec * solrecarg, int * flagptrarg, void (*getnextarg)(Recset *, Rec *, int *, EasyReduceMode), EasyReduceMode modearg )
{
  Recset bkupcopy;
  int j;
  int reclen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  Rec * savelastsolrec = rec_create(numunits); 

#if EASYDEBUG
  fprintf(stderr,"easy_reduce_to_chase: input bp is %d, val is %d\n",bparg,valarg); 
#endif

  recset_init(&bkupcopy,reclen);
  recset_copy(rsetarg,&bkupcopy);     /* bkup has randrec */

  *flagptrarg = 0;                    /* initialize */
  j = 0;
  
  while(j < 2 && *flagptrarg == 0){
    rec_copy(solrecarg,savelastsolrec,numunits);
    
#if EASYDEBUG
    fprintf(stderr,"easy_reduce_to_chase: BEFORE call %d to CHASE (in backtrack loop); flag is %d, recset size is %d, and workingcopy is\n",j,*flagptrarg,recset_size(rsetarg)); 
    recset_print(rsetarg,stderr);  /* i think i lost a rec second time around */
#endif

    /* reduce the current recset */
    /* chase returns flag = 0 if bad decision; -1 if completely done
       (abort); 1 for normal; */
    easy_chase(rsetarg,bparg,valarg^j,solrecarg,flagptrarg);
    
#if EASYDEBUG
    fprintf(stderr,"easy_reduce_to_chase: AFTER call %d to CHASE -- solution rec is ,flag is %d\n",j,*flagptrarg);
    rec_print(solrecarg,reclen,stderr);
#endif

    if(*flagptrarg == 1 || *flagptrarg == -1){
#if EASYDEBUG
      fprintf(stderr,"easy_reduce_to_chase: new recset size is %d : \n",recset_size(rsetarg));
      recset_print(rsetarg,stderr);
#endif 

      if(modearg == SUCCESS_POPULAR_BTRACK_RANDOM){
        if(j==0) 
          easy_reduce_popular(rsetarg,solrecarg,flagptrarg,modearg);
        else
          easy_reduce_random(rsetarg,solrecarg,flagptrarg,modearg);
      } else 
        getnextarg(rsetarg,solrecarg,flagptrarg,modearg);    /* recursive call */

      if(*flagptrarg==0){
        rec_copy(savelastsolrec,solrecarg,numunits);    /* restore last sol rec */

        recset_final(rsetarg); 
        recset_init(rsetarg,reclen); 
        recset_copy(&bkupcopy,rsetarg);
#if EASYDEBUG
        fprintf(stderr,"easy_reduce_to_chase: restored recset size is %d : \n",recset_size(rsetarg));
        recset_print(rsetarg,stderr);
#endif 
      }

    } else {   /* must be zero */
      rec_copy(savelastsolrec,solrecarg,numunits);    /* restore last sol rec */

      recset_final(rsetarg); 
      recset_init(rsetarg,reclen); 
      recset_copy(&bkupcopy,rsetarg);
#if EASYDEBUG
      fprintf(stderr,"easy_reduce_to_chase: restored recset size is %d : \n",recset_size(rsetarg));
      recset_print(rsetarg,stderr);
#endif 
    }
    j++;
  }  /* end plen while loop */
 
  rec_destroy(savelastsolrec);      /* can no longer backtrack from here */
  recset_final(&bkupcopy);
}


/* input rsetarg has xrarg - very much like
   easy_reduce_to_chase, except for unitary (1 bit) case v.46 */ 
static void easy_chase_unitary(Recset * rsetarg, int xarg, Rec * solrecarg, int * flagptrarg)
{
  Recset bkupcopy;
  Perm notdcperm;
  int plen;
  int j;
  int bp, v;
  int reclen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);
  Rec * savelastsolrec = rec_create(numunits); 
  Rec * xrec = recset_getrec(rsetarg,xarg);

#if EASYDEBUG
  fprintf(stderr,"easy_chase_unitary: input xrec is, reclen is %d \n",reclen); 
  rec_print(xrec,reclen,stderr);
#endif
  
  /* pick an arbitrary literal (i.e. bit position) */
  perm_init(&notdcperm,reclen);
  perm_notdontcares(&notdcperm,xrec,reclen);
  plen = perm_get_length(&notdcperm);  

#if EASYDEBUG
  fprintf(stderr,"easy_chase_unitary: number of not *'s in xrec (plen) is %d\n",plen); 
  rec_print(xrec,reclen,stderr);
#endif

  bp = perm_get(&notdcperm,0);       /* just want the first index */
  perm_final(&notdcperm);
  v = rec_getbit(xrec,bp);

  recset_init(&bkupcopy,reclen);
  recset_copy(rsetarg,&bkupcopy);

  *flagptrarg = 0;                    /* initialize */
  j = 0;

  while(j < 2 && *flagptrarg == 0){
    rec_copy(solrecarg,savelastsolrec,numunits);

#if EASYDEBUG
    fprintf(stderr,"easy_chase_unitary: in backtrack loop %d out of at most 2 times; flag is %d, recset is \n",j,*flagptrarg); 
    recset_print(rsetarg,stderr);
#endif

    /* reduce the current recset */
    /* chase returns flag = 0 if bad decision; -1 if completely done
       (abort); 1 for normal; */
    easy_chase(rsetarg,bp,v^j,solrecarg,flagptrarg);
    
#if EASYDEBUG
    fprintf(stderr,"easy_chase_unitary: after call %d to chase -- solution rec is ,flag is %d\n",j,*flagptrarg);
    rec_print(solrecarg,reclen,stderr);
#endif

    if(*flagptrarg == 1 || *flagptrarg == -1){
#if EASYDEBUG
      fprintf(stderr,"easy_chase_unitary:  resulting recset : \n");
      recset_print(rsetarg,stderr);
#endif 

    } else {   /* must be zero */
      rec_copy(savelastsolrec,solrecarg,numunits);    /* restore last sol rec */

      recset_final(rsetarg);
      recset_init(rsetarg,reclen);
      recset_copy(&bkupcopy,rsetarg);
    }
    j++;
  }  /* end plen while loop 1 */

  recset_final(&bkupcopy);
  rec_destroy(savelastsolrec);

#if EASYDEBUG
  fprintf(stderr,"easy_chase_unitary: returning flag %d\n",*flagptrarg);
#endif
}


/* based on Davis-Putnam-Logemann-Loveland CHASE algorithm v.45 modifies flag = 0 if bad
   decision; -1 if completely done; 1 for normal; rsetarg is modified;
   solrecarg is also possibly modified.
*/
static void easy_chase(Recset * rsetarg, int bitposarg, int valarg, Rec * solrecarg, int * flagptrarg)
{
  Recset cache;
  int sz, i;
  int numunits = recset_numunits(rsetarg);
  int reclen = recset_length(rsetarg);
  Rec * xrec = rec_create(numunits);

  *flagptrarg = 1;           /* initialize flag to default value */

  rec_setbit(xrec,bitposarg,valarg,numunits);
  rec_setbit(solrecarg,bitposarg,valarg^1,numunits); /* complement value for positive */
#if EASYDEBUG
  fprintf(stderr,"easy_chase: setting bit %d to value %d in solution rec\n",bitposarg,valarg^1);
  rec_print(solrecarg,reclen,stderr);
#endif
  sz = recset_size(rsetarg);      /* size before the split */
  /*  fprintf(stderr,"easy_chase: recset size before the split is  %d\n",sz); */
  
  recset_init(&cache,reclen);
  
  /* delete all clauses from recset that contain the literal x (bp and value) */
  recset_split_subsumed(xrec,rsetarg,&cache);
  
  rec_destroy(xrec);
  recset_final(&cache);
  
  sz = recset_size(rsetarg);      /* new size after the split */
  
  /*  fprintf(stderr,"easy_chase: recset size after the split is  %d\n",sz); */

  if (sz == 0) {
    
     *flagptrarg = -1;           /* abort DPLL procedure; solrec changed */
     
  } else { /* delete the literal x' (x-complement) from all clauses
              remaining in recset unless an empty clause is
              encountered */    

    Recset rsetcopy;          /* use rsetarg copy for iterating;
                                 addrec under MANAGED_GROWTH can
                                 change it underneath you and messup
                                 the i-count v.62 */
    recset_init(&rsetcopy,reclen);
    recset_copy(rsetarg,&rsetcopy);
    recset_final(rsetarg);    /* start fresh */
    recset_init(rsetarg,reclen);
    
    i = recset_size(&rsetcopy);                   /* v.64 again */
    if (i == 0) *flagptrarg = -1;          /* abort DPLL procedure; solrec changed v.64 */

    while(i-- > 0 && *flagptrarg > 0) {           /* go from end to beginning of recset */
      Rec * testrec = recset_getrec(&rsetcopy,i);

#if EASYDEBUG
      fprintf(stderr,"easy_chase: testing record %d, bit position %d\n",i,bitposarg);
      rec_print(testrec,reclen,stderr);
#endif
      
      if(!rec_isdc(testrec,bitposarg)){
        if(rec_size(testrec,numunits) == 1) {  /* to-be empty clause means a bad decision */
          *flagptrarg = 0;                     /* short-circuit */
        } else {                               /* maintain rstree invariant v.45 */
          recset_removerec(&rsetcopy,i);
          rec_setbit(testrec,bitposarg,DONTCARE,numunits);             /* irreversible!! */
          recset_addrec(testrec,rsetarg);      /* possible duplicate, destroyed here */
        }
      } else {                             /* if dc at bitpos just move the rec as-is v.62 */
          recset_removerec(&rsetcopy,i);
          recset_addrec(testrec,rsetarg);      /* possible duplicate, destroyed here */
      }
    }  /* end while loop */

    /*    recset_merge(&rsetcopy,rsetarg);    in case any remain, though not necessary */
    recset_final(&rsetcopy);               /* done with the copy v.62 */

  } /* end else */
  
  /* ok to continue the unit chase (recursively) */
  if(*flagptrarg > 0){
    int unitaryrecindex = recset_getrec_lastofsize_index(rsetarg,1);
    if(unitaryrecindex >=0){
      
#if EASYDEBUG
      fprintf(stderr,"easy_chase: reduce to chase unit clause index %d \n",unitaryrecindex);
#endif
      
      easy_chase_unitary(rsetarg,unitaryrecindex,solrecarg,flagptrarg);
    }

#if EASYDEBUG
    fprintf(stderr,"easy_chase: flag is %d, solrec is \n",*flagptrarg);
    rec_print(solrecarg,reclen,stderr);
#endif
  }
  
#if EASYDEBUG
  fprintf(stderr,"easy_chase: returning flag=%d\n",*flagptrarg); 
#endif
}


/* based on Davis-Putnam-Logemann-Loveland algorithm v.45 returns
   reduced rsetarg and truth assignment in solrecarg and chase flag;
   recursive helper for recset_reduce v.46 */
static void easy_reduce_random(Recset * rsetarg, Rec * solrecarg, int * flagptrarg, EasyReduceMode modearg)
{
  int bp,v;
  int sz = recset_size(rsetarg);
  int reclen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);

  /* pick an arbitrary clause (i.e. record) */  

  if(*flagptrarg > 0 && sz > 0 && rec_size(solrecarg,numunits) < reclen) {    /* no need to also check sz > 0 since its flag=-1 */
    int randidx;
    Rec * randrec;
    int trycount = 0;

    while(trycount < sz){
      Perm notdcperm;
      int plen;
      int i;

      randidx = nextRand(sz);
      randrec = recset_getrec(rsetarg,randidx);
      
      /* pick an arbitrary literal (i.e. bit position) */
      perm_init(&notdcperm,reclen);
      perm_notdontcares(&notdcperm,randrec,reclen);
      plen = perm_get_length(&notdcperm);
      
#if EASYDEBUG
      fprintf(stderr,"easy_reduce_random: number of not *'s in randrec (plen) is %d\n",plen); 
      rec_print(randrec,reclen,stderr);
#endif
      
      assert(plen > 0);
      perm_randomize(&notdcperm,plen);
      i = 0;
      while(i < plen){                       
        bp = perm_get(&notdcperm,i);       /* just want the first index */
        if(rec_isdc(solrecarg,bp)) break;  /* that's not already set  v.46  */
#if EASYDEBUG
        fprintf(stderr,"easy_reduce_random: trying another bit position, %d already in solution rec\n",bp);
#endif
        i++;
      }

      perm_final(&notdcperm);      
      if(i < plen) break;
      trycount++;
    } /* end try while */   
    


    if(trycount >= sz) {      
      fprintf(stderr,"easy_reduce_random: couldn't find an unset bit position in solution that was also in the perm.\n");
      
      /* this should never happen - implies all bits in solution are
         set, so what are we doing here! */
 
      exit(errneg3);
    }

    v = rec_getbit(randrec,bp);

#if EASYDEBUG
    fprintf(stderr,"easy_reduce_random: BEFORE call to reduce_to_chase -- flag is %d, recset size is %d, bp is %d, val is %d, randrec is , and solution rec is \n",*flagptrarg,sz,bp,v);
    rec_print(randrec,reclen,stderr);
    rec_print(solrecarg,reclen,stderr);
    fprintf(stderr,"easy_reduce_random: and recset is \n");
    recset_print(rsetarg,stderr);
#endif

    
    if(modearg == ALTERNATE_POPULAR_RANDOM)
      easy_reduce_to_chase(rsetarg,bp,v,solrecarg,flagptrarg,easy_reduce_popular,modearg);
    else
      easy_reduce_to_chase(rsetarg,bp,v,solrecarg,flagptrarg,easy_reduce_random,modearg);

    sz = recset_size(rsetarg);
    
#if EASYDEBUG
    fprintf(stderr,"easy_reduce_random: AFTER call to reduce_to_chase -- flag is %d, recset size is %d, solution rec is\n",*flagptrarg,sz);
    rec_print(solrecarg,recset_length(rsetarg),stderr);
#endif
    
  } /* endif */
  
#if EASYDEBUG
  fprintf(stderr,"easy_reduce_random: leaving with this recset, size %d; and flag %d\n",sz,*flagptrarg); 
  recset_print(rsetarg,stderr);
  if(sz > 0 && *flagptrarg==-1){
    fprintf(stderr,"easy_reduce_random: notice flag is -1 but recset is not empty??\n");
  }
#endif
}


/* based on Davis-Putnam-Logemann-Loveland algorithm, returns reduced
   rsetarg and truth assignment in solrecarg and chase flag; recursive
   helper for recset_reduce based on popular bit selection v.50 */
static void easy_reduce_popular(Recset * rsetarg, Rec * solrecarg, int * flagptrarg, EasyReduceMode modearg)
{
  int bp,v;
  int sz = recset_size(rsetarg);
  int reclen = recset_length(rsetarg);
  int numunits = recset_numunits(rsetarg);

  if(*flagptrarg > 0 && sz > 0 && rec_size(solrecarg,numunits) < reclen) {    /* no need to also check sz > 0 since its flag=-1 */
    Freq * bitfreq;
    unsigned int ** sortarray;
    int i = 0;
    
    recset_update_bit_frequency(rsetarg);
    bitfreq = recset_getbitfrequency(rsetarg);

    
    /* pick most popular literal (i.e. bit position) not already in solrecarg */  

    sortarray = malloc(sizeof(unsigned int*) * (reclen));
    if(sortarray==NULL){
      fprintf(stderr,"easy_reduce_popular: out of memory condition for sort array\n");
      exit(err12);
    }
    
    freq_sort(bitfreq,sortarray);

    while(i < reclen){                       
      bp = freq_convert_to_index(bitfreq,sortarray[i]);
      
      if(rec_isdc(solrecarg,bp)){  /* that's not already set  v.46  */
        v = recset_popular_bitposition_value(rsetarg,bp); 
        if (v != DONTCARE) {             /* and is not DC everywhere v.50 */
          break;
        }
      }

#if EASYDEBUG
      fprintf(stderr,"easy_reduce_popular: trying another bit position, %d already in solution rec, %d in sortarray\n",bp,i);
#endif

      i++;
    } /* end try while */ 
    
    if(i >= reclen) {
      fprintf(stderr,"easy_reduce_popular: couldn't find an unset bit position in solution that was also in the perm, i is %d, solution is \n",i);
      rec_print(solrecarg,reclen,stderr);
      
      /* this should never happen - implies all bits in solution are set,
         so what are we doing here! */
      exit(errneg3);
    }
    
#if EASYDEBUG
    fprintf(stderr,"easy_reduce_popular: BEFORE call to reduce_to_chase -- flag is %d, recset size is %d, bp is %d, val is %d, i is %d and solution rec is \n",*flagptrarg,sz,bp,v,i);
    rec_print(solrecarg,reclen,stderr);
    fprintf(stderr,"easy_reduce_popular: and recset is \n");
    recset_print(rsetarg,stderr);
#endif
    free(sortarray);
    
    if(modearg == ALTERNATE_POPULAR_RANDOM)
      easy_reduce_to_chase(rsetarg,bp,v,solrecarg,flagptrarg,easy_reduce_random,modearg);
    else
      easy_reduce_to_chase(rsetarg,bp,v,solrecarg,flagptrarg,easy_reduce_popular,modearg);

    sz = recset_size(rsetarg);
    
#if EASYDEBUG
    fprintf(stderr,"easy_reduce_popular: AFTER call to reduce_to_chase -- flag is %d, recset size is %d, solution rec is\n",*flagptrarg,sz);
    rec_print(solrecarg,recset_length(rsetarg),stderr);
#endif

  } /* endif */
  
#if EASYDEBUG
  fprintf(stderr,"easy_reduce_popular: leaving with this recset, size %d; and flag %d\n",sz,*flagptrarg); 
  recset_print(rsetarg,stderr);
  if(sz > 0 && *flagptrarg==-1){
    fprintf(stderr,"easy_reduce_popular: notice flag is -1 but recset is not empty??\n");
  }
#endif
}


/* based on Davis-Putnam-Logemann-Loveland algorithm v.45 returns
   reduced rsetarg and truth assignment in solrecarg and chase flag,
   error return errneg2 if too many specified bits */
int easy_reduce_sat(Recset * rsetarg, Rec * solrecarg, EasyReduceMode reducemodearg, int satarg)
{
  int maxrecsz;
  int numunits = recset_numunits(rsetarg);
  int flag = 1;

  /* assumes fresh solution records */
  assert(rec_size(solrecarg,numunits) == 0);

  /* assume 2-SAT (3-SAT for testing only) */
  if(satarg != SAT2 && satarg != SAT3)
    return errneg2;

  /* check for 2-SAT (i.e. max 2 specified bit record size) */
  maxrecsz = recset_max_recordsize(rsetarg);

  if(maxrecsz > satarg){
    /*    fprintf(stderr,"easy_reduce: WARNING only 2 specified bits per record allowed (found %d); try morphing with this command: ./negdb -c 1 -t 1000 -m 2\n", maxrecsz); */
    return errneg2;       /* flag is -2 */
  }
  
  if(reducemodearg == ALL_RANDOM)
    easy_reduce_random(rsetarg,solrecarg,&flag,reducemodearg);
  else
    easy_reduce_popular(rsetarg,solrecarg,&flag,reducemodearg);
  
  return flag;
}


/* though we don't expect to reduce 3SAT problems in practice, for
   testing purposes we wanted to see that the DPLL Algorithm worked
   for a small case. v.46 */
void test_easy_reduce_simple_fullbit(int karg)
{
  Recset rset;
  Recset rsetcopy;
  Rec * solutionrec;
  Rec * addrec;
  int numunits,i,rtn,mode;
  int len = karg;
  int endloop = 1 << len;   /* 8 */

  fprintf(stderr,"starting test_easy_reduce_simple_fullbit..k is %d\n",karg);

  assert(karg==2 || karg==3);

  recset_init(&rset,len);
  numunits = recset_numunits(&rset);

  solutionrec = rec_create(numunits);

  /* create a fully specified empty db - no *'s */
  for(i = 0; i < endloop ; i++) {
    char str[9];
    Rec * rec = rec_create(numunits);
    itobs(i,str,len);
    rec_string2rec(rec,len,str,1);  /* use one for binmode */
    recset_addrec(rec,&rset);
  }

  recset_init(&rsetcopy,len);
  recset_copy(&rset,&rsetcopy);

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    fprintf(stderr,"test_easy_reduce_simple_fullbit: testing empty MODE %d..\n", mode);
    if(easy_reduce_sat(&rsetcopy,solutionrec,mode,karg) > 0 ) {
      fprintf(stderr,"test_easy_reduce_simple_fullbit: empty (%d-SAT) ndb shows a solution using mode %d\n",karg,mode);
      rec_print(solutionrec,len,stderr);
      rec_destroy(solutionrec);
      recset_final(&rset);
      recset_final(&rsetcopy);
      exit(1);
    }
    rec_init(solutionrec,numunits);
    recset_final(&rsetcopy);
    recset_init(&rsetcopy,len);
    recset_copy(&rset,&rsetcopy);
  }

  addrec = recset_getrec(&rsetcopy,endloop-1);
  recset_removerec(&rsetcopy,endloop-1);
  rec_destroy(addrec);

  addrec = recset_getrec(&rset,endloop-1);
  recset_removerec(&rset,endloop-1);

  fprintf(stderr,"test_easy_reduce_simple_fullbit: added rec to recset ");
  rec_print(addrec,len,stderr);

  for(mode=0; mode < LASTREDUCEMODE; mode++){
    fprintf(stderr,"test_easy_reduce_simple_fullbit: testing 1 rec MODE %d..\n", mode);
    rtn = easy_reduce_sat(&rsetcopy,solutionrec,mode,karg);
    if(rtn==0 || rtn==errneg2) {
      fprintf(stderr,"test_easy_reduce_simple_fullbit: 1 rec ndb shows no solutions (rtn is %d) using mode %d",rtn,mode);
      rec_print(solutionrec,len,stderr);
      recset_print(&rset,stderr);
      rec_destroy(solutionrec);
      rec_destroy(addrec);
      recset_final(&rset);
      exit(2);
    }

    if(!rec_equal(solutionrec,addrec,numunits)){ 
      fprintf(stderr,"test_easy_reduce_simple_fullbit: 1 rec ndb shows (rtn is %d) wrong solution using mode %d, not added rec ",rtn, mode);
      rec_print(solutionrec,len,stderr);
      rec_print(addrec,len,stderr);
      rec_destroy(solutionrec);
      rec_destroy(addrec);
      recset_final(&rset);
      recset_final(&rsetcopy);
      exit(3);
    }
    rec_init(solutionrec,numunits);
    recset_final(&rsetcopy);
    recset_init(&rsetcopy,len);
    recset_copy(&rset,&rsetcopy);
  }

  rec_destroy(solutionrec);
  rec_destroy(addrec);
  recset_final(&rset);
  recset_final(&rsetcopy);

  fprintf(stdout,"completed easy reduce simple fullbit  test\n");
}
