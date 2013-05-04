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
 * Filename      : test.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : NDB Runtime Unit Tests
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Thu May  8 14:45:14 2008 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include <time.h>
#include "runtime.h"
#include "freq.h"
#include "negdb.h"
#include "newndb.h"
#include "easy.h"
#include "error.h"
#include "names.h"
#include "test.h"

static void check_bitfreq(Recset * rsarg, Runtime * rtarg)
{
  if( recset_save_and_checkbitfreq(rsarg,rtarg,"./rndb.sav") > 0){
    fprintf(stderr,"check bit freq failed\n");
    exit(err19);
  }
}

/*
 * TESTS BEGIN
 */

void test_runtime(int lenarg, int minbitsarg)
{
  Rec * rec1;
  Rec * rec2;
  Rec * rec3;
  int sz, csz, nsz;
  int numunits, binmode;
  Runtime rt;
  Recset * cache = recset_create();
  Recset * ndb = recset_create();
  char * dbrec1 = "100101111011011010101110";   /* rec from DB   */
  char * dbrec2 = "1110*010**1*0*10*******1";  /* rec from ndb */
  char * dbrec3 = "111010101111011011111111";  /* filled in rec from ndb */

  fprintf(stderr,"starting test_runtime..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setN(&rt,1);
  numunits = rec_calcnumunits(lenarg);

  runtime_setMinBits(&rt,minbitsarg);
  /*  runtime_setMinBitsSpecifiedFlag(&rt); unset to give cleanup something to do v.60.1 */

  fprintf(stdout,"using seed %d\n",runtime_getSeed(&rt));

  rec1 = rec_create(numunits);
  rec2 = rec_create(numunits);
  rec3 = rec_create(numunits);
  binmode = runtime_getBinMode(&rt);
  rec_string2rec(rec1,lenarg,dbrec1,binmode);
  rec_string2rec(rec2,lenarg,dbrec2,binmode);
  rec_string2rec(rec3,lenarg,dbrec3,binmode);

  fprintf(stdout,"at build\n");
  recset_build(ndb,&rt,"./RNDB.test",ndbformat);
  sz = recset_size(ndb);

  if( sz != 18720 ) {              /* was 18204 post v.59; back to 18720 v.64 */
    fprintf(stderr,"unexpectedly read in %d unique records, not 18720\n",sz);
    exit(50); 
  }

  fprintf(stdout,"first query\n");
  if(recset_query(ndb,rec1)){
    fprintf(stderr,"oops, record <%s> found---not in DB\n",dbrec1);
    exit(51);
  }

  fprintf(stdout,"second query\n");
  if(! recset_query(ndb,rec2)){
    fprintf(stderr,"oops, record <%s> NOT found---in DB\n",dbrec2);
    exit(52);
  }

  if(! recset_query(ndb,rec3)){
    fprintf(stderr,"oops, record <%s> NOT found---in DB\n",dbrec3);
    exit(53);
  }
  
  check_bitfreq(ndb,&rt);

  printf("/* DO A DIFF between rndb.sav and original RNDB.test, both cracked and sorted first;\n last done and passed v.59.10 */\n");

  if(freq_get(recset_getbitfrequency(ndb),0) != 2521) {   /* was 2412 post v.59; v.64 back */
    fprintf(stderr, "bitfreq for bit 0 is %d, not 2521\n",freq_get(recset_getbitfrequency(ndb),0));
    exit(54); 
  }

  if(freq_get(recset_getbitfrequency(ndb),23) != 10012) {  /* was 9710 post v.59; v.64 back */
    fprintf(stderr, "bitfreq for bit 23 is %d, not 10012\n",freq_get(recset_getbitfrequency(ndb),23));
    exit(55); 
  }
#if 1
  fprintf(stdout,"test split here\n");
  recset_split(rec3, ndb, cache);
  csz = recset_size(cache);
  nsz = recset_size(ndb);
  if ( csz + nsz != sz) {
    fprintf(stderr,"cache size is %d\n",csz);
    fprintf(stderr,"new ndb size is %d\n",nsz);
    exit(18);
  }

  if(recset_query(ndb,rec3)){
    fprintf(stderr,"oops, record <%s> found---not split\n",dbrec3);
    exit(56);
  }

  if(! recset_query(cache,rec3)){
    fprintf(stderr,"oops, record <%s> found in cache---not split\n",dbrec3);
    exit(57);
  }

  if(!SANITY){
    fprintf(stderr,"test_runtime: calling sanity check\n");
    recset_sanitycheck_tree(ndb);
  }

  /*  recset_tree_clear(ndb);  v.15 */
  fprintf(stdout,"test merge here\n");
  recset_merge(cache,ndb);  /* v.15 */

  if(!SANITY){
    fprintf(stderr,"test_runtime: calling sanity check\n");
    recset_sanitycheck_tree(ndb);
  }

#endif

#if 1
  fprintf(stderr,"online remove rec 1 now..\n");
  if(online_remove(dbrec1,ndb,&rt)) {
    exit(21);
  }

  if(!recset_query(ndb,rec1)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB---not removed from DB\n",dbrec1);
    exit(58);
  }

  check_bitfreq(ndb,&rt);

  fprintf(stderr,"online add rec 1 now..\n");
  if(online_add(dbrec1,ndb,&rt)) {
    exit(24);
  }

  if(recset_query(ndb,rec1)){
    fprintf(stderr,"oops, record <%s> found---not added to DB\n",dbrec1);
    exit(59);
  }
  check_bitfreq(ndb,&rt);
#endif

  {
    int cleanloop = 0;
    int netclean, netdiff, newsize;
    
    fprintf(stderr,"online testclean..\n");

    runtime_setCleanTests(&rt,1000); /* moved here to avoid cleanup option v.67 */
    netclean = -1;

    while(netclean < 0 && cleanloop < 3) {
      cleanloop++;
      nsz = recset_size(ndb);
      netclean = test_clean(ndb,&rt);
      newsize = recset_size(ndb);
      netdiff = newsize - nsz;
      
      fprintf(stdout,"(clean %d) new ndb size is %d --- a difference of %d\n\n",
              cleanloop,
              newsize,
              netdiff
              );
    
      if ( netdiff != netclean) {
        fprintf(stderr,"after clean tau %d, database size grew from %d to %d, net difference of %d, BUT clean results net total was %d\n",
                runtime_getTau(&rt),
                nsz,
                newsize,
                netdiff,
                netclean
                );
        exit(33);
      }

      if ( newsize > nsz) {
        fprintf(stderr,"after clean tau %d, database size grew from %d to %d\n",
                runtime_getTau(&rt),
                nsz,
                newsize
                );
        exit(74);
      }
      
      if(recset_query(ndb,rec1)){
        fprintf(stderr,"oops, record <%s> found---not added to DB---after cleaning\n",dbrec1);
        exit(60);
      }
    } /* end while loop */
  }

  recset_destroy(ndb);
  recset_destroy(cache);
  rec_destroy(rec1);
  rec_destroy(rec2);
  rec_destroy(rec3);
  
  fprintf(stdout,"completed runtime test\n");
}


void test_runtime_empty(int lenarg, int minbitsarg)
{
  Runtime rt;
  Rec * rec1;
  int sz;
  int numunits;
  CleanResults cleanResults;
  Recset * ndb = recset_create();
  char * dbrec1 = "100101111011011010101110";   /* rec from DB */

  fprintf(stderr,"starting test_runtime_empty..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setN(&rt,1);
  runtime_seedRandom(&rt,777);
  runtime_setMinBits(&rt,3);
  runtime_setMinBitsSpecifiedFlag(&rt);

  recset_setlength(ndb,lenarg);

  numunits = rec_calcnumunits(lenarg);
  runtime_setMinBits(&rt,minbitsarg);

  fprintf(stdout,"numunits is %d for lenarg %d\n", numunits,lenarg);

  rec1 = rec_create(numunits);
  rec_string2rec(rec1,lenarg,dbrec1,runtime_getBinMode(&rt));

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);
  
  if(!recset_query(ndb,rec1)){
    fprintf(stderr,"oops, record <%s> NOT found---already in DB\n",dbrec1);
    recset_save(ndb,&rt,"rndb-40.txt");
    recset_tree_save(ndb,"rndb-40-tree.txt");
    exit(3);
  }

  online_add(dbrec1,ndb,&rt);  

  recset_tree_clear(ndb);

  if(recset_query_match(ndb,rec1) != recset_query(ndb,rec1) ){
    fprintf(stderr,"EEK!! subsume and match queries not the same for fully specified record <%s>\n", dbrec1);
    exit(7);
  }

  recset_tree_clear(ndb);

  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(4);
  }

  if(!recset_ischanged(ndb)) {
    fprintf(stderr,"oops, ndb WAS changed, should really save\n");
    exit(5);
  }

  cleanup(ndb,&rt,NULL,0,&cleanResults); 

  if(recset_query(ndb,rec1)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB--after cleanup\n",dbrec1);
    exit(43);
  }

#if 0
  fprintf(stderr,"starting test_runtime_empty..unnegate and compare tests\n");
  /* too expensive v.57.4 */
  {
    Recset * compset = recset_create();
    recset_complement(ndb,compset,&rt);
    
    if(!recset_query(compset,rec1)) {
      fprintf(stderr,"oops, record <%s> not found in un_NDB---in DB?\n",dbrec1);
      exit(44);
    }
    
    if(! recset_compare_destructively(ndb,compset,&rt)) {
      fprintf(stderr,"runtime_empty_test: ndb and un_NDB are the same??\n");
      exit(44);
    }
    
    recset_destroy(compset);
  }
#endif
  
  recset_destroy(ndb);
  rec_destroy(rec1);
  
  fprintf(stderr,"starting test_runtime_empty..expand test\n");
  /* test expand recsize */
  {
    int recsz = runtime_getMinBits(&rt);
    runtime_setMinBits(&rt,recsz+2);      /* at 15 this does something */
    ndb = recset_create();
    fprintf(stderr,"starting test_runtime_empty..building recset\n");
    recset_build(ndb,&rt,"./RNDB.test",ndbformat);
    fprintf(stderr,"starting test_runtime_empty..destroying recset\n");
    recset_destroy(ndb);
  }

  fprintf(stdout,"completed runtime empty test\n");
}


void test_runtime_empty_randomSATformula(int lenarg, int minbitsarg)
{
  Runtime rt;
  Rec * rec1;
  int sz;
  int numunits;
  int i;

  Recset * ndb = recset_create();

  fprintf(stderr,"\nstarting test_runtime_empty_randomSATformula..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setN(&rt,0);
  runtime_setMinBits(&rt,minbitsarg);
  runtime_setMinBitsSpecifiedFlag(&rt);
  recset_setlength(ndb,lenarg);
  numunits = recset_numunits(ndb);

  fprintf(stdout,"numunits is %d for lenarg %d, minbits %d\n",
          numunits,lenarg,minbitsarg);

  rec1 = rec_create(numunits);
  rec_random(rec1,lenarg,numunits);

  empty_ndb_create_randomSATformula(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);
  
  if(!recset_query(ndb,rec1)){
    fprintf(stderr,"oops, random record NOT found---already in DB\n");
    rec_print(rec1,lenarg,stderr);
    exit(4);
  }

  /* test hamming distance of one (1 bit difference) is not in DB by mistake */
  for (i = 0; i < lenarg; i++) {
    int v = rec_getbit(rec1,i);
    assert(v>=0);
    rec_setbit(rec1,i,!v,numunits);
    if(!recset_query(ndb,rec1)){
      fprintf(stderr,"oops, this random record with bit <%d> FLIPPED was found in DB---not in RNDB\n",i);
      rec_print(rec1,lenarg,stderr);
      exit(9);
    }
    rec_setbit(rec1,i,v,numunits);
  }

  recset_destroy(ndb);
  rec_destroy(rec1);
  
  fprintf(stdout,"completed runtime random SAT formula empty test\n");
}


void test_runtime_singleton(int lenarg, int minbitsarg)
{
  Runtime rt;
  int sz;
  int numunits;
  int i;

  Recset * ndb = recset_create();
  char * dbrec1 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111";  /* haixia's TEST */
  char * dbrec0 = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"; 

  fprintf(stderr,"\nstarting test_runtime_singleton..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setN(&rt,0);           /* v.35 */
  runtime_setMinBits(&rt,minbitsarg);
  runtime_seedRandom(&rt,0);

  recset_setlength(ndb,lenarg); 
  numunits = recset_numunits(ndb);

  fprintf(stdout,"numunits is %d for lenarg %d\n", numunits,lenarg);

  runtime_setInput(&rt,dbrec1);   /* v.70 */
  runtime_setup_multiinput(&rt);  /* v.70 */

  singleton_ndb_create(runtime_getMultiInput(&rt,runtime_getBinMode(&rt)),ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"singleton ndb has %d unique records\n",sz);
  
  /* use online query instead on recset_query */
  /* online_query returns true if the string is not in the database */
  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(49);
  }

  /* this string would show up with 2-hidden allequal check mistake */
  if(!online_query(dbrec0,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> not found in RNDB---in DB\n",dbrec0);
    exit(9);
  }

#if 0 && (defined(NORANDSATLENLIMIT) || defined(ISCHARDQR))
  fprintf(stderr,"bypassing superfluous tests\n");
#else
 {
  Rec * rec1 = rec_create(numunits);
  rec_string2rec(rec1,lenarg,dbrec1,runtime_getBinMode(&rt));

  /* test hamming distance of one (1 bit difference) is not in DB by mistake */
  for (i = 0; i < lenarg; i++) {
    rec_setbit(rec1,i,0,numunits);
    if(!recset_query(ndb,rec1)){
      fprintf(stderr,"oops, this ALL ONES record with bit <%d> ZERO was found in DB---not in RNDB\n",i);
      rec_print(rec1,lenarg,stderr);
      rec_destroy(rec1);
      exit(6);
    }
    rec_setbit(rec1,i,1,numunits);
  }
  rec_destroy(rec1);
 }
#endif
    
  if(!recset_ischanged(ndb)) {
    fprintf(stderr,"oops, ndb WAS changed, should really save\n");
    exit(42);
  }

  recset_destroy(ndb);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime singleton test\n");
}


void test_runtime_singleton_ascii(int lenarg, int minbitsarg)
{
  Runtime rt;
  int sz;
  int numunits;

  Recset * ndb = recset_create();
  char * dbrec1 = "hello world ";

  fprintf(stderr,"\nstarting test_runtime_singleton_ascii..\n");

  runtime_init(&rt);
  runtime_setN(&rt,0);         /* v.35 */
  runtime_setMinBits(&rt,minbitsarg);

  recset_setlength(ndb,lenarg); 
  numunits = recset_numunits(ndb);

  fprintf(stdout,"numunits is %d for lenarg %d\n", numunits,lenarg);

  runtime_setInput(&rt,dbrec1);   /* v.70 */
  runtime_setup_multiinput(&rt);  /* v.70 */

  singleton_ndb_create(runtime_getMultiInput(&rt,runtime_getBinMode(&rt)),ndb,&rt);

  sz = recset_size(ndb);
  fprintf(stderr,"singleton ndb has %d unique records\n",sz);
  
  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(149);
  }

  /* this string would show up with 2-hidden allequal check mistake */
  if(!online_query("hello Wovld ",ndb,&rt)){
    fprintf(stderr,"oops, record <hello Wovld> not found in RNDB---in DB\n");
    exit(19);
  }

  if(!online_query("hello       ",ndb,&rt)){
    fprintf(stderr,"oops, record <hello> not found in RNDB---in DB\n");
    exit(19);
  }

  recset_destroy(ndb);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime singleton ascii test\n");
}


/* v.70 test two hidden singleton solutions */
void test_runtime_singleton_double(int lenarg, int minbitsarg)
{
  Runtime rt;
  int sz;
  int numunits;
  int i;

  Recset * ndb = recset_create();
  char * dbrec1 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111";  /* haixia's TEST */
  char * dbrec0 = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"; 

  fprintf(stderr,"\nstarting test_runtime_singleton_double..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setN(&rt,0);           /* v.35 */
  runtime_setMinBits(&rt,minbitsarg);
  runtime_seedRandom(&rt,0);

  recset_setlength(ndb,lenarg); 
  numunits = recset_numunits(ndb);

  fprintf(stdout,"numunits is %d for lenarg %d\n", numunits,lenarg);

  runtime_setInput(&rt,dbrec1);   
  runtime_setup_multiinput(&rt);  
  runtime_setInput(&rt,dbrec0);   
  runtime_setup_multiinput(&rt);  

  singleton_ndb_create(runtime_getMultiInput(&rt,runtime_getBinMode(&rt)),ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"singleton ndb has %d unique records\n",sz);
  
  /* use online query instead on recset_query */
  /* online_query returns true if the string is not in the database */
  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(19);
  }

  /* NOTE: if either of these fail, double check rstree_query_match
     results using recset_query_match_slow in recset_query_match */

  if(online_query(dbrec0,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec0);
    exit(29);
  }

#if 0 && (defined(NORANDSATLENLIMIT) || defined(ISCHARDQR))
  fprintf(stderr,"bypassing superfluous tests\n");
#else
 {
  Rec * rec1 = rec_create(numunits);
  rec_string2rec(rec1,lenarg,dbrec1,runtime_getBinMode(&rt));

  /* test hamming distance of one (1 bit difference) is not in DB by mistake */
  for (i = 0; i < lenarg; i++) {
    rec_setbit(rec1,i,0,numunits);
    if(!recset_query(ndb,rec1)){
      fprintf(stderr,"oops, this ALL ONES record with bit <%d> ZERO was found in DB---not in RNDB\n",i);
      rec_print(rec1,lenarg,stderr);
      rec_destroy(rec1);
      exit(6);
    }
    rec_setbit(rec1,i,1,numunits);
  }

  rec_init(rec1,numunits);
  rec_string2rec(rec1,lenarg,dbrec0,runtime_getBinMode(&rt));
  for (i = 0; i < lenarg; i++) {
    rec_setbit(rec1,i,1,numunits);
    if(!recset_query(ndb,rec1)){
      fprintf(stderr,"oops, this ALL ZEROS record with bit <%d> ONE was found in DB---not in RNDB\n",i);
      rec_print(rec1,lenarg,stderr);
      rec_destroy(rec1);
      exit(6);
    }
    rec_setbit(rec1,i,0,numunits);
  }
  
  rec_destroy(rec1);
 }
#endif
    
  if(!recset_ischanged(ndb)) {
    fprintf(stderr,"oops, ndb WAS changed, should really save\n");
    exit(42);
  }

  recset_destroy(ndb);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime singleton double test\n");
}


void test_runtime_ascii()
{
  Runtime rt;
  Rec * rec1;
  int sz;
  int len = 24;
  Recset * ndb = recset_create();
  char * dbrec1 = "a  ";

  fprintf(stderr,"starting test_runtime_ascii..\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  runtime_setMinBits(&rt,8);
  runtime_setMinBitsSpecifiedFlag(&rt);

  recset_setlength(ndb,len); 

  rec1 = rec_create(recset_numunits(ndb));
  rec_string2rec(rec1,len,dbrec1,runtime_getBinMode(&rt));

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);
  
 if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--already in empty DB\n",dbrec1);
    exit(61);
  }

  online_add(dbrec1,ndb,&rt);  

  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(62);
  }

  if(!recset_ischanged(ndb)) {
    fprintf(stderr,"oops, ndb was changed, should save\n");
    exit(63);
  }

  online_remove(dbrec1,ndb,&rt);

  if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--in DB after just removed\n",dbrec1);
    exit(64);
  }

  recset_destroy(ndb);
  rec_destroy(rec1);

  fprintf(stdout,"completed runtime ascii test\n");
}


void test_runtime_ascii2()
{
  Runtime rt;
  int sz;
  int len = 8;
  Recset * ndb = recset_create();
  char * dbrec1 = "a";
  char * dbrec2 = "b";
  char * rndbname = "rndb-ascii2test.txt";

  fprintf(stderr,"starting test_runtime_ascii2..\n");

  runtime_init(&rt);
  runtime_seedRandom(&rt,333);
  runtime_setMinBits(&rt,3);
  runtime_setMinBitsSpecifiedFlag(&rt);

  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);

 if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--already in empty DB\n",dbrec1);
    exit(61);
  }

 /* recset_tree_clear(ndb); */
#if 1
  recset_save(ndb,&rt,rndbname);
  recset_save(ndb,&rt,"rndb-ascii2-new.txt");
  recset_destroy(ndb);
  ndb = recset_create();
  recset_build(ndb,&rt,rndbname,ndbformat);
  recset_save(ndb,&rt,"rndb-ascii2-newcopy.txt");
  recset_tree_save(ndb,"rndb-ascii2-newcopytree.txt");
#endif

  runtime_seedRandom(&rt,333);
  online_add(dbrec1,ndb,&rt);  

  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(62);
  }
  
  /*recset_tree_clear(ndb); */
#if 1
  recset_save(ndb,&rt,rndbname); 

  if(recset_ischanged(ndb)) {
    fprintf(stderr,"oops, ndb was changed, and saved\n");
    exit(63);
  }

  recset_destroy(ndb);
  ndb = recset_create();
  recset_build(ndb,&rt,rndbname,ndbformat);
#endif

  if(!online_query(dbrec2,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--really not in DB\n",dbrec2);
    fprintf(stderr,"oops, ndb size is %d,%d\n",recset_size(ndb),recset_tree_size(ndb));
    exit(66);
  }
  recset_destroy(ndb);
  fprintf(stdout,"completed runtime ascii test 2\n");
}


void test_runtime_ascii3()
{
  Runtime rt;
  int sz;
  int len = 8;
  Recset * ndb = recset_create();
  char * dbrec1 = "a";
  char * dbrec2 = "z";

  char * rndbname = "rndb-ascii3test.txt";

  fprintf(stderr,"starting test_runtime_ascii3..\n");

  runtime_init(&rt);
  runtime_seedRandom(&rt,333);
  runtime_setMinBits(&rt,3);
  runtime_setMinBitsSpecifiedFlag(&rt);

  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);

 if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--already in empty DB\n",dbrec1);
    exit(61);
  }

 online_add(dbrec1,ndb,&rt);

 if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB--already added\n",dbrec1);
    exit(62);
  }

 online_add(dbrec2,ndb,&rt);

 if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB--already added\n",dbrec1);
    exit(62);
  }

  online_remove(dbrec1,ndb,&rt);  

  if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB---just removed\n",dbrec1);
    exit(6);
  }
  
  recset_save(ndb,&rt,rndbname);
  recset_destroy(ndb);

  runtime_init(&rt); /* don't set the length or the minbits */
  ndb = recset_create();
  recset_build(ndb,&rt,rndbname,ndbformat);

  if(runtime_getMinBits(&rt) > 3) {
    fprintf(stderr,"oops, using %d for minbits in recset_build\n",runtime_getMinBits(&rt));
    exit(9);
  }

  recset_destroy(ndb);
  fprintf(stdout,"completed runtime ascii test 3\n");
}


void test_runtime_query()
{
  Runtime rt;
  Rec * rec1, * rec2;
  int sz, numunits;
  Recset * ndb;
  int len = 88;
  char * dbrec1 = "hello world";
  char * dbrec2 = "hello      ";

  fprintf(stderr,"starting test_runtime_query..\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  runtime_setMinBits(&rt,8);
  runtime_setMinBitsSpecifiedFlag(&rt);

  ndb = recset_create();
  recset_setlength(ndb,len); 
  numunits = recset_numunits(ndb);

  rec1 = rec_create(numunits);
  rec_string2rec(rec1,len,dbrec1,runtime_getBinMode(&rt));

  rec2 = rec_create(numunits);
  rec_string2rec(rec2,len,dbrec2,runtime_getBinMode(&rt));

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stdout,"empty db has %d unique records\n",sz);
  
 if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oopBs, record <%s> NOT found in RNDB--already in empty DB\n",dbrec1);
    exit(64);
  }

  online_add(dbrec1,ndb,&rt);  

  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(65);
  }

  online_add(dbrec2,ndb,&rt);  

  if(online_query(dbrec2,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec2);
    exit(66);
  }

  if(!recset_ischanged(ndb)) {
    fprintf(stderr,"oops, ndb was changed!\n");
    exit(67);
  }

  recset_save(ndb,&rt,runtime_getName(&rt));

  recset_destroy(ndb);
  rec_destroy(rec1);
  rec_destroy(rec2);
  
  fprintf(stdout,"query without knowing the length\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  
  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));

  fprintf(stdout, "Record Length is %d\n",recset_length(ndb));

  if(recset_length(ndb) != len) {
    fprintf(stdout, "Record Length is suppose to be 88\n");
    exit(7);
  }

  if (!recset_size(ndb)) {
    fprintf(stdout,"oops, no RNDB read..\n");
    exit(8);
  }

  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record [%s] found in RNDB--in DB\n",dbrec1);
    exit(68);
  }

  /* NO PADDING! does it pad properly */ 
  if(online_query(dbrec2,ndb,&rt)){
    fprintf(stderr,"oops, record [%s] found in RNDB--in DB\n",dbrec2);
    exit(69);
  }

  recset_destroy(ndb);
  fprintf(stdout,"completed runtime query test\n\n");
}


void test_runtime_query_partial()
{
  Runtime rt;
  Rec * rec1, * rec2, * rec3;
  int sz, numunits;
  Recset * ndb;
  int len = 24;
  char * dbrec1 = "jes";  /* simple scheme FN,occupation,LN */
  char * dbrec2 = "spf";
  char * dbrec3 = "eet";
  char * qrec1 = "*e*";    /* all the engineers 2 */
  char * qrec2 = "*f*";    /* all the farmers - none */
  char * qrec3 = "***";    /* all 3 */
  char * qrec7 = "esd";    /* not in db */
  char * qrec8 = "js";     /* project fn ln for engineers v.32 */
  char * qrec9 = "et";
  char * qrec10 = "sf";

  fprintf(stderr,"starting test_runtime_query_partial..\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  runtime_setMinBits(&rt,5);
  runtime_setMinBitsSpecifiedFlag(&rt);

  runtime_seedRandom(&rt,777);

  ndb = recset_create();
  recset_setlength(ndb,len); 
  numunits = recset_numunits(ndb);

  rec1 = rec_create(numunits);
  rec_string2rec(rec1,len,dbrec1,runtime_getBinMode(&rt));

  rec2 = rec_create(numunits);
  rec_string2rec(rec2,len,dbrec2,runtime_getBinMode(&rt));

  rec3 = rec_create(numunits);
  rec_string2rec(rec3,len,dbrec3,runtime_getBinMode(&rt));

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stdout,"empty db has %d unique records\n",sz);
  
 if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--already in empty DB\n",dbrec1);
    exit(164);
  }

  online_add(dbrec1,ndb,&rt);  

  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(165);
  }

  online_add(dbrec2,ndb,&rt);  

  if(online_query(dbrec2,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec2);
    exit(166);
  }

  online_add(dbrec3,ndb,&rt);  

  if(online_query(dbrec3,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec3);
    exit(163);
  }

  if(!recset_ischanged(ndb)) {
    fprintf(stderr,"oops, ndb was changed!\n");
    exit(167);
  }

  /* secondary output */
  runtime_setNameOutputFile(&rt,"rndb-1.cnf");
  runtime_setFormatOutputFile(&rt,"cnf");

  recset_save(ndb,&rt,runtime_getName(&rt));

  recset_destroy(ndb);
  rec_destroy(rec1);
  rec_destroy(rec2);
  rec_destroy(rec3);
  runtime_final(&rt);


  {  
    fprintf(stdout,"partial query without knowing the length\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"cnf");
    runtime_setName(&rt,"rndb-1.cnf");
    
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != len) {
      fprintf(stdout, "Record Length is suppose to be 24\n");
      exit(7);
    }
    
    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no RNDB read..\n");
      exit(8);
    }

    runtime_setCommand(&rt,'Q');

    if(online_query(qrec1,ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qrec1);
      exit(168);
    }
    
    recset_destroy(ndb);
    runtime_final(&rt);

    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
    
    /* online_query returns true if the string is not in the database */
    /* solution: jes, eet */
    if(online_query(dbrec1,ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [%s]\n",dbrec1);
      exit(41);
    }
    if(online_query(dbrec3,ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [%s]\n",dbrec3);
      exit(41);
    }
    /* online_query returns false if the string is in the database */
    if(!online_query(dbrec2,ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",dbrec2);
      exit(42);
    }    
    /* online_query returns false if the string is in the database */
    if(!online_query(qrec7,ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qrec7);
      exit(42);
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }


  {  
    fprintf(stdout,"partial query with cnf output\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != len) {
      fprintf(stdout, "Record Length is suppose to be 24\n");
      exit(10);
    }
    
    if (!recset_size(ndb)) {
      fprintf(stdout,"oops, no RNDB read..\n");
      exit(10);
    }
    
    runtime_setCommand(&rt,'Q'); 
    runtime_setNameOutputFile(&rt,"partial-ndb-1.cnf");
    runtime_setFormatOutputFile(&rt,"cnf");

    if(online_query(qrec1,ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qrec1);
      exit(168);
    }
    
    recset_destroy(ndb);
    runtime_final(&rt);
    fprintf(stdout,"TRY crack_cnf.pl partial-ndb-1.cnf to verify exactly 2 solutions: jes eet\n\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"cnf");

    ndb = recset_create();
    recset_build(ndb,&rt,"partial-ndb-1.cnf",runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != len) {
      fprintf(stdout, "Record Length of partial query cnf file is suppose to be 24\n");
      exit(6);
    }

    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no partial RNDB cnf file read..\n");
      exit(8);
    }

    if(online_query(dbrec1,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB that match [%s]\n",dbrec1);
      exit(14);
    }

    if(online_query(dbrec3,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB that match [%s]\n",dbrec3);
      exit(165);
    }
    
    if(!online_query(dbrec2,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB that match [%s]\n",dbrec2);
      exit(166);
    }

    if(!online_query(qrec7,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB that match [%s]\n",qrec7);
      exit(167);
    }

    recset_destroy(ndb);
    runtime_final(&rt);
  }

  
#if 1
  {
    fprintf(stdout,"partial query with unpresent string\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);

    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');
    runtime_setNameOutputFile(&rt,"partial-1.txt");

    if(online_query(qrec2,ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qrec2);
      exit(168);
    }
    
    recset_destroy(ndb);
    runtime_final(&rt);

    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,"partial-1.txt",ndbformat);
    
    recset_destroy(ndb);
  }
#endif

#if 1
  {
    fprintf(stdout,"partial query with all dontcares\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');

    if(online_query(qrec3,ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qrec3);
      exit(168);
    }
    
    recset_destroy(ndb);
    
    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
        
    /* online_query returns true if the string is not in the database */
    /* solution: jes, spf, eet */
    if(online_query(dbrec1,ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [%s]\n",dbrec1);
      exit(31);
    }
    if(online_query(dbrec2,ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [%s]\n",dbrec2);
      exit(31);
    }
    if(online_query(dbrec3,ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [%s]\n",dbrec3);
      exit(31);
    }
    /* online_query returns false if the string is in the database */
    if(!online_query(qrec7,ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qrec7);
      exit(32);
    }
    recset_destroy(ndb);
  }
#endif

#if 1
  {
    fprintf(stdout,"partial eq query with Project option in cnf format\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');
    runtime_setNameOutputFile(&rt,"partial-ndb-3.cnf");
    runtime_setFormatOutputFile(&rt,"cnf");

    runtime_setNegTheta(&rt,"eq");
    runtime_setProjOption(&rt);      /* v.32 */

    online_query(qrec1,ndb,&rt);

    recset_destroy(ndb);
    runtime_final(&rt);
    fprintf(stdout,"TRY crack_cnf.pl partial-ndb-3.cnf to verify exactly 2 solutions: js et\n\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"cnf");

    ndb = recset_create();
    recset_build(ndb,&rt,"partial-ndb-3.cnf",runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != 16) {
      fprintf(stdout, "Record Length of partial query 3 cnf file is suppose to be 16\n");
      exit(6);
    }

    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no partial RNDB cnf file read..\n");
      exit(8);
    }

    if(online_query(qrec8,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec8);
      exit(14);
    }

    if(online_query(qrec9,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec9);
      exit(165);
    }
    
    if(!online_query(qrec10,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec10);
      exit(166);
    }

    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif

#if 1
  {
    fprintf(stdout,"partial eq query with Project option in ndb format\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');
    runtime_setNameOutputFile(&rt,"partial-ndb-3.txt");
    runtime_setFormatOutputFile(&rt,"ndb");

    runtime_setNegTheta(&rt,"eq");
    runtime_setProjOption(&rt);      /* v.32 */

    online_query(qrec1,ndb,&rt);

    recset_destroy(ndb);
    runtime_final(&rt);
    fprintf(stdout,"TRY crack_ndb.pl partial-ndb-3.txt to verify exactly 2 solutions: js et\n\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"ndb");

    ndb = recset_create();
    recset_build(ndb,&rt,"partial-ndb-3.txt",runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != 16) {
      fprintf(stdout, "Record Length of partial query 3 ndb file is suppose to be 16\n");
      exit(6);
    }

    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no partial RNDB ndb file read..\n");
      exit(8);
    }

    if(online_query(qrec8,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec8);
      exit(14);
    }

    if(online_query(qrec9,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec9);
      exit(165);
    }
    
    if(!online_query(qrec10,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec10);
      exit(166);
    }

    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif


#if 1
  {
    fprintf(stdout,"partial eq query with Project option in ndb format\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');
    runtime_setNameOutputFile(&rt,"partial-ndb-3.txt");
    runtime_setFormatOutputFile(&rt,"ndb");

    runtime_setNegTheta(&rt,"eq");
    runtime_setProjOption(&rt);      /* v.32 */

    online_query(qrec1,ndb,&rt);

    recset_destroy(ndb);
    runtime_final(&rt);
    fprintf(stdout,"TRY crack_ndb.pl partial-ndb-3.txt to verify exactly 2 solutions: js et\n\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"ndb");

    ndb = recset_create();
    recset_build(ndb,&rt,"partial-ndb-3.txt",runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != 16) {
      fprintf(stdout, "Record Length of partial query 3 ndb file is suppose to be 16\n");
      exit(6);
    }

    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no partial RNDB ndb file read..\n");
      exit(8);
    }

    if(online_query(qrec8,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec8);
      exit(14);
    }

    if(online_query(qrec9,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec9);
      exit(165);
    }
    
    if(!online_query(qrec10,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec10);
      exit(166);
    }

    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif

  /* as of v.32 the Project option doesn't work with ascii */
#if 0
  {
    fprintf(stdout,"partial lt query with Project option\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');
    runtime_setNameOutputFile(&rt,"partial-ndb-4.cnf");
    runtime_setFormatOutputFile(&rt,"cnf");

    runtime_setNegTheta(&rt,"lt");
    runtime_setProjOption(&rt);      /* v.32 */

    online_query(qrec2,ndb,&rt);

    recset_destroy(ndb);
    runtime_final(&rt);
    fprintf(stdout,"TRY crack_cnf.pl partial-ndb-4.cnf to verify exactly 2 solutions: js et\n\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"cnf");

    ndb = recset_create();
    recset_build(ndb,&rt,"partial-ndb-4.cnf",runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != 16) {
      fprintf(stdout, "Record Length of partial query 4 cnf file is suppose to be 16\n");
      exit(6);
    }

    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no partial RNDB cnf file read..\n");
      exit(8);
    }

    if(online_query(qrec8,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 4 that match [%s]\n",qrec8);
      exit(14);
    }

    if(online_query(qrec9,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 4 that match [%s]\n",qrec9);
      exit(165);
    }
    
    if(!online_query(qrec10,ndb,&rt)){
      fprintf(stderr,"oops, records found in partial NDB 4 that match [%s]\n",qrec10);
      exit(166);
    }

    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif

  test_runtime_copyproject(); /* must come after query_partial */

  fprintf(stdout,"completed runtime partial query test\n\n");
}


/* uses RNDB.txt built in test_runtime_partial test: contains 3
   solutions: jes, spf, eet; after copyproject the result is
   everything except js, et */
void test_runtime_copyproject()
{
  Runtime rt;
  Recset * ndb;
  int sz;
  char * qrec1 = "*e*";    /* all the engineers 2 */
  char * qrec8 = "js";     /* project fn ln for engineers v.32 */
  char * qrec9 = "et";
  char * qrec10 = "sf";
  fprintf(stdout,"copyproject with Project option in ndb format\n");

  runtime_init(&rt);
    
  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));

  runtime_setCommand(&rt,'U');
  online_complement(ndb,&rt);

  recset_destroy(ndb);
  runtime_final(&rt);

  runtime_init(&rt);
  runtime_setName(&rt,"un_NDB.txt");

  runtime_setCommand(&rt,'P');
  runtime_setNameOutputFile(&rt,"partial-ndb-3p.txt");
  runtime_setFormatOutputFile(&rt,"ndb");
  
  runtime_setProjOption(&rt);      /* v.65.2 */

  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));

  online_copyproject(qrec1,ndb,&rt);

  recset_destroy(ndb);
  runtime_final(&rt);
  
  fprintf(stdout,"TRY cat partial-ndb-3p.txt to see exactly 2 solutions (in binary): js et\n\n");

  runtime_init(&rt);

  ndb = recset_create();
  recset_build(ndb,&rt,"partial-ndb-3p.txt",runtime_getFormat(&rt));
    
  fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
  
  if(recset_length(ndb) != 16) {
    fprintf(stdout, "Record Length of partial query 3 ndb file is suppose to be 16\n");
    exit(6);
  }

  if (! (sz = recset_size(ndb))) {
    fprintf(stdout,"oops, no partial RNDB ndb file read..\n");
    exit(8);
  }

  if (sz != 2) {
    fprintf(stdout,"oops, this partial ndb file should have 2 records exactly..\n");
    exit(7);
  }

  /* reverse the meaning of the queries here */

  if(! online_query(qrec8,ndb,&rt)){
    fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec8);
    exit(14);
  }

  if(! online_query(qrec9,ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 3 that match [%s]\n",qrec9);
      exit(165);
    }
    
  if(online_query(qrec10,ndb,&rt)){
    fprintf(stderr,"oops, records found in partial NDB 3 that match [%s]\n",qrec10);
    exit(166);
  }

  recset_destroy(ndb);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime copyproject test\n\n");
}


void test_runtime_query_partial_binary()
{
  Runtime rt;
  Rec * rec1, * rec2, * rec3;
  int sz, numunits;
  Recset * ndb;
  int len = 3;
  char * dbrec1 = "001";  
  char * dbrec2 = "100";
  char * dbrec3 = "011";
  char * qrec1 = "0*1";   /* was *0* */
  char * qrec2 = "*00";   /* v.32 */

  fprintf(stderr,"starting test_runtime_query_partial_binary..\n");

  runtime_init(&rt);
  runtime_setN(&rt,0);
  runtime_setMinBits(&rt,2);
  runtime_setMinBitsSpecifiedFlag(&rt);

  runtime_seedRandom(&rt,333);
  runtime_setBinMode(&rt);

  ndb = recset_create();
  recset_setlength(ndb,len); 
  numunits = recset_numunits(ndb);

  rec1 = rec_create(numunits);
  rec_string2rec(rec1,len,dbrec1,runtime_getBinMode(&rt));

  rec2 = rec_create(numunits);
  rec_string2rec(rec2,len,dbrec2,runtime_getBinMode(&rt));

  rec3 = rec_create(numunits);
  rec_string2rec(rec3,len,dbrec3,runtime_getBinMode(&rt));

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stdout,"empty db has %d unique records\n",sz);
  
 if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--already in empty DB\n",dbrec1);
    exit(164);
  }

  online_add(dbrec1,ndb,&rt);  

  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(165);
  }

  online_add(dbrec2,ndb,&rt);  

  if(online_query(dbrec2,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec2);
    exit(166);
  }

  online_add(dbrec3,ndb,&rt);  

  if(online_query(dbrec3,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec3);
    exit(163);
  }

  if(!recset_ischanged(ndb)) {
    fprintf(stderr,"oops, ndb was changed!\n");
    exit(167);
  }

  /* secondary output */
  runtime_setNameOutputFile(&rt,"rndb-2.cnf");
  runtime_setFormatOutputFile(&rt,"cnf");

  recset_save(ndb,&rt,runtime_getName(&rt));

  recset_destroy(ndb);
  rec_destroy(rec1);
  rec_destroy(rec2);
  rec_destroy(rec3);
  runtime_final(&rt);


  {  
    fprintf(stdout,"partial binary query without knowing the length\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"cnf");
    runtime_setName(&rt,"rndb-2.cnf");
    runtime_setBinMode(&rt);
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != len) {
      fprintf(stdout, "Record Length is suppose to be 3\n");
      exit(7);
    }
    
    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no RNDB read..\n");
      exit(8);
    }

    runtime_setCommand(&rt,'Q');

    if(online_query(qrec1,ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qrec1);
      exit(168);
    }
    
    recset_destroy(ndb);
    runtime_final(&rt);

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
        
    /* online_query returns true if the string is not in the database */
    /* solution: 001,011 */
    if(online_query(dbrec1,ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [%s]\n",dbrec1);
      exit(6);
    }
    if(online_query(dbrec3,ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [%s]\n",dbrec3);
      exit(61);
    }
    /* online_query returns false if the string is in the database */
    if(!online_query(dbrec2,ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",dbrec2);
      exit(62);
    }    
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  {  
    fprintf(stdout,"partial binary query with cnf output\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != len) {
      fprintf(stdout, "Record Length is suppose to be 3\n");
      exit(7);
    }
    
    if (!recset_size(ndb)) {
      fprintf(stdout,"oops, no RNDB read..\n");
      exit(8);
    }
    
    runtime_setCommand(&rt,'Q'); 
    runtime_setNameOutputFile(&rt,"partial-ndb-2.cnf");
    runtime_setFormatOutputFile(&rt,"cnf");

    if(online_query(qrec1,ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qrec1);
      exit(168);
    }
    
    recset_destroy(ndb);
    runtime_final(&rt);
    fprintf(stdout,"TRY crack_cnf.pl -b partial-ndb-2.cnf to verify exactly 2 solutions: 011 and 001\n\n");


#if 1
  {
    fprintf(stdout,"partial eq inary query with Project option in cnf format\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');
    runtime_setNameOutputFile(&rt,"partial-ndb-5.cnf");
    runtime_setFormatOutputFile(&rt,"cnf");

    runtime_setNegTheta(&rt,"eq");
    runtime_setProjOption(&rt);      /* v.32 */

    online_query(qrec2,ndb,&rt);

    recset_destroy(ndb);
    runtime_final(&rt);
    fprintf(stdout,"TRY crack_cnf.pl partial-ndb-5.cnf to verify exactly 1 solution: 1\n\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"cnf");
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,"partial-ndb-5.cnf",runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != 1) {
      fprintf(stdout, "Record Length of partial query 5 cnf file is suppose to be 1\n");
      exit(6);
    }

    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no partial RNDB cnf file read..\n");
      exit(8);
    }

    if(online_query("1",ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 5 that match [1]\n");
      exit(14);
    }

    if(!online_query("0",ndb,&rt)){
      fprintf(stderr,"oops, records found in partial NDB 5 that match [0]\n");
      exit(165);
    }
    
    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif

#if 1
  {
    fprintf(stdout,"partial eq binary query with Project option in ndb format\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');
    runtime_setNameOutputFile(&rt,"partial-ndb-5.txt");
    runtime_setFormatOutputFile(&rt,"ndb");

    runtime_setNegTheta(&rt,"eq");
    runtime_setProjOption(&rt);      /* v.32 */

    online_query(qrec2,ndb,&rt);

    recset_destroy(ndb);
    runtime_final(&rt);
    fprintf(stdout,"TRY crack_ndb.pl partial-ndb-5.txt -b to verify exactly 1 solution: 1\n\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"ndb");
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,"partial-ndb-5.txt",runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != 1) {
      fprintf(stdout, "Record Length of partial query 5 ndb file is suppose to be 1\n");
      exit(6);
    }

    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no partial RNDB ndb file read..\n");
      exit(8);
    }

    if(online_query("1",ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 5 that match [1]\n");
      exit(14);
    }

    if(!online_query("0",ndb,&rt)){
      fprintf(stderr,"oops, records found in partial NDB 5 that match [0]\n");
      exit(165);
    }
    
    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif


  /* as of v.32 the Project option doesn't work with non equal partial queries
     when there are bits to the left of the query string */
#if 0
  {
    fprintf(stdout,"partial gt binary query with Project option\n");

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setBinMode(&rt);
    
    ndb = recset_create();
    recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));
    
    runtime_setCommand(&rt,'Q');
    runtime_setNameOutputFile(&rt,"partial-ndb-6.cnf");
    runtime_setFormatOutputFile(&rt,"cnf");

    runtime_setNegTheta(&rt,"gt");
    runtime_setProjOption(&rt);      /* v.32 */

    online_query(qrec2,ndb,&rt);

    recset_destroy(ndb);
    runtime_final(&rt);

    runtime_init(&rt);
    runtime_setN(&rt,1);
    runtime_setFormat(&rt,"cnf");
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,"partial-ndb-6.cnf",runtime_getFormat(&rt));
    
    fprintf(stdout, "Record Length is %d\n",recset_length(ndb));
    
    if(recset_length(ndb) != 1) {
      fprintf(stdout, "Record Length of partial query 6 cnf file is suppose to be 1\n");
      exit(6);
    }

    if (! (sz = recset_size(ndb))) {
      fprintf(stdout,"oops, no partial RNDB cnf file read..\n");
      exit(8);
    }

    if(online_query("0",ndb,&rt)){
      fprintf(stderr,"oops, no records found in partial NDB 6 that match [0]\n");
      exit(14);
    }
    
    if(!online_query("1",ndb,&rt)){
      fprintf(stderr,"oops, records found in partial NDB 6 that match [1]\n");
      exit(166);
    }

    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif

    fprintf(stdout,"completed runtime partial query binary test\n\n");
  }
}


void test_runtime_query_partial_binary2()
{
  Runtime rt;
  Recset * ndb;
  int i;
  int len = 3;             /* even 2-bit test fails when len=3 */ 
  int endval = 1 << len; 
  char * qrec1 = "**1";
  char * qrec0 = "**0";

  fprintf(stderr,"starting test_runtime_query_partial_binary2..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,2);  /* absolutely needed for even 2-bit test to work */
  runtime_setMinBitsSpecifiedFlag(&rt);

  ndb = recset_create();
  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt);

  /* create a negative db with the odd binary numbers from 1 to 8 */
  for (i = 1; i <= endval; i+=2) {
    char str[9];
    itobs(i,str,len);
    fprintf(stderr,"--------------\nadd rec %d: %s.\n",i,str);
    online_add(str,ndb,&rt);
  }

  recset_save(ndb,&rt,runtime_getName(&rt));

  runtime_setProjOption(&rt);

  runtime_setNameOutputFile(&rt,"rndb-odd-partial.txt");
  runtime_setFormatOutputFile(&rt,"ndb");

  online_query(qrec1,ndb,&rt);    /* partial query for odd numbers*/

  runtime_setNameOutputFile(&rt,"rndb-even-partial.txt");
  online_query(qrec0,ndb,&rt);    /* partial query for evens */

  runtime_clearProjOption(&rt);
  runtime_setNameOutputFile(&rt,"rndb-even-partial-noproj.txt");
  online_query(qrec0,ndb,&rt);    /* partial query evens, without project */

  recset_destroy(ndb);
  runtime_final(&rt);
  
  /* query for all 2-bit numbers */
  runtime_init(&rt);
  runtime_setFormat(&rt,"ndb");
  runtime_setName(&rt,"rndb-odd-partial.txt");
    
  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));

  len = recset_length(ndb);
  endval = 1 << len;  /* 4 */

  for(i=0; i < endval; i++) {
    char qstr[9];
    itobs(i,qstr,len);
    if(online_query(qstr,ndb,&rt)) {
      fprintf(stderr,"oops, record found in RNDB that matches [%s]\n",qstr);
      exit(16);
    }
  }

  recset_destroy(ndb);
  runtime_setName(&rt,"rndb-even-partial-noproj.txt");
    
  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));

  len = recset_length(ndb);
  endval = 1 << len;  /* 8 */

  /* query for no 3-bit solutions */
  for(i=0; i < endval; i++) {
    char qstr[9];
    itobs(i,qstr,len);
    if(!online_query(qstr,ndb,&rt)) {
      fprintf(stderr,"oops, record found in DB that matches [%s]\n",qstr);
      exit(15);
    }
  }

  recset_destroy(ndb);
  runtime_setName(&rt,"rndb-even-partial.txt");
    
  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),runtime_getFormat(&rt));

  len = recset_length(ndb);
  endval = 1 << len;  /* 4 */

  /* query for no 2-bit solutions */
  for(i=0; i < endval; i++) {
    char qstr[9];
    itobs(i,qstr,len);
    if(!online_query(qstr,ndb,&rt)) {
      fprintf(stderr,"oops, record found in DB that matches [%s]\n",qstr);
      exit(14);
    }
  }

  recset_destroy(ndb);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime binary test 2\n\n");
}


#define Q1 1
#define Q2 1
#define Q3 1
void test_runtime_select_theta_binary()
{
  Runtime rt;
  Recset * ndb;
  int numunits, i;
  char * qrec1 = "*****1*0";
  char * qrec2 = "*****100";   /* rec 4 from DB */
  char * qrec3 = "****1000";   /* rec 8 from DB */
  int len = 8;
  int endval = 8;
  char * rndb8name = "rndb-8.txt";

  fprintf(stderr,"starting runtime select theta binary tests..\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  runtime_seedRandom(&rt,777);
  runtime_setBinMode(&rt);

  ndb = recset_create();
  recset_setlength(ndb,len); 
  numunits = recset_numunits(ndb);

  fprintf(stderr,"numunits is %d.\n",numunits);

  empty_ndb_create(ndb,&rt); 

  /* create a negative db with all the binary numbers from 1 to 8 */
  for (i = 1; i <= endval; i++) {
    char str[9];
    itobs(i,str,len);
    fprintf(stderr,"--------------\nadd rec %d: %s.\n",i,str);
    online_add(str,ndb,&rt);
  }

  recset_save(ndb,&rt,rndb8name);
  recset_destroy(ndb);
  runtime_final(&rt);
  
  /* finished creating database with eight records to query */
#if Q2
  {
    fprintf(stdout,"\n+++++++select Neg Theta EQ binary test q2\n\n");  
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"ne"); /* same as Neg Equal */
    
    online_query(qrec2,ndb,&rt);   /* saves results in partial-ndb.txt */
    
    recset_destroy(ndb);
    runtime_final(&rt);
    
    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
    
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q2 eq %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt); 
      /* online_query returns true if the string is not in the database */
      /* solution: all but 4 is in the database, 4 is not in the database */
      if(q && (i!=4 && i!=0) ){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(161);
      }
      /* online_query returns false if the string is in the database */
      if(!q && (i==4 || i==0) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(2);
      }
    }  
    recset_destroy(ndb);
    runtime_final(&rt);
  }
  
  {
    /* NEG THETA LE  */
    fprintf(stdout,"\n++++++++select neg theta LE binary test q2\n\n");
    runtime_init(&rt);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"gt");  /* user point-of-view */

    online_query(qrec2,ndb,&rt);   /* saves results in partial-ndb.txt */
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q2 le %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution is 5,6,7 BUT NOT 8?? */
      if(q && (i==5 || i==6 || i==7) ){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(2);
      }
      /* online_query returns false if the string is in the database */
      if(!q && (i!=5 && i!=6 && i!=7) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(3);
      }
    }    
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  /* NEG THETA GE */
  {
    fprintf(stdout,"\n++++++++select neg theta GE binary test q2\n\n");
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');

    runtime_setNegTheta(&rt,"lt");  /* user point-of-view */

    online_query(qrec2,ndb,&rt);   /* saves results in partial-ndb.txt */

    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);

    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q2 ge %d\n",i);
      itobs(i,qstr,8);
      q = online_query(qstr,ndb,&rt); 
      /* online_query returns true if the string is not in the database */
      /* solution 1,2,3, AND 8 */
      if(q && ((i==1 || i==2 || i==3 || i==8) && i!=0)){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(3);
      }
      /* online_query returns false if the string is in the database */
      if(!q && ((i!=1 && i!=2 && i!=3 && i!=8) || i==0)){
        fprintf(stderr,"oops, no records found in RNDB that match [%s] %d\n",qstr,i);
        exit(4);
      }
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  /* NEG THETA NE */
  {
    fprintf(stdout,"\n++++++++select neg theta NE binary test q2\n\n");

    runtime_init(&rt);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"eq"); /* user point-of-view */

    online_query(qrec2,ndb,&rt);   /* saves results in partial-ndb.txt */

    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q2 ne %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution: only 4 is in the database */
      if(q && (i==4) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(4);
      }
      /* online_query returns false if the string is in the database */
      if(!q && (i!=4) ){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(5);
      }
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif

#if Q3
 {
    fprintf(stdout,"\n+++++++select Neg Theta EQ binary test q3\n\n");  
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    
    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);

    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"ne"); /* same as Neg Equal */
    
    online_query(qrec3,ndb,&rt);   /* saves results in partial-ndb.txt */
    
    recset_destroy(ndb);
    runtime_final(&rt);
    
    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
    
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q3 eq %d\n",i);
      itobs(i,qstr,8);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution is 1,2,3,4,5,6,7 */
      if(q && (i>=1 && i < 8 ) ){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(5);
      }
      /* online_query returns false if the string is in the database */
      if(!q && (i==8) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(6);
      }
    }  
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  {
    /* NEG THETA LE  */
    fprintf(stdout,"\n++++++++select neg theta LE binary test q3\n\n");
    runtime_init(&rt);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"gt");            /* user point-of-view */

    online_query(qrec3,ndb,&rt);   /* saves results in partial-ndb.txt */
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    for (i = 0; i <= endval; i++) {
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q3 le %d\n",i);
      itobs(i,qstr,len);
      /* online_query returns true if the string is not in the database */
      /* solution is empty  */
      /* online_query returns false if the string is in the database */
      if(!online_query(qstr,ndb,&rt) && (i>=0 && i<=8) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(7);
      }
    }    
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  /* NEG THETA GT */
  {
    fprintf(stdout,"\n++++++++select neg theta GT binary test q3\n\n");
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');

    runtime_setNegTheta(&rt,"le");  /* user point-of-view */

    online_query(qrec3,ndb,&rt);   /* saves results in partial-ndb.txt */

    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q3 gt %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution all 1-8 */
      if(q && ((i>=1 && i<=8) && i!=0)){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(167);
      }
      /* online_query returns false if the string is in the database */      
      if(!q && (i==0) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s] %d\n",qstr,i);
        exit(168);
      }
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  /* NEG THETA NE */
  {
    fprintf(stdout,"\n++++++++select neg theta NE binary test q3\n\n");

    runtime_init(&rt);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"eq");

    online_query(qrec3,ndb,&rt);   /* saves results in partial-ndb.txt */

    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q3 ne %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution only 8 */
      if(q && (i==8) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(9);
      }
      /* online_query returns false if the string is in the database */
      if(!q && (i!=8) ){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(8);
      }
    }

    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif

#if Q1
  {
    fprintf(stdout,"\n+++++++select Neg Theta EQ binary test q1\n\n");  
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"ne"); /* same as Neg Equal */
    
    online_query(qrec1,ndb,&rt);   /* saves results in partial-ndb.txt */
    
    recset_destroy(ndb);
    runtime_final(&rt);
    
    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
    
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q1 eq %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution: 1,2,3,5,7,8 (all but 4 and 6) are in the database */
      if(q && (i==1 || i==2 || i==3 || i==5 || i==7 || i==8 ) ){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(1);
      }
      /* online_query returns false if the string is in the database */
      if(!q && (i==0 || i==4 || i==6) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(2);
      }
    }  
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  {
    /* NEG THETA LE  */
    fprintf(stdout,"\n++++++++select neg theta LE binary test q1\n\n");
    runtime_init(&rt);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"gt");  /* user point-of-view */

    online_query(qrec1,ndb,&rt);   /* saves results in partial-ndb.txt */
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q1 le %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution is 5,7 BUT NOT 8 */
      if(q && (i==5 || i==7) ){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(2);
      }
      /* online_query returns false if the string is in the database */
      if(!q && (i!=5 && i!=7) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(3);
      }
    }    
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  /* NEG THETA GE */
  {
    fprintf(stdout,"\n++++++++select neg theta GE binary test q1\n\n");
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');

    runtime_setNegTheta(&rt,"lt");  /* user point-of-view */

    online_query(qrec1,ndb,&rt);   /* saves results in partial-ndb.txt */

    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q1 ge %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution 1,2,3, AND 8 */
      if(q && ((i==1 || i==2 || i==3 || i==8) && i!=0)){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(3);
      }
      /* online_query returns false if the string is in the database */
      if(!q && ((i!=1 && i!=2 && i!=3 && i!=8) || i==0)){
        fprintf(stderr,"oops, no records found in RNDB that match [%s] %d\n",qstr,i);
        exit(4);
      }
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  /* NEG THETA NE */
  {
    fprintf(stdout,"\n++++++++select neg theta NE binary test q1\n\n");

    runtime_init(&rt);
    runtime_setBinMode(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"eq"); /* user point-of-view */

    online_query(qrec1,ndb,&rt);   /* saves results in partial-ndb.txt */

    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setBinMode(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    for (i = 0; i <= endval; i++) {
      int q;
      char qstr[9];
      fprintf(stdout,"--------------\nneg theta select q1 ne %d\n",i);
      itobs(i,qstr,len);
      q = online_query(qstr,ndb,&rt);
      /* online_query returns true if the string is not in the database */
      /* solution is 4 and 6 */
      if(q && (i==4 || i==6) ){
        fprintf(stderr,"oops, no records found in RNDB that match [%s]\n",qstr);
        exit(5);
      }
      /* online_query returns false if the string is in the database */
      if(!q && (i!=4 && i!=6) ){
        fprintf(stderr,"oops, records found in RNDB that match [%s]\n",qstr);
        exit(4);
      }
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }
#endif

  fprintf(stdout,"completed runtime select theta binary tests\n\n");
}


void test_runtime_select_theta_ascii()
{
  Runtime rt;
  Recset * ndb;
  int numunits;
  char * qrec1 = "d";
  int len = 8;
  char * rndb8name = "rndb-8-letter.txt";

  fprintf(stderr,"starting runtime select theta ascii test..\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  runtime_seedRandom(&rt,555);

  ndb = recset_create();
  recset_setlength(ndb,len); 
  numunits = recset_numunits(ndb);

  empty_ndb_create(ndb,&rt); 

  online_add("a",ndb,&rt);
  online_add("d",ndb,&rt);
  online_add("h",ndb,&rt);
  online_add("j",ndb,&rt);

  recset_save(ndb,&rt,rndb8name);
  recset_destroy(ndb);
  runtime_final(&rt);
  
  /* finished creating database with 4 records to query */
  {
    /* NEG THETA LE  */
    fprintf(stdout,"\n++++++++select neg theta LE ascii test q1=d\n\n");
    runtime_init(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"gt");  /* user point-of-view */

    online_query(qrec1,ndb,&rt);   /* saves results in partial-ndb.txt */
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    /* online_query returns true if the string is not in the database */
    /* solution is h,j */
    if(online_query("h",ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [h]\n");
      exit(12);
    }
    if(online_query("j",ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [j]\n");
      exit(12);
    }

    /* online_query returns false if the string is in the database */
    if(!online_query("a",ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [a]\n");
      exit(13);
    }
    if(!online_query("d",ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [d]\n");
      exit(13);
    }
    
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  /* NEG THETA GE */
  {
    fprintf(stdout,"\n++++++++select neg theta GE ascii test q1=d\n\n");
    runtime_init(&rt);
    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');

    runtime_setNegTheta(&rt,"lt");  /* user point-of-view */

    online_query(qrec1,ndb,&rt);   /* saves results in partial-ndb.txt */
    
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);

    /* online_query returns true if the string is not in the database */
    /* solution a */
    if(online_query("a",ndb,&rt)){
      fprintf(stderr,"oops, records found in RNDB that match [a]\n");
      exit(14);
    }
    /* online_query returns false if the string is in the database */
    if(!online_query("d",ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [d]\n");
      exit(14);
    }
    if(!online_query("h",ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [h]\n");
      exit(14);
    }
    if(!online_query("j",ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [j]\n");
      exit(14);
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  fprintf(stdout,"completed runtime select theta ascii test\n\n");
}


void test_runtime_select_theta_ascii_2()
{
  Runtime rt;
  Recset * ndb;
  char * qrec1 = "c*";
  char * qrec2 = "*c";
  int len = 16;
  char * rndb8name = "rndb-8-2letters.txt";

  fprintf(stderr,"starting runtime select theta ascii test 2 letters..\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  runtime_seedRandom(&rt,111);

  ndb = recset_create();
  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt); 

  online_add("cd",ndb,&rt);
  online_add("dc",ndb,&rt);

  recset_save(ndb,&rt,rndb8name);
  recset_destroy(ndb);
  runtime_final(&rt);
  
  /* finished creating database with 2 records to query */
  {
    /* NEG THETA LE  */
    fprintf(stdout,"\n++++++++select neg theta LE ascii test 2 cd\n\n");
    runtime_init(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"gt");  /* user point-of-view */

    online_query("cd",ndb,&rt);   /* saves results in partial-ndb.txt */
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    /* online_query returns true if the string is not in the database */
    /* solution is dc */
    if(online_query("dc",ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [dc]\n");
      exit(15);
    }
    /* online_query returns false if the string is in the database */
    if(!online_query("cd",ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [cd]\n");
      exit(15);
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }


  /* finished creating database with 2 records to query */
  {
    /* NEG THETA LE  */
    fprintf(stdout,"\n++++++++select neg theta LE ascii test 2 q1\n\n");
    runtime_init(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"gt");  /* user point-of-view */

    online_query(qrec1,ndb,&rt);   /* saves results in partial-ndb.txt */
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    /* online_query returns true if the string is not in the database */
    /* solution is dc */
    if(online_query("dc",ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [dc]\n");
      exit(15);
    }
    /* online_query returns false if the string is in the database */
    if(!online_query("cd",ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [cd]\n");
      exit(15);
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  {
    /* NEG THETA LE  */
    fprintf(stdout,"\n++++++++select neg theta LE ascii test 2 q2\n\n");
    runtime_init(&rt);

    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');
    runtime_setNegTheta(&rt,"gt");  /* user point-of-view */

    online_query(qrec2,ndb,&rt);   /* saves results in partial-ndb.txt */
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);
  
    /* online_query returns true if the string is not in the database */
    /* solution is cd */
    if(online_query("cd",ndb,&rt) ){
      fprintf(stderr,"oops, records found in RNDB that match [cd]\n");
      exit(16);
    }
    /* online_query returns false if the string is in the database */
    if(!online_query("dc",ndb,&rt) ){
      fprintf(stderr,"oops, no records found in RNDB that match [dc]\n");
      exit(16);
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }
  
  /* NEG THETA GT */
  {
    fprintf(stdout,"\n++++++++select neg theta GE ascii test 2 q1\n\n");
    runtime_init(&rt);
    ndb = recset_create();
    recset_build(ndb,&rt,rndb8name,ndbformat);
    runtime_setCommand(&rt,'Q');

    runtime_setNegTheta(&rt,"le");  /* user point-of-view */

    online_query(qrec1,ndb,&rt);   /* saves results in partial-ndb.txt */
    
    recset_destroy(ndb);
    runtime_final(&rt);

    /* now query the resultant partial ndb */
    
    runtime_init(&rt);
    runtime_setCommand(&rt,'Q');

    ndb = recset_create();
    recset_build(ndb,&rt,PARTIALNDBNAME,ndbformat);

    /* online_query returns true if the string is not in the database */
    /* solution cd */
    if(online_query("cd",ndb,&rt)){
      fprintf(stderr,"oops, records found in RNDB that match [cd]\n");
      exit(17);
    }
    /* online_query returns false if the string is in the database */
    if(!online_query("dc",ndb,&rt)){
      fprintf(stderr,"oops, no records found in RNDB that match [dc]\n");
      exit(14);
    }
    recset_destroy(ndb);
    runtime_final(&rt);
  }

  fprintf(stdout,"completed runtime select theta ascii test 2 letters\n\n");
}


void test_runtime_join_binary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1a = "001";
  char * dbrec1b = "101";
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  char * jc1 = "1,2";
  char * jc2 = "0,1";
  int len = 3;

  fprintf(stdout,"test_runtime_join_binary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets to be joined */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1a,ndb1,&rt);
  if(online_query(dbrec1a,ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_binary: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec1b,ndb1,&rt);
  if(online_query(dbrec1b,ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_binary: rec2 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_join_binary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_join_binary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_join_binary: now for the join..\n");

  runtime_setRelOperator(&rt,"J");
  runtime_setOrder1(&rt,jc1);
  runtime_setOrder2(&rt,jc2);

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_join_binary: verify the join..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 4){
    fprintf(stderr,"test_runtime_join_binary: RNDB3 record length is %d, not 4\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  for(i = 0; i < 16 ; i++) {
    int q;
    char str[5];
    itobs(i,str,4);
    q = online_query(str,ndb1,&rt);
    if(!q && (i!=2 && i!=10) ) {
      fprintf(stderr,"test_runtime_join_binary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i == 2 || i==10 ) ) {
      fprintf(stderr,"test_runtime_join_binary: oops, record <%s> found in RNDB\n",str);
      exit(4);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_join_binary completed\n\n");
}


void test_runtime_join_ascii()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits;
  char * dbrec1a = "abc";
  char * dbrec1b = "cde";
  char * dbrec2a = "ac";
  char * dbrec2b = "de";
  char * jc1 = "2";
  char * jc2 = "1";
  char * jc2bad = "0";
  int len1 = 24;
  int len2 = 16;

  fprintf(stdout,"test_runtime_join_ascii starting..\n");

  runtime_init(&rt);

  /* create the two recsets to be joined */
  ndb1 = recset_create();
  recset_setlength(ndb1,len1);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1a,ndb1,&rt);
  if(online_query(dbrec1a,ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec1b,ndb1,&rt);
  if(online_query(dbrec1b,ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: rec2 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len2);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_join_ascii: now for the join..\n");

  runtime_setRelOperator(&rt,"J");
  runtime_setOrder1(&rt,jc1);
  runtime_setOrder2(&rt,jc2);

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_join_ascii: verify the join..\n");

  runtime_init(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  /* online_query returns false if the string is in the database */
  if(online_query("abca",ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: oops, [abca] present in RNDB3.txt\n");
    exit(3);
  }

  if(online_query("cded",ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: oops, [cded] present in RNDB3.txt\n");
    exit(3);
  }

  if(!online_query("abcd",ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: oops, [abcd] not present in RNDB3.txt\n");
    exit(3);
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_join_ascii: now for the empty join..\n");
  /* this test uses the previously built RNDB.txt and RNDB2.txt */

  runtime_init(&rt);
  runtime_setRelOperator(&rt,"J");
  runtime_setOrder1(&rt,jc1);
  runtime_setOrder2(&rt,jc2bad);

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_join_ascii: verify the empty join..\n");

  runtime_init(&rt);
  runtime_seedRandom(&rt,4);

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  fprintf(stdout,"test_runtime_join_ascii: note: verify with crack_ndb.pl that RNDB3 has no solutions\n"); 
  
  /* online_query returns false if the string is in the database */
  if(!online_query("abca",ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: oops, [abca] not present in RNDB3.txt\n");
    exit(4);
  }

  if(!online_query("abcc",ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: oops, [abcc] present in RNDB3.txt\n");
    exit(4);
  }

  if(!online_query("cdee",ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: oops, [cdee] present in RNDB3.txt\n");
    exit(4);
  }

  if(!online_query("cded",ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_ascii: oops, [cded] present in RNDB3.txt\n");
    exit(4);
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_join_ascii completed\n\n");
}


void test_runtime_intersection_binary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1a = "001";
  char * dbrec1b = "101";
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;

  fprintf(stdout,"test_runtime_intersection_binary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets to be intersected */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1a,ndb1,&rt);
  if(online_query(dbrec1a,ndb1,&rt)){
    fprintf(stderr,"test_runtime_intersection_binary: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec1b,ndb1,&rt);
  if(online_query(dbrec1b,ndb1,&rt)){
    fprintf(stderr,"test_runtime_intersection_binary: rec2 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_intersection_binary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_intersection_binary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_intersection_binary: now for the intersection..\n");

  runtime_setRelOperator(&rt,"I");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_intersection_binary: verify the intersection..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_intersection_binary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 1) ) {
      fprintf(stderr,"test_runtime_intersection_binary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && i==1) {
      fprintf(stderr,"test_runtime_intersection_binary: oops, record <%s> found in RNDB\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_intersection_binary completed\n\n");
}


void test_runtime_crossproduct_binary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1a = "001";
  char * dbrec1b = "101";
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;
  int len2 = 6;

  fprintf(stdout,"test_runtime_crossproduct_binary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets to be crossed */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1a,ndb1,&rt);
  if(online_query(dbrec1a,ndb1,&rt)){
    fprintf(stderr,"test_runtime_crossproduct_binary: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec1b,ndb1,&rt);
  if(online_query(dbrec1b,ndb1,&rt)){
    fprintf(stderr,"test_runtime_crossproduct_binary: rec2 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_crossproduct_binary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_crossproduct_binary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_crossproduct_binary: now for the crossproduct..\n");

  runtime_setRelOperator(&rt,"C");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_crossproduct_binary: verify the crossproduct..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len2){
    fprintf(stderr,"test_runtime_crossproduct_binary: RNDB3 record length is %d, not 6\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  for(i = 0; i < 64 ; i++) {
    int q;
    char str[7];
    itobs(i,str,len2);
    q = online_query(str,ndb1,&rt);
    if(!q && (i!=9 && i!=10 && i!=41 && i!=42) ) {
      fprintf(stderr,"test_runtime_crossproduct_binary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i == 9 || i==10 || i==41 || i == 42) ) {
      fprintf(stderr,"test_runtime_crossproduct_binary: oops, record <%s> found in RNDB\n",str);
      exit(4);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_crossproduct_binary completed\n\n");
}


void test_runtime_crossproduct_ascii()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits;
  char * dbrec1a = "abc";
  char * dbrec1b = "def";
  char * dbrec2a = "xyz";
  char * dbrec2b = "123";
  int len = 24;

  fprintf(stdout,"test_runtime_crossproduct_ascii starting..\n");

  runtime_init(&rt);

  /* create the two recsets to be crossed */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1a,ndb1,&rt);
  if(online_query(dbrec1a,ndb1,&rt)){
    fprintf(stderr,"test_runtime_crossproduct_ascii: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec1b,ndb1,&rt);
  if(online_query(dbrec1b,ndb1,&rt)){
    fprintf(stderr,"test_runtime_crossproduct_ascii: rec2 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_crossproduct_ascii: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_crossproduct_ascii: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_crossproduct_ascii: now for the crossproduct..\n");

  runtime_setRelOperator(&rt,"C");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_crossproduct_ascii: verify the crossproduct..\n");

  runtime_init(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 48){
    fprintf(stderr,"test_runtime_crossproduct_ascii: RNDB3 record length is %d, not 48\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  if(online_query("abcxyz",ndb1,&rt) ) {
    fprintf(stderr,"test_runtime_crossproduct_ascii: oops, record [abcxyz] found in RNDB---not in DB\n");
    exit(3);
  }

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_crossproduct_ascii completed\n\n");
}


void test_runtime_join_ends_binary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i, endloop;
  char * dbrec1a = "001";
  char * dbrec1b = "101";
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;
  int len2 = 6;

  fprintf(stdout,"test_runtime_join_ends_binary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets to be joined */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1a,ndb1,&rt);
  if(online_query(dbrec1a,ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_binary_end: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec1b,ndb1,&rt);
  if(online_query(dbrec1b,ndb1,&rt)){
    fprintf(stderr,"test_runtime_join_binary_end: rec2 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_join_ends_binary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_join_ends_binary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_join_ends_binary: now for the join..with null join conditions\n");

  runtime_setRelOperator(&rt,"J");
  runtime_setOrder1(&rt,"");
  runtime_setOrder2(&rt,"");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_join_ends_binary: verify the join..same as cross product\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len2){
    fprintf(stderr,"test_runtime_join_ends_binary: RNDB3 record length is %d, not 6\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  for(i = 0; i < 64 ; i++) {
    int q;
    char str[7];
    itobs(i,str,len2);
    q = online_query(str,ndb1,&rt);
    if(!q && (i!=9 && i!=10 && i!=41 && i!=42) ) {
      fprintf(stderr,"test_runtime_join_ends_binary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i == 9 || i==10 || i==41 || i == 42) ) {
      fprintf(stderr,"test_runtime_join_ends_binary: oops, record <%s> found in RNDB\n",str);
      exit(3);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"\ntest_runtime_join_ends_binary: now for the join..with full join conditions\n");

  runtime_setRelOperator(&rt,"J");
  runtime_setOrder1(&rt,"0-2");
  runtime_setOrder2(&rt,"0-2");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_join_ends_binary: verify the join..same as intersection\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_join_ends_binary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  endloop = 1 << len;   /* 8 */

  /* online_query returns false if the string is in the database */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 1) ) {
      fprintf(stderr,"test_runtime_join_ends_binary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && i==1) {
      fprintf(stderr,"test_runtime_join_ends_binary: oops, record <%s> found in RNDB\n",str);
      exit(3);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_join_ends_binary completed\n\n");
}


void test_runtime_union_binary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1a = "001";
  char * dbrec1b = "101";
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;

  fprintf(stdout,"test_runtime_union_binary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets to be unioned */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1a,ndb1,&rt);
  if(online_query(dbrec1a,ndb1,&rt)){
    fprintf(stderr,"test_runtime_union_binary: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec1b,ndb1,&rt);
  if(online_query(dbrec1b,ndb1,&rt)){
    fprintf(stderr,"test_runtime_union_binary: rec2 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_union_binary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_union_binary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_union_binary: now for the union..\n");

  runtime_setRelOperator(&rt,"U");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_union_binary: verify the union..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_union_binary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i!=1 && i!=2 && i!=5) ) {
      fprintf(stderr,"test_runtime_union_binary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if( q && (i==1 || i==2 || i==5) ) {
      fprintf(stderr,"test_runtime_union_binary: oops, record <%s> found in RNDB\n",str);
      exit(3);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_union_binary completed\n\n");
}


void test_runtime_difference_binary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1a = "001";
  char * dbrec1b = "101";
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;

  fprintf(stdout,"test_runtime_difference_binary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets to be diff'ed */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1a,ndb1,&rt);
  if(online_query(dbrec1a,ndb1,&rt)){
    fprintf(stderr,"test_runtime_difference_binary: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec1b,ndb1,&rt);
  if(online_query(dbrec1b,ndb1,&rt)){
    fprintf(stderr,"test_runtime_difference_binary: rec2 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_difference_binary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_difference_binary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_difference_binary: now for the difference..\n");

  runtime_setRelOperator(&rt,"D");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_difference_binary: verify the difference..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_difference_binary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* answer has only the solutinos in DB1 that are not in DB2, {101}*/
  /* online_query returns false if the string is in the database */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i!=5) ) {
      fprintf(stderr,"test_runtime_difference_binary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if( q && (i==5) ) {
      fprintf(stderr,"test_runtime_difference_binary: oops, record <%s> found in RNDB\n",str);
      exit(3);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_difference_binary completed\n\n");
}


void test_runtime_complete8bit()
{
  Runtime rt;
  Rec * rec1;
  int sz;
  int i;
  int endtest;
  int len = 8;
  Recset * ndb = recset_create();
  char * dbrec1 = "00000000";
  char str[9];

  fprintf(stderr,"starting test_runtime_complete8bit..\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  runtime_setMinBits(&rt,3);
  runtime_setMinBitsSpecifiedFlag(&rt);

  runtime_setBinMode(&rt); 
  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);

  rec1 =  rec_create(recset_numunits(ndb));
  rec_string2rec(rec1,len,dbrec1,runtime_getBinMode(&rt));
  
#if 1
 if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--already in empty DB\n",dbrec1);
    exit(70);
 }
 
 endtest = (1 << 8);      /* 256 */
 for(i = 0; i < endtest ; i++) {
   ctobs(i,str);
    if(!online_query(str,ndb,&rt)) {
      fprintf(stderr,"oops, record <%s> not found in RNDB---in DB\n",str);
      exit(29);
    }
  }

  online_add(dbrec1,ndb,&rt);  
  fprintf(stderr,"added rec [%s]\n",dbrec1);

  if(online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",dbrec1);
    exit(71);
  }

  for(i = 1; i < endtest ; i++) {
    ctobs(i,str);
    if(!online_query(str,ndb,&rt)) {
      fprintf(stderr,"oops, record <%s> not found in RNDB---in DB\n",str);
      exit(30);
    }
  }

  online_remove(dbrec1,ndb,&rt);
  fprintf(stderr,"removed rec [%s]\n",dbrec1);
  if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"oops, record <%s> NOT found in RNDB--in DB after just removed\n",dbrec1);
    exit(72);
  }
#endif

  for(i = 0; i < endtest ; i++) {
    ctobs(i,str);
    if(!online_query(str,ndb,&rt)) {
      fprintf(stderr,"oops, record <%s> not found in RNDB---in DB after 'a' just removed\n",
	      str);
      exit(31);
    }
  }
  
  recset_tree_save(ndb,"rndb-beforeclean-tree.txt");
  
  /*  recset_tree_clear(ndb);   v.15 */
  runtime_setCleanTests(&rt,1000); /* moved close to clean test v.67 */
  test_clean(ndb,&rt);
  
  recset_tree_save(ndb,"rndb-afterclean-tree.txt");
  
  recset_tree_clear(ndb);
  
  for(i = 0; i < endtest ; i++) {
    ctobs(i,str);
    if(!online_query(str,ndb,&rt)) {
      fprintf(stderr,"oops, record <%s> not found in RNDB---in DB after clean test\n",
              str);
      recset_tree_save(ndb,"rndb-afterclean-tree.txt");
      exit(32);
    }
  }

recset_destroy(ndb);
rec_destroy(rec1);

fprintf(stdout,"completed runtime complete 8bit test\n\n");
}


/* add a quarter DB full records to NDB, remove them, and verify the
   NDB is empty v.36.1 -- mike groat's test Fall 2006 */
void test_runtime_random_qtrfull_8bit(int sarg)
{
  Runtime rt;
  Perm P;
  int sz;
  int i;
  int endtest, qtrfull;
  char str[9];
  int len = 8;
  Recset * ndb = recset_create();


  fprintf(stderr,"starting test_runtime_random_qtrfull_8bit..\n");

  runtime_init(&rt);
  runtime_setN(&rt,1);
  runtime_setMinBits(&rt,3);
  runtime_setMinBitsSpecifiedFlag(&rt);
  runtime_setBinMode(&rt);
  runtime_seedRandom(&rt, sarg);

  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);

  endtest = (1 << 8);        /* 256 */
  qtrfull = (1 << 6);        /* 64 */

  perm_init(&P,endtest);
  perm_randomize(&P,qtrfull);

  /* add randomly chosen records so that DB is 1/4 full */
  for(i = 0; i < qtrfull ; i++) {
    int val = perm_get(&P,i);
    ctobs(val,str);

    /* verify not already "added" to NDB */
    if(!online_query(str,ndb,&rt)) {
      fprintf(stderr,"oops, record <%s> not found in RNDB---in DB\n",str);
      perm_final(&P);
      exit(29);
    }

    online_add(str,ndb,&rt);  
    fprintf(stderr,"added rec #%d [%s]\n",i,str);

    /* verify "added" to NDB */
    if(online_query(str,ndb,&rt)){
      fprintf(stderr,"oops, record <%s> found in RNDB---not in DB\n",str);
      exit(71);
    }
  }

  sz = recset_size(ndb);
  fprintf(stderr,"1/4 full db has %d unique negative records\n",sz);


  /* remove the records so that DB is empty again */
  for(i = 0; i < qtrfull ; i++) {
    int val = perm_get(&P,i);
    ctobs(val,str);

    online_remove(str,ndb,&rt);  
    fprintf(stderr,"removed rec #%d [%s]\n",i,str);

    /* verify "removed" from NDB */
    if(!online_query(str,ndb,&rt)) {
      fprintf(stderr,"oops, record <%s> not found in RNDB---in DB\n",str);
      perm_final(&P);
      exit(29);
    }
  }

  sz = recset_size(ndb);
  fprintf(stderr,"1/4 emptied db has %d unique negative records\n",sz);

  /* verify NDB is empty DB */
  for(i = 0; i < endtest ; i++) {
    ctobs(i,str);
    if(!online_query(str,ndb,&rt)) {
      fprintf(stderr,"oops, record <%s> not found in RNDB---in DB\n",str);
      exit(29);
    }
  }

  sz = recset_size(ndb);
  fprintf(stderr,"Verified 1/4 emptied db has %d unique negative records\n",sz);

  recset_destroy(ndb);
  perm_final(&P);

  fprintf(stdout,"completed runtime random qtrfull 8bit test\n\n");
}


void test_runtime_patterns()
{
  Runtime rt;
  Rec * rec1;
  int sz;
  int i;
  int endtest;
  int len = 8;
  Recset * ndb = recset_create();
  char * dbrec1 = "*******1";
  char * dbrec0 = "*******0";
  char * dbrec2 = "********";
  char str[9];

  fprintf(stderr,"starting test_runtime_patterns..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt); 

  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);

  rec1 =  rec_create(recset_numunits(ndb));
  rec_string2rec(rec1,len,dbrec1,runtime_getBinMode(&rt));
  
  endtest = (1 << len);      /* 256 */
  for(i = 0; i < endtest ; i++) {
    itobs(i,str,len);
    if(!online_query(str,ndb,&rt)) {
      fprintf(stderr,"oops, record <%s> not found in RNDB---in DB\n",str);
      exit(9);
    }
  }

  online_add(dbrec1,ndb,&rt);  
  fprintf(stderr,"added pattern rec [%s]\n",dbrec1);

  /* DB should contain all odd numbers */
  for(i = 0; i < endtest ; i++) {
    itobs(i,str,len);             /* does not reverse the bit order */
    if( online_query(str,ndb,&rt) && (i%2)) {
      fprintf(stderr,"test_runtime_patterns: oops, record <%s> (%d) wrong, found in RNDB--not in DB\n",str,i);
      exit(3);
    }
  }

 /* adding 0 after a 1 pattern, gives all combinations, a full DB*/

  online_add(dbrec0,ndb,&rt);  
  fprintf(stderr,"added pattern rec [%s]\n",dbrec0);

  for(i = 0; i < endtest ; i++) {
    itobs(i,str,len);
    if( online_query(str,ndb,&rt)) {
      fprintf(stderr,"test_runtime_patterns: oops, record <%s> (%d) wrong, not in DB\n",str,i);
      exit(19);
    }
  }


  /* removing 0 leaves a 1 pattern only */

  online_remove(dbrec0,ndb,&rt);  
  fprintf(stderr,"removing pattern rec [%s]\n",dbrec0);

  for(i = 0; i < endtest ; i++) {
    itobs(i,str,len);             /* does not reverse the bit order */
    if( online_query(str,ndb,&rt) && (i%2)) {
      fprintf(stderr,"test_runtime_patterns: oops, record <%s> (%d) wrong, not in DB\n",str,i);
      exit(13);
    }
  }

#if 1
 /* adding all *'s, makes NDB empty and DB completely full */
  online_add(dbrec2,ndb,&rt);  
  fprintf(stderr,"added pattern rec [%s]\n",dbrec2);

  for(i = 0; i < endtest ; i++) {
    itobs(i,str,len);
    if(online_query(str,ndb,&rt)) {
      fprintf(stderr,"test_runtime_patterns: oops, record <%s> found in RNDB---not in DB\n",str);
      exit(19);
    }
  }

  /* removing 1 pattern leaves only evens (0's) */

  online_remove(dbrec1,ndb,&rt);  
  fprintf(stderr,"removing pattern rec [%s]\n",dbrec1);

  for(i = 0; i < endtest ; i++) {
    itobs(i,str,len);             /* does not reverse the bit order */
    if( online_query(str,ndb,&rt) && !(i%2)) {
      fprintf(stderr,"test_runtime_patterns: oops, record <%s> (%d) wrong, not in DB\n",str,i);
      exit(15);
    }
  }

#endif

  recset_destroy(ndb);
  rec_destroy(rec1);

  fprintf(stdout,"completed runtime patterns test\n\n");
}


void test_runtime_patterns2()
{
  Runtime rt;
  int i, endtest;
  int len = 4;
  Recset * ndb = recset_create();
  char * dbrec1 = "1*1*";
  char * dbrec0 = "*0**";

  fprintf(stderr,"starting test_runtime_patterns2..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt); 

  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt);

  online_add(dbrec1,ndb,&rt);  
  fprintf(stderr,"added pattern rec [%s]\n",dbrec1);

  endtest = (1 << len);      

  /* DB should contain 1010, 1011, 1110, 1111 = 10,11,14,15 */
  for(i = 0; i < endtest ; i++) {
    int q;
    char str[9];
    itobs(i,str,len);             /* does not reverse the bit order */
    q = online_query(str,ndb,&rt);
    if( q && (i==10 || i==11 || i==14 || i==15)) {
      fprintf(stderr,"test_runtime_patterns: oops, record <%s> (%d) wrong, found in RNDB--not in DB\n",str,i);
      exit(3);
    }
    if( !q && (i!=10 && i!=11 && i!=14 && i!=15)) {
      fprintf(stderr,"test_runtime_patterns2: oops, record <%s> (%d) wrong, found in DB\n",str,i);
      exit(4);
    }
  }

  online_add(dbrec0,ndb,&rt);  
  fprintf(stderr,"added pattern rec [%s]\n",dbrec0);

  recset_save(ndb,&rt,runtime_getName(&rt));

  /* DB should contain all except 4,5,6,7 and 12,13 (*1**) */
  for(i = 0; i < endtest ; i++) {
    int q;
    char str[9];
    itobs(i,str,len);             /* does not reverse the bit order */
    q = online_query(str,ndb,&rt);
    if( q && (i<4 || (i>7 && i!=12 && i!=13)) ) {
      fprintf(stderr,"test_runtime_patterns: oops, record <%s> (%d) wrong, found in RNDB--not in DB\n",str,i);
      exit(5);
    }
    if( !q && (i==4 || i==5 || i==6 || i==7 || i==12 || i==13 )) {
      fprintf(stderr,"test_runtime_patterns2: oops, record <%s> (%d) wrong, found in DB\n",str,i);
      exit(6);
    }
  }

  recset_destroy(ndb);
  fprintf(stdout,"completed runtime patterns 2 test\n\n");
}


void test_runtime_pattern_sets()
{
  Runtime rt;
  int sz;
  int i;
  int endtest;
  int len = 3;
  Recset * ndb = recset_create();
  char * dbrec1 = "**1";
  char * dbrec4 = "1**";
  char * dbrec3 = "*11";
  char str[9];

  fprintf(stderr,"starting test_runtime_pattern_sets..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt); 

  recset_setlength(ndb,len); 

  empty_ndb_create(ndb,&rt);
  sz = recset_size(ndb);
  fprintf(stderr,"empty db has %d unique records\n",sz);

  online_add(dbrec1,ndb,&rt);  
  fprintf(stderr,"added pattern rec [%s]\n",dbrec1);

 /* adding 4 after 1 pattern for the 'union' of two sets */

  online_add(dbrec4,ndb,&rt);  
  fprintf(stderr,"added pattern rec [%s]\n",dbrec4);

  endtest = (1 << len);      /* 8 */
  for(i = 0; i < 4 ; i++) {
    itobs(i,str,len);             /* does not reverse the bit order */
    if( online_query(str,ndb,&rt) && (i%2)) {
      fprintf(stderr,"test_runtime_pattern_sets: oops, record <%s> (%d) wrong, in RNDB, not in DB\n",str,i);
      exit(4);
    }
  }

  for(i = 4; i < endtest ; i++) {
    itobs(i,str,len);             /* does not reverse the bit order */
    if(online_query(str,ndb,&rt)) {
      fprintf(stderr,"test_runtime_pattern_sets: oops, record <%s> (%d) wrong, not in DB\n",str,i);
      exit(14);
    }
  }

  /* removing 3 pattern only for the 'set difference' */

  online_remove(dbrec3,ndb,&rt);  
  fprintf(stderr,"removing pattern rec [%s]\n",dbrec3);

  for(i = 0; i < 3 ; i++) {
    itobs(i,str,len);             /* does not reverse the bit order */
    if( online_query(str,ndb,&rt) && (i%2)) {
      fprintf(stderr,"test_runtime_pattern_sets: oops, record <%s> (%d) wrong, not in DB\n",str,i);
      exit(7);
    }
  }

  itobs(3,str,len);             /* does not reverse the bit order */
  if(!online_query(str,ndb,&rt)) {
    fprintf(stderr,"test_runtime_pattern_sets: oops, record <%s> (3) wrong, in DB\n",str);
    exit(17);
  }

  endtest--;
  for(i = 4; i < endtest ; i++) {
    itobs(i,str,len);             /* does not reverse the bit order */
    if(online_query(str,ndb,&rt)) {
      fprintf(stderr,"test_runtime_pattern_sets: oops, record <%s> (%d) wrong, not in DB\n",str,i);
      exit(27);
    }
  }

  itobs(7,str,len);             /* does not reverse the bit order */
  if(!online_query(str,ndb,&rt)) {
    fprintf(stderr,"test_runtime_pattern_sets: oops, record <%s> (7) wrong, in DB\n",str);
    exit(17);
  }

  recset_destroy(ndb);
  fprintf(stdout,"completed runtime pattern sets test\n\n");
}

static int test_runtime_easy_helper(Recset * rsetarg, Runtime * rtarg, int maxsatarg);


/* this test verifies (and documents) the imprecise negative "binary
   union" operation that uses Negative Intersection of
   patterns. primary use: program verification. v.41, v.51 */
void test_runtime_imprecise_binary_union_of_patterns()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i, endloop;
  char * dbrec1 = "**1";
  char * dbrec4 = "1**";  /* 5 temporarily */
  int len = 3;
  int imprecise = 0;

  fprintf(stderr,"starting test_runtime_imprecise_binary_union_of_patterns..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets of patterns to be intersected */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns: rec1 in ndb1 not added to DB\n");
    exit(10);
  }


  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec4,ndb2,&rt);
  if(online_query(dbrec4,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns: rec4 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns: now for the intersection and followup..\n");

  runtime_setRelOperator(&rt,"I");

  ndb1 = recset_create();
  recset_setlength(ndb1,len);

  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns: verify the intersection..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }

  endloop = 1 << len;   /* 8 */

  /* online_query returns false if the string is in the database 
     the combination of 1 and 4 (i.e. 5) is the only "precise" member */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 5) ) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns: oops, record <%s> not found in RNDB---in DB\n",str);
      /* don't have to exit here */
      imprecise++;
    }
    if(i==5 && q ) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns: oops, record <%s> found in RNDB, not in DB\n",str);
      exit(4);
    }
  }

  fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns: %d out of %d imprecise records in DB\n",imprecise,endloop);

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns completed\n\n");
}


/* Jorge's example (2 recs per db, 1 in common, and no unset
   positions). This test verifies (and documents) the imprecise
   negative "binary union" operation that uses Negative Intersection
   of patterns. primary use: program verification. v.41, v.51 */
void test_runtime_imprecise_binary_union_of_patterns2()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i, endloop;
  char * dbrec1 = "**1";
  char * dbrec5 = "1*1";
  char * dbrec2 = "*1*";
  int len = 3;
  int imprecise = 0;

  fprintf(stderr,"starting test_runtime_imprecise_binary_union_of_patterns2..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets of patterns to be intersected */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns2: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec5,ndb1,&rt);
  if(online_query(dbrec5,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns2: rec5 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec1,ndb2,&rt);
  if(online_query(dbrec1,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns2: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2,ndb2,&rt);
  if(online_query(dbrec2,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns2: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns: now for the intersection and followup..\n");

  runtime_setRelOperator(&rt,"I");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns: verify the intersection..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns2: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }

  endloop = 1 << len;   /* 8 */

  /* online_query returns false if the string is in the database; 
     odd numbers are the only "precise" members */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    /* check that even's are not in DB */
    if(!q && !(i%2) ) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns2: oops, record <%s> not found in RNDB---in DB\n",str);
      /* don't have to exit here */
      imprecise++;
    }
    /* check that odd's are in DB */
    if( q && (i%2) ) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns2: oops, record <%s> found in RNDB (not in DB)\n",str);
      exit(4);
    }
  }

  fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns2: %d out of %d imprecise records in DB\n",imprecise,endloop);

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns2 completed\n\n");
}


/* Slightly bigger case: 5 bits, 2 unset bits, one db has 2 recs with
   a bit in common). This test verifies (and documents) the imprecise
   negative "binary union" operation that uses Negative
   Intersection of patterns. primary use: program
   verification. v.41, v.51 */
void test_runtime_imprecise_binary_union_of_patterns3()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i, endloop;
  char * dbrec1 = "****1";
  char * dbrec3 = "***11";
  char * dbrec8 = "*1***";
  int len = 5;
  int imprecise = 0;
  fprintf(stderr,"starting test_runtime_imprecise_binary_union_of_patterns3..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,2);
  runtime_setMinBitsSpecifiedFlag(&rt);
  runtime_setNewMode(&rt,1);    /* one-bit simplification mode */

  /* create the two recsets of patterns to be intersected */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec3,ndb1,&rt);
  if(online_query(dbrec3,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns3: rec3 in ndb1 not added to DB\n");
    exit(11);
  }

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns3: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec8,ndb2,&rt);
  if(online_query(dbrec8,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns3: rec8 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns3: now for the intersection and followup..\n");

  runtime_setRelOperator(&rt,"I");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns3: verify the intersection..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns3: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }
  
#if 0
  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns3: first fill-in the unset bit positions..magically\n");
  {
    char * dbrec16 = "1****";  /* filler */
    char * dbrec4 = "**1**";   /* filler */
    Rec * rec16 = rec_create(recset_numunits(ndb1));
    Rec * rec4 = rec_create(recset_numunits(ndb1));

    rec_string2rec(rec16,len,dbrec16,runtime_getBinMode(&rt));
    recset_addrec(rec16,ndb1);

    rec_string2rec(rec4,len,dbrec4,runtime_getBinMode(&rt));
    recset_addrec(rec4,ndb1);
  }
#endif

  endloop = 1 << len;

  /* online_query returns false if the string is in the database 
     the combination of 1,3 and 9 (i.e. 9 and 11) are the only "precise" members */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[9];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 9 && i != 11) ) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns3: oops, record <%s> not found in RNDB---in DB\n",str);
      /* don't have to exit here */
      imprecise++;
    }
    if(q && (i==9 || i==11 )) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns3: oops, record <%s> found in RNDB---not in DB\n",str);
      exit(4);
    }
  }

  fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns3: %d out of %d imprecise records in DB\n",imprecise,endloop);

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns3 completed\n\n");
}


/* Slightly bigger case: 4 bits, 0 unset bits, both db's have 2 recs,
   one with a bit in common). This test verifies (and documents) the
   imprecise negative "binary union" operation that uses Negative
   Intersection of patterns. primary use: program
   verification. v.41, v.51 */
void test_runtime_imprecise_binary_union_of_patterns4()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i, endloop;
  char * dbrec1 = "***1";
  char * dbrec6 = "*11*";
  char * dbrec4 = "*1**";
  char * dbrec8 = "1***";
  int len = 4;
  int imprecise = 0;
  fprintf(stderr,"starting test_runtime_imprecise_binary_union_of_patterns4..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets of patterns to be intersected */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns4: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec6,ndb1,&rt);
  if(online_query(dbrec6,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns4: rec6 in ndb1 not added to DB\n");
    exit(11);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec8,ndb2,&rt);
  if(online_query(dbrec8,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns4: rec8 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec4,ndb2,&rt);
  if(online_query(dbrec4,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns4: rec4 in ndb2 not added to DB\n");
    exit(3);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns4: now for the intersection and followup..\n");

  runtime_setRelOperator(&rt,"I");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns4: verify the intersection..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns4: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }
  
  endloop = 1 << len;

  /* online_query returns false if the string is in the database the
     combination of 1,6 and 8,4 (i.e. 5,9,6 and 14) are the only
     "precise" members */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[9];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 5 && i != 9 && i != 6 && i != 14) ) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns4: oops, record <%s> not found in RNDB---in DB\n",str);
      /* don't have to exit here */
      imprecise++;
    }
    if(q && (i==5 || i==9 || i==6 || i==14)) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns4: oops, record <%s> found in RNDB---not in DB\n",str);
      exit(4);
    }
  }

  fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns4: %d out of %d imprecise records in DB\n",imprecise,endloop);

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns4 completed\n\n");
}


/* Jorge's second example (3 recs per db, 1 in common, and no unset
   positions). This test verifies (and documents) the imprecise
   negative "binary union" operation that uses Negative Intersection
   of patterns. primary use: program verification. v.41, v.51 */
void test_runtime_imprecise_binary_union_of_patterns5()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i, endloop;
  char * dbrec1 = "**1";
  char * dbrec5 = "1*1";
  char * dbrec3 = "*11";
  char * dbrec4 = "1**";
  char * dbrec7 = "111";
  int len = 3;
  int imprecise = 0;

  fprintf(stderr,"starting test_runtime_imprecise_binary_union_of_patterns5..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets of patterns to be intersected */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec3,ndb1,&rt);
  if(online_query(dbrec3,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: rec3 in ndb1 not added to DB\n");
    exit(11);
  }

  online_add(dbrec5,ndb1,&rt);
  if(online_query(dbrec5,ndb1,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: rec5 in ndb1 not added to DB\n");
    exit(12);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec4,ndb2,&rt);
  if(online_query(dbrec4,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: rec4 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec3,ndb2,&rt);
  if(online_query(dbrec3,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: rec3 in ndb2 not added to DB\n");
    exit(3);
  }

  online_add(dbrec7,ndb2,&rt);
  if(online_query(dbrec7,ndb2,&rt)){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: rec7 in ndb2 not added to DB\n");
    exit(7);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns5: now for the intersection and followup..\n");

  runtime_setRelOperator(&rt,"I");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns5: verify the intersection..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }

  endloop = 1 << len;   /* 8 */

  /* online_query returns false if the string is in the database; 
     {101,011,111} are the only "precise" members */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i!=3 && i!=5 && i!=7) ) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: oops, record <%s> not found in RNDB---in DB\n",str);
      /* don't have to exit here */
      imprecise++;
    }
    if( q && (i==3 || i==5 || i==7) ) {
      fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: oops, record <%s> found in RNDB (not in DB)\n",str);
      exit(4);
    }
  }

  fprintf(stderr,"test_runtime_imprecise_binary_union_of_patterns5: %d out of %d imprecise records in DB\n",imprecise,endloop);

  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_imprecise_binary_union_of_patterns5 completed\n\n");
}



/* this test verifies (and documents) the Eric Trias' negative "binary
   union" operation. Different than Negative Union. primary use:
   program verification. v.55 */
void test_runtime_binary_union_of_bits()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1 = "001";
  char * dbrec4 = "100";
  int len = 3;
  int endloop;

  fprintf(stderr,"starting test_runtime_binary_union_of_bits..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,runtime_getName(&rt));
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec4,ndb2,&rt);
  if(online_query(dbrec4,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits: rec4 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_binary_union_of_bits: now for the BU and followup..\n");

  runtime_setRelOperator(&rt,"S");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_binary_union_of_bits: verify..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_binary_union_of_bits: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }

  endloop = 1 << len;   /* 8 */

  /* online_query returns false if the string is in the database 
     the combination of 1 and 4 (i.e. 5) is the only member */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 5) ) {
      fprintf(stderr,"test_runtime_binary_union_of_bits: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(i==5 && q ) {
      fprintf(stderr,"test_runtime_binary_union_of_bits: oops, record <%s> found in RNDB\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_binary_union_of_bits completed\n\n");
}


/* Jorge's example (2 recs per db, 1 in common, and no unset
   positions). This test verifies (and documents) Eric Trias'
   negative "binary union" operation. Different than Negative
   Union. primary use: program verification. v.55 */
void test_runtime_binary_union_of_bits2()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1 = "001";
  char * dbrec5 = "101";
  char * dbrec2 = "010";
  int len = 3;
  int endloop;

  fprintf(stderr,"starting test_runtime_binary_union_of_bits2..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits2: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec5,ndb1,&rt);
  if(online_query(dbrec5,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits2: rec5 in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,"rndb-1.txt");
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec1,ndb2,&rt);
  if(online_query(dbrec1,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits2: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2,ndb2,&rt);
  if(online_query(dbrec2,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits2: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,"rndb-2.txt");
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_binary_union_of_bits2: now for the BU and followup..\n");

  runtime_setRelOperator(&rt,"S");
  runtime_setName(&rt,"rndb-1.txt");
  runtime_setName2(&rt,"rndb-2.txt");
  runtime_setNameOutputFile(&rt,"rndb-1B2.txt");
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,runtime_getName(&rt),ndbformat);

  online_relationaloperation(ndb1,&rt);
  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_binary_union_of_bits2: verify the binary union..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,"rndb-1B2.txt",ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_binary_union_of_bits2: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }

  endloop = 1 << len;   /* 8 */

  /* online_query returns false if the string is in the database; 
     odd numbers are the only members */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    /* check that even's are not in DB */
    if(!q && !(i%2) ) {
      fprintf(stderr,"test_runtime_binary_union_of_bits2: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    /* check that odd's are in DB */
    if( q && (i%2) ) {
      fprintf(stderr,"test_runtime_binary_union_of_bits2: oops, record <%s> found in RNDB (not in DB)\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_binary_union_of_bits2 completed\n\n");
}


/* Slightly bigger case: 5 bits, 2 unset bits, one db has 2 recs with
   a bit in common). This test verifies (and documents) Eric Trias'
   negative "binary union" operation. Different than Negative
   Union. primary use: program verification. v.55 */
void test_runtime_binary_union_of_bits3()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1 = "00001";
  char * dbrec3 = "00011";
  char * dbrec8 = "01000";
  int len = 5;
  int endloop;
  fprintf(stderr,"starting test_runtime_binary_union_of_bits3..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets */

  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits3: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec3,ndb1,&rt);
  if(online_query(dbrec3,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits3: rec3 in ndb1 not added to DB\n");
    exit(11);
  }

  recset_save(ndb1,&rt,"rndb-1.txt");
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec8,ndb2,&rt);
  if(online_query(dbrec8,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits3: rec8 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_binary_union_of_bits3: now for the BU and followup..\n");

  runtime_setRelOperator(&rt,"S");

  runtime_setName(&rt,"rndb-1.txt");
  runtime_setName2(&rt,NDBNAME2);
  runtime_setNameOutputFile(&rt,"rndb-1B2.txt");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,runtime_getName(&rt),ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_binary_union_of_bits3: verify..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,"rndb-1B2.txt",ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_binary_union_of_bits3: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }
  
  endloop = 1 << len;

  /* online_query returns false if the string is in the database 
     the combination of 1,3 and 9 (i.e. 9 and 11) are the only members */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[9];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 9 && i != 11) ) {
      fprintf(stderr,"test_runtime_binary_union_of_bits3: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i==9 || i==11 )) {
      fprintf(stderr,"test_runtime_binary_union_of_bits3: oops, record <%s> found in RNDB---not in DB\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_binary_union_of_bits3 completed\n\n");
}


/* Slightly bigger case: 4 bits, 0 unset bits, both db's have 2 recs,
   one with a bit in common). This test verifies (and documents) Eric
   Trias' negative "binary union" operation. Different than Negative
   Union. primary use: program verification. changed to reflect a real
   lhs for amgu unification w=f(x,y) v.66 */
void test_runtime_binary_union_of_bits4()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1 = "0001";
  char * dbrec5 = "0101";       /* changed from 6 in v.66 */
  char * dbrec4 = "0100";
  char * dbrec8 = "1000";
  int len = 4;
  int endloop;
  fprintf(stderr,"starting test_runtime_binary_union_of_bits4..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits4: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec5,ndb1,&rt);
  if(online_query(dbrec5,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits4: rec5 in ndb1 not added to DB\n");
    exit(11);
  }

  recset_save(ndb1,&rt,"rndb-1.txt");
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec8,ndb2,&rt);
  if(online_query(dbrec8,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits4: rec8 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec4,ndb2,&rt);
  if(online_query(dbrec4,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits4: rec4 in ndb2 not added to DB\n");
    exit(3);
  }

  recset_save(ndb2,&rt,"rndb-2.txt");
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_binary_union_of_bits4: now for the BU and followup..\n");

  runtime_setRelOperator(&rt,"S");
  runtime_setName(&rt,"rndb-1.txt");
  runtime_setName2(&rt,"rndb-2.txt");
  runtime_setNameOutputFile(&rt,"rndb-1B2-4bits.txt");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,runtime_getName(&rt),ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_binary_union_of_bits4: verify..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,"rndb-1B2-4bits.txt",ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_binary_union_of_bits4: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }
  
  endloop = 1 << len;

  /* online_query returns false if the string is in the database the
     combination of 1,5 and 8,4 (i.e. 5,9, and 13) are the only
     members v.66 */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[9];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i!=5 && i!=9 && i!=13 ) ) {
      fprintf(stderr,"test_runtime_binary_union_of_bits4: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i==5 || i==9 || i==13 )) {
      fprintf(stderr,"test_runtime_binary_union_of_bits4: oops, record <%s> found in RNDB---not in DB\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_binary_union_of_bits4 completed\n\n");
}


/* Jorge's second example (3 recs per db, 1 in common, and no unset
   positions). This test verifies Eric Trias' negative "binary union"
   operation. Different than Negative Union. primary use: program
   verification. v.55 */
void test_runtime_binary_union_of_bits5()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits, i;
  char * dbrec1 = "001";
  char * dbrec5 = "101";
  char * dbrec3 = "011";
  char * dbrec4 = "100";
  char * dbrec7 = "111";
  int len = 3;
  int endloop;

  fprintf(stderr,"starting test_runtime_binary_union_of_bits5..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits5: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec3,ndb1,&rt);
  if(online_query(dbrec3,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits5: rec3 in ndb1 not added to DB\n");
    exit(11);
  }

  online_add(dbrec5,ndb1,&rt);
  if(online_query(dbrec5,ndb1,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits5: rec5 in ndb1 not added to DB\n");
    exit(12);
  }

  recset_save(ndb1,&rt,"rndb-1.txt");
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec4,ndb2,&rt);
  if(online_query(dbrec4,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits5: rec4 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec3,ndb2,&rt);
  if(online_query(dbrec3,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits5: rec3 in ndb2 not added to DB\n");
    exit(3);
  }

  online_add(dbrec7,ndb2,&rt);
  if(online_query(dbrec7,ndb2,&rt)){
    fprintf(stderr,"test_runtime_binary_union_of_bits5: rec7 in ndb2 not added to DB\n");
    exit(7);
  }

  recset_save(ndb2,&rt,"rndb-2.txt");
  recset_destroy(ndb2);


  fprintf(stdout,"test_runtime_binary_union_of_bits5: now for the BU and followup..\n");

  runtime_setRelOperator(&rt,"S");
  runtime_setName(&rt,"rndb-1.txt");
  runtime_setName2(&rt,"rndb-2.txt");
  runtime_setNameOutputFile(&rt,"rndb-1B2.txt");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,runtime_getName(&rt),ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_binary_union_of_bits5: verify..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,"rndb-1B2.txt",ndbformat);

  if(recset_length(ndb1) != len){
    fprintf(stderr,"test_runtime_binary_union_of_bits5: RNDB3 record length is %d, not %d\n",
            recset_length(ndb1),len);
    exit(3);
  }

  endloop = 1 << len;   /* 8 */

  /* online_query returns false if the string is in the database; 
     {101,011,111} are the only members */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,ndb1,&rt);
    if(!q && (i!=3 && i!=5 && i!=7) ) {
      fprintf(stderr,"test_runtime_binary_union_of_bits5: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if( q && (i==3 || i==5 || i==7) ) {
      fprintf(stderr,"test_runtime_binary_union_of_bits5: oops, record <%s> found in RNDB (not in DB)\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_binary_union_of_bits5 completed\n\n");
}


/* uses 12-bit RNDB-3673.test negative database containing 3673
   positive records; does partial queries for x,y,z (the first
   three positions; unions y and z partials; binary unions x and yz;
   result is two records  (v.60.1) */
void test_runtime_binary_union_3673()
{
  Runtime rt;
  Recset * ndb;
  int starttime, endtime;    /* for BU timing */
  int sz;
  int minbits = 1;           /* force compressions */
  int len = 12;
  char * dbrecx = "1***********";
  char * dbrecy = "*1**********";
  char * dbrecz = "**1*********";
  char * dbrec0 = "000000000000"; /* not in the result, and any combo of last 9 bits */
  char * dbrec1 = "001000000000";
  char * dbrec2 = "010000000000";
  char * dbrec3 = "011000000000";
  char * dbrec4 = "100000000000";
  char * dbrec5 = "101000000000"; /* in the result, and any combo of the last 9 bits */
  char * dbrec6 = "110000000000";
  char * dbrec7 = "111000000000";


  fprintf(stderr,"starting test_runtime_binary_union_3673..\n");

  fprintf(stdout,"test_runtime_binary_union_3673: do partial queries for each variable x,y,z\n");
  ndb = recset_create();

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setName(&rt,"RNDB-3673.test");
  runtime_setNameOutputFile(&rt,"rndb-3673x.txt");
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);
  sz = recset_size(ndb);
  online_query(dbrecx,ndb,&rt);
  recset_final(ndb);
  runtime_final(&rt);

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setNameOutputFile(&rt,"rndb-3673y.txt");
  recset_init(ndb,len);
  online_query(dbrecy,ndb,&rt);
  recset_final(ndb);
  runtime_final(&rt);

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setNameOutputFile(&rt,"rndb-3673z.txt");
  recset_init(ndb,len);
  online_query(dbrecz,ndb,&rt);
  runtime_final(&rt);
  recset_final(ndb);

  fprintf(stdout,"test_runtime_binary_union_3673: check partial result sizes for efficiency\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setName(&rt,"rndb-3673x.txt");
  recset_init(ndb,len);
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);
  if (recset_size(ndb) > sz) {
    fprintf(stdout,"test_runtime_binary_union_3673: partial query x is not as small as it could be (195); use RELOPS_EFFICIENCY_STEP (nsql.h) set to one; OR specify minbits in EVERY partial query\n");
    exit(9);
  }
  recset_final(ndb);
  runtime_final(&rt);


  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setName(&rt,"rndb-3673y.txt");
  recset_init(ndb,len);
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);
  if (recset_size(ndb) > sz) {
    fprintf(stdout,"test_runtime_binary_union_3673: partial query y is not as small as it could be (185); use RELOPS_EFFICIENCY_STEP (nsql.h) set to one; OR specify minbits in EVERY partial query\n");
    exit(10);
  }
  recset_final(ndb);
  runtime_final(&rt);

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setName(&rt,"rndb-3673z.txt");
  recset_init(ndb,len);
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);
  if (recset_size(ndb) > sz) {
    fprintf(stdout,"test_runtime_binary_union_3673: partial query z is not as small as it could be (186); use RELOPS_EFFICIENCY_STEP (nsql.h) set to one; OR specify minbits in EVERY partial query\n");
    exit(9);
  }
  recset_final(ndb);
  runtime_final(&rt);

  recset_destroy(ndb);

  fprintf(stdout,"test_runtime_binary_union_3673: negative Union y and z\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,minbits);
  runtime_setMinBitsSpecifiedFlag(&rt);
  runtime_setRelOperator(&rt,"U");
  runtime_setName(&rt,"rndb-3673y.txt");
  runtime_setName2(&rt,"rndb-3673z.txt");
  runtime_setNameOutputFile(&rt,"rndb-3673-yUz.txt");

  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);

  online_relationaloperation(ndb,&rt);
  runtime_final(&rt);
  recset_destroy(ndb);

  fprintf(stdout,"test_runtime_binary_union_3673: negative Binary Union x and yUz\n");
  starttime = time(0);

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,minbits);
  runtime_setMinBitsSpecifiedFlag(&rt); /* comment out to skip compress for timing */
  runtime_setRelOperator(&rt,"S");
  runtime_setName(&rt,"rndb-3673x.txt");
  runtime_setName2(&rt,"rndb-3673-yUz.txt");

  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);

  online_relationaloperation(ndb,&rt);
  runtime_final(&rt);
  recset_destroy(ndb);

  endtime = time(0) - starttime;
  fprintf(stdout,"BU elapsed time: %d seconds\n",endtime);

  if(endtime > 30) {      /* change to 30 for VALTESTS, o.w. 10 secs is enough */          
    fprintf(stderr,"test_runtime_binary_union_3673: BU operation running slower than 10 secs\n");
    exit(3);
  }

  fprintf(stdout,"test_runtime_binary_union_3673: verify [0***********, *00*********]..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb = recset_create();
  recset_build(ndb,&rt,NDBNAME3,ndbformat);

  fprintf(stdout,"test_runtime_binary_union_3673: RNDB3 BU result is: \n");
  recset_print(ndb,stdout);

  if(recset_size(ndb) != 2){
    fprintf(stderr,"test_runtime_binary_union_3673: RNDB3 size is %d, not 2\n",
            recset_size(ndb));
    exit(3);
  }
  
  if(!online_query(dbrec0,ndb,&rt)){
    fprintf(stderr,"test_runtime_binary_union_3673: rec0 not in result\n");
    exit(8);
  }

  if(!online_query(dbrec1,ndb,&rt)){
    fprintf(stderr,"test_runtime_binary_union_3673: rec1 not in result\n");
    exit(1);
  }

  if(!online_query(dbrec2,ndb,&rt)){
    fprintf(stderr,"test_runtime_binary_union_3673: rec2 not in result\n");
    exit(2);
  }

  if(!online_query(dbrec3,ndb,&rt)){
    fprintf(stderr,"test_runtime_binary_union_3673: rec3 not in result\n");
    exit(3);
  }

  if(!online_query(dbrec4,ndb,&rt)){
    fprintf(stderr,"test_runtime_binary_union_3673: rec4 not in result\n");
    exit(4);
  }

  if(online_query(dbrec5,ndb,&rt)){
    fprintf(stderr,"test_runtime_binary_union_3673: rec5 in result\n");
    exit(5);
  }

  if(online_query(dbrec6,ndb,&rt)){
    fprintf(stderr,"test_runtime_binary_union_3673: rec6 in result\n");
    exit(6);
  }

  if(online_query(dbrec7,ndb,&rt)){
    fprintf(stderr,"test_runtime_binary_union_3673: rec7 in result\n");
    exit(7);
  }

  recset_destroy(ndb);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_binary_union_3673 completed\n\n");
}


/* test BU star closure: sh = {00001,00010,00100,01000,10000}; You
 should obtain the powerset minus empty set S* (jorge emails,12/9/07)
*/
void test_runtime_binary_union_star_closure()
{
  Runtime rt;
  Recset * ndb;
  int len = 6;      /* str allocation supports upto 5-7 bits */
  int i, endloop;
  
  fprintf(stdout,"test_runtime_binary_union_star_closure: starting..\n");
  
  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb = recset_create();
  recset_setlength(ndb,len);
  
  empty_ndb_create(ndb,&rt);
  
  i = 1;
  endloop = 1 << len;
  
  fprintf(stdout,"test_runtime_binary_union_star_closure: endloop is <%d>, starting at %d\n",endloop,i);

  while(i < endloop) {
    char str[8];
    itobs(i,str,len);
    fprintf(stdout,"test_runtime_binary_union_star_closure: adding <%s>\n",str);
    online_add(str,ndb,&rt);
    i = i << 1;
  }

  recset_save(ndb,&rt,NDBNAME2);

  fprintf(stdout,"test_runtime_binary_union_star_closure: do starnegbinaryunion..\n");
  runtime_setRelOperator(&rt,"S");
  online_relationaloperation(ndb,&rt);
  
  recset_destroy(ndb);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_binary_union_star_closure: verify..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  ndb = recset_create();
  recset_build(ndb,&rt,NDBNAME3,ndbformat);

  /* online_query returns false if the string is in the database */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[8];
    itobs(i,str,len);
    q = online_query(str,ndb,&rt);
    if(!q && (i == 0) ) {
      fprintf(stderr,"test_runtime_binary_union_star_closure: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(13);
    }
    if(q && i!=0) {
      fprintf(stderr,"test_runtime_binary_union_star_closure: oops, record <%s> found in RNDB\n",str);
      exit(13);
    }
  }

  recset_destroy(ndb);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_binary_union_star_closure completed\n\n");
}



  
void test_runtime_onebit_morph() 
{
  Runtime rt;
  int n, numunits;
  int len = 8;
  Recset * rsetptr = recset_create();
  char * dbrec1 = "01000000";
  char * dbrec2 = "00101000";

  fprintf(stderr,"starting test_runtime_onebit_morph..\n");

  runtime_init(&rt);
  runtime_setMinBits(&rt,2);
  runtime_setMinBitsSpecifiedFlag(&rt);
  runtime_setBinMode(&rt);
  runtime_setNewMode(&rt,1);    /* one-bit simplification mode */
  runtime_setCleanTests(&rt,1000);
  runtime_setTau(&rt,1);        /* okay if ndb gets more records */

  recset_setlength(rsetptr,len);
  numunits =  recset_numunits(rsetptr);

  empty_ndb_create(rsetptr,&rt);

  n = recset_size(rsetptr);
  fprintf(stderr,"empty db has %d unique records\n",n);

  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  online_add(dbrec1,rsetptr,&rt);

  if(online_query(dbrec1,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_onebit_morph: rec 1 <%s> is not in DB\n",dbrec1);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(11);
  }

  online_add(dbrec2,rsetptr,&rt);

  if(online_query(dbrec2,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_onebit_morph: rec 2 <%s> is not in DB\n",dbrec2);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(12);
  }

  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  runtime_setMinBits(&rt,1);    /* note: minbit 1 no longer has rstree problems */

  test_clean(rsetptr,&rt);

  n = recset_size(rsetptr);
  fprintf(stderr,"after cleanup 1, empty db has %d unique records\n",n);
  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  runtime_setMinBits(&rt,2);
  test_clean(rsetptr,&rt);

  n = recset_size(rsetptr);
  fprintf(stderr,"after cleanup 2, empty db has %d unique records\n",n);
  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  online_remove(dbrec1,rsetptr,&rt);

  if(!online_query(dbrec1,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_onebit_morph: rec 1 <%s> is in DB after remove\n",dbrec1);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(11);
  }

  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  runtime_setMinBits(&rt,1);    /* note: minbit 1 has rstree problems */
  test_clean(rsetptr,&rt); 

  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  runtime_setMinBits(&rt,2);    /* note: minbit 1 has rstree problems */
  test_clean(rsetptr,&rt); 

  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  online_remove(dbrec2,rsetptr,&rt);
  
  if(!online_query(dbrec2,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_onebit_morph: rec 2 <%s> is in DB after remove\n",dbrec1);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(4);
  }

  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));
  
  runtime_setMinBits(&rt,1);    /* note: minbit 1 has rstree problems */
  test_clean(rsetptr,&rt); 

  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  runtime_setMinBits(&rt,2);    /* note: minbit 1 has rstree problems */
  test_clean(rsetptr,&rt); 
  
  fprintf(stderr,"max record size is %d\n",recset_max_recordsize(rsetptr));

  recset_destroy(rsetptr);
  runtime_final(&rt);

  fprintf(stdout,"completed runtime onebit morph test\n");
}


void test_runtime_emptyp() 
{
  Runtime rt;
  Rec * recsol;
  Rec * rectest;
  Rec * rectest2;
  int n, results, numunits, mode;
  int len = 8;
  int minbits = 2;
  Recset * rsetptr = recset_create();
  char * dbrec1 = "01000000";
  char * dbrec2 = "00101000";

  fprintf(stderr,"starting test_runtime_emptyp..\n");

  runtime_init(&rt);
  runtime_setMinBits(&rt,minbits);
  runtime_setMinBitsSpecifiedFlag(&rt);

  runtime_setBinMode(&rt);
  runtime_setNewMode(&rt,1);    /* one-bit simplification mode */
  runtime_seedRandom(&rt,7);   

  recset_setlength(rsetptr,len);
  numunits =  recset_numunits(rsetptr);

  empty_ndb_create(rsetptr,&rt);

  n = recset_size(rsetptr);
  fprintf(stderr,"empty db has %d unique records\n",n);

  test_runtime_easy_helper(rsetptr,&rt,minbits);

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    runtime_setReduceMode(&rt,mode);
    results = recset_notemptyp(rsetptr,&rt);
    if (results != 0){
      fprintf(stderr,"test_runtime_emptyp: empty db results is %d using mode %d\n",results,mode);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(1);
    }
  }

  online_add(dbrec1,rsetptr,&rt);

  if(online_query(dbrec1,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_emptyp: rec 1 <%s> is not in DB\n",dbrec1);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(2);
  }

  test_runtime_easy_helper(rsetptr,&rt,SAT2);

  recsol = rec_create(numunits);
  rectest = rec_create(numunits);
  rec_string2rec(rectest,len,dbrec1,runtime_getBinMode(&rt));

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    results = recset_notemptyp_solution(rsetptr,recsol,mode,minbits);
    if (results <=0){
      fprintf(stderr,"test_runtime_emptyp: one record ndb emptyp results is %d using mode %d\n",results,mode);
      rec_print(recsol,len,stderr);
      rec_destroy(recsol);
      rec_destroy(rectest);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(3);
    }

    fprintf(stdout,"\ntest_runtime_emptyp: found first solution using mode %d: \n",mode);
    rec_print(recsol,len,stdout);
    
    if(!rec_equal(rectest,recsol,numunits)){
      fprintf(stderr,"test_runtime_emptyp: recsols not equal to rec 1 (%s) using mode %d\n",dbrec1,mode);
      rec_destroy(recsol);
      rec_destroy(rectest);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(4);
    }
    rec_init(recsol,numunits);
  }

  online_add(dbrec2,rsetptr,&rt);

  if(online_query(dbrec2,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_emptyp: rec 2 <%s> is not in DB\n",dbrec2);
    rec_destroy(recsol);
    rec_destroy(rectest);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(5);
  }

  test_runtime_easy_helper(rsetptr,&rt,minbits);

  rectest2 = rec_create(numunits);
  rec_string2rec(rectest2,len,dbrec2,runtime_getBinMode(&rt));

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    results = recset_notemptyp_solution(rsetptr,recsol,mode,minbits);
    if (results <= 0){
      fprintf(stderr,"test_runtime_emptyp: two record ndb emptyp results is %d using mode %d\n",results,mode);
      rec_destroy(recsol);
      rec_destroy(rectest);
      rec_destroy(rectest2);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(6);
    }
    
    if(!rec_equal(rectest,recsol,numunits)  && !rec_equal(rectest2,recsol,numunits)){
      fprintf(stderr,"test_runtime_emptyp: recsols not equal to rec 1 (%s) or rec 2 (%s) using mode %d\n",dbrec1,dbrec2,mode);
      rec_destroy(recsol);
      rec_destroy(rectest);
      rec_destroy(rectest2);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(6);
    }
    rec_init(recsol,numunits);
  }
  
  online_remove(dbrec1,rsetptr,&rt);

  if(!online_query(dbrec1,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_emptyp: rec 1 <%s> is in DB after remove\n",dbrec1);
    rec_destroy(recsol);
    rec_destroy(rectest);
    rec_destroy(rectest2);  
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(7);
  }
  
  test_runtime_easy_helper(rsetptr,&rt,minbits);

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    results = recset_notemptyp_solution(rsetptr,recsol,mode,minbits);
    if (results <= 0){
      fprintf(stderr,"test_runtime_emptyp: one record ndb emptyp results is %d using mode %d\n",results,mode);
      rec_destroy(recsol);
      rec_destroy(rectest);
      rec_destroy(rectest2);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(8);
    }

    fprintf(stdout,"\ntest_runtime_emptyp: found 2nd solution using mode %d: \n",mode);
    rec_print(recsol,len,stdout);
    
    if(!rec_equal(rectest2,recsol,numunits)){
      fprintf(stderr,"test_runtime_emptyp: recsols not equal to rec 2 (%s) using mode %d\n",dbrec2,mode);
      rec_destroy(recsol);
      rec_destroy(rectest);
      rec_destroy(rectest2);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(9);
    }
    rec_init(recsol,numunits);
  }

  online_remove(dbrec2,rsetptr,&rt);

  if(!online_query(dbrec2,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_emptyp: rec 2 <%s> is in DB after remove\n",dbrec1);
    rec_destroy(recsol);
    rec_destroy(rectest);
    rec_destroy(rectest2);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(10);
  }

  test_runtime_easy_helper(rsetptr,&rt,minbits);

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    results = recset_notemptyp_solution(rsetptr,recsol,mode,minbits);

    if (results != 0){
      fprintf(stderr,"test_runtime_emptyp: no records left ndb, emptyp results is %d using mode %d\n",results,mode);
      rec_destroy(recsol);
      rec_destroy(rectest);
      rec_destroy(rectest2);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(11);
    }

    fprintf(stdout,"\ntest_runtime_emptyp: found no solutions: \n");
    rec_print(recsol,len,stdout);
    n = recset_size(rsetptr);
    fprintf(stderr,"after emptyp with no records left, ndb has %d unique records, using mode %d\n",n,mode);
    
    rec_init(recsol,numunits);
  }
  
  rec_destroy(rectest);
  rec_destroy(rectest2);
  rec_destroy(recsol);
  recset_destroy(rsetptr);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime emptyp test\n");
}


void test_runtime_emptyp_100bit(int minbitarg) 
{
  Runtime rt;
  Rec * recsol;
  Rec * rectest;
  int n, results, numunits, mode;
  int len = 100;
  Recset * rsetptr = recset_create();
  char * dbrec1 = "1110010000111011001111100011110110111101010101001111101101000100111000101011100110010011010010100100";

  fprintf(stderr,"starting test_runtime_emptyp_100bit..\n");

  assert(minbitarg==2 || minbitarg==3);

  runtime_init(&rt);
  runtime_setMinBits(&rt,2);
  runtime_setMinBitsSpecifiedFlag(&rt);

  runtime_setBinMode(&rt);
  runtime_setNewMode(&rt,1);    /* one-bit simplification mode */
  runtime_seedRandom(&rt,1196357540);   /* good seed v.63, was 0 */

  recset_setlength(rsetptr,len);
  numunits =  recset_numunits(rsetptr);

  empty_ndb_create(rsetptr,&rt);

  n = recset_size(rsetptr);
  fprintf(stderr,"empty db has %d unique records\n",n);

  test_runtime_easy_helper(rsetptr,&rt,SAT2);

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    runtime_setReduceMode(&rt,mode);
    results = recset_notemptyp(rsetptr,&rt);
    if (results != 0){
      fprintf(stderr,"test_runtime_emptyp_100bit: empty db results is %d using mode %d\n",results,mode);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(1);
    }
  }

  runtime_setMinBits(&rt,minbitarg);
  online_add(dbrec1,rsetptr,&rt);

  if(online_query(dbrec1,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_emptyp_100bit: rec 1 <%s> is not in DB\n",dbrec1);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(2);
  }

  recsol = rec_create(numunits);

  test_runtime_easy_helper(rsetptr,&rt,minbitarg);

  rectest = rec_create(numunits);
  rec_string2rec(rectest,len,dbrec1,runtime_getBinMode(&rt));
  
  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    results = recset_notemptyp_solution(rsetptr,recsol,mode,minbitarg);
    if (results <=0){
      fprintf(stderr,"test_runtime_emptyp_100bit: one record ndb emptyp results is %d using mode %d\n",results,mode);
      rec_destroy(recsol);
      rec_destroy(rectest);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(3);
    }
    
    fprintf(stdout,"\ntest_runtime_emptyp: found first solution using mode %d: \n",mode);
    rec_print(recsol,len,stdout);

    if(!rec_equal(rectest,recsol,numunits)){
      fprintf(stderr,"test_runtime_emptyp_100bit: recsols not equal to rec 1 (%s) using mode %d\n",dbrec1,mode);
      rec_destroy(recsol);
      rec_destroy(rectest);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(4);
    }
    rec_init(recsol,numunits);
  }

  rec_destroy(rectest);
  rec_destroy(recsol);
  recset_destroy(rsetptr);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime emptyp 100 bit test\n");
}


/* this works with all easy_random_popular, and the combination of
   popular when successful (i.e. not backtracking), o.w. random amd
   starting with popular v.50 */
void test_runtime_emptyp_100bit_singleton_with_2solutions() 
{
  Runtime rt;
  int n, results, numunits, mode;
  int len = 100;
  Recset * rsetptr = recset_create();
  char * dbrec1 = "0110100011110111110001100100000010101110101000100010101001110101110011000110111110001000011000001101";
  int seed_2solution = 1175749506;     /* two solutions */

  fprintf(stderr,"starting test_runtime_emptyp_100bit_singleton_with_2solutions..\n");

  runtime_init(&rt);
  runtime_setMinBits(&rt,3);
  runtime_setBinMode(&rt);
  runtime_seedRandom(&rt,seed_2solution);   
  
  recset_setlength(rsetptr,len);
  numunits =  recset_numunits(rsetptr);

  runtime_setInput(&rt,dbrec1);   /* v.70 */
  runtime_setup_multiinput(&rt);  /* v.70 */

  singleton_ndb_create(runtime_getMultiInput(&rt,runtime_getBinMode(&rt)),rsetptr,&rt);

  n = recset_size(rsetptr);
  fprintf(stderr,"singleton db has %d unique records\n",n);

  if(online_query(dbrec1,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_emptyp_100bit_singleton_with_2solutions: rec 1 <%s> is not in DB\n",dbrec1);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(2);
  }

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    runtime_setReduceMode(&rt,mode);
    results = recset_notemptyp(rsetptr,&rt);
    if (results != 1){
      fprintf(stderr,"test_runtime_emptyp_100bit_singleton_with_2solutions: singleton db with 2 solutions results is %d, using mode %d\n",results,mode);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(1);
    }
  }

  recset_destroy(rsetptr);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime emptyp 100 bit singleton (2 solution) test\n");
}


/* same as test with 2 solutions except using a different seed;
   haven't found a way to reverse this one yet, so we test for
   failure v.50 */
void test_runtime_emptyp_100bit_singleton_infamous() 
{
  Runtime rt;
  int n, results, numunits, mode;
  int len = 100;
  Recset * rsetptr = recset_create();
  char * dbrec1 = "0110100011110111110001100100000010101110101000100010101001110101110011000110111110001000011000001101";
  int seed_infamous = 1175749450;    /* infamous */

  fprintf(stderr,"starting test_runtime_emptyp_100bit_singleton_infamous..\n");

  runtime_init(&rt);
  runtime_setMinBits(&rt,3);
  runtime_setBinMode(&rt);
  runtime_seedRandom(&rt,seed_infamous);   
  
  recset_setlength(rsetptr,len);
  numunits =  recset_numunits(rsetptr);

  runtime_setInput(&rt,dbrec1);   /* v.70 */
  runtime_setup_multiinput(&rt);  /* v.70 */

  singleton_ndb_create(runtime_getMultiInput(&rt,runtime_getBinMode(&rt)),rsetptr,&rt);

  n = recset_size(rsetptr);
  fprintf(stderr,"singleton db has %d unique records\n",n);

  if(online_query(dbrec1,rsetptr,&rt)) {
    fprintf(stderr,"test_runtime_emptyp_100bit_singleton_infamouse: rec 1 <%s> is not in DB\n",dbrec1);
    recset_destroy(rsetptr);
    runtime_final(&rt);
    exit(2);
  }

  for(mode=0; mode < LASTREDUCEMODE; mode++){  
    runtime_setReduceMode(&rt,mode);
    results = recset_notemptyp(rsetptr,&rt);
    /* it is SO HARD that if we succeed that would be notable! YEAH we succeed now*/
    if (results == 0){
      fprintf(stderr,"test_runtime_emptyp_100bit_singleton: singleton db INFAMOUS results is %d, using mode %d\n",results,mode);
      recset_destroy(rsetptr);
      runtime_final(&rt);
      exit(1);
    }
  }
  
  recset_destroy(rsetptr);
  runtime_final(&rt);
  fprintf(stdout,"completed runtime emptyp 100 bit singleton infamous test\n");
}


/* help to ensure recset has easy-to-reverse record sizes (2 specified
   bits) (of course this does not always work, o.w. P=NP ); uses
   cleanup rather than morph value for Tau. v.51 */
static int test_runtime_easy_helper(Recset * rsetarg, Runtime * rtarg, int maxsatarg)
{
  int  maxrecsz = recset_max_recordsize(rsetarg);
  fprintf(stderr,"test_runtime_easy_helper: BEFORE max record size is %d\n",maxrecsz);
  
  if(maxrecsz > maxsatarg) {
    int cleancount = 0;
    
    runtime_setCleanTests(rtarg,1000);
    runtime_setTau(rtarg,0);      
    runtime_setMinBits(rtarg,maxsatarg-1);
    test_clean(rsetarg,rtarg);

    runtime_setMinBits(rtarg,maxsatarg);    
    maxrecsz = recset_max_recordsize(rsetarg);

    while(maxrecsz != maxsatarg && cleancount < 100){  /* don't let it stay less than */
      test_clean(rsetarg,rtarg);
      maxrecsz = recset_max_recordsize(rsetarg);
      cleancount++;
    }
  }

  fprintf(stderr,"test_runtime_easy_helper: AFTER max record size is %d\n",maxrecsz);
  return (maxrecsz > maxsatarg ? errneg2 : maxrecsz);
}


/* double negative test, complements a negative database with multiple
   solutions; based on Eric Trias' insight v.55; changed test name
   from test_runtime_recset_complement v.60 */
void test_complement(){
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  Recset * compset;
  int numunits, i;
  char * dbrec1 = "001";
  char * dbrec3 = "011";
  char * dbrec5 = "101";
  int len = 3;
  int endloop;

  fprintf(stderr,"starting test_complement..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create(ndb1,&rt); 

  online_add(dbrec1,ndb1,&rt);
  if(online_query(dbrec1,ndb1,&rt)){
    fprintf(stderr,"test_complement: rec1 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec3,ndb1,&rt);
  if(online_query(dbrec3,ndb1,&rt)){
    fprintf(stderr,"test_complement: rec3 in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrec5,ndb1,&rt);
  if(online_query(dbrec5,ndb1,&rt)){
    fprintf(stderr,"test_complement: rec5 in ndb1 not added to DB\n");
    exit(10);
  }

  compset = recset_create();
  recset_complement(ndb1,compset,&rt);   /* complement of ndb1 */
  recset_save(compset,&rt,"rndb-compset.txt");

  ndb2 = recset_create();
  recset_complement(compset,ndb2,&rt);   /* ndb2 should equal ndb1 */
  recset_save(ndb2,&rt,NDBNAME2);

  fprintf(stdout,"test_complement: verify..\n");

  endloop = 1 << len;   /* 8 */

  /* online_query returns false if the string is in the database; 
     {001,011,101} are the only members */
  for(i = 0; i < endloop ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,compset,&rt);
    if(q && (i!=1 && i!=3 && i!=5) ) {
      fprintf(stderr,"test_complement: oops, record <%s> not found in complement---not in DB\n",str);
      exit(3);
    }
    if( !q && (i==1 || i==3 || i==5) ) {
      fprintf(stderr,"test_complement: oops, record <%s> found in complement (in DB)\n",str);
      exit(4);
    }

    q = online_query(str,ndb2,&rt);
    if(!q && (i!=1 && i!=3 && i!=5) ) {
      fprintf(stderr,"test_complement: oops, record <%s> not found in ndb2---in DB\n",str);
      exit(3);
    }
    if( q && (i==1 || i==3 || i==5) ) {
      fprintf(stderr,"test_complement: oops, record <%s> found in ndb2 (not in DB)\n",str);
      exit(4);
    }
  }

#if 1
  if(online_compare(ndb1,&rt)) {
      fprintf(stderr,"test_complement: oops, ndb1 and the complement of the complement are different\n");
      exit(5);
  }

  runtime_setName2(&rt,"rndb-compset.txt");
  if(!online_compare(ndb1,&rt)) {
      fprintf(stderr,"test_complement: oops, ndb1 and its complement are the same\n");
      exit(6);
  }
#endif

  recset_destroy(ndb1);
  recset_destroy(ndb2);
  recset_destroy(compset);
  runtime_final(&rt);
  fprintf(stdout,"test_complement completed\n\n");
}


/* tests that compare is correct regardless of input order when one
   ndb is a subset of the other; for example, this breaks if
   recset_compare didn't check the sizes first. v.56 */
void test_runtime_compare(int modearg){
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits;
  char * dbreca = "a";
  char * dbrecb = "b";
  char * dbrecc = "c";
  char * dbrecd = "d";
  int len = 8;

  fprintf(stderr,"starting test_runtime_compare..\n");

  runtime_init(&rt);

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create_basic(ndb1,len); 

  online_add(dbreca,ndb1,&rt);
  if(online_query(dbreca,ndb1,&rt)){
    fprintf(stderr,"test_compare: reca in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrecb,ndb1,&rt);
  if(online_query(dbrecb,ndb1,&rt)){
    fprintf(stderr,"test_compare: recb in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrecc,ndb1,&rt);
  if(online_query(dbrecc,ndb1,&rt)){
    fprintf(stderr,"test_compare: recc in ndb1 not added to DB\n");
    exit(10);
  }

  online_add(dbrecd,ndb1,&rt);
  if(online_query(dbrecd,ndb1,&rt)){
    fprintf(stderr,"test_compare: recd in ndb1 not added to DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,"rndb-5.txt");

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create(ndb2,&rt); 

  online_add(dbreca,ndb2,&rt);
  if(online_query(dbreca,ndb2,&rt)){
    fprintf(stderr,"test_compare: reca in ndb2 not added to DB\n");
    exit(10);
  }

  online_add(dbrecb,ndb2,&rt);
  if(online_query(dbrecb,ndb2,&rt)){
    fprintf(stderr,"test_compare: recb in ndb2 not added to DB\n");
    exit(10);
  }

  online_add(dbrecc,ndb2,&rt);
  if(online_query(dbrecc,ndb2,&rt)){
    fprintf(stderr,"test_compare: recc in ndb2 not added to DB\n");
    exit(10);
  }

  recset_save(ndb2,&rt,"rndb-6.txt");

  fprintf(stdout,"test_runtime_compare: verify..1,2 order\n");
  runtime_setName2(&rt,"rndb-6.txt");
  
  /* 0 if same */
  if(! online_compare(ndb1,&rt)){
    fprintf(stderr,"test_runtime_compare: ndb1 and ndb2 are not the SAME!\n");
    exit(11);
  }

  runtime_setName2(&rt,"rndb-5.txt");
  if(! online_compare(ndb2,&rt)){
    fprintf(stderr,"test_runtime_compare: ndb2 and ndb1 are not the SAME!\n");
    exit(12);
  }

  recset_destroy(ndb1);
  recset_destroy(ndb2);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_compare completed\n\n");
}


/* like test_runtime_compare, however, it begins with the
   "complements" created by removing records from a full DB, tests
   that compare is correct in mode 1 v.65 */
void test_runtime_compare_directly(int modearg){
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits;
  char * dbreca = "a";
  char * dbrecb = "b";
  char * dbrecc = "c";
  char * dbrecd = "d";
  int len = 8;

  fprintf(stderr,"starting test_runtime_compare_directly..\n");

  runtime_init(&rt);
  runtime_setMinBits(&rt,1);

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  /* ndb1 has no records */

  online_remove(dbreca,ndb1,&rt);
  if(!online_query(dbreca,ndb1,&rt)){
    fprintf(stderr,"test_compare_directly: reca not in ndb1, not removed from DB\n");
    exit(10);
  }

  online_remove(dbrecb,ndb1,&rt);
  if(!online_query(dbrecb,ndb1,&rt)){
    fprintf(stderr,"test_compare_directly: recb not in ndb1, not removed from DB\n");
    exit(10);
  }

  online_remove(dbrecc,ndb1,&rt);
  if(!online_query(dbrecc,ndb1,&rt)){
    fprintf(stderr,"test_compare_directly: recc not in ndb1, not removed from DB\n");
    exit(10);
  }

  online_remove(dbrecd,ndb1,&rt);
  if(!online_query(dbrecd,ndb1,&rt)){
    fprintf(stderr,"test_compare_directly: recd not in ndb1, not removed from DB\n");
    exit(10);
  }

  recset_save(ndb1,&rt,"rndb-7.txt");

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  /* ndb2 has no records */

  online_remove(dbreca,ndb2,&rt);
  if(!online_query(dbreca,ndb2,&rt)){
    fprintf(stderr,"test_compare_directly: reca not in ndb2, not remove from DB\n");
    exit(10);
  }

  online_remove(dbrecb,ndb2,&rt);
  if(!online_query(dbrecb,ndb2,&rt)){
    fprintf(stderr,"test_compare_directly: recb not in ndb2, not remove from DB\n");
    exit(10);
  }


  online_remove(dbrecc,ndb2,&rt);
  if(!online_query(dbrecc,ndb2,&rt)){
    fprintf(stderr,"test_compare_directly: recc not in ndb2, not remove from DB\n");
    exit(10);
  }

  recset_save(ndb2,&rt,"rndb-8.txt");

  fprintf(stdout,"test_runtime_compare_directly: verify..1,2 order\n");
  runtime_setName2(&rt,"rndb-8.txt");
  runtime_setCompareMode(&rt,modearg);           /* v.65, v.72.8 was 1  */

  /* 0 if same */
  if(! online_compare(ndb1,&rt)){
    fprintf(stderr,"test_runtime_compare_directly: ndb1 and ndb2 are not the SAME!\n");
    exit(11);
  }

  runtime_setName2(&rt,"rndb-7.txt");
  if(! online_compare(ndb2,&rt)){
    fprintf(stderr,"test_runtime_compare_directly: ndb2 and ndb1 are not the SAME!\n");
    exit(12);
  }

  recset_destroy(ndb1);
  recset_destroy(ndb2);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_compare_directly completed\n\n");
}


/* like test_runtime_compare, however, it begins with the
   "complements" created by removing records from a full DB, tests
   that compare is correct in mode 2 v.65 */
void test_runtime_compare_directly_mode2(int modearg){
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits;
  char * dbreca = "a";
  char * dbrecb = "b";
  char * dbrecc = "c";
  char * dbrecd = "d";
  int len = 8;

  fprintf(stderr,"starting test_runtime_compare_directly_mode2..\n");

  runtime_init(&rt);
  runtime_setMinBits(&rt,len);   /* no *'s fully specified */

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  /* ndb1 has no records */

  online_remove(dbreca,ndb1,&rt);
  online_remove(dbrecb,ndb1,&rt);
  online_remove(dbrecc,ndb1,&rt);
  online_remove(dbrecd,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-27.txt");

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  /* ndb2 has no records */

  online_remove(dbreca,ndb2,&rt);
  online_remove(dbrecb,ndb2,&rt);
  online_remove(dbrecc,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-28.txt");

  fprintf(stdout,"test_runtime_compare_directly_mode2: verify..1,2 order\n");
  runtime_setName2(&rt,"rndb-28.txt");
  runtime_setCompareMode(&rt,modearg);           /* v.72.7, v.72.8 was 2 */

  /* 0 if same */
  if(! online_compare(ndb1,&rt)){
    fprintf(stderr,"test_runtime_compare_directly_mode2: ndb1 and ndb2 are not the SAME!\n");
    exit(11);
  }

  runtime_setName2(&rt,"rndb-27.txt");
  if(! online_compare(ndb2,&rt)){
    fprintf(stderr,"test_runtime_compare_directly_mode2: ndb2 and ndb1 are not the SAME!\n");
    exit(12);
  }

  recset_destroy(ndb1);
  recset_destroy(ndb2);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_compare_directly_mode2 completed\n\n");
}


/* based on problem example from jorge. v.57.3 */
void test_runtime_compare_binary(int modearg){
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits;
  char * dbrec3 = "0011";
  char * dbrec4 = "0100";
  char * dbrec7 = "0111";
  char * dbrec9 = "1001";
  char * dbrecb = "1011";
  char * dbrecd = "1101";
  char * dbrecf = "1111";
  int len = 4;

  fprintf(stderr,"starting test_runtime_compare_binary..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_seedRandom(&rt,7);   

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  empty_ndb_create_basic(ndb1,len); /* v.61.1 */

  online_add(dbrec3,ndb1,&rt);
  online_add(dbrec4,ndb1,&rt);
  online_add(dbrec7,ndb1,&rt);
  online_add(dbrec9,ndb1,&rt);
  online_add(dbrecb,ndb1,&rt);
  online_add(dbrecd,ndb1,&rt);
  online_add(dbrecf,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-3.txt");

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  empty_ndb_create_basic(ndb2,len);  /* v.61.1 */

  /* use same records, but different order (v.61.1) */
  online_add(dbrecd,ndb2,&rt);
  online_add(dbrec3,ndb2,&rt);
  online_add(dbrec4,ndb2,&rt);
  online_add(dbrecb,ndb2,&rt);
  online_add(dbrecf,ndb2,&rt);
  online_add(dbrec9,ndb2,&rt);
  online_add(dbrec7,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-4.txt");

  fprintf(stdout,"test_runtime_compare_binary: verify..1,2 order\n");
  runtime_setName2(&rt,"rndb-4.txt");
  
  /* comparing a file with recset, just a difference in record order
     can change the result if the number of minbits is not len, with
     insert; no longer the case v.61.3 since we're matching rather
     than looking for equality */
  /* 0 if same */
  if(online_compare(ndb1,&rt)){
    fprintf(stderr,"test_runtime_compare_binary: ndb1 and ndb2 are the SAME!\n");
    exit(11);
  }

  /* compare the other file with the other recset, just a difference
     in record order can change the result if the number of minbits is
     not len, with insert; no longer the case with v.61.3 */

  runtime_setName2(&rt,"rndb-3.txt");
  if(online_compare(ndb2,&rt)){
    fprintf(stderr,"test_runtime_compare_binary: ndb2 and ndb1 are the SAME!\n");
    exit(12);
  }

  recset_destroy(ndb1);
  recset_destroy(ndb2);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_compare binary completed\n\n");
}


/* based on problem example from jorge. v.57.3, and other directly
   comparison test v.65; updated to work without managed_growth option v.67 */
void test_runtime_compare_binary_directly_same(int modearg){
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits;
  char * dbrec3 = "0011";
  char * dbrec4 = "0100";
  char * dbrec7 = "0111";
  char * dbrec9 = "1001";
  char * dbrecb = "1011";
  char * dbrecd = "1101";
  char * dbrecf = "1111";
  int len = 4;

  fprintf(stderr,"starting test_runtime_compare_binary_directly_same..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_seedRandom(&rt,7);   
  runtime_setMinBits(&rt,1);  /* get most *'s */

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  /* ndb1 has no records */

  online_remove(dbrec3,ndb1,&rt);
  online_remove(dbrec4,ndb1,&rt);
  online_remove(dbrec7,ndb1,&rt);
  online_remove(dbrec9,ndb1,&rt);
  online_remove(dbrecb,ndb1,&rt);
  online_remove(dbrecd,ndb1,&rt);
  online_remove(dbrecf,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-9.txt");

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  /* ndb2 has no records */

  /* use same records, but different order */
  online_remove(dbrecd,ndb2,&rt);
  online_remove(dbrec3,ndb2,&rt);
  online_remove(dbrec4,ndb2,&rt);
  online_remove(dbrecb,ndb2,&rt);
  online_remove(dbrecf,ndb2,&rt);
  online_remove(dbrec9,ndb2,&rt);
  online_remove(dbrec7,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-10.txt");
  recset_destroy(ndb1);
  recset_destroy(ndb2);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_compare_binary_directly_same: verify..1,2 order\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);

  runtime_setName(&rt,"rndb-9.txt");
  runtime_setName2(&rt,"rndb-10.txt");

  runtime_setCompareMode(&rt,modearg);     /* v.65, v.72.8 was 1 */

  /* comparing a file with recset, just a difference in record order
     can change the result if the number of minbits is not len, without
     MANAGED_GROWTH (v.67) */

#ifndef MANAGED_GROWTH
  fprintf(stdout,"test_runtime_compare_binary_directly_same: since no MANAGED_GROWTH, set minbits to %d\n",len);
  runtime_setMinBits(&rt,len);    /* mode 1 only works with managed_growth unless no dc's v.67 */
#endif

  ndb1 = recset_create();
  recset_build(ndb1,&rt,runtime_getName(&rt),ndbformat);

  /* 0 if same */
  if(online_compare(ndb1,&rt)){
    fprintf(stderr,"test_runtime_compare_binary_directly_same: ndb1 and ndb2 are the SAME!\n");
    exit(11);
  }

  runtime_final(&rt);
  recset_destroy(ndb1);

  /* compare the other file with the other recset, just a difference
     in record order can change the result if the number of minbits is
     not len, with insert; no longer the case with v.61.3 */

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  runtime_setName(&rt,"rndb-10.txt");
  runtime_setName2(&rt,"rndb-9.txt");

  runtime_setCompareMode(&rt,modearg);     /* v.65, v.72.8 was 1 */

#ifndef MANAGED_GROWTH
  fprintf(stdout,"test_runtime_compare_binary_directly_same: since no MANAGED_GROWTH, set minbits to %d\n",len);
  runtime_setMinBits(&rt,len);    /* mode 1 only works with managed_growth unless no dc's v.67 */
#endif


  ndb2 = recset_create();
  recset_build(ndb2,&rt,runtime_getName(&rt),ndbformat);

  if(online_compare(ndb2,&rt)){
    fprintf(stderr,"test_runtime_compare_binary_directly_same: ndb2 and ndb1 are the SAME!\n");
    exit(12);
  }

  recset_destroy(ndb2);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_compare_binary_directly_same completed\n\n");
}



/* based on problem example from jorge. v.57.3, and other directly
   comparison test v.65; updated to work without managed_growth option v.67 */
void test_runtime_compare_binary_directly_same_mode2(int modearg){
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int numunits;
  char * dbrec3 = "0011";
  char * dbrec4 = "0100";
  char * dbrec7 = "0111";
  char * dbrec9 = "1001";
  char * dbrecb = "1011";
  char * dbrecd = "1101";
  char * dbrecf = "1111";
  int len = 4;

  fprintf(stderr,"starting test_runtime_compare_binary_directly_same_mode2..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_seedRandom(&rt,7);   
  runtime_setMinBits(&rt,1);  /* get most *'s */

  /* create the two recsets */
  ndb1 = recset_create();
  recset_setlength(ndb1,len);
  numunits = recset_numunits(ndb1);

  /* ndb1 has no records */

  online_remove(dbrec3,ndb1,&rt);
  online_remove(dbrec4,ndb1,&rt);
  online_remove(dbrec7,ndb1,&rt);
  online_remove(dbrec9,ndb1,&rt);
  online_remove(dbrecb,ndb1,&rt);
  online_remove(dbrecd,ndb1,&rt);
  online_remove(dbrecf,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-25.txt");

  ndb2 = recset_create();
  recset_setlength(ndb2,len);
  numunits = recset_numunits(ndb2);

  /* ndb2 has no records */

  /* use same records, but different order */
  online_remove(dbrecd,ndb2,&rt);
  online_remove(dbrec3,ndb2,&rt);
  online_remove(dbrec4,ndb2,&rt);
  online_remove(dbrecb,ndb2,&rt);
  online_remove(dbrecf,ndb2,&rt);
  online_remove(dbrec9,ndb2,&rt);
  online_remove(dbrec7,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-26.txt");
  recset_destroy(ndb1);
  recset_destroy(ndb2);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode2: verify..1,2 order\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);

  runtime_setName(&rt,"rndb-25.txt");
  runtime_setName2(&rt,"rndb-26.txt");

  runtime_setCompareMode(&rt,modearg);     /* v.72.7, v.72.8 */

  /* not sure mode 2 works in this case ???? */
  /* comparing a file with recset, just a difference in record order
     can change the result if the number of minbits is not len, without
     MANAGED_GROWTH (v.67) */

#ifndef MANAGED_GROWTH
  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode2: since no MANAGED_GROWTH, set minbits to %d\n",len);
  runtime_setMinBits(&rt,len);    /* mode 1 only works with managed_growth unless no dc's v.67 */
#endif

  ndb1 = recset_create();
  recset_build(ndb1,&rt,runtime_getName(&rt),ndbformat);

  /* 0 if same */
  if(online_compare(ndb1,&rt)){
    fprintf(stderr,"test_runtime_compare_binary_directly_same_mode2: ndb1 and ndb2 are the SAME!\n");
    exit(11);
  }

  runtime_final(&rt);
  recset_destroy(ndb1);

  /* compare the other file with the other recset, just a difference
     in record order can change the result if the number of minbits is
     not len, with insert; no longer the case with v.61.3 */

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  runtime_setName(&rt,"rndb-25.txt");
  runtime_setName2(&rt,"rndb-26.txt");

  runtime_setCompareMode(&rt,modearg);     /* v.72.7, v.72.8 */

#ifndef MANAGED_GROWTH
  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode2: since no MANAGED_GROWTH, set minbits to %d\n",len);
  runtime_setMinBits(&rt,len);    /* mode 1 only works with managed_growth unless no dc's v.67 */
#endif


  ndb2 = recset_create();
  recset_build(ndb2,&rt,runtime_getName(&rt),ndbformat);

  if(online_compare(ndb2,&rt)){
    fprintf(stderr,"test_runtime_compare_binary_directly_same_mode2: ndb2 and ndb1 are the SAME!\n");
    exit(12);
  }

  recset_destroy(ndb2);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode2 completed\n\n");
}


/* based on amgu process v.68 */
/* 110000000000100000 is the only difference, it's in RNDB-un1.txt only */
void test_runtime_compare_binary_directly_different(int modearg){
  Runtime rt;
  Recset * ndb;

  fprintf(stderr,"starting test_runtime_compare_binary_directly_different..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setName(&rt,"RNDB-un_1.test");
  runtime_setName2(&rt,"RNDB-un_2.test");

  runtime_setCompareMode(&rt,modearg);

#ifndef MANAGED_GROWTH
 {
   int len = 18;
   fprintf(stdout,"test_runtime_compare_binary_directly_different: since no MANAGED_GROWTH, set minbits to %d\n",len);
   runtime_setMinBits(&rt,len);    /* mode 1 only works with managed_growth unless no dc's v.67 */
 }
#endif

  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);

  if(!online_compare(ndb,&rt)){               /* returns 0 if same */
    fprintf(stderr,"test_runtime_compare_binary_directly_different: ndb2 and ndb1 are DIFFERENT!\n");
    exit(15);
  }

  /* 110000000000100000 is the culprit */

  recset_destroy(ndb);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_compare_binary_directly_different completed\n\n");
}



/* based on amgu process v.68 */
/* 110000000000100000 is the only difference, it's in RNDB-un1.txt only */
/* this test BREAKS mode 0 v.72.7.1 */
void test_runtime_compare_binary_different(int modearg){
  Runtime rt;
  Recset * ndb;

  fprintf(stderr,"starting test_runtime_compare_binary_different..\n");

  /* unnegate the complement test files first */
  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setName(&rt,"RNDB-un_1.test");
  runtime_setNameOutputFile(&rt,"rndb-doubleUN_1.txt");

  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);
  
  runtime_setCommand(&rt,'U');
  online_complement(ndb,&rt);
  recset_destroy(ndb);
  runtime_final(&rt);

  /* do the second one in the same way */
  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setName(&rt,"RNDB-un_2.test");
  runtime_setNameOutputFile(&rt,"rndb-doubleUN_2.txt");

  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);
  
  runtime_setCommand(&rt,'U');
  online_complement(ndb,&rt);
  recset_destroy(ndb);
  runtime_final(&rt);

  fprintf(stderr,"test_runtime_compare_binary_different: time to compare...\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setName(&rt,"rndb-doubleUN_1.txt");
  runtime_setName2(&rt,"rndb-doubleUN_2.txt");
  runtime_setCompareMode(&rt,modearg);

  ndb = recset_create();
  recset_build(ndb,&rt,runtime_getName(&rt),ndbformat);

  if(!online_compare(ndb,&rt)){               /* returns 0 if same */
    fprintf(stderr,"test_runtime_compare_binary_different: ndb2 and ndb1 are DIFFERENT!\n");
    exit(15);
  }

  /* 110000000000100000 is the culprit */

  recset_destroy(ndb);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_compare_binary_different completed\n\n");
}



/* based on Eric's counter-example that breaks mode 1 v.72.7.1 */
void test_runtime_compare_binary_directly_same_mode1(int modearg){
  Runtime rt;
  Recset * cdb1;
  Recset * cdb2;
  int numunits;
  char * dbrec4 = "0100";
  char * dbrec5 = "0101";
  char * dbrec6 = "0110";
  char * dbrec7 = "0111";
  char * dbrec8 = "1000";
  char * dbreca = "1010";
  char * dbrecc = "1100";
  char * dbrece = "1110";
  int len = 4;

  fprintf(stderr,"starting test_runtime_compare_binary_directly_same_mode1..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_seedRandom(&rt,7);   
  runtime_setMinBits(&rt,1);  /* get most *'s */

  /* create the two recsets */
  cdb1 = recset_create();
  recset_setlength(cdb1,len);
  numunits = recset_numunits(cdb1);

  /* cdb1 has no records */

  online_remove(dbrec4,cdb1,&rt);
  online_remove(dbrec5,cdb1,&rt);
  online_remove(dbrec6,cdb1,&rt);
  online_remove(dbrec7,cdb1,&rt);
  online_remove(dbrec8,cdb1,&rt);
  online_remove(dbreca,cdb1,&rt);
  online_remove(dbrecc,cdb1,&rt);
  online_remove(dbrece,cdb1,&rt);

  recset_save(cdb1,&rt,"rndb-29.txt");

  cdb2 = recset_create();
  recset_setlength(cdb2,len);
  numunits = recset_numunits(cdb2);

  /* cdb2 has no records */
  runtime_setMinBits(&rt,len);  /* get no *'s */
  /* use same records, but no *'s */
  online_remove(dbrec4,cdb2,&rt);
  online_remove(dbrec5,cdb2,&rt);
  online_remove(dbrec6,cdb2,&rt);
  online_remove(dbrec7,cdb2,&rt);
  online_remove(dbrec8,cdb2,&rt);
  online_remove(dbreca,cdb2,&rt);
  online_remove(dbrecc,cdb2,&rt);
  online_remove(dbrece,cdb2,&rt);

  recset_save(cdb2,&rt,"rndb-30.txt");
  recset_destroy(cdb1);
  recset_destroy(cdb2);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode1: verify..\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);

  runtime_setName(&rt,"rndb-29.txt"); 
  runtime_setName2(&rt,"rndb-30.txt"); 

  runtime_setCompareMode(&rt,modearg);    

#ifndef MANAGED_GROWTH
  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode1: since no MANAGED_GROWTH, set minbits to %d\n",len);
  runtime_setMinBits(&rt,len);    /* mode 1 only works with managed_growth unless no dc's v.67 */
#endif
  
  cdb1 = recset_create();
  recset_build(cdb1,&rt,runtime_getName(&rt),ndbformat);

  /* 0 if same */
  if(online_compare(cdb1,&rt)){
    fprintf(stderr,"test_runtime_compare_binary_directly_same_mode1: cdb1 and cdb2 are the SAME!\n");
    exit(11);
  }

  runtime_final(&rt);
  recset_destroy(cdb1);

   fprintf(stdout,"test_runtime_compare_binary_directly_same_mode1: verify with test..\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);

  runtime_setName(&rt,"RNDB-mode1.test"); /* positive ternary that Eric devised */
  runtime_setName2(&rt,"rndb-30.txt"); 

  runtime_setCompareMode(&rt,modearg);    

#ifndef MANAGED_GROWTH
  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode1: since no MANAGED_GROWTH, set minbits to %d\n",len);
  runtime_setMinBits(&rt,len);    /* mode 1 only works with managed_growth unless no dc's v.67 */
#endif

  cdb1 = recset_create();
  recset_build(cdb1,&rt,runtime_getName(&rt),ndbformat);

  /* 0 if same */
  if(online_compare(cdb1,&rt)){
    fprintf(stderr,"test_runtime_compare_binary_directly_same_mode1: cdb1 and cdb2 are the SAME!\n");
    exit(12);
  }

  runtime_final(&rt);
  recset_destroy(cdb1);

   fprintf(stdout,"test_runtime_compare_binary_directly_same_mode1: verify with test again..\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);

  runtime_setName(&rt,"RNDB-mode1.test");
  runtime_setName2(&rt,"rndb-29.txt"); 

  runtime_setCompareMode(&rt,modearg);    

#ifndef MANAGED_GROWTH
  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode1: since no MANAGED_GROWTH, set minbits to %d\n",len);
  runtime_setMinBits(&rt,len);    /* mode 1 only works with managed_growth unless no dc's v.67 */
#endif

  cdb1 = recset_create();
  recset_build(cdb1,&rt,runtime_getName(&rt),ndbformat);

  /* 0 if same */
  if(online_compare(cdb1,&rt)){
    fprintf(stderr,"test_runtime_compare_binary_directly_same_mode1: cdb1 and cdb2 are the SAME!\n");
    exit(13);
  }

  runtime_final(&rt);
  recset_destroy(cdb1);

  fprintf(stdout,"test_runtime_compare_binary_directly_same_mode1 completed\n\n");
}



void test_runtime_intersection_boundary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int i;
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;

  fprintf(stdout,"test_runtime_intersection_boundary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the three recsets to be intersected, two at a time */
  ndb1 = recset_create();
  empty_ndb_create_basic(ndb1,len); 
  recset_save(ndb1,&rt,runtime_getName(&rt));

  /* create empty's complement, saved to default, un_NDB.txt */
  runtime_setCommand(&rt,'U');
  online_complement(ndb1,&rt);
  runtime_setCommand(&rt,'x');

  recset_destroy(ndb1);

  runtime_final(&rt);
  runtime_init(&rt);
  runtime_setBinMode(&rt);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_intersection_boundary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_intersection_boundary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_intersection_boundary: now for the intersection with empty..\n");

  runtime_setRelOperator(&rt,"I");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_intersection_boundary: verify the intersection..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_intersection_boundary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  /* empty ndb negI x is emptyndb */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(!q ) {
      fprintf(stderr,"test_runtime_intersection_boundary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_intersection_boundary: now for the intersection with full..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setRelOperator(&rt,"I");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,UNNDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);
  
  fprintf(stdout,"test_runtime_intersection_boundary: verify the intersection..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_intersection_boundary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  /* full ndb negI x is x */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 1 && i!=2) ) {
      fprintf(stderr,"test_runtime_intersection_boundary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i==1 || i==2)) {
      fprintf(stderr,"test_runtime_intersection_boundary: oops, record <%s> found in RNDB\n",str);
      exit(4);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_intersection_boundary completed\n\n");
}


void test_runtime_union_boundary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int i;
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;

  fprintf(stdout,"test_runtime_union_boundary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the three recsets to be unioned, two at a time */
  ndb1 = recset_create();
  empty_ndb_create_basic(ndb1,len); 
  recset_save(ndb1,&rt,runtime_getName(&rt));

  /* create empty's complement, saved to default, un_NDB.txt */
  runtime_setCommand(&rt,'U');
  online_complement(ndb1,&rt);
  runtime_setCommand(&rt,'x');
  
  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_union_boundary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_union_boundary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_union_boundary: now for the union with empty..\n");

  runtime_setRelOperator(&rt,"U");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_union_boundary: verify the union..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_union_boundary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  /* empty ndb negU x is x */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 1 && i!=2) ) {
      fprintf(stderr,"test_runtime_union_boundary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i==1 || i==2)) {
      fprintf(stderr,"test_runtime_union_boundary: oops, record <%s> found in RNDB\n",str);
      exit(4);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_union_boundary: now for the union with full..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setRelOperator(&rt,"U");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,UNNDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);
  
  fprintf(stdout,"test_runtime_union_boundary: verify the union with full..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_union_boundary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  /* full ndb negU x is full */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(q) {
      fprintf(stderr,"test_runtime_union_boundary: oops, record <%s> found in RNDB---not in DB\n",str);
      exit(3);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_union_boundary completed\n\n");
}


void test_runtime_difference_boundary()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int i;
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;

  fprintf(stdout,"test_runtime_difference_boundary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  /* create the three recsets to be intersected, two at a time */
  ndb1 = recset_create();
  empty_ndb_create_basic(ndb1,len); 
  recset_save(ndb1,&rt,runtime_getName(&rt));

  /* create empty's complement, full, saved to default, un_NDB.txt */
  runtime_setCommand(&rt,'U');
  online_complement(ndb1,&rt);
  runtime_setCommand(&rt,'x');

  recset_destroy(ndb1);

  ndb2 = recset_create();
  recset_setlength(ndb2,len);

  empty_ndb_create(ndb2,&rt);

  online_add(dbrec2a,ndb2,&rt);
  if(online_query(dbrec2a,ndb2,&rt)){
    fprintf(stderr,"test_runtime_difference_boundary: rec1 in ndb2 not added to DB\n");
    exit(2);
  }

  online_add(dbrec2b,ndb2,&rt);
  if(online_query(dbrec2b,ndb2,&rt)){
    fprintf(stderr,"test_runtime_difference_boundary: rec2 in ndb2 not added to DB\n");
    exit(2);
  }

  recset_save(ndb2,&rt,NDBNAME2);
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_difference_boundary: now for the difference with empty..\n");

  runtime_setRelOperator(&rt,"D");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_difference_boundary: verify the difference with empty..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_difference_boundary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  /* empty ndb negDiff x is empty */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if( !q ) {
      fprintf(stderr,"test_runtime_difference_boundary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_difference_boundary: now for the difference with full..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setRelOperator(&rt,"D");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,UNNDBNAME,ndbformat);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);
  
  fprintf(stdout,"test_runtime_difference_boundary: verify the difference with full..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_difference_boundary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }


  /* online_query returns false if the string is in the database */
  /* full ndb negDiff x is x' */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(!q && (i == 1 || i==2) ) {
      fprintf(stderr,"test_runtime_difference_boundary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }

    if(q && (i!=1 && i!=2)) {
      fprintf(stderr,"test_runtime_difference_boundary: oops, record <%s> found in RNDB\n",str);
      exit(4);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_difference_boundary: now for the other difference with empty..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setRelOperator(&rt,"D");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME2,ndbformat);
  runtime_setName2(&rt,NDBNAME);

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_difference_boundary: verify the other difference with empty..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_difference_boundary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }

  /* online_query returns false if the string is in the database */
  /* x negDiff emptyndb is x */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(!q && (i != 1 && i!=2) ) {
      fprintf(stderr,"test_runtime_difference_boundary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i==1 || i==2)) {
      fprintf(stderr,"test_runtime_difference_boundary: oops, record <%s> found in RNDB\n",str);
      exit(4);
    }
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_difference_boundary: now for the other difference with full..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setRelOperator(&rt,"D");

  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME2,ndbformat);
  runtime_setName2(&rt,UNNDBNAME);
  
  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);
  
  fprintf(stdout,"test_runtime_difference_boundary: verify the other difference with full..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  if(recset_length(ndb1) != 3){
    fprintf(stderr,"test_runtime_difference_boundary: RNDB3 record length is %d, not 3\n",
            recset_length(ndb1));
    exit(3);
  }


  /* online_query returns false if the string is in the database */
  /* x negDiff full ndb is empty */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(!q ) {
      fprintf(stderr,"test_runtime_difference_boundary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_difference_boundary completed\n\n");
}


void test_runtime_compare_compressed_db_and_complement_ndb(int modearg)
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  char * dbrec0 = "000";
  char * dbrec1 = "001";
  char * dbrec4 = "100";
  int len = 3;

  fprintf(stdout,"test_runtime_compare_compressed_db_and_complement_ndb..starting\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);

  ndb1 = recset_create();
  full_ndb_create_basic(ndb1,len); 

  online_remove(dbrec0,ndb1,&rt);
  online_remove(dbrec1,ndb1,&rt);
  online_remove(dbrec4,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-11.txt");

  ndb2 = recset_create();
  empty_ndb_create_basic(ndb2,len); 

  online_add(dbrec0,ndb2,&rt);
  online_add(dbrec1,ndb2,&rt);
  online_add(dbrec4,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-12.txt");

  fprintf(stdout,"test_runtime_compare_compressed_db_and_complement_ndb..complement ndb 2\n");

  runtime_setCommand(&rt,'U');
  online_complement(ndb2,&rt);     /* in un_NDB.txt */
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_compare_ndb_and_complement..compare\n");

  runtime_setName2(&rt,"un_NDB.txt");

  if(online_compare(ndb1,&rt)){
    fprintf(stderr,"test_runtime_compare_compressed_db_and_complement_ndb: ndb2 and ndb1 are DIFFERENT with mode 0!\n");
    exit(13);
  }

  runtime_setCompareMode(&rt,modearg);   /* v.72.8 was 1 */

  if(online_compare(ndb1,&rt)){
    fprintf(stderr,"test_runtime_compare_compressed_db_and_complement_ndb: ndb2 and ndb1 are DIFFERENT with mode 1!\n");
    exit(13);
  }

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_compare_compressed_db_and_complement_ndb completed\n");
}

/* test case with no *'s v.72 */
void test_runtime_relevance()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int i;
  char * dbrec1 = "001";
  char * dbrec2 = "010";
  char * dbrec4 = "100";
  char * dbrec5 = "101";
  int len = 3;

  fprintf(stdout,"test_runtime_relevance..starting\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,len);

  ndb1 = recset_create();
  full_ndb_create_basic(ndb1,len); 

  online_remove(dbrec1,ndb1,&rt);
  online_remove(dbrec2,ndb1,&rt);
  online_remove(dbrec4,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-14.txt");

  ndb2 = recset_create();
  full_ndb_create_basic(ndb2,len); 

  online_remove(dbrec5,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-15.txt");
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_relevance: time..to do it\n");

  runtime_setName2(&rt,"rndb-15.txt");
  runtime_setRelOperator(&rt,"R");

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_relevance: verify...\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  /* online_query normally returns false if the string is in the database */
  /* here, we interpret the opposite meaning because we're working in the positive */
  /* result should contain only 1 and 4 */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(q && (i != 1 && i != 4) ) {
      fprintf(stderr,"test_runtime_relevance: oops, record <%s> found in RNDB3\n",str);
      exit(3);
    }
    if(!q && ( i==1 || i == 4)) {
      fprintf(stderr,"test_runtime_relevance: oops, record <%s> not found in RNDB3\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_relevance completed\n");
}


/* this second relevance test has *'s in the input set */
void test_runtime_relevance2()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int i;
  char * dbrec1 = "001";
  char * dbrec2 = "010";
  char * dbrec4 = "100";
  char * dbrec5 = "101";
  char * dbrecq1 = "**1";
  char * dbrecq4 = "1**";
  int len = 3;

  fprintf(stdout,"test_runtime_relevance2..starting\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,1);

  ndb1 = recset_create();
  full_ndb_create_basic(ndb1,len); 

  online_remove(dbrec1,ndb1,&rt);
  online_remove(dbrec2,ndb1,&rt);
  online_remove(dbrec4,ndb1,&rt);
  online_remove(dbrec5,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-16.txt");

  ndb2 = recset_create();
  full_ndb_create_basic(ndb2,len); 

  online_remove(dbrecq1,ndb2,&rt);
  online_remove(dbrecq4,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-17.txt");
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_relevance2: time..to do it\n");

  runtime_setName2(&rt,"rndb-17.txt");
  runtime_setRelOperator(&rt,"U");

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_relevance2: verify...\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  /* online_query normally returns false if the string is in the database */
  /* here, we interpret the opposite meaning because we're working in the positive */
  /* result should contain only 1 and 4 and 5 */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(q && (i != 1 && i != 4 && i != 5) ) {
      fprintf(stderr,"test_runtime_relevance2: oops, record <%s> found in RNDB3\n",str);
      exit(3);
    }
    if(!q && ( i==1 || i == 4 || i == 5)) {
      fprintf(stderr,"test_runtime_relevance2: oops, record <%s> not found in RNDB3\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_relevance2 completed\n");
}


/* this third relevance test is based on the first Jorge example,
given SH = {x,xz,y,yz,z} the relevance of x should be {x, xz}, but we
were also getting {z} in the initial implementation of
rec_relevance v.71.1 */
void test_runtime_relevance3()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int i;
  char * dbrecx = "100";
  char * dbrecxz = "101";
  char * dbrecy = "010";
  char * dbrecyz = "011";
  char * dbrecz = "001";
  char * dbrecqx = "1**";
  int len = 3;

  fprintf(stdout,"test_runtime_relevance3..starting\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,1);

  ndb1 = recset_create();
  full_ndb_create_basic(ndb1,len); 

  online_remove(dbrecx,ndb1,&rt);
  online_remove(dbrecxz,ndb1,&rt);
  online_remove(dbrecy,ndb1,&rt);
  online_remove(dbrecyz,ndb1,&rt);
  online_remove(dbrecz,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-18.txt");

  ndb2 = recset_create();
  full_ndb_create_basic(ndb2,len); 

  online_remove(dbrecqx,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-19.txt");
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_relevance3: time..to do it\n");

  runtime_setName2(&rt,"rndb-19.txt");
  runtime_setRelOperator(&rt,"U");

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_relevance3: verify...\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  /* online_query normally returns false if the string is in the database */
  /* here, we interpret the opposite meaning because we're working in the positive */
  /* result should contain only 4 and 5 */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[3];
    itobs(i,str,3);
    q = online_query(str,ndb1,&rt);
    if(q && (i != 4 && i != 5) ) {
      fprintf(stderr,"test_runtime_relevance3: oops, record <%s> found in RNDB3\n",str);
      exit(3);
    }
    if(!q && ( i == 4 || i == 5)) {
      fprintf(stderr,"test_runtime_relevance3: oops, record <%s> not found in RNDB3\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_relevance3 completed\n");
}


/* simple case, where csh = { y, xy} and we want the x's, which should produce xy
   regardless of whether csh is represented as *1 or {01,11} v.71.1 */
void test_runtime_relevance4()
{
  Runtime rt;
  Recset * ndb1;
  Recset * ndb2;
  int i;
  char * dbrecqx = "1*";
  char * dbrecy = "01";
  char * dbrecxy = "11";
  int len = 2;

  fprintf(stdout,"test_runtime_relevance4..starting\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,1);

  ndb1 = recset_create();
  full_ndb_create_basic(ndb1,len); 

  online_remove(dbrecy,ndb1,&rt);
  online_remove(dbrecxy,ndb1,&rt);

  recset_save(ndb1,&rt,"rndb-20.txt");

  ndb2 = recset_create();
  full_ndb_create_basic(ndb2,len); 

  online_remove(dbrecqx,ndb2,&rt);

  recset_save(ndb2,&rt,"rndb-21.txt");
  recset_destroy(ndb2);

  fprintf(stdout,"test_runtime_relevance4: time..to do it\n");

  runtime_setName2(&rt,"rndb-21.txt");
  runtime_setRelOperator(&rt,"U");

  online_relationaloperation(ndb1,&rt);

  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_relevance4: verify...\n");
  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  ndb1 = recset_create();
  recset_build(ndb1,&rt,NDBNAME3,ndbformat);

  /* online_query normally returns false if the string is in the database */
  /* here, we interpret the opposite meaning because we're working in the positive */
  /* result should contain only 3 */
  for(i = 0; i < 4 ; i++) {
    int q;
    char str[3];
    itobs(i,str,2);
    q = online_query(str,ndb1,&rt);
    if(q && (i != 3) ) {
      fprintf(stderr,"test_runtime_relevance4: oops, record <%s> found in RNDB3\n",str);
      exit(3);
    }
    if(!q && ( i == 3)) {
      fprintf(stderr,"test_runtime_relevance4: oops, record <%s> not found in RNDB3\n",str);
      exit(4);
    }
  }
  recset_destroy(ndb1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_relevance4 completed\n");
}


void test_runtime_setdifference_binary()
{
  Runtime rt;
  Recset * db1;
  Recset * db2;
  int numunits, i;
  char * dbrec1a = "001";
  char * dbrec1b = "101";
  char * dbrec2a = "001";
  char * dbrec2b = "010";
  int len = 3;

  fprintf(stdout,"test_runtime_setdifference_binary starting..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  runtime_setMinBits(&rt,len);

  /* create the two recsets to be diff'ed */
  db1 = recset_create();
  recset_setlength(db1,len);
  numunits = recset_numunits(db1);

  full_ndb_create_basic(db1,len); 

  online_remove(dbrec1a,db1,&rt);
  online_remove(dbrec1b,db1,&rt);
  recset_save(db1,&rt,"rndb-22.txt");

  db2 = recset_create();
  recset_setlength(db2,len);
  numunits = recset_numunits(db2);

  full_ndb_create_basic(db2,len); 

  online_remove(dbrec2a,db2,&rt);
  online_remove(dbrec2b,db2,&rt);

  recset_save(db2,&rt,"rndb-23.txt");
  recset_destroy(db2);

  fprintf(stdout,"test_runtime_setdifference_binary: now for the difference..\n");

  runtime_setName2(&rt,"rndb-23.txt");
  runtime_setRelOperator(&rt,"O");

  online_relationaloperation(db1,&rt);

  recset_destroy(db1);
  runtime_final(&rt);

  fprintf(stdout,"test_runtime_setdifference_binary: verify the difference..\n");

  runtime_init(&rt);
  runtime_setBinMode(&rt);
  
  db1 = recset_create();
  recset_build(db1,&rt,NDBNAME3,ndbformat);

  /* answer has only the solutinos in DB1 that are not in DB2, {101}*/
  /* online_query returns false if the string is in the database */
  /* reverse results meaning for ternary positive dbs */
  for(i = 0; i < 8 ; i++) {
    int q;
    char str[4];
    itobs(i,str,len);
    q = online_query(str,db1,&rt);
    if(!q && (i==5) ) {
      fprintf(stderr,"test_runtime_setdifference_binary: oops, record <%s> not found in RNDB---in DB\n",str);
      exit(3);
    }
    if(q && (i!=5) ) {
      fprintf(stderr,"test_runtime_setdifference_binary: oops, record <%s> found in RNDB\n",str);
      exit(3);
    }
  }

  recset_destroy(db1);
  runtime_final(&rt);
  fprintf(stdout,"test_runtime_setdifference_binary completed\n\n");
}

