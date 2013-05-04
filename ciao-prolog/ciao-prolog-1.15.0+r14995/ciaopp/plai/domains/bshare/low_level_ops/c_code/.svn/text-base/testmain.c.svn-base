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
  * Filename      : testmain.c
  * Language      : C using the GNU gcc compiler 
  * 
  * Purpose       : NDB Test Main
  * 
  * Creator       : E.S. Ackley
  * Creation Date : Thu Dec 30 11:54:56 2004 
  * Updated       : Fri May  9 09:07:08 2008 
  *
  * Comments      : based on the Fernando Esponda NDB Algorithms
  *
  * -------------------------------------------------------------------------
  */
#include "perm.h"
#include "freq.h"
#include "test.h"

#if (defined NORANDSATLENLIMIT)
#error in MAKEFILE: NORANDSATLENLIMIT PRODUCES MULTIPLE TEST SOLUTIONS - PLEASE REMOVE BEFORE RECOMPILING TESTMAIN
#endif 

#if (defined ISCHARDQR)
#error in MAKEFILE: ISCHARDQR PRODUCES MULTIPLE TEST SOLUTIONS - PLEASE REMOVE BEFORE RECOMPILING TESTMAIN
#endif 


int main () 
{
  seed_random(1);

#if 1
  test_rec();
  test_rec_int();
  test_rec_or();
  test_perm();
  test_freq();
  test_freq_sort(10);
  test_recset();
  test_recset_split(); 
  test_recset_split_match(); 
  test_runtime_empty(24,7); 
  test_runtime_empty_randomSATformula(100,3); 
  test_runtime_singleton(100,3); 
  test_runtime_singleton_ascii(96,3); 
  test_runtime_singleton_double(100,3); 
  test_runtime(24,7);
  test_runtime_ascii();
  test_runtime_ascii2();
  test_runtime_ascii3();
  test_runtime_query();
  test_runtime_query_partial(); 
  test_runtime_query_partial_binary();
  test_runtime_query_partial_binary2(); 
  test_runtime_select_theta_binary();
  test_runtime_select_theta_ascii();
  test_runtime_select_theta_ascii_2();
  test_runtime_join_binary();
  test_runtime_join_ascii();
  test_runtime_intersection_binary();
  test_runtime_crossproduct_binary();
  test_runtime_crossproduct_ascii();
  test_runtime_union_binary();
  test_runtime_join_ends_binary(); /* after union test */
  test_runtime_difference_binary();
  test_runtime_union_boundary();
  test_runtime_intersection_boundary();
  test_runtime_difference_boundary();
  test_runtime_complete8bit(); 
  test_runtime_random_qtrfull_8bit(3);
  test_runtime_onebit_morph();
  test_runtime_patterns();
  test_runtime_patterns2();
  test_runtime_pattern_sets();
  test_runtime_compare_binary(3);
  test_runtime_compare_binary_directly_same(3);
  test_runtime_compare_binary_directly_same_mode2(2);
  test_rec_hammingdistance();
  test_complement();
  test_runtime_compare(3);
  test_runtime_compare_directly(3);
  test_runtime_compare_directly_mode2(3);
  test_runtime_compare_compressed_db_and_complement_ndb(3);
  test_runtime_compare_binary_directly_different(3);
  test_runtime_compare_binary_different(3);            /* breaks mode 0 */
  test_runtime_compare_binary_directly_same_mode1(3);  /* breaks mode 1 */
  test_runtime_relevance4();
  test_runtime_relevance3();
  test_runtime_relevance2();
  test_runtime_relevance();
  test_runtime_setdifference_binary();
#endif


  /* cpu intensive, test separately */ 
#if 0
 test_easy_reduce_simple_fullbit(3); 
 test_runtime_emptyp();
 test_runtime_emptyp_100bit(3);   /* minbitarg */
 test_runtime_emptyp_100bit_singleton_with_2solutions();
 test_runtime_emptyp_100bit_singleton_infamous();
#endif


#if 1
 test_runtime_imprecise_binary_union_of_patterns();
 test_runtime_imprecise_binary_union_of_patterns2();  /* jorge's examples */
 test_runtime_imprecise_binary_union_of_patterns3();
 test_runtime_imprecise_binary_union_of_patterns4();
 test_runtime_imprecise_binary_union_of_patterns5();  /* jorge's examples */
#endif

#if 1
 test_runtime_binary_union_of_bits();
 test_runtime_binary_union_of_bits2();  /* jorge's examples */
 test_runtime_binary_union_of_bits3();
 test_runtime_binary_union_of_bits4();
 test_runtime_binary_union_of_bits5();  /* jorge's examples */
 test_runtime_binary_union_3673();
 test_runtime_binary_union_star_closure();
#endif

 return 0;
}
