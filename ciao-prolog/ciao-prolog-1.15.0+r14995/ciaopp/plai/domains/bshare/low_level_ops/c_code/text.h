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
 * Filename      : test.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       :  NDB Runtime Tests
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Tue Feb 23 00:51:03 2007 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBTESTS_H
#define UNMNDBTESTS_H

void test_runtime(int lenarg, int minbitsarg);
void test_runtime_empty(int lenarg, int minbitsarg);
void test_runtime_empty_randomSATformula(int lenarg, int minbitsarg); /* v.20 */
void test_runtime_singleton(int lenarg, int minbitsarg); /* v.21 */
void test_runtime_singleton_ascii(int lenarg, int minbitsarg); /* v.21 */
void test_runtime_ascii(void);
void test_runtime_ascii2(void);
void test_runtime_ascii3(void);
void test_runtime_query(void);
void test_runtime_query_partial(void);
void test_runtime_query_partial_binary(void);
void test_runtime_query_partial_binary2(void);
void  test_runtime_select_theta_binary(void);
void test_runtime_select_theta_ascii(void);
void test_runtime_select_theta_ascii_2(void);
void test_runtime_binary_join(void);
void test_runtime_ascii_join(void);
void test_runtime_binary_intersection(void);
void test_runtime_binary_crossproduct(void);
void test_runtime_ascii_crossproduct(void);
void test_runtime_binary_join_ends(void);
void test_runtime_binary_union(void);
void test_runtime_complete8bit(void);
void test_runtime_random_qtrfull_8bit(int sarg);
void test_runtime_patterns(void);
void test_runtime_patterns2(void);
void test_runtime_pattern_sets(void);
void test_runtime_onebit_morph(void);
void test_runtime_emptyp(void);
void test_runtime_emptyp_100bit(int minbitarg);
void test_runtime_emptyp_100bit_singleton_with_2solutions(void);
void test_runtime_emptyp_100bit_singleton_infamous(void);

void test_runtime_imprecise_binary_union_of_patterns(void);
void test_runtime_imprecise_binary_union_of_patterns2(void);
void test_runtime_imprecise_binary_union_of_patterns3(void);
void test_runtime_imprecise_binary_union_of_patterns4(void);
void test_runtime_imprecise_binary_union_of_patterns5(void);

void test_runtime_binary_union_of_patterns(void);
void test_runtime_binary_union_of_patterns2(void);
void test_runtime_binary_union_of_patterns3(void);
void test_runtime_binary_union_of_patterns4(void);
void test_runtime_binary_union_of_patterns5(void);

#endif
