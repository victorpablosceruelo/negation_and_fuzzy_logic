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
 * Filename      : perm.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for Permutations
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Wed Oct 10 16:54:06 2007 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBPERM_H
#define UNMNDBPERM_H

#include <stdio.h>
#include "recset.h"

struct freq;             /* forward declaration */

/* no longer stackable  v. 61 */
typedef struct perm {
  unsigned int * pi;
  unsigned int * pir;
  int len;
} Perm;

int nextRand(int narg);
int coinflip(void);
double randd();

void seed_random(int seedarg);
int perm_seed_random(int narg);

Perm * perm_create(int numunitsarg);   /* v.27 */
void perm_init(Perm * parg, int lenarg); 

/* return index at position i */
#define perm_get(p,i) ((p)->pi[(i)])
#define perm_get_length(p) ((p)->len)

int perm_get_reverse(Perm *parg, int idxarg);
int perm_index_presentp(Perm * parg, int idxarg, int lenarg);

void perm_set_length(Perm *parg, int lenarg);
void perm_swap(Perm * parg, int xarg, int yarg);

int perm_dontcares(Perm * parg, Rec * rarg, int reclenarg);
int perm_notdontcares(Perm * parg, Rec * rarg, int reclenarg);
void perm_randomize(Perm * parg, int newlenarg) ;
void perm_weighted_roulette(Perm * parg, struct freq * farg);
void perm_ordered(Perm * parg, struct freq * farg);
int perm_list2perm (Perm * parg, char * listarg, int binmodearg); /* v.27 */

int perm_recset_subset(Perm * parg, Rec * rarg, Recset * rsetarg);  /* UNTESTED */

void perm_print(Perm * parg, FILE * fdarg); 
void perm_print_rec(Perm * parg, Rec * rarg, NegFileFormat nffarg, FILE * fdarg);

void perm_copy_rec(Perm * parg, Rec * fmrarg, Rec * torarg, int numunitsarg); /* v.30 */

void perm_final(Perm * parg);
void perm_destroy(Perm * parg);

void test_perm (void); 

#endif
