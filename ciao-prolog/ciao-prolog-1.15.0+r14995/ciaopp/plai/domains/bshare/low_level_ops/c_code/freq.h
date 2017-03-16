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
 * Filename      : freq.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for Bit Frequencies
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Wed Apr  4 08:26:04 2007 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBFREQ_H
#define UNMNDBFREQ_H

#include <stdio.h>

struct perm;            /* forward declaration */

typedef struct freq {
  unsigned int * bit;
  int len;
} Freq;


Freq * freq_create(int numintsarg);

#define freq_get_length(f) ((f)->len)

void freq_init(Freq * farg, int lenarg); 
void freq_init_random(Freq * farg, int lenarg); 
void freq_init_copy (Freq * fmfarg, Freq * tofarg);

#define freq_increment(f,i) ((f)->bit[(i)]++) 
#define freq_decrement(f,i) ((f)->bit[(i)]--) 

int freq_get(Freq * farg, int idxarg);
int freq_most(Freq * farg);
int freq_max(Freq * farg);
int freq_min(Freq * farg);
int freq_accumpercent(Freq * farg, int totarg, int percentarg);

int freq_equal (Freq * f1arg, Freq * f2arg);

void freq_sort(Freq * farg, unsigned int ** sortarrayarg);
int freq_convert_to_index(Freq * farg, unsigned int * ptrarg);

void freq_print_stats(Freq * farg, FILE * fdarg, int dbsizearg); 
void freq_print(Freq * farg, FILE * fdarg); 

void freq_destroy(Freq * farg);

void test_freq (void); 
void test_freq_sort(int flenarg);

#endif
