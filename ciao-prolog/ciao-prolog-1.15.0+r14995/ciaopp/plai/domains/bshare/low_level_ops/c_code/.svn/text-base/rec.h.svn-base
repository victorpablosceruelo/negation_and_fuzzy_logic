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
 * Filename      : rec.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for NDB Records
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Thu Apr 10 15:16:21 2008 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBREC_H
#define UNMNDBREC_H

#include <stdio.h>
#include <assert.h>

#define INTSIZE (sizeof(int) << 3)
#define UNITSIZE (sizeof(long) << 3)

#ifdef ISCHARDQR
#define MINRECLEN 1000       /* allow at least up to 1000 bit records */
#define NUMUNITS ((MINRECLEN+UNITSIZE-1)/UNITSIZE)  
#else
#define NUMUNITS 10          /* default */
#endif

#define MAXBITS (NUMUNITS * UNITSIZE)
#define HALFSIZE (sizeof(long) << 2)
#define ONE ((Rec) 1)
#define HALFMASK ((ONE << HALFSIZE) - 1)
#define CHAR_T 8

/* Rec is an array of unsigned longs alternating between mask and val
(hopefully to take advantage of computer's cache buffer):
mask0,val0,mask1,val1...maskn,valn,hashsize and ending with the hash
and size; n is numunits needed for this record length */

typedef unsigned long Rec;

enum {DONTCARE=-1};

int nextRand(int narg);

/* return true if bitarg is-a 'don't care' symbol */
static __inline__ int rec_isdc ( Rec * rarg, int bitarg) 
{
  int mi = (bitarg / UNITSIZE) << 1;
  int bi = bitarg % UNITSIZE;
  Rec mm = ONE << bi;
  return (!(rarg[mi] & mm ));   /* if same s.t. mask is set, then return zero */
}


#define lv(r,mi,bi) ((((r)[(mi)] >> (bi) & 1) ? ((r)[(mi+1)] >> (bi) & 1) : DONTCARE))

/* if same s.t. mask is set, return value 0,1 */
static __inline__ int loadvalue ( Rec * rarg, int miarg, int biarg) 
{
  return lv(rarg,(miarg << 1),biarg);
}


/* return value of bit at bitarg, or 'dontcare' enum */
static __inline__ int rec_getbit(Rec * rarg, int bitarg) 
{
  return loadvalue(rarg,bitarg / UNITSIZE,bitarg % UNITSIZE);
}

#define rec_size(r,n) ((int)(((r)[(n)<<1]) & HALFMASK))

int rec_calcnumunits(int lenarg);


__inline__ int rec_subsumed_query(Rec * recXarg, Rec * recYarg, int numintsarg);
__inline__ int rec_equal(Rec * rarg1, Rec * rarg2, int numintsarg);

Rec * rec_create(int numintsarg);
void rec_init (Rec * rarg, int numintsarg);
void rec_random(Rec * rarg, int lenarg, int numunitsarg);
void rec_coalesce(Rec * rxarg, Rec * ryarg, Rec * rzarg, int numunitsarg);
void rec_flip(Rec * recXarg, Rec * recYarg, Rec * recRarg, int numunitsarg);

void rec_setbit ( Rec * rarg, int bitarg, int valarg, int numintsarg);
void rec_flipbit ( Rec * rarg, int bitarg);

void rec_print(Rec * rarg, int lenarg, FILE * fdarg);

int rec_read_cnf(Rec * rarg, int numunitsarg, FILE * fdarg);
int rec_read(Rec * rarg, int numintsarg, FILE * fdarg);

void itobs(unsigned int inputarg, char * strarg, int lenarg);
void ctobs(unsigned char carg, char * strarg);
int rec_string2rec (Rec * rarg, int lenarg, char * sarg, int binarg);
int rec_int2rec (Rec * rarg, int lenarg, unsigned int iarg, int numunitsarg);
void rec_and(Rec * recXarg, Rec * recYarg, Rec * recRarg, int numunitsarg);
void rec_or(Rec * recXarg, Rec * recYarg, Rec * recRarg, int numunitsarg);
int rec_hammingdistance(Rec * recXarg, Rec * recYarg, int numunitsarg);

void rec_copy(Rec * fromarg, Rec * toarg, int numintsarg);
void rec_copy_bitbybit(Rec * fromarg, Rec * toarg, int lenarg, int startarg, int numunitsarg);
void rec_copy_complement(Rec * fromarg, Rec * toarg, int lenarg, int numunitsarg);
int rec_checkhash(Rec * rarg, int numunitsarg);
Rec * rec_shrink(Rec * fromarg, int numintsarg);

int rec_dontcare_match(Rec * recXarg, Rec * recYarg, int numintsarg);
int rec_firstbitdiff(Rec * r1arg, Rec * r2arg, int numintsarg);

void rec_destroy(Rec * rarg); 
 
void test_rec(void);
void test_rec_int(void);
void test_rec_or(void);
void test_rec_hammingdistance(void);

void recsubsumed_stats(int numintsarg);
__inline__ int rec_subsumed_query_stingy(Rec * recXarg, Rec * recYarg, int numunitsarg);
__inline__ int rec_match_query(Rec * recXarg, Rec * recYarg, int numunitsarg) ;

int rec_relevant(Rec * qrarg, Rec * rarg, int numunitsarg);
#endif
