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
 * Filename      : recset.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for a Set of NDB Records
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Mon Apr 21 11:21:18 2008 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBRECSET_H
#define UNMNDBRECSET_H
#include "rec.h"
#include "nsql.h"
#include "easy.h"

#define NODUPCHK 0
#define DUPCHK 1

#define SANITY 0

struct freq;          /* forward declaration */
struct perm;
struct node;
struct runtime;

typedef enum fileformat {noformat, ndbformat, cnfformat} NegFileFormat;

typedef struct recset {
  Rec ** collection;   /* pointer to array of pointers to recs */
  int numrecs;         /* number of recs in array */
  int capacity;        /* greater than numrecs */
  int updateflag;      /* zero if unchanged; 1 if changed */
  int recbitlen;       /* number of bits in a record v.27 */ 
  struct freq * bitfreq;   /* number of times each bit position is used */
  struct freq * recszfreq; /* max number of specified bits in recs v.45 */ 
  struct node * root;  /* tree structure */
} Recset;

int recset_getupdateflag(Recset * rsetarg);
void recset_setupdateflag(Recset * rsetarg);
void recset_clearupdateflag(Recset * rsetarg);

Recset * recset_create(void);
void recset_init(Recset * rsetarg, int lenarg); 
void recset_final(Recset * rsetarg); 
void recset_destroy(Recset * rsetarg);  /* frees the recset and its collection of recs */

int recset_addrec(Rec * rarg, Recset * rsetarg);
void recset_removerec(Recset * rsetarg, int idxarg);
int recset_deleterec(Rec * rarg, Recset * rsetarg);
void recset_delete_subsumed(Recset * rset1arg, Recset * rset2arg); /* v.36 */
int recset_size(Recset * rsetarg);
int recset_length(Recset * rsetarg);     /* v.27 number of bits per record */
void recset_setlength(Recset * rsetarg, int lenarg);               /* v.27 */
int recset_numunits(Recset * rsetarg);      /* v.27 calculated from length */
int recset_maxbits(Recset * rsetarg);     /* v.27 calculated from numunits */

struct freq * recset_getrecsizefrequency(Recset * rsetarg);
void recset_setrecsizefrequency(Recset *rsetarg, struct freq * farg);
void recset_update_recsize_frequency(Recset * rsarg);
int recset_max_recordsize(Recset * rsetarg);                       /* v.45 */
int recset_min_recordsize(Recset * rsetarg);                       /* v.51 */
int recset_most_recordsize(Recset * rsetarg);                      /* v.57 */
int recset_popular_bitposition_value(Recset * rsetarg, int bparg); /* v.50 */
int recset_notemptyp(Recset * rsetarg, struct runtime * rtarg); /* v.45  EASYSAT */
int recset_notemptyp_solution(Recset * rsetarg, Rec * solrecarg, EasyReduceMode reducemodearg, int ksatarg);
int recset_complement(Recset * rsetarg, Recset * newrsetarg, struct runtime * rtarg); /* v.57.5,v.59.7,v.59.13 */
int recset_compare_destructively(Recset * rset1arg, Recset * rset2arg, struct runtime * rtarg); /* v.56 */

__inline__ Rec * recset_getrec(Recset * rsetarg, int idxarg);
int recset_getrec_index(Recset * rsarg, Rec * rarg);
int recset_getrec_lastofsize_index(Recset * rsarg, int recszarg);
struct freq * recset_getbitfrequency(Recset * rsetarg);
void recset_setbitfrequency(Recset *rsetarg, struct freq * farg);
void recset_update_bit_frequency(Recset * rsarg);
int recset_save_and_checkbitfreq(Recset * rsarg, struct runtime * rtarg, char * filenamearg);

/* persistent functions that read/write from/to filenamearg */
int recset_build(Recset * rsarg, struct runtime * rtarg, char * filenamearg, NegFileFormat ffarg);
void recset_expandrecsize(Recset * rsarg, struct runtime * rtarg);
int recset_ischanged(Recset * rsetarg);
void recset_save(Recset * rsarg, struct runtime * rtarg, char * filenamearg);
void recset_tree_save(Recset * rsarg, char * filenamearg);
void recset_save_secondary(Recset * rsarg, struct runtime * rtarg, int cmprsarg);
void recset_save_secondary_projected(Rec * rarg, Recset * rsarg, struct runtime * rtarg);
void recset_print(Recset * rsetarg, FILE * fdarg);

int recset_query(Recset * rsetarg, Rec * rarg);
int recset_query_size(Recset * rsetarg, Rec * rarg);
int recset_query_all(Recset * rset1arg, Recset * rset2arg);
int recset_query_subsumed(Recset * rsetarg, Rec * rarg);
int recset_query_subsumed_size(Recset * rsetarg, Rec * rarg);
int recset_query_match(Recset * rsetarg, Rec * rarg);
int recset_query_match_size(Recset * rsetarg, Rec * rarg);
int recset_query_match_all(Recset * rset1arg, Recset * rset2arg);
int recset_query_match_none(Recset * rset1arg, Recset * rset2arg);
int recset_perm_query(Rec * rarg, Recset * rsetarg, struct perm * parg, int numunitsarg);

void recset_split(Rec * rarg, Recset * rsetarg, Recset * newrsetarg);
void recset_split_subsumed(Rec * rarg, Recset * rsetarg, Recset * newrsetarg);
void recset_split_match(Rec * rarg, Recset * rsetarg, Recset * newrsetarg);
int recset_merge(Recset * fromrsetarg, Recset * torsetarg);
int recset_equivalence(Recset * rset1arg, Recset * rset2arg, struct runtime * rtarg);

void recset_select(Rec * rarg, Recset * ndbarg, struct runtime * rtarg);
int recset_join(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);
int recset_crossproduct(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);
int recset_intersection(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);
int recset_union(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);
int recset_binaryunion(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);
int recset_negbinaryunionstar(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);
int recset_difference(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);

/* exclusively for fully-specified positive db's */
int recset_setdiff(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);
int recset_relevance(Recset * ndb1arg, Recset * ndb2arg, Recset * ndb3arg, struct runtime * rtarg);


int recset_copy(Recset * fmrsetarg, Recset * torsetarg);
int recset_partial_copy(Rec * rarg, Recset * rsetarg, Recset * newrsetarg, struct runtime * rtarg); /* v.66.1 */
void recset_tree_clear(Recset * rsetarg);
int recset_tree_size(Recset * rsetarg);

void recset_sanitycheck_tree(Recset * rsetarg);

void test_recset(void);
void test_recset_split(void);
void test_recset_split_match(void);

#endif
