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
 * Filename      : rstree.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for a Tree of NDB Records
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Sun Jul 10 08:55:09 2005 
 * Updated       : Tue Oct  2 22:18:00 2007 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBRSTREE_H
#define UNMNDBRSTREE_H
#include "rec.h"

#define COUNT_NODES 0

struct freq;          /* forward declaration */
struct perm;
struct runtime;

typedef struct node {
  struct node * zeroptr;
  struct node * oneptr;
  struct node * dcptr; 
  unsigned short bitloc;
  unsigned short flags;  /* 0 = node; 1 = rec; bits 0,1,2 for zero,one,dcptr */
} RecsetTreeNode;

RecsetTreeNode * rstree_createnode(unsigned short bitlocationarg);

void rstree_destroy(RecsetTreeNode * rootarg); 

int rstree_addrec(Rec * rarg, RecsetTreeNode * rootarg, Recset * rsetarg);
int rstree_removerec(Recset * rsarg, Rec * rarg);

int rstree_exists(RecsetTreeNode * rootarg, Rec * rarg, Recset * rsetarg);

int rstree_query(RecsetTreeNode * selfarg, Rec * rarg, int numintsarg);
int rstree_query_size(Recset * rsarg, Rec * rarg, int numunitsarg);
int rstree_query_subsumed_size(Recset * rsarg, Rec * rarg, int numintsarg);
int rstree_query_subsumed(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg);
int rstree_query_match(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg);
int rstree_query_match_size(Recset * rsarg, Rec * rarg, int numintsarg);
int rstree_size(RecsetTreeNode * nodearg);
int rstree_save(RecsetTreeNode * rootarg, int lenarg, char * filenamearg);
int rstree_print(RecsetTreeNode * nodearg, int lenarg, FILE * fdarg);

void rstree_node_stats(void);
#endif
