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
 * Filename      : rstree.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for a Tree of NDB Records
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Tue Mar 11 14:12:59 2008 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include "negdb.h"
#include "rstree.h"
#include "error.h"

#define ZEROBIT 0
#define ONEBIT 1
#define DCBIT 2

#define rstree_isrec(n,v) (((n)->flags >> (v)) & 1)
#define rstree_setrec(n,v) ((n)->flags=(n)->flags|(1 << (v)))
#define rstree_clearrec(n,v) ((n)->flags=(n)->flags & ~(1 << (v)))
#define rstree_getbitlocation(n) ((n)->bitloc)
#define rstree_convertdc(v) ((v)==DONTCARE ? DCBIT : (v))

static int RSTREE_NODES = 0;
static int RSTREE_NODES_MAX = 0;

static void rstree_init(RecsetTreeNode * rootarg, unsigned short bitlocarg) 
{
  rootarg->flags = 0;
  rootarg->bitloc = bitlocarg;
  rootarg->zeroptr = NULL;
  rootarg->oneptr = NULL;
  rootarg->dcptr = NULL;
}


RecsetTreeNode * rstree_createnode(unsigned short bitlocationarg)
{
  RecsetTreeNode * node = (RecsetTreeNode *) malloc(sizeof(RecsetTreeNode));
  if(node == NULL) {
    fprintf(stderr,"rstree_nodecreate: failed to allocate node\n");
    exit(err12);
  }
  rstree_init(node,bitlocationarg);
#if COUNT_NODES
  if(++RSTREE_NODES > RSTREE_NODES_MAX) {
    RSTREE_NODES_MAX = RSTREE_NODES;
  }
#endif
  return node;
}


static __inline__ void rstree_freenode(RecsetTreeNode * nodearg)
{
  free(nodearg);
#if COUNT_NODES
  RSTREE_NODES--;
#endif
}


static RecsetTreeNode * rstree_getnode(RecsetTreeNode * selfarg, int recbitarg)
{
  if(recbitarg == 0)
    return selfarg->zeroptr;
  else { 
    if(recbitarg == 1) 
      return selfarg->oneptr;
    else {
      if(recbitarg == 2) 
        return selfarg->dcptr;
      else
        exit(err17);
    }
  }
  return NULL;
}


static void rstree_setnode(RecsetTreeNode * selfarg, int recbitarg, RecsetTreeNode * narg)
{
  if(recbitarg == 0)
    selfarg->zeroptr = narg;
  else { 
    if(recbitarg == 1) 
      selfarg->oneptr = narg;
    else{
      if(recbitarg == 2) 
        selfarg->dcptr = narg;
      else
        exit(err17);
    }
  }
  /* flags caller dependent */
}


static void rstree_clear_node(RecsetTreeNode * selfarg, int recbitarg)
{
  if(recbitarg == 0)
    selfarg->zeroptr = NULL;
  else { 
    if(recbitarg == 1) 
      selfarg->oneptr = NULL;
    else{
      if(recbitarg == 2)
        selfarg->dcptr = NULL;
      else
        exit(err17);
    }
  }
  rstree_clearrec(selfarg,recbitarg);       /* clear flag */ 
  assert(!rstree_isrec(selfarg,recbitarg));
}


/* return number of non-null pointers */
static int rstree_node_size(RecsetTreeNode *nodearg) 
{
  int i;
  int count = 0;
  for(i =0; i <= DCBIT; i++){
    if(rstree_getnode(nodearg,i)!= NULL) 
      count++;
  }
  return count;
}


/* return index in node that is not null, o.w. -1 */
static int rstree_lastnode_index(RecsetTreeNode *nodearg) 
{
  int i;
  for(i = 0; i <= DCBIT; i++){
    if(rstree_getnode(nodearg,i) != NULL)
      return i;
  }
  return -1;
}


static Rec * rstree_findarec(RecsetTreeNode * selfarg) 
{
  int i;
  for(i = 0; i <= DCBIT; i++){
    if(rstree_isrec(selfarg,i))
      return (Rec *) rstree_getnode(selfarg,i);
  }
  i = rstree_lastnode_index(selfarg); 
  if(i < 0) 
    return NULL;
  return rstree_findarec(rstree_getnode(selfarg,i));
}


static RecsetTreeNode* rstree_removerec_node(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg) 
{
  RecsetTreeNode * node;
  int mybitloc;
  int recbitval;

  mybitloc = rstree_getbitlocation(selfarg);

  recbitval  = rec_getbit(rarg,mybitloc);
  recbitval = rstree_convertdc(recbitval);
  node = rstree_getnode(selfarg,recbitval);

  if(node != NULL){
    if(rstree_isrec(selfarg,recbitval)){   /* node is rec ptr, check if equal */
      if(rec_equal(rarg,(Rec *) node,numunitsarg)) {
        rstree_clear_node(selfarg,recbitval); /* selfarg might now be a single node */
        if(rstree_node_size(selfarg) == 1)  /* only single nodes compressed */ 
          return selfarg;
        else 
          return NULL;
      } else {                              /* always check for single v.45 ?? */  
        fprintf(stderr,"rstree_removerec_node: recs FAILED the equal test:\n");
        exit(err17);
      }
    } else {                                /* not a rec; compress single nodes */
      RecsetTreeNode * scrapnode = rstree_removerec_node(node,rarg,numunitsarg);
      if(scrapnode != NULL){
        int solobit = rstree_lastnode_index(scrapnode);
        rstree_setnode(selfarg,recbitval,rstree_getnode(scrapnode,solobit));
        if(rstree_isrec(scrapnode,solobit))
          rstree_setrec(selfarg,recbitval);
        else
          rstree_clearrec(selfarg,recbitval);
        
        rstree_freenode(scrapnode);
      }
    }
  }
  return NULL;
}


int rstree_removerec(Recset * rsarg, Rec * rarg) 
{
  if(rsarg->root == NULL) return 0;

#if SANITY
  if(rstree_exists(rsarg->root,rarg,rsarg)){
    fprintf(stderr,"rstree_removerec: before removal rec exists in tree:\n");
  } else {
    fprintf(stderr,"rstree_removerec: before removal, rec doesn't exist in tree:\n");
  }
  rec_print(rarg,recset_length(rsarg),stderr);
#endif


  rstree_removerec_node(rsarg->root,rarg,recset_numunits(rsarg));

#if SANITY
  if(rstree_exists(rsarg->root,rarg,rsarg)){
    fprintf(stderr,"rstree_removerec: rec still exists in tree:\n");
    rec_print(rarg,recset_length(rsarg),stderr);
    exit(err17);
  } else {
    fprintf(stderr,"rstree_removerec: rec doesn't exist in tree:\n");
    rec_print(rarg,recset_length(rsarg),stderr);
  }
#endif
  
  return 1;
}


/* private */
static RecsetTreeNode * rstree_addrec_node(Rec * rarg, RecsetTreeNode * selfarg, int reclenarg, int numunitsarg, int * dupflagptrarg) 
{
  int mybitloc = rstree_getbitlocation(selfarg);
  int recbitval = rec_getbit(rarg,mybitloc);
  RecsetTreeNode * node;

  assert(mybitloc < reclenarg && mybitloc >= 0);

  recbitval = rstree_convertdc(recbitval);
  node = rstree_getnode(selfarg,recbitval);

  if(node == NULL) {
    Rec* neighborrec = rstree_findarec(selfarg);
    int diffbit;

    if(neighborrec == NULL && mybitloc > 0){
      fprintf(stderr,"rstree_addrec_node: couldn't find a neighbor rec\n");
      exit(err17);
    }
    
    diffbit = rec_firstbitdiff(rarg,neighborrec,numunitsarg);

    if( mybitloc > diffbit){     /* setup new node (similar to below) */
      int newbitval, newbitval2;
      RecsetTreeNode * newnode = rstree_createnode(diffbit);
      newbitval = rec_getbit(rarg,diffbit);
      newbitval = rstree_convertdc(newbitval);
      rstree_setnode(newnode,newbitval,(RecsetTreeNode *) rarg); /* really a rec* */
      rstree_setrec(newnode,newbitval);
      
      newbitval2 = rec_getbit(neighborrec,diffbit);
      newbitval2 = rstree_convertdc(newbitval2);
      assert(newbitval != newbitval2);
      
      /* newnode must come before us, put selfarg in newnode instead */
      rstree_setnode(newnode,newbitval2,selfarg);
      return newnode;
    }
    
    /* mybitloc <= diffbit OR mybitloc is root */
    /* ok to put here */
    rstree_setnode(selfarg,recbitval,(RecsetTreeNode *) rarg); /* really a rec* */
    rstree_setrec(selfarg,recbitval);
    assert(rstree_isrec(selfarg,recbitval));
    return NULL;
  } /* end of NULL case */
  
  if(rstree_isrec(selfarg,recbitval)){  /* node is a rec ptr */
    RecsetTreeNode * newnode;
    int newbitval, newbitval2;
    int diffbit = rec_firstbitdiff((Rec *) node, rarg, numunitsarg);
    assert(diffbit < reclenarg);
    assert(diffbit != mybitloc);

    /*    fprintf(stderr,"first bit diff is %d from level %d\n",diffbit,mybitloc);
    printrec(rarg,reclenarg,stderr);
    printrec((Rec*)node,reclenarg,stderr);  */

    if(diffbit < 0 ){  /* a duplicate record */
      /*      fprintf(stderr,"rstree_addrec_node: appears to be a duplicate\n"); 
      printrec((Rec *) node,reclenarg,stderr);
      printrec(rarg,reclenarg,stderr); */
      *dupflagptrarg = 1;
      return NULL;
    }

    /* setup new node */
    newnode = rstree_createnode(diffbit);
    newbitval = rec_getbit(rarg,diffbit);
    newbitval = rstree_convertdc(newbitval);
    rstree_setnode(newnode,newbitval,(RecsetTreeNode *) rarg); /* really a rec* */
    rstree_setrec(newnode,newbitval);

    newbitval2 = rec_getbit((Rec *) node,diffbit);
    newbitval2 = rstree_convertdc(newbitval2);
    assert(newbitval != newbitval2);

    if(mybitloc < diffbit){
      rstree_setnode(newnode,newbitval2,node);   /* really a rec* */
      rstree_setrec(newnode,newbitval2);
      rstree_setnode(selfarg,recbitval,newnode);
      rstree_clearrec(selfarg,recbitval);
      return NULL;         /* done */
    } 

    /* newnode must come before us, put selfarg in newnode instead */
    rstree_setnode(newnode,newbitval2,selfarg);
    return newnode;
    
  } else {                         /* not a rec yet */

    RecsetTreeNode * newnode = rstree_addrec_node(rarg, node, reclenarg,numunitsarg,dupflagptrarg);
    int newbitloc;

    if( newnode == NULL) return NULL;                    /* nothing left to do */

    newbitloc = rstree_getbitlocation(newnode);
    if(newbitloc == mybitloc){
      fprintf(stderr,"rstree_addrec_node: newbitloc is %d, mybitloc is %d\n",
              newbitloc,mybitloc);
      exit(err17);
    }

    if(mybitloc < newbitloc){     /* newnode goes here */
      rstree_setnode(selfarg,recbitval,newnode);
      rstree_clearrec(selfarg,recbitval);  /* redundant ?? */
      return NULL;                /* done */

    } else {                      /* replace self as the treenode here */
      int j;
      for(j = 0; j <= DCBIT; j++) {
        if(rstree_getnode(newnode,j) != NULL && !rstree_isrec(newnode,j)){
          rstree_setnode(newnode,j,selfarg);
          break;
        }
      }
      return newnode;            /* continue up the line */
    }
  }
  return NULL;
}


int rstree_addrec(Rec * rarg, RecsetTreeNode * rootarg, Recset * rsetarg)
{
  int dupflag = 0;
  rstree_addrec_node(rarg,rootarg,recset_length(rsetarg),recset_numunits(rsetarg),&dupflag);

#if SANITY
  if (dupflag==0)
    fprintf(stderr,"rstree_addrec: added rec: ");
  else 
    fprintf(stderr,"rstree_addrec: duplicate rec: ");
  rec_print(rarg,recset_length(rsetarg),stderr); 
#endif

  return dupflag;
}


/* return true if arg rec already exists exactly in recset, o.w. false */
int rstree_exists(RecsetTreeNode * selfarg, Rec * rarg, Recset * rsetarg) 
{
  int mybitloc = rstree_getbitlocation(selfarg);
  int recbitval = rec_getbit(rarg,mybitloc);
  RecsetTreeNode * node;

  assert(mybitloc < recset_length(rsetarg));

  recbitval = rstree_convertdc(recbitval);
  node = rstree_getnode(selfarg,recbitval);
  
  if(node != NULL){
    if(rstree_isrec(selfarg,recbitval)){   /* node is rec ptr, check if equal */
      return rec_equal(rarg,(Rec *) node,recset_numunits(rsetarg));
    } else {                          /* check next node/bit */
      if(rstree_exists(node,rarg,rsetarg))
        return 1; 
    }
  }
  return 0;
}


/* return true if rec found in recset, that is, rarg is subsumed by a
   rec in recset  */  
int rstree_query(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg) 
{
  int mybitloc;
  int recbitval;
  RecsetTreeNode * node;

  if(selfarg == NULL)
    return 0;
  
  mybitloc = rstree_getbitlocation(selfarg);
  recbitval = rec_getbit(rarg,mybitloc);
  
  recbitval = rstree_convertdc(recbitval);
  node = rstree_getnode(selfarg,recbitval);
   
  if(node != NULL){
    if(rstree_isrec(selfarg,recbitval)){   /* node is rec ptr, check if equal */
      if(rec_subsumed_query(rarg,(Rec *) node,numunitsarg))
        return 1;
    } else {                          /* check next node/bit */
      if(rstree_query(node,rarg,numunitsarg))
        return 1; 
    }
  }
  
  if(recbitval == DCBIT)
    return 0;
  if(rstree_isrec(selfarg,DCBIT))   /* node is rec ptr, check if equal */
    return rec_subsumed_query(rarg,(Rec *) selfarg->dcptr,numunitsarg);
  return rstree_query(selfarg->dcptr,rarg,numunitsarg);
}


/* counts recs in recset, such that, rarg is subsumed by it v.17 */
static int rstree_query_counter(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg) 
{
  int mybitloc;
  int recbitval;
  RecsetTreeNode * node;
  int count = 0;

  if(selfarg == NULL)
    return count;
  
  mybitloc = rstree_getbitlocation(selfarg);
  recbitval = rec_getbit(rarg,mybitloc);
  
  recbitval = rstree_convertdc(recbitval);
  node = rstree_getnode(selfarg,recbitval);
   
  if(node != NULL){
    if(rstree_isrec(selfarg,recbitval)){   /* node is rec ptr, check if equal */
      if(rec_subsumed_query(rarg,(Rec *) node,numunitsarg))
        count += 1;
    } else {                          /* check next node/bit */
      count += rstree_query_counter(node,rarg,numunitsarg);
    }
  }
  
  if(recbitval != DCBIT){             /* also check the dontcares */
    if(rstree_isrec(selfarg,DCBIT))   /* node is rec ptr, check if a match */
      count += rec_subsumed_query(rarg,(Rec *) selfarg->dcptr,numunitsarg);
    else 
      count += rstree_query_counter(selfarg->dcptr,rarg,numunitsarg);
  }
  return count;
}


int rstree_query_size(Recset * rsarg, Rec * rarg, int numunitsarg) 
{
  return rstree_query_counter(rsarg->root,rarg,numunitsarg);
}


/* counts recs in recset that are subsumed by rarg, that is for every
   specified bit in rarg, the ndb pattern has the bit specified the
   same, and for the dontcare case in the rarg, it doesn't matter what
   the ndb pattern is, 0, 1, or dc, so check them all */
static int rstree_query_subsumed_counter(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg) 
{
  int mybitloc;
  int recbitval;
  RecsetTreeNode * node;
  int count = 0;

  if(selfarg == NULL)
    return count;

  mybitloc = rstree_getbitlocation(selfarg);
  recbitval = rec_getbit(rarg,mybitloc);

  recbitval = rstree_convertdc(recbitval);
  node = rstree_getnode(selfarg,recbitval);

  if(node != NULL){
    if(rstree_isrec(selfarg,recbitval)){  /* node is rec ptr, check if subsumes */
      if(rec_subsumed_query((Rec *) node,rarg,numunitsarg))
        count += 1;
    } else {                          /* check next node/bit */
      count += rstree_query_subsumed_counter(node,rarg,numunitsarg);
    }
  }

  if (recbitval==DCBIT) {
    int i;
    for(i = 0; i <= 1; i++){
      node = rstree_getnode(selfarg,i);
      if(node != NULL){
        if(rstree_isrec(selfarg,i)){   /* node is rec ptr, check if equal */
          if(rec_subsumed_query((Rec *) node,rarg,numunitsarg))
            count += 1;
        } else {                          /* check next node/bit */
          count += rstree_query_subsumed_counter(node,rarg,numunitsarg);
        }
      }
    }
  }
  return count;
}


/* return number of records in recset subsumed by rarg, that is for
   every specified bit in rarg, the ndb pattern has the bit specified
   the same, and for the dontcare case in the rarg, it doesn't matter
   what the ndb pattern is, 0, 1, or dc, so check them all */
int rstree_query_subsumed_size(Recset * rsarg, Rec * rarg, int numunitsarg) 
{
  return rstree_query_subsumed_counter(rsarg->root,rarg,numunitsarg);
}


/* return true if rec found in recset that is subsumed by rarg; fixed v.52  */  
int rstree_query_subsumed(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg) 
{
  int mybitloc;
  int recbitval;
  RecsetTreeNode * node;

  if(selfarg == NULL)
    return 0;
  
  mybitloc = rstree_getbitlocation(selfarg);
  recbitval = rec_getbit(rarg,mybitloc);
  
  recbitval = rstree_convertdc(recbitval);
  node = rstree_getnode(selfarg,recbitval);
   
  if(node != NULL){
    if(rstree_isrec(selfarg,recbitval)){   /* node is rec ptr, check if eq */
      if(rec_subsumed_query((Rec *) node,rarg,numunitsarg))
        return 1;                     /* node is subsumed by rarg */
    } else {                          /* check next node/bit */
      if(rstree_query_subsumed(node,rarg,numunitsarg))
        return 1; 
    }
  }
  
  if(recbitval == DCBIT)
    return 0;
  if(rstree_isrec(selfarg,DCBIT))   /* node is rec ptr, check if equal */
    return rec_subsumed_query((Rec *) selfarg->dcptr,rarg,numunitsarg);
  return rstree_query_subsumed(selfarg->dcptr,rarg,numunitsarg);
}


/* return true if a rec pattern in recset matches rarg, that is for
   every specified bit in rarg, the ndb pattern has the bit specified
   the same, OR DONTCARE; and for the dontcare case in the rarg, it
   doesn't matter what the ndb pattern is, 0, 1, or dc, so check them
   all v.40 */
int rstree_query_match(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg) 
{
  int mybitloc;
  int recbitval;
  RecsetTreeNode * node;

  if(selfarg == NULL)
    return 0;

  mybitloc = rstree_getbitlocation(selfarg);
  recbitval = rec_getbit(rarg,mybitloc);
  recbitval = rstree_convertdc(recbitval);

  node = rstree_getnode(selfarg,recbitval);

  if(node != NULL){
    if(rstree_isrec(selfarg,recbitval)){  /* node is rec ptr, check if match */
      if(rec_match_query((Rec *) node,rarg,numunitsarg))
        return 1;
    } else {                          /* check next node/bit */
      if(rstree_query_match(node,rarg,numunitsarg))
        return 1;
    }
  }

  if (recbitval==DCBIT) {       /* v.70 */
    int i;
    for(i = 0; i <= 1; i++){
      node = rstree_getnode(selfarg,i);
      if(node != NULL){
        if(rstree_isrec(selfarg,i)){   /* node is rec ptr, check if equal */
          if(rec_match_query((Rec *) node,rarg,numunitsarg))
            return 1;
        } else {                          /* check next node/bit */
          if (rstree_query_match(node,rarg,numunitsarg))
            return 1;
        }
      }
    }
  } else {    /* for match, the record can be dontcare v.38 */
    node = rstree_getnode(selfarg,DCBIT);
    if(node != NULL){
      if(rstree_isrec(selfarg,DCBIT)){   /* node is rec ptr, check if equal */
        if(rec_match_query((Rec *) node,rarg,numunitsarg))
          return 1;
      } else {                          /* check next node/bit */
        if(rstree_query_match(node,rarg,numunitsarg))
          return 1;
      }
    }
  }
  return 0;  /* rstree_query_match(node,rarg,numunitsarg) */
}



/* returns count of recs in recset that matches rarg, that is for
   every specified bit in rarg, the ndb pattern has the bit specified
   the same, OR DONTCARE; and for the dontcare case in the rarg, it
   doesn't matter what the ndb pattern is, 0, 1, or dc, so check them
   all v.38 */
static int rstree_query_match_counter(RecsetTreeNode * selfarg, Rec * rarg, int numunitsarg) 
{
  int mybitloc;
  int recbitval;
  RecsetTreeNode * node;
  int count = 0;

  if(selfarg == NULL)
    return count;

  mybitloc = rstree_getbitlocation(selfarg);
  recbitval = rec_getbit(rarg,mybitloc);
  recbitval = rstree_convertdc(recbitval);

  node = rstree_getnode(selfarg,recbitval);

  if(node != NULL){
    if(rstree_isrec(selfarg,recbitval)){  /* node is rec ptr, check if match */
      if(rec_match_query((Rec *) node,rarg,numunitsarg))
        count++;
    } else {                          /* check next node/bit */
      count += rstree_query_match_counter(node,rarg,numunitsarg);
    }
  } 

  if (recbitval==DCBIT) {
    int i;
    for(i = 0; i <= 1; i++){
      node = rstree_getnode(selfarg,i);
      if(node != NULL){
        if(rstree_isrec(selfarg,i)){   /* node is rec ptr, check if equal */
          if(rec_match_query((Rec *) node,rarg,numunitsarg))
            count++;
        } else {                          /* check next node/bit */
          count += rstree_query_match_counter(node,rarg,numunitsarg);
        }
      }
    }
  } else {    /* for match, the record can be dontcare v.38 */
    node = rstree_getnode(selfarg,DCBIT);
    if(node != NULL){
      if(rstree_isrec(selfarg,DCBIT)){   /* node is rec ptr, check if equal */
        if(rec_match_query((Rec *) node,rarg,numunitsarg))
          count++;
      } else {                          /* check next node/bit */
        count += rstree_query_match_counter(node,rarg,numunitsarg);
      }
    }
  }
  return count;
}


/* return number of recs in recset arg, that match rarg, that is for
   every specified bit in rarg, the ndb pattern has the bit specified
   the same or dontcare, and for the dontcare case in the rarg, it
   doesn't matter what the ndb pattern is, 0, 1, or dc, so check them
   all v.38 */
int rstree_query_match_size(Recset * rsarg, Rec * rarg, int numunitsarg) 
{
  return rstree_query_match_counter(rsarg->root,rarg,numunitsarg);
}


int rstree_size(RecsetTreeNode * nodearg)
{
  int count = 0;
  int i;
  RecsetTreeNode * node;
  for(i = 0; i <= DCBIT ; i++) {
    node = rstree_getnode(nodearg,i);
    if(node != NULL) {
      if(rstree_isrec(nodearg,i))
        count++;
      else 
        count += rstree_size(node);
    }
  }
  return count;
}


int rstree_print(RecsetTreeNode * nodearg, int lenarg, FILE * fdarg)
{
  int i;
  RecsetTreeNode * node;

  for(i = 0; i <= DCBIT ; i++) {
    node = rstree_getnode(nodearg,i);
    if(node != NULL) {
      if(rstree_isrec(nodearg,i))
        rec_print((Rec *) node,lenarg,fdarg);
      else 
        rstree_print(node,lenarg,fdarg);
    }
  }
  return 1;
}


int rstree_save(RecsetTreeNode * rootarg, int lenarg, char * filenamearg)
{
  FILE * fd = fopen(filenamearg,"w");
  
  if( fd == NULL ) {
    fprintf(stderr,"file open error on rsree save\n"); 
    exit(err14);
  }
  
  rstree_print(rootarg,lenarg,fd);
  
  fclose(fd);
  /*  Print(rtarg,"Saved tree-records in %s\n",filenamearg); */
  return 1;
}


void rstree_destroy(RecsetTreeNode * nodearg)
{
  int i;

  if(nodearg == NULL)
    return;

  for(i = 0; i <= DCBIT ; i++) {
    RecsetTreeNode * node = rstree_getnode(nodearg,i);
    if(node != NULL && ! rstree_isrec(nodearg,i))
      rstree_destroy(node);
  }
  /* rstree_init(nodearg,0);  clear memory */
  rstree_freenode(nodearg);
}


void rstree_node_stats()
{
  fprintf(stderr,"rstree_node_stats: %d nodes, %d max ever\n",RSTREE_NODES,RSTREE_NODES_MAX);
}


