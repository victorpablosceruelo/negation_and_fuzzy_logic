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
 * Filename      : nsql.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Provides Negative SQL functions
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Sun May 14 15:16:41 2006 
 * Updated       : Fri Apr 11 09:55:37 2008 
 *
 * Comments      : based on the Fernando Esponda and Eric Trias 
 *                 Negative Database Algebra
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNEGSQLFUNCS_H
#define UNMNEGSQLFUNCS_H

#include "rec.h"

struct recset;
struct perm;
struct runtime;

#define RELOP_EFFICIENCY_STEP 1         /* useful for partial queries
                                           when minbits not used v.60.1 */
#define RELOP_MIDCLEANUP_OPTION 1       /* effects Diff and BU v.57;
                                           uneffective with MANAGED_GROWTH v.62 */
#define PATTERN_BIT_VALUE ONE

typedef enum negthetas {O_bool, O_eq, O_ne, O_lt, O_le, O_gt, O_ge} NegTheta;

int  nsql_thetaselect( Rec * rarg, struct recset * rsetarg, struct recset * newrsetarg, struct runtime * rtarg);
int nsql_join(struct recset * ndb1arg, struct recset * ndb2arg, struct recset * ndb3arg, struct perm * joincond1, struct perm * joincond2, struct runtime * rtarg);
int nsql_intersection(struct recset * ndb1arg, struct recset * ndb2arg, struct recset * ndb3arg, struct runtime * rtarg);
int nsql_crossproduct(struct recset * ndb1arg, struct recset * ndb2arg, struct recset * ndb3arg, struct runtime * rtarg);
int nsql_union(struct recset * ndb1arg, struct recset * ndb2arg, struct recset * ndb3arg, struct runtime * rtarg);
/* int nsql_binaryunion(struct recset * ndb3arg, struct runtime * rtarg); */
int nsql_binaryunion(struct recset * cndb1arg, struct recset * cndb2arg, struct recset * cndb3arg, struct runtime * rtarg);
int nsql_negbinaryunionstar(struct recset * ndb1arg, struct recset * ndb2arg, struct recset * ndb3arg, struct runtime * rtarg);
int nsql_difference(struct recset * ndb1arg, struct recset * ndb2arg, struct recset * ndb3arg, struct runtime * rtarg);

/* these two functions are strictly for fully-specified positive databases only */
int nsql_setdiff(struct recset * db1arg, struct recset * db2arg, struct recset * db3arg, struct runtime * rtarg);
int nsql_relevance(struct recset * db1arg, struct recset * db2arg, struct recset * db3arg, struct runtime * rtarg);

#endif
