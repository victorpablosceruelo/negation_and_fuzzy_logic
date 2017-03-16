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
 * Filename      : newndb.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for New NDB Algorithms
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Fri Mar 24 09:07:30 2006 
 * Updated       : Fri Dec 14 03:20:22 2007 
 *
 * Comments : based on the Fernando Esponda Empty NDB Algorithms; a
 * well-known random SAT formula for epsilon non-empty hard instances;
 * Haixia Jia's q-hidden algorithm for new singleton negative
 * databases; basic empty and full single negative record ndb.
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNEWNDB_H
#define UNMNEWNDB_H

#include "runtime.h"

int empty_ndb_create(Recset * rsarg, Runtime * rtarg);
int empty_ndb_create_onebit(Recset * rsarg, Runtime * rtarg);
int empty_ndb_create_randomSATformula(Recset * rsarg, Runtime * rtarg);
int singleton_ndb_create(Recset * multiinputsetarg, Recset * rsarg, Runtime * rtarg);

int empty_ndb_create_basic(Recset * rsarg, int lenarg);
int full_ndb_create_basic(Recset * rsarg, int lenarg); /* v.66 */

#endif
