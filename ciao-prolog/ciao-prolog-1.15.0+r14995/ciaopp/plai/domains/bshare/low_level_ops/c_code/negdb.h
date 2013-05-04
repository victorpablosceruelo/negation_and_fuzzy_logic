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
 * Filename      : negdb.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for NDB Update Algorithms
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Thu Oct  4 23:02:50 2007 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBNEGDB_H
#define UNMNDBNEGDB_H

#include "runtime.h"
#include "perm.h"
#include "freq.h"
#include "cleanresults.h"

typedef enum negpattgenflags {wo_npg, with_npg} Negpattgen;

int insert(Rec * rarg, Recset * rsetarg, Runtime * rtarg, Negpattgen npgflag, int narg);
int append(Rec * rarg, Recset * rsetarg, Runtime * rtarg, Negpattgen npgflag);

int delete(Rec * yarg, Rec * xarg, Recset * rsetarg, Runtime * rtarg, Negpattgen npgarg);
int offline_add_record(Rec * rarg, Recset * rsetarg, Runtime * rtarg, Negpattgen npgarg);

int cleanup(Recset * ndbarg, Runtime * rtarg, FILE * fdarg, int iterarg, struct cleanresults * cleanresultsarg);
int new_cleanup(Recset * ndbarg, Runtime * rtarg, FILE * fdarg, int iterarg, struct cleanresults * cleanresultsarg);
void cleanup_plus_option(Recset * rsetarg, Runtime * rtarg);
void cleanup_plus_option_minimum(Recset * rsetarg, Runtime * rtarg, int minarg);
int compress(Recset * rsetarg, Runtime * rtarg, int minarg);
int managed_growth(Rec * rarg, Recset * rsetarg); /* v.64 */
#endif
