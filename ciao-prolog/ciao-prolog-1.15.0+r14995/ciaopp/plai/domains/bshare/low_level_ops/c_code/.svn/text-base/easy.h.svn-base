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
 * Filename      : easy.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for Easy-to-Reverse NDB functions
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Wed Mar 28 03:36:21 2007 
 * Updated       : Wed Sep 12 07:50:25 2007 
 *
 * Comments : based on the Davis-Putnam SAT Solver and the Fernando
 * Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBEASY_H
#define UNMNDBEASY_H

#include "rec.h"

struct recset;
struct runtime;

#define SAT2 2
#define SAT3 3
#define EASYSAT 3

typedef enum rmode { ALL_RANDOM, ALL_POPULAR, ALTERNATE_POPULAR_RANDOM, SUCCESS_POPULAR_BTRACK_RANDOM, LASTREDUCEMODE } EasyReduceMode;

int easy_reduce_sat(struct recset * rsetarg, Rec * solrecarg, EasyReduceMode reducemodearg, int satarg);

void test_easy_reduce_simple_fullbit(int karg);
#endif
