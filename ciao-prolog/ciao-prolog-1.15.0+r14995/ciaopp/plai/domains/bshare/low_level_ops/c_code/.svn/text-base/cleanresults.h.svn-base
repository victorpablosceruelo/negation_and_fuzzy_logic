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
 * Filename      : cleanresults.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for Clean Results
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Mon Jul  4 10:07:47 2005 
 * Updated       : Mon Jul  4 10:07:50 2005 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBCLEAN_H
#define UNMNDBCLEAN_H

#include "runtime.h"

typedef struct cleanresults {
  int added;
  int deleted;
  int passed;
} CleanResults;

void cleanresults_set (CleanResults * crarg, int aarg, int darg, int parg);
void cleanresults_clear(CleanResults * crarg);
int cleanresults_getNet(CleanResults * crarg);
int cleanresults_getAdds(CleanResults * crarg);
int cleanresults_getDeletes(CleanResults * crarg);
int cleanresults_getPassed(CleanResults * crarg);
void cleanresults_sum (CleanResults * crtotalarg, CleanResults * crarg);

void cleanresults_print (CleanResults * crarg, int iterarg, FILE * fdarg);

#endif
