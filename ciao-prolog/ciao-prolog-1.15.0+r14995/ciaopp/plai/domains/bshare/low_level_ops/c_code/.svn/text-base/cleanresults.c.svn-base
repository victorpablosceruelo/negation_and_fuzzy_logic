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
 * Filename      : cleanresults.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for Clean Results
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Mon Jul  4 10:04:39 2005 
 * Updated       : Mon Jul  4 10:04:43 2005 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include "cleanresults.h"

void cleanresults_set (CleanResults * crarg, int aarg, int darg, int parg)
{
  crarg->added = aarg;
  crarg->deleted = darg;
  crarg->passed = parg;
}


void cleanresults_clear (CleanResults * crarg)
{
  crarg->added = 0;
  crarg->deleted = 0;
  crarg->passed = 0;
}


int cleanresults_getNet(CleanResults * crarg)
{ 
  return crarg->added - crarg->deleted;
}


int cleanresults_getAdds(CleanResults * crarg)
{ 
  return crarg->added;
}


int cleanresults_getDeletes(CleanResults * crarg)
{ 
  return crarg->deleted;
}


int cleanresults_getPassed(CleanResults * crarg)
{ 
  return crarg->passed;
}


void cleanresults_sum (CleanResults * crtotalarg, CleanResults * crarg)
{
  crtotalarg->added += crarg->added;
  crtotalarg->deleted += crarg->deleted;
  crtotalarg->passed += crarg->passed;
}


void cleanresults_print (CleanResults * crarg, int iterarg, FILE * fdarg)
{
  fprintf(fdarg,"%d %d %d %d %d \n",
	  iterarg,
	  crarg->added,
	  crarg->deleted,
	  cleanresults_getNet(crarg),
	  crarg->passed
	  );
}
