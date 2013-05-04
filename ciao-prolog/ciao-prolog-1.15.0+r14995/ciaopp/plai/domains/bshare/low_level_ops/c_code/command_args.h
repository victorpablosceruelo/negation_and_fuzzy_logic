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
 * Filename      : command_args.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Common functions to process command line arguments
 * 
 * Creator       : Elena Settanni Ackley
 * Creation Date : Nov  8 1999 
 * Updated       : Tue Nov 20 15:40:27 2007 
 *
 * -------------------------------------------------------------------------
 */

#ifndef ES_COMMAND_ARGS_H
#define ES_COMMAND_ARGS_H

struct runtime;

/* ordered by the letter of the command argument */
typedef enum { addrec, binmode, cleanuprec, copyproj, oneshot,
               testdir, emptyp, emptypmodes, \
	       filename, fileformat, extrabits, highwater, singleton, \
               filename2, fileformat2, minbitscompress, \
               reclen, maxreclen, minbits, maxminrecsz, newndb, newndbmodes, \
	       outfilename, outfileformat, percent, projopt, queryrec, thetaquery,\
	       rmrec, seed, cleantests, cleantestsplus, unnegate, version, whileone, \
               relop, joincond1, joincond2, sizeiteration, compare, \
	       thishelp, lastact } caction;

void usage_reminder_abort(void);
int command_arguments(int argc, char ** argv, struct runtime * rtarg);

#endif


