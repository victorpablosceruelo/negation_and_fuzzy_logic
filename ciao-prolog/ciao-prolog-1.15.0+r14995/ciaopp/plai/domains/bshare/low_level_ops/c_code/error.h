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
 * Filename      : errors.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Provides Negative Database Error Messages
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Fri Jul  7 15:51:49 2006 
 * Updated       : Sun Mar  9 08:08:51 2008 
 *
 * Comments      : long time coming..
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBERRORMSGS_H
#define UNMNDBERRORMSGS_H

/* ordered by the number of the error message */
#define errneg1 -1   /* invalid character or length */
#define errneg2 -2   /* too many specified bits per rec */
#define errneg3 -3   /* invariant: known bits in solution are no longer in recset */
#define err2 2       /* file in use */
#define err3 3       /* invalid command */
#define err4 4       /* null input */
#define err5 5       /* bad record length input */
#define err6 6       /* ndb size is zero */
#define err7 7       /* record length is zero */
#define err8 8       /* bad length record */
#define err9 9       /* file unreadable (see err14) */
#define err10 10     /* bad file header */
#define err11 11     /* bad join condition */
#define err12 12     /* insufficient memory */
#define err13 13     /* inconsistent cache size */
#define err14 14     /* file unwritable (see err9) */
#define err15 15     /* bad input parameter */
#define err16 16     /* invalid rec value */
#define err17 17     /* rstree invariant broken */
#define err18 18     /* bad script command */
#define err19 19     /* check bit freq failed */
#define err20 20     /* insufficient records */
#define err21 21     /* insufficient record size (too few specified bits) */
#define err22 22     /* cleanup doesn't add up */
#endif
