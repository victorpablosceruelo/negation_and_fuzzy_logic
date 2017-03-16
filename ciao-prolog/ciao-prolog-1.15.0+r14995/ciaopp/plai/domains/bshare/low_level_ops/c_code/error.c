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
/* -------------------------------------------------------------------------
 * Filename      : errors.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Provides Negative Database Error Messages
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Fri Jul  7 15:51:49 2006 
 * Updated       : Fri Jul  7 15:51:52 2006 
 *
 * Comments      : long time coming..
 *
 * -------------------------------------------------------------------------
 */
#include "error.h"

static char * ERRMSG[] = {
  "",
  "",
  "Database in use, try again later or startover",
  "Invalid command",
  "Null input"
};

#define et_count ((sizeof(ERRMSG)/sizeof(ERRMSG[0])))  

char * error_message(errId errarg) {
  return ERRMSG[errarg];
}
