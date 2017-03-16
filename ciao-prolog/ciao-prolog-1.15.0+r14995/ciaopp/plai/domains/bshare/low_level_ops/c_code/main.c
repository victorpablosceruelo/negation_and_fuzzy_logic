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
 * Filename      : main.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : NDB Main
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Thu Dec 30 11:54:56 2004 
 * Updated       : Thu Dec 30 11:55:03 2004 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <time.h>
#include "perm.h"
#include "runtime.h"
#include "recset.h"
#include "command_args.h"

int main(int argc, char ** argv ) 
{
  Runtime RT;
  Recset * NDB;
  int LEN = 0;
  int starttime = time(0);
  int rtn = 0;

  runtime_init(&RT);

  LEN = command_arguments(argc,argv,&RT);
  if(!runtime_getSeed(&RT))
    runtime_seedRandom(&RT,0);

  NDB = recset_create();
#if 0
  if(!runtime_getBinMode(&RT)){
    LEN = LEN << 3;   /* convert to bits */
  }
#endif
  recset_setlength(NDB,LEN);

  if(runtime_getHoldName(&RT)==NULL)
    rtn = one_shot(NDB,&RT);
  else
    rtn = outer_loop(NDB,&RT);

  recset_destroy(NDB);

  fprintf(stdout,"total elapsed time: %ld seconds",time(0) - starttime);
  PrintBR(&RT);

  runtime_final(&RT);
  /*  fprintf(stderr,"main returning %d\n",rtn); */
  return rtn;
}
