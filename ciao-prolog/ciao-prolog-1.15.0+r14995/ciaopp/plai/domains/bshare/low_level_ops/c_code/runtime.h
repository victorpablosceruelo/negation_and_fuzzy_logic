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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA
*/
/*
 * -------------------------------------------------------------------------
 * Filename      : runtime.h
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for NDB Runtime Parameters
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Wed Apr 23 13:23:21 2008 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#ifndef UNMNDBRUNTIME_H
#define UNMNDBRUNTIME_H

#include "recset.h"

struct perm;        /* forward declaration */

typedef struct runtime {
  int minbits;    /* minimum number of specified bits, r, also known as max_key */
  int n;          /* number of extra bits, previously know as STEP6, g, default is 0 */
  int emptyn;     /* also known as EMPTYSTEP6, default is 1 */
  int sizetest;   /* size test iteration > 0 */
  int tau;        /* if 0, calc tau=2^(r-|key|), o.w. input */
  int cleantests; /* if 1, perform calc num of cleantests; default 0 */
  char newndbmode;/* 0 by spec, 1 for simplified 1-bit, 3 is randomSATformula v.20 */
  unsigned char binmode;
  unsigned char oneshot;   /* web demo */
  char cmd;
  char * input;
  unsigned int seed;
  char * ndbname;
  char * ndbname2;       /* v.27 for joins, and other rel alg functions */
  char * outputname;     /* output for partials,selects,etc; optional secondary */ 
  char ndbformat;          
  char ndbformat2;       /* v.27 for joins, and other rel alg functions */
  char outputformat;
  char negtheta;    /* theta select - six enum choices v.24 */ 
  char * holdname;  /* command file name v.17 */
  int percent;      /* if !minbits && !percent then minbits is minkey */
  int highwater;    /* max deleted cache size on online adds, o.w. hold */
  int lock;         /* count lock files open v.54 */
  char reducemode;  /* 0 for all_random, 1 for all_popular, 2 alternate pop and rand, 3 pop on success, rand on backtrack v.51 */
  char minbitsspecifiedflag;  /* set to 1 if minbits was specified v.57 */
  unsigned char projopt; /* project option for partial queries v.31 */
  char relop;       /* relational operator - relop enum v.27 */
  char * order1;    /* v.27 join condition 1 */
  char * order2;    /* v.27 join condition 2 */
  char * testdatadir; /* v.54 dir for clean.dat, adds.dat, sizes.dat, distrib7- */
  char comparemode; /* v.65 0 = complements and compares; 1 = compares directly */
  char spare1;
  char spare2;
  char spare3;
  Recset * multiinputa;
  Recset * multiinputb;
} Runtime;

#define MAXLINE 255
typedef enum reloperator {op_noop, op_join, op_intersect, op_cross, op_union, op_binunion, op_starnegbu, op_difference, op_relevance, op_onlyinfirstdb} RelOp;
char * runtime_version(void);

char * new_string_concat(char * str1arg, char * str2arg);

void runtime_init (Runtime * runarg); 
void runtime_final(Runtime * runarg);

int runtime_setup_oneshot(Runtime * rtarg, char webcmdarg, char * strarg); 
int runtime_setup_multiinput(Runtime * runarg); /* v.70 */

#define runtime_getCommand(r) ((r)->cmd)
#define runtime_getInput(r) ((r)->input)
void runtime_setInput(Runtime * runarg, char * strarg); /* v.70 for multiinput */
Recset * runtime_getMultiInput(Runtime * runarg, int binmodearg); /* v.70 */

/* binmode is 0 for ascii, 1 for binary */
#define runtime_getBinMode(r) ((int)((r)->binmode))
char runtime_convertBinMode(int modearg);

void runtime_setBinMode(Runtime * runarg);
void runtime_clearBinMode(Runtime * runarg);

#define runtime_getProjOption(r) ((int)((r)->projopt)) 
void runtime_setProjOption(Runtime * runarg);      
void runtime_clearProjOption(Runtime * runarg);

int runtime_getOneShot(Runtime * runarg);
void runtime_setOneShot(Runtime * runarg);
void runtime_clearOneShot(Runtime * runarg);

#define runtime_getMinBits(rt) ((rt)->minbits)
#define runtime_getMinBitsSpecifiedFlag(rt) ((rt)->minbitsspecifiedflag)
void runtime_setMinBits(Runtime * runarg, int narg);
void runtime_setMinBitsSpecifiedFlag(Runtime * runarg);  /* v.57.5 */

#define runtime_getN(rt) ((rt)->n)
void runtime_setN(Runtime * runarg, int narg);

int runtime_getEmptyN (Runtime * runarg);
void runtime_setEmptyN(Runtime * runarg, int narg);

void runtime_setNewMode(Runtime * runarg, int narg);

int runtime_getReduceMode (Runtime * runarg);
void runtime_setReduceMode(Runtime * runarg, int narg);

int runtime_getCompareMode (Runtime * runarg);
void runtime_setCompareMode(Runtime * runarg, int narg);

int runtime_setNegTheta(Runtime * runarg, char * strarg);
NegTheta runtime_getNegTheta(Runtime * runarg);
char * runtime_convert2Theta(NegTheta ntarg);

#define runtime_getSizetest(r) ((r)->sizetest)
void runtime_setSizetest(Runtime * runarg, int narg);

#define runtime_getTau(r) ((r)->tau)
void runtime_setTau(Runtime * runarg, int narg);

int runtime_getCleanTests(Runtime * runarg);
void runtime_setCleanTests(Runtime * runarg, int narg);

#define runtime_getSeed(r) ((r)->seed)
void runtime_seedRandom(Runtime * runarg, int narg);

char * runtime_getName(Runtime * runarg);
void runtime_setName(Runtime * runarg, char * namearg);

NegFileFormat runtime_getFormat(Runtime * runarg);
char * runtime_convert2Format(NegFileFormat formatarg);
void runtime_setFormat(Runtime * runarg, char * namearg);

char * runtime_getName2(Runtime * runarg);
void runtime_setName2(Runtime * runarg, char * namearg);

NegFileFormat runtime_getFormat2(Runtime * runarg);
void runtime_setFormat2(Runtime * runarg, char * namearg);

char * runtime_getNameOutputFile(Runtime * runarg);
void runtime_setNameOutputFile(Runtime * runarg, char * namearg);

NegFileFormat runtime_getFormatOutputFile(Runtime * runarg);
void runtime_setFormatOutputFile(Runtime * runarg, char * namearg);

char * runtime_getTestDataDirectory(Runtime * runarg);
void runtime_setTestDataDirectory(Runtime * runarg, char * dirarg);

char * runtime_getHoldName(Runtime * runarg);
void runtime_setHoldName(Runtime * runarg, char * namearg);

#define runtime_getPercent(rt) ((rt)->percent)
void runtime_setPercent(Runtime * runarg, int parg);

void runtime_setHighWaterMark(Runtime * runarg, int hwmarg);

RelOp runtime_getRelOperator(Runtime * runarg);
RelOp runtime_setRelOperator(Runtime * runarg, char * listarg);
char * runtime_convert2RelOperator(RelOp oparg);

char * runtime_getOrder1(Runtime * runarg);
void runtime_setOrder1(Runtime * runarg, char * listarg);

char * runtime_getOrder2(Runtime * runarg);
void runtime_setOrder2(Runtime * runarg, char * listarg);

int runtime_getLock(Runtime * runarg);

int one_shot(Recset * ndbarg, Runtime * rtarg);
int outer_loop(Recset * ndbarg, Runtime * rtarg);

int online_query(char * strarg, Recset * ndbarg, Runtime * rtarg);
int online_copyproject(char * strarg, Recset * ndbarg, Runtime * rtarg); /* v.65.1 */

int online_add(char * strarg, Recset * ndbarg, Runtime * rtarg);
int online_remove(char * strarg, Recset * ndbarg, Runtime * rtarg);
int online_relationaloperation(Recset * ndbarg, Runtime * rtarg);
int online_compare(Recset * ndbarg, Runtime * rtarg); /* v.56 */
int online_complement(Recset * ndbarg, Runtime * rtarg); /* v.56 */



int test_clean ( Recset * ndbarg, Runtime * rtarg);

void print_sizetest(Recset * rsetarg, Runtime * rtarg);

void Print(Runtime * rtarg, char *fmt,...);
void Printd(Runtime * rtarg, char *fmt,...);
void PrintBR(Runtime * rtarg);

#endif
