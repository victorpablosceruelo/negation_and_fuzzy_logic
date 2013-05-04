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
 * Filename      : runtime.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Responsible for NDB Runtime Parameters
 * 
 * Creator       : E.S. Ackley
 * Creation Date : Tue Dec 28 17:16:59 2004 
 * Updated       : Wed Apr 23 12:31:21 2008 
 *
 * Comments      : based on the Fernando Esponda NDB Algorithms
 *
 * -------------------------------------------------------------------------
 */
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include "negdb.h"
#include "newndb.h"
#include "easy.h"
#include "error.h"
#include "names.h"

static char * version = "v.72.8";
static void runtime_clearMultiInput(Runtime * runarg, int binmodearg);

char * runtime_version()
{
  return version;
}


void runtime_init (Runtime * runarg) 
{
  runarg->minbits = 0;   
  runarg->n = 0;                 /* STEP4 of insert - extra bits */
  runarg->emptyn = 0;         /* PRUNE EMPTY Recs to emptyn bits */
  runarg->sizetest = 0;               /* size test iteration > 0 */ 
  runarg->tau = 0;         /* clean if cache >= tau, tau=0 is flag to calculate*/
  runarg->cleantests = 0;  /* do cleanup  cleantests times */
  runarg->newndbmode = 0; /* 0 by spec, 1 for simplified 1-bit, 3 is random SAT formula v.20 */
  runarg->binmode = 0;            /* 1 = binary mode; 0 = ascii; */
  runarg->oneshot = 0;                       /* one command only */
  runarg->cmd = 'x';                            /* N, A, R, Q */
  runarg->input = NULL;                      /* input for cmd */
  runarg->seed = 0;
  runarg->ndbname = NULL;
  runarg->ndbname2 = NULL;
  runarg->outputname = NULL; 
  runarg->ndbformat = noformat;
  runarg->ndbformat2 = noformat;
  runarg->outputformat = noformat; /* output for partials,selects,etc;opt extra */ 
  runarg->negtheta = O_bool;     /* +eq is the default v.24, O_bool in v.26 */
  runarg->holdname = NULL;   /* cmds to execute in hold file v.17 */
  runarg->percent = 0;   /* expand recsize to greatest num bits that
                            still has <= this % accumulated
                            recs (used if -m unspecified/zero) 
                            default is minkey v.14 */
  runarg->highwater = 0; /* online_adds: max deleted cache sz v.17 */
  runarg->lock = 0;      /* count of lock files v.54   */
  runarg->reducemode = 0;  /* v.51 easy reduction mode */
  runarg->minbitsspecifiedflag = 0;  /* set to 1 if minbits was specified v.57 */
  runarg->projopt = 0;     /* v.31 project option for partial queries */
  runarg->relop = op_noop; /* v.27 relational operator */
  runarg->order1 = NULL;   /* v.27 join condition 1 */
  runarg->order2 = NULL;   /* v.27 join condition 2 */
  runarg->testdatadir = NULL; /* v.54 dir for .dat files, defaults to cwd (.) */
  runarg->comparemode = 0; /* v.65 0 = complements and compares; 1 = compares directly */
  runarg->spare1 = 0;
  runarg->spare2 = 0;
  runarg->spare3 = 0;
  runarg->multiinputa = NULL; /* v.70 multi hidden solutions in singletons - ascii */
  runarg->multiinputb = NULL; /* v.70 multi hidden solutions in singletons - binary */
}


/* return 0 if normal; 1 if normal but no length to compare; o.w. error */
static int verify_input_length(Runtime * rtarg, int lenarg)
{
  char * input;
  int slen;
  int len = lenarg;

  if(!len) return 1;  /* ok, no length to compare against, for filenames */

  input = runtime_getInput(rtarg);

  if(input==NULL){
    fprintf(stderr,"UNVERIFIED: input is null\n");
    return err4;
  }

  slen = strlen(input);

  if(! runtime_getBinMode(rtarg)) {  /* ascii mode divides lenarg by char size */
    len = len/8;
  }

  if(slen != len) {
    fprintf(stdout,"UNVERIFIED: length of input <PRE>[%s]</PRE> is %d, not %d\n",
            input,slen,len);
    return err5;
  }

  /*    fprintf(stdout,"verified length of input [%s] is %d, %d\n",
          input,slen,len); */
  return 0; 
}


__inline__ void runtime_setCommand(Runtime * runarg, char cmdarg)
{
  runarg->cmd = cmdarg;
}


static void runtime_finalInput(Runtime * runarg)
{
  if(runarg->input != NULL){
    free(runarg->input);
    runarg->input = NULL;
  }
}


static void runtime_finalMultiInput(Runtime * runarg) 
{
  runtime_clearMultiInput(runarg,0);
  runtime_clearMultiInput(runarg,1);
}


static void runtime_finalOrder(Runtime * runarg)
{
  if( runarg->order1 != NULL ){
    free(runarg->order1);
    runarg->order1 = NULL;
  }

  if( runarg->order2 != NULL ){
    free(runarg->order2);
    runarg->order2 = NULL;
  }
}


static void runtime_finalHoldName(Runtime * runarg)
{
  if(runarg->holdname != NULL){
    free(runarg->holdname);
    runarg->holdname = NULL;
  }
}


static void runtime_finalName(Runtime * runarg)
{
  if(runarg->ndbname != NULL){
    free(runarg->ndbname);
    runarg->ndbname = NULL;
  }
  if(runarg->ndbname2 != NULL){
    free(runarg->ndbname2);
    runarg->ndbname2 = NULL;
  }
  if(runarg->outputname != NULL){
    free(runarg->outputname);
    runarg->outputname = NULL;
  }
  if(runarg->testdatadir != NULL){    /* v.54 */
    free(runarg->testdatadir);
    runarg->testdatadir = NULL;
  }
}


void runtime_final(Runtime * runarg)
{
  runtime_finalInput(runarg);
  runtime_finalOrder(runarg);
  runtime_finalName(runarg);
  runtime_finalHoldName(runarg);
  runtime_finalMultiInput(runarg);
}


static char * new_string(char * strarg)
{
  char * newstr;
  int slen;
  if(strarg==NULL) return NULL;
  slen = strlen(strarg) + 1;
  newstr = malloc(sizeof(char) * slen);  
  memcpy(newstr,strarg,slen);
  return newstr;
}


char * new_string_concat(char * str1arg, char * str2arg)
{
  char * newstr;
  int slen, slen1, slen2;
  if(str1arg==NULL && str2arg==NULL) return NULL;
  slen1 = strlen(str1arg);
  slen2 = strlen(str2arg);
  slen = slen1 + slen2 + 1;
  newstr = malloc(sizeof(char) * slen);  
  memcpy(newstr,str1arg,slen1);
  memcpy(newstr+slen1,str2arg,slen2+1);
  return newstr;
}


void runtime_setInput(Runtime * runarg, char * strarg)
{
  if(strarg==NULL) return;   /* switched to first rather than second step v.27 */
  runarg->input = new_string(strarg);
}


/* remove newline; return new string length, -1 if error */
static int chop(char * strarg)
{
  int slen = strlen(strarg);
  char c = strarg[slen-1];
  if(c != '\n'){
    fprintf(stderr,"input (%d) is much longer than allow (%d): last character [%c] is not a newline\n",
            slen,MAXLINE,c);
    return -1;
  } 

  strarg[slen-1] = '\0';     /* remove the newline */
  return slen-1;
}


/* this is used by the outer_loop and assumes and removes the newline
   at the end of the strarg; frees any previous input */
static void runtime_cleanInput(Runtime * runarg, char * strarg)
{
  runtime_finalInput(runarg);
  if(chop(strarg) < 0) return;
  runtime_setInput(runarg,strarg);
}


int runtime_setup_oneshot(Runtime * runarg, char webcmdarg, char * strarg) 
{
  /*  fprintf(stderr,"got cmd %c and input <%s>\n",webcmdarg,strarg); */
  if(runtime_getCommand(runarg) != 'x') return 0; /* only one command v.17 */
  runtime_setCommand(runarg,webcmdarg);
  runtime_setInput(runarg,strarg);
  return 1;
}


Recset * runtime_getMultiInput(Runtime * runarg, int binmodearg)
{
  if(binmodearg == 0)
    return runarg->multiinputa;
  return runarg->multiinputb;
}


static void runtime_setMultiInput(Runtime * runarg, Recset * multisetarg, int binmodearg)
{
  if(binmodearg == 0)
    runarg->multiinputa = multisetarg;
  else
    runarg->multiinputb = multisetarg;
}


static void runtime_clearMultiInput(Runtime * runarg, int binmodearg)
{
  recset_destroy(runtime_getMultiInput(runarg,binmodearg));
  runtime_setMultiInput(runarg,NULL,binmodearg);
}


/* use a recset to collect multiple input strings as recs v.70 */
int runtime_setup_multiinput(Runtime * runarg)
{
  int reclen;
  Recset * multiseta = runtime_getMultiInput(runarg,0);
  Recset * multisetb = runtime_getMultiInput(runarg,1);
  int binmode = runtime_getBinMode(runarg);

  if( multiseta == NULL && multisetb == NULL){        /* first input only */
    char * input = runtime_getInput(runarg);

    if(input==NULL){    
      fprintf(stderr,"UNVERIFIED: multi input is null\n");
      return err4;
    }
    if(binmode!=1) {       /* if 1, no need for a */
      multiseta = recset_create();
      recset_setlength(multiseta,strlen(input)*CHAR_T);
      runtime_setMultiInput(runarg,multiseta,0);
    }                      /* either way, need b  */
    multisetb = recset_create();
    recset_setlength(multisetb,strlen(input));
    runtime_setMultiInput(runarg,multisetb,1);
  } 

  /* if binmode is set only do binary mode */
  if(binmode == 1) { /* clear out ascii copy */
    if(multiseta != NULL){
      runtime_clearMultiInput(runarg,0);
    }
    /* add the new input to multisetb only */
    reclen = recset_length(multisetb);
    if(verify_input_length(runarg,reclen) == 0){    /* length is correct */
      Rec * inputrec = rec_create(recset_numunits(multisetb));
      if(rec_string2rec(inputrec,reclen,runtime_getInput(runarg),1) == errneg1) {
        runtime_clearMultiInput(runarg,1); /* can't do binary mode either */
        rec_destroy(inputrec);
        return errneg1;
      }
      insert(inputrec,multisetb,runarg,wo_npg,0); /* essentially append */
      rec_destroy(inputrec);
    } else {
      fprintf(stderr,"UNVERIFIED: singleton binary input length is invalid\n");
      return err8;
    }
    runtime_finalInput(runarg); /* clear way for another */
    return 0;      /* yeah, normal */

  } else { /* ascii, or still possibly binary, at this point */

    if(multisetb != NULL) {        /* add the new input to multisetb */
      reclen = recset_length(multisetb);
      if(verify_input_length(runarg,reclen*CHAR_T) == 0){  /* adjust length in this case verify will divide since not in binmode yet */
        Rec * inputrec = rec_create(recset_numunits(multisetb));
        if(rec_string2rec(inputrec,reclen,runtime_getInput(runarg),1) == errneg1) {
          runtime_clearMultiInput(runarg,1); /* can't do binary mode */
          rec_destroy(inputrec);
        } else {
          insert(inputrec,multisetb,runarg,wo_npg,0); /* essentially append */
          rec_destroy(inputrec);
        }
      } else {
        fprintf(stderr,"UNVERIFIED: singleton binary input length is invalid\n");
        runtime_clearMultiInput(runarg,1); /* can't do binary mode */
        return err8;
      }
    }
    
    if(multiseta != NULL) {        /* add the new input to multiseta */
      reclen = recset_length(multiseta);
      if(verify_input_length(runarg,reclen) == 0){    /* length is correct */
        Rec * inputrec = rec_create(recset_numunits(multiseta));
        if(rec_string2rec(inputrec,reclen,runtime_getInput(runarg),0) <= 0) {
          runtime_clearMultiInput(runarg,0); /* can't do ascii mode */
          rec_destroy(inputrec);
          exit(err16);
        } else {
          insert(inputrec,multiseta,runarg,wo_npg,0); /* essentially append */
          rec_destroy(inputrec);
        }
      } else {
        fprintf(stderr,"UNVERIFIED: singleton ascii input length is invalid\n");
        runtime_clearMultiInput(runarg,0); /* can't do ascii mode */
        return err8;
      }
    } else {    /* no ascii */
      if(runtime_getMultiInput(runarg,1) == NULL){ /* make sure b is null */
        fprintf(stderr,"ERROR: hopeless singleton, neither ascii nor binary is valid\n");
        exit(err16); /* hopeless */
      }
    }
  }
  runtime_finalInput(runarg); /* clear way for another */
  return 0;
} 


char runtime_convertBinMode(int modearg)
{
  if(modearg==0) return 'a';
  else return 'b';
}


/* set binary mode */
void runtime_setBinMode(Runtime * runarg)
{
  runarg->binmode = 1;
}


void runtime_clearBinMode(Runtime * runarg)
{
  runarg->binmode = 0;
}


void runtime_setProjOption(Runtime * runarg)
{
  runarg->projopt = 1;
}


void runtime_clearProjOption(Runtime * runarg)
{
  runarg->projopt = 0;
}


int runtime_getOneShot(Runtime * runarg)
{
  return (int) runarg->oneshot;
}


void runtime_setOneShot(Runtime * runarg)
{
  runarg->oneshot = 1;
}


void runtime_clearOneShot(Runtime * runarg)
{
  runarg->oneshot = 0;
}


void runtime_setMinBits(Runtime * runarg, int narg)
{
  runarg->minbits = narg;
}


void runtime_setMinBitsSpecifiedFlag(Runtime * runarg)
{
  runarg->minbitsspecifiedflag = 1;   /* v.57.5 */
}


void runtime_setN(Runtime * runarg, int narg)
{
  runarg->n = narg;
}


int runtime_getEmptyN (Runtime * runarg) 
{
  return runarg->emptyn;
}


void runtime_setEmptyN(Runtime * runarg, int narg)
{
  runarg->emptyn = narg;
}


static int runtime_getNewMode (Runtime * runarg) 
{
  return runarg->newndbmode;
}


void runtime_setNewMode(Runtime * runarg, int narg)
{
  runarg->newndbmode = narg;
}


/* v.51 */
int runtime_getReduceMode (Runtime * runarg) 
{
  return runarg->reducemode;
}


/* v.51 */
void runtime_setReduceMode(Runtime * runarg, int narg)
{
  runarg->reducemode = narg;
}


/* v.65 */
int runtime_getCompareMode (Runtime * runarg) 
{
  return runarg->comparemode;
}


void runtime_setCompareMode(Runtime * runarg, int narg)
{
  runarg->comparemode = narg;
}


NegTheta runtime_getNegTheta(Runtime * runarg) 
{
  return (NegTheta) runarg->negtheta;
}


int runtime_setNegTheta(Runtime * runarg, char * strarg)
{
  runarg->negtheta = O_bool;              /* default set */
  if ( strarg[0] == 'n' && strarg[1] == 'e' )
    runarg->negtheta = O_eq;
  else {
    if ( strarg[0] == 'e' && strarg[1] == 'q' )
      runarg->negtheta = O_ne;
    else {  /* try greater */
      if ( strarg[0] == 'g') {
        if ( strarg[1] == 'e' )
          runarg->negtheta = O_lt;
        else {
          if ( strarg[1] == 't' )
            runarg->negtheta = O_le;
          else 
            return -1;
        } 
      } else {  /* try less */
        if ( strarg[0] == 'l') {
          if ( strarg[1] == 'e' )
            runarg->negtheta = O_gt;
          else {
            if ( strarg[1] == 't' )
              runarg->negtheta = O_ge;
            else 
              return -1;
          }
        } else
          return -1;
      }
    }
  }
  return runarg->negtheta;
}


/* return the equivalent 2 letter neumonic positive theta */
char * runtime_convert2Theta(NegTheta ntarg) 
{
  if(ntarg == O_eq) return "ne";
  if(ntarg == O_ne) return "eq";
  if(ntarg == O_lt) return "ge";
  if(ntarg == O_le) return "gt";
  if(ntarg == O_gt) return "le";
  if(ntarg == O_ge) return "lt";
  return "bool";
}


void runtime_setSizetest(Runtime * runarg, int narg)
{
  runarg->sizetest = narg;
}


void runtime_incrementSizetest(Runtime * runarg)
{
  runarg->sizetest++;
}


void runtime_setTau(Runtime * runarg, int narg)
{
  runarg->tau = narg;
}


int runtime_getCleanTests(Runtime * runarg)
{
  return runarg->cleantests;
}


void runtime_setCleanTests(Runtime * runarg, int narg)
{
  runarg->cleantests = narg;
}


/* only called by runtime_seedRandom v.27 */
static void runtime_setSeed(Runtime * runarg, int narg)
{
  runarg->seed = narg;
}


void runtime_seedRandom(Runtime * runarg, int narg)
{
  int seed = perm_seed_random(narg);
  runtime_setSeed(runarg,seed);
  fprintf(stdout,"using seed %d",seed);
  PrintBR(runarg);
}


/* v.54 could be null which defaults to cwd */
char * runtime_getTestDataDirectory(Runtime * runarg)
{
  return runarg->testdatadir;
}


void runtime_setTestDataDirectory(Runtime * runarg, char * dirarg)
{
  int slen;
  if(dirarg == NULL) return;
  slen = strlen(dirarg) - 1;
  /* remove any trailing slashes */
  if( (dirarg[slen] == '/') || (dirarg[slen] == '\\')) {
    dirarg[slen] = '\0';
  }
  if(runarg->testdatadir != NULL)
    free(runarg->testdatadir);
  runarg->testdatadir = new_string(dirarg);
}


char * runtime_getName(Runtime * runarg)
{
  if(runarg->ndbname == NULL){
    if(runtime_getFormat(runarg) == cnfformat)
      return CNFNAME;
    else
      return NDBNAME;
  }
  return runarg->ndbname;
}


void runtime_setName(Runtime * runarg, char * namearg)
{
  if(runarg->ndbname != NULL)
    free(runarg->ndbname);
  runarg->ndbname = new_string(namearg);
}


NegFileFormat runtime_getFormat(Runtime * runarg)
{
  if(runarg->ndbformat == noformat)
    return ndbformat;
  return runarg->ndbformat;
}

char * runtime_convert2Format(NegFileFormat formatarg) 
{
  if(formatarg == ndbformat) 
    return "ndb";
  else {
    if (formatarg == cnfformat) 
      return "cnf";
    else 
      return "none";
  }
  return "none";
}


void runtime_setFormat(Runtime * runarg, char * formatarg)
{
  if(formatarg[0]=='n')
    runarg->ndbformat = ndbformat;
  else{
      if(formatarg[0]=='c')
        runarg->ndbformat = cnfformat;
      else
        runarg->ndbformat = noformat;      /* default case */
  }
}

char * runtime_getName2(Runtime * runarg)
{
  if(runarg->ndbname2 == NULL){
    if(runtime_getFormat2(runarg) == cnfformat)
      return CNFNAME2;
    else
      return NDBNAME2;
  }
  return runarg->ndbname2;
}


void runtime_setName2(Runtime * runarg, char * namearg)
{
  if(runarg->ndbname2 != NULL)
    free(runarg->ndbname2);
  runarg->ndbname2 = new_string(namearg);
}


NegFileFormat runtime_getFormat2(Runtime * runarg)
{
  if(runarg->ndbformat2 == noformat)
    return ndbformat;
  return runarg->ndbformat2;
}


void runtime_setFormat2(Runtime * runarg, char * formatarg)
{
  if(formatarg[0]=='n')
    runarg->ndbformat2 = ndbformat;
  else{
      if(formatarg[0]=='c')
        runarg->ndbformat2 = cnfformat;
      else
        runarg->ndbformat2 = noformat;      /* default case */
  }
}


char * runtime_getNameOutputFile(Runtime * runarg)
{
  char cmd = runtime_getCommand(runarg);
  int fmt = runtime_getFormatOutputFile(runarg);

  if(runarg->outputname == NULL) {  /* could be null if no secondary output */
    if( cmd == 'Q' || cmd == 'P' ){
      return (fmt == cnfformat ? PARTIALCNFNAME : PARTIALNDBNAME);
    } else {                        /* secondary output */
      if( cmd == 'X'){
        return (fmt == cnfformat ? CNFNAME3 : NDBNAME3) ;
      } else {
        if (cmd == 'U') {
          return (fmt == cnfformat ? UNCNFNAME : UNNDBNAME) ;
        } else {
          if(fmt == cnfformat)
            return CNFNAME;
          else {
            /*          fprintf(stderr,"secondary output file name is null [%s] and command is [%c]\n",
                        runarg->outputname,
                        runtime_getCommand(runarg)
                        );
            */
          }
        }
      }
    }
  }
  return runarg->outputname;
}


void runtime_setNameOutputFile(Runtime * runarg, char * namearg)
{
  if(runarg->outputname != NULL)
    free(runarg->outputname);
  runarg->outputname = new_string(namearg);
}


/* default output format for partial queries and relational operations
   and unnegate is ndb; o.w. optional for extra secondary output */
NegFileFormat runtime_getFormatOutputFile(Runtime * runarg)
{
  char cmd = runtime_getCommand(runarg);
  if(runarg->outputformat == noformat && (cmd == 'Q' || cmd == 'X' || cmd == 'U'))
    return ndbformat;
  return runarg->outputformat;
}


void runtime_setFormatOutputFile(Runtime * runarg, char * formatarg)
{
  if(formatarg[0]=='n')
    runarg->outputformat = ndbformat;
  else{
      if(formatarg[0]=='c')
        runarg->outputformat = cnfformat;
      else
        runarg->outputformat = noformat;      /* default case */
  }
}


char * runtime_getHoldName(Runtime * runarg)
{
  return runarg->holdname;
}


void runtime_setHoldName(Runtime * runarg, char * namearg)
{
  if(runarg->holdname != NULL)
    free(runarg->holdname);
  runarg->holdname = new_string(namearg);
}


void runtime_setPercent(Runtime * runarg, int parg)
{
  assert(parg >= 0);
  runarg->percent = parg;
}


#define runtime_getHighWaterMark(rt) ((rt)->highwater)
void runtime_setHighWaterMark(Runtime * runarg, int hwmarg)
{
  assert(hwmarg >= 0);
  runarg->highwater = hwmarg;
}


RelOp runtime_getRelOperator(Runtime * runarg)
{
  return runarg->relop;
}


RelOp runtime_setRelOperator(Runtime * runarg, char * listarg)
{
  char op = listarg[0];

  switch (op) {
  case 'j':
  case 'J':
    runarg->relop = op_join;
    break;
  case 'i':
  case 'I':
    runarg->relop = op_intersect;
    break;
  case 'c':
  case 'C':
    runarg->relop = op_cross;
    break;
  case 'u':
  case 'U':
    runarg->relop = op_union;
    break;
  case 'b':
  case 'B':
    runarg->relop = op_binunion;
    break;
  case 's':
  case 'S':
    runarg->relop = op_starnegbu;
    break;
  case 'd':
  case 'D':
    runarg->relop = op_difference;
    break;
  case 'r':
  case 'R':
    runarg->relop = op_relevance;
    break;
  case 'o':
  case 'O':
    runarg->relop = op_onlyinfirstdb;
    break;
  default:
  runarg->relop = op_noop;
  };
  runtime_setCommand(runarg,'X');
  return runarg->relop;
}


/* return the equivalent 2 letter neumonic positive theta */
char * runtime_convert2RelOperator(RelOp oparg) 
{
  if(oparg == op_join) return "Join";
  if(oparg == op_intersect) return "Intersection";
  if(oparg == op_cross) return "CrossProduct";
  if(oparg == op_union) return "Union";
  if(oparg == op_binunion) return "BinaryUnion";
  if(oparg == op_starnegbu) return "StarNegBinaryUnion";
  if(oparg == op_difference) return "Difference";
  if(oparg == op_relevance) return "RelevanceCDB";
  if(oparg == op_onlyinfirstdb) return "OnlyInFirstDB";
  return "NoOp";
}


char * runtime_getOrder1(Runtime * runarg) 
{
  return runarg->order1;
}


void runtime_setOrder1(Runtime * runarg, char * listarg) {
  if(runarg->order1 != NULL)
    free(runarg->order1);
  runarg->order1 = new_string(listarg);
}


char * runtime_getOrder2(Runtime * runarg) 
{
  return runarg->order2;
}


void runtime_setOrder2(Runtime * runarg, char * listarg) {
  if(runarg->order2 != NULL)
    free(runarg->order2);
  runarg->order2 = new_string(listarg);
}


/* returns count of lock files v.54 */
int runtime_getLock(Runtime * runarg) 
{
  return runarg->lock;
}


static void runtime_unlock(Runtime * runarg, char * filenamearg) 
{
  char * LOCKNDBNAME = new_string_concat(filenamearg,".LCK");
  unlink(LOCKNDBNAME);
  free(LOCKNDBNAME);
  runarg->lock -= 1;
}


static int runtime_lock(Runtime * runarg, char * lockfilenamearg) 
{
  FILE * lockfile = fopen(lockfilenamearg,"w");
  int lock = 0;
  if (!lockfile) {
      fprintf(stdout,"lockfile [%s] exists afterall",lockfilenamearg);
      PrintBR(runarg);
  } else {
    pid_t mypid = getpid(); 
    fprintf(lockfile,"%u",mypid);
    fclose(lockfile);
    /*    fprintf(stdout,"my pid is [%u]<BR>",mypid); */
    runarg->lock += 1;
    lock = 1;
  }
  return lock;
}


static int runtime_read_with_intention_to_write_later(Runtime * runarg, char * filenamearg)
{
  int returnOK = 0;
  char * LOCKNDBNAME = new_string_concat(filenamearg,".LCK");
  FILE * lockfile = fopen(LOCKNDBNAME, "r");
  /* open file.lock - get a pid, if any, and see if still alive */
  if(!lockfile){
    returnOK = 1;
  } else {
    int rmlockok = 0;
    pid_t pid = 0;

    fscanf(lockfile,"%d",&pid);
    fclose(lockfile);
    /*   fprintf(stdout,"lock pid is [%u]<BR>",pid); */

    if(pid) {    /*   && (mypid != pid) */
      int ret=kill(pid,0);      
      if(ret==-1 && errno==ESRCH) rmlockok = 1;
    } else {   /* pid exists, check age */
      struct stat stat_p;
      time_t  lockmodtime;
      time_t  currenttime;
      stat(LOCKNDBNAME,&stat_p);
      lockmodtime = stat_p.st_mtime;
      currenttime = time(&currenttime);
      if (currenttime - lockmodtime > 3600) {
	rmlockok = 1;
      }
    }
    if(rmlockok == 1) {
      runtime_unlock(runarg, filenamearg);
      returnOK = 1;
    }
  }
  if(returnOK == 1) {
    returnOK = runtime_lock(runarg, LOCKNDBNAME);
  }
  free(LOCKNDBNAME);
  return returnOK;
}


/* requires -z arg (greater than zero) to output data to adds.dat */
/* ##iteration/DBsize Dxsize addedrecs maxkey ndbsize length */
static void print_add_data(Recset * rsetarg,Runtime * rtarg, int dxarg, int addarg)
{
  FILE * fd;
  char * testdir;
  int sizetest = runtime_getSizetest(rtarg);

  if(!sizetest) { return; }

  testdir = runtime_getTestDataDirectory(rtarg);
  if(testdir == NULL) {
    fd = fopen("./adds.dat","a");
  }  else {
    char * fname = new_string_concat(testdir,"/adds.dat");
    if( fname == NULL) {
      fprintf(stderr,"filename creation error for print add data\n"); 
      exit(err12);
    }
    fd = fopen(fname,"a");
    free(fname);
  }
  
  if( fd == NULL ) {
    fprintf(stderr,"file open error on print add data\n"); 
    exit(err14);
  }

  /* ##iteration/DBsize Dxsize addedrecs maxkey ndbsize length */
  fprintf(fd,"%d %d %d %d %d %d\n",
	  sizetest,
	  dxarg,
	  addarg,
	  runtime_getMinBits(rtarg),
	  recset_size(rsetarg),
	  recset_length(rsetarg)
	  );
  fclose(fd);
}


/* return the number of records in rsetarg that have more than minbits specified */
static int count_bigrecs(Recset * rsetarg, Runtime * rtarg)
{
  int i;
  int minbits = runtime_getMinBits(rtarg);
  int lastrec = recset_size(rsetarg);
  int big = 0;
  int numunits = rec_calcnumunits(recset_length(rsetarg));

  for(i = lastrec -1; i >= 0; i--) {
    if (rec_size(recset_getrec(rsetarg,i),numunits) > minbits) {
      big++;
    }
  }

  PrintBR(rtarg);
  fprintf(stdout,"bigrecs: %d recs > %d specified bits",big,minbits);
  PrintBR(rtarg);
  PrintBR(rtarg);
  return big;
}


void print_sizetest(Recset * rsetarg, Runtime * rtarg) 
{
  FILE * fd;
  char * testdir;
  char cmd;
  
  testdir = runtime_getTestDataDirectory(rtarg);
  if(testdir == NULL) {
    fd = fopen("./sizes.dat","a");
  }  else {
    char * fname = new_string_concat(testdir,"/sizes.dat");
    if( fname == NULL) {
      fprintf(stderr,"filename creation error for print sizetest data\n"); 
      exit(err12);
    }
    fd = fopen(fname,"a");
    free(fname);
  }

  if( fd == NULL ) {
    fprintf(stderr,"file open error on print sizetest data\n"); 
    exit(err14);
  }

  cmd = runtime_getCommand(rtarg);

  if (cmd == 'U') cmd = 'u'; /* NegUnion conflicts with un-negate v.56 */

  /* fill in the relational operator for cmd X v.54 */
  if (cmd == 'X') {
    RelOp op = runtime_getRelOperator(rtarg);
    char * theta = runtime_convert2RelOperator(op);
    cmd = theta[0];
    if (cmd == 'C') cmd = 'P'; /* CrossProduct C conflicts with Clean */
    if (cmd == 'R') cmd = 'r'; /* Relevance R conflicts with Remove */
  }

  /* ##iteration/DBsize ndbsize cmd recs>r r l n tau*/
  fprintf(fd,"%d %d %c %d %d %d %d %d %d %d %d\n",
	  runtime_getSizetest(rtarg),
	  recset_size(rsetarg),
	  cmd,
	  count_bigrecs(rsetarg,rtarg),
	  runtime_getMinBits(rtarg),
	  recset_length(rsetarg),
	  runtime_getN(rtarg),
	  runtime_getTau(rtarg),
          recset_min_recordsize(rsetarg),
          recset_max_recordsize(rsetarg),
          recset_most_recordsize(rsetarg)
	  );
  fclose(fd);
}


int outer_loop(Recset * ndbarg, Runtime * rtarg)
{
  char str[MAXLINE];      
  char * filename;
  FILE *fd;
  int dbsize = -1;
  int dbadds = 0;
  int dbaddnots = 0;
  int starttime = time(0);
  int rtn = 0;

  if (!runtime_read_with_intention_to_write_later(rtarg, runtime_getName(rtarg))){
    fprintf(stdout,"Database in use, try again later or startover");
    PrintBR(rtarg);
    return err2;
  }
    
  filename = runtime_getHoldName(rtarg);
  if(filename == NULL){
    fprintf(stderr,"no script file to parse (use -w)\n");
    exit(err4);
  }
  
  fd = fopen(filename,"r");
  
  if(fd==NULL){
    fprintf(stderr,"unable to open %s\n",filename);
    exit(err9);
  }

  runtime_setSizetest(rtarg,0);   /* can be modified with Z cmd v.69.2 */

  while(fgets(str,MAXLINE,fd) != NULL){
    int num;
    char cmd = str[0];
    
    num = strlen(str);
    if(num <= 0 ) break;
    
    if(str[num-1] != '\n'){
      fprintf(stderr,"outer_loop: command [%c] too long, no newline, [%c] instead\n",
              cmd,str[num-1]);
      exit(err18);
    }
  
    runtime_setCommand(rtarg,cmd);
    
    switch (cmd) {
    case 'A':
      assert( dbsize >= 0);  
      assert(recset_length(ndbarg) > 0);
      runtime_cleanInput(rtarg,str+1);
      fprintf(stdout,"ADD #%d: ",dbadds+1);
      if(online_add(runtime_getInput(rtarg),ndbarg,rtarg))
        dbadds++;
      else
        dbaddnots++;
      runtime_incrementSizetest(rtarg);
      print_sizetest(ndbarg,rtarg);
      break;

    case 'B':
      num = atoi(str+1);            /* note: no num is also zero */
      if(num==0){
        runtime_clearBinMode(rtarg);
        fprintf(stdout,"Not in binary mode\n");
      } else {
        runtime_setBinMode(rtarg);
        fprintf(stdout,"In binary mode\n");
      }
      break;

    case 'C' :
      num = atoi(str+1);
      runtime_setCleanTests(rtarg,num);   
      runtime_setTau(rtarg,0);
      if(runtime_getCleanTests(rtarg) > 0) {
        test_clean(ndbarg,rtarg);
      } else {
        CleanResults cleanResults;
        fprintf(stderr,"single clean\n");
        cleanup(ndbarg,rtarg,NULL,0,&cleanResults);
      }
      runtime_incrementSizetest(rtarg);
      print_sizetest(ndbarg,rtarg);
      break;

    case 'D':
      if(chop(str+1) > 1) {            /* v.69.2 */
        if(str[1] == '0') {
          runtime_setTestDataDirectory(rtarg,str+2);
          fprintf(stdout,"Test Data Directory is [%s]\n",
                  runtime_getTestDataDirectory(rtarg));
        } else {
          if(str[1] == '1') {
            runtime_setName(rtarg,str+2);
            fprintf(stdout,"NDB File Name is [%s]\n",
                    runtime_getName(rtarg));
          } else {
            if (str[1] == '2') {
              runtime_setName2(rtarg,str+2);
              fprintf(stdout,"NDB File Name 2 is [%s]\n",
                      runtime_getName2(rtarg));
            } else { 
              if (str[1] == '3') {
                runtime_setNameOutputFile(rtarg,str+2);
                fprintf(stdout,"Secondary Output File Name is [%s]\n",
                        runtime_getNameOutputFile(rtarg));
              } else {
                fprintf(stderr,"D[0|1|2|3]name, where 0 is the name of the test directory, 1 is file name of RNDB, 2 is file name of RNDB2, and 3 is file name of output file RNDB3 or partial-ndb.\n");
                exit(err18);
              }
            }
          }
        }
      }
      break;
        
    case 'E':
      num = atoi(str+1);
      if(runtime_getN(rtarg) != num){
        runtime_setN(rtarg,num);
        fprintf(stdout,"Extra bits is %d\n",num);
      }
      break;

    case 'F':
      if(str[1] == '1') {
        runtime_setFormat(rtarg,str+2);
        fprintf(stdout,"NDB File Format is [%s]\n",
                runtime_convert2Format(runtime_getFormat(rtarg)));
      } else {
        if (str[1] == '2') {
          runtime_setFormat2(rtarg,str+2);
          fprintf(stdout,"NDB File Format2 is [%s]\n",
                  runtime_convert2Format(runtime_getFormat2(rtarg)));
        } else {if (str[1] == '3') {
            runtime_setFormatOutputFile(rtarg,str+2);
            fprintf(stdout,"Secondary Output File Format is [%s]\n",
                    runtime_convert2Format(runtime_getFormatOutputFile(rtarg)));
          } else {
            fprintf(stderr,"F[1|2|3][ndb|cnf], where 1 is file format of RNDB, 2 is format of RNDB2, and 3 is format of output file RNDB3 or partial-ndb.\n");
            exit(err18);
          }
        }
      }
      break;

    case 'H':
      num = atoi(str+1);
      if(runtime_getHighWaterMark(rtarg) != num){
        runtime_setHighWaterMark(rtarg,num);
        fprintf(stdout,"High Water Mark for adds is %d\n",num);
      }
      break;

    case 'I' :
      assert(recset_length(ndbarg) > 0);
      if(recset_size(ndbarg) != 0) {
        int lensav = recset_length(ndbarg);
        fprintf(stdout,"destroying previous negative database..\n");
        recset_final(ndbarg);
        recset_init(ndbarg,lensav);
      }
      fprintf(stdout,"creating a new singleton negative database..\n");
      singleton_ndb_create(runtime_getMultiInput(rtarg,runtime_getBinMode(rtarg)),ndbarg,rtarg);
      runtime_incrementSizetest(rtarg);
      print_sizetest(ndbarg,rtarg);
      break;
      
    case 'J':
      break;

    case 'K':
      num = atoi(str+1);
      if (runtime_getMinBits(rtarg) != num) {
        runtime_setMinBits(rtarg,num);
        fprintf(stdout,"Minumum Bits set to %d\n",num);
#if EXPAND_INPUT_NDB || ! MANAGED_GROWTH
        fprintf(stdout,"expanding record size to %d minimum bits..\n",num);
        PrintBR(rtarg);
        recset_expandrecsize(ndbarg,rtarg);
#endif
        /*        runtime_setMinBitsSpecifiedFlag(rtarg);    v.69.2, comment out v.72.6 */
      }
      break;

    case 'L':
      num = atoi(str+1);
      if(recset_length(ndbarg) != num) {
        recset_setlength(ndbarg,num);
        fprintf(stdout,"New Record Length is exactly %d\n",recset_length(ndbarg));
      }
      break;

    case 'M' :
      num = atoi(str+1);
      runtime_setCleanTests(rtarg,num);   
      runtime_setTau(rtarg,1);
      if(runtime_getCleanTests(rtarg) > 0) {
        test_clean(ndbarg,rtarg);
      } else {
        CleanResults cleanResults;
        fprintf(stderr,"single morph\n");
        cleanup(ndbarg,rtarg,NULL,0,&cleanResults);
      }
      runtime_incrementSizetest(rtarg);
      print_sizetest(ndbarg,rtarg);
      break;

    case 'N' :
      num = atoi(str+1);             /* note: no num is also zero */
      if(recset_size(ndbarg) != 0) {
        int lensav = recset_length(ndbarg);
        fprintf(stdout,"destroying previous negative database..\n");
        recset_final(ndbarg);
        recset_init(ndbarg,lensav);
      }
      fprintf(stdout,"creating a new empty negative database..mode %d\n", num);
      if(num==3){
        empty_ndb_create_randomSATformula(ndbarg,rtarg); /* record length >= 100 */
      } else {
        if(num==1){
          empty_ndb_create_onebit(ndbarg,rtarg);  /* used prior to march 2006 */
        } else {
          if(num==2){                             /* powerset- blank- v.69.2 */
            full_ndb_create_basic(ndbarg,recset_length(ndbarg));
          } else {
            if(num==4){                           /* all *'s - v.69.2 */
              empty_ndb_create_basic(ndbarg,recset_length(ndbarg));
            } else {                              /* default per FE spec */
              empty_ndb_create(ndbarg,rtarg);
            }
          }
        }
      }
      runtime_incrementSizetest(rtarg);
      print_sizetest(ndbarg,rtarg);
      break;
      
    case 'O':
      if(recset_size(ndbarg) != 0) {
        recset_final(ndbarg);
        recset_init(ndbarg,0);
      }
      fprintf(stdout,"OPENING negative database %s..\n",runtime_getName(rtarg));
      recset_build(ndbarg,rtarg,runtime_getName(rtarg),runtime_getFormat(rtarg));
      if(runtime_getBinMode(rtarg))
        fprintf(stdout,"record length is %d (binary mode).\n",recset_length(ndbarg));
      else
        fprintf(stdout,"record length is %d letters.\n",recset_length(ndbarg)/8);

      runtime_incrementSizetest(rtarg);
      print_sizetest(ndbarg,rtarg);
      break;

    case 'P':
      num = atoi(str+1);
      if(runtime_getPercent(rtarg) != num){
        runtime_setPercent(rtarg,num);
        fprintf(stdout,"Percent threshold for minimum record size will be %d%% after the next 'W'rite and 'O'pen commands.\n",num);
      }
      break;

    case 'Q':
      assert( dbsize >= 0);  
      assert(recset_length(ndbarg) > 0);
      runtime_cleanInput(rtarg,str+1);
      rtn = online_query(runtime_getInput(rtarg),ndbarg,rtarg);
      break;

    case 'R':
      assert( dbsize >= 0);  
      assert(recset_length(ndbarg) > 0);
      runtime_cleanInput(rtarg,str+1);
      online_remove(runtime_getInput(rtarg),ndbarg,rtarg);
      runtime_incrementSizetest(rtarg);
      print_sizetest(ndbarg,rtarg);
      break;

    case 'S':
      num = atoi(str+1);
      runtime_seedRandom(rtarg,num);
      break;

    case 'T':
      runtime_setNegTheta(rtarg,str+1);
      fprintf(stdout,"Theta is %s (%d)\n",
              runtime_convert2Theta(runtime_getNegTheta(rtarg)),
              runtime_getNegTheta(rtarg));
      break;

    case 'U':
      recset_save_and_checkbitfreq(ndbarg,rtarg,runtime_getName(rtarg));
      break;

    case 'W':
      if(recset_ischanged(ndbarg)) {   /* size after update */
        recset_save(ndbarg,rtarg,runtime_getName(rtarg));
        fprintf(stdout,"NDB size is %d (was %d).",
                recset_size(ndbarg),
                dbsize
                );
      } else {
        fprintf(stdout,"(no change) "); /* v.57 */
        fprintf(stdout,"NDB size is still %d.",   
                dbsize
                );
      }
      PrintBR(rtarg);
      break;

    case 'X':   
      if (runtime_setRelOperator(rtarg,str+1) == op_noop) {
	fprintf(stderr,"Invalid relational operator [%c] -- use X[Join|Intersection|Union|CrossProduct|BinaryUnion|Difference].", str[1]);
        exit(err18);
      }
      break;

    case 'Y':
      if( runtime_getCommand(rtarg) != 'X' || runtime_getRelOperator(rtarg) != op_join)
        fprintf(stderr,"Relational Operator is required, use XJoin");
      else {
        if(str[1] == 'a') 
          runtime_setOrder1(rtarg,str+2);
        else {
          if(str[1] == 'b')
            runtime_setOrder1(rtarg,str+2);
          else {
            fprintf(stderr,"Y[a|b]list, where a is join cond 1, b is join cond 2, and list is comma-delimited ordered bit indexes/letters\n");
            exit(err18);
          }
        }
      }
      break;

    case 'Z':
      num = atoi(str+1);
      runtime_setSizetest(rtarg,num);
      break;

    case '#':
    case '\n':
      break;

    default:
      fprintf(stderr,"outer_loop: invalid command [%c]\n",cmd);
      exit(err18);
    };

    dbsize = recset_size(ndbarg);
  } /* end while loop */
  
  fclose(fd);

  if(recset_ischanged(ndbarg)) {  
    recset_save(ndbarg,rtarg,runtime_getName(rtarg));
    fprintf(stdout,"NDB size is %d (was %d).",
            recset_size(ndbarg),
            dbsize
            );
  } else {
    fprintf(stdout,"(no change) ");     /* v.57 */
    fprintf(stdout,"NDB size is still %d.",
            dbsize
            );
  }
  PrintBR(rtarg);      

  runtime_unlock(rtarg,runtime_getName(rtarg));

  fprintf(stdout,"elapsed time: %ld seconds",time(0) - starttime);
  PrintBR(rtarg);
  fprintf(stdout,"%d adds, %d add nots",dbadds,dbaddnots);
  PrintBR(rtarg);
  return rtn;
}


/* return results from command, where 0 is normal; 1 is normal, but
   not found; and 2 and above for errors .*/
int one_shot(Recset * ndbarg, Runtime * rtarg)
{
  int dbsize, len, starttime;
  char cmd = runtime_getCommand(rtarg);
  int rtn = 0;
  /*  fprintf(stdout,"one shot: command is %c<BR>",cmd); */
  
  if(cmd == 'N' || cmd == 'I') {
    if(cmd == 'I'){
        rtn = singleton_ndb_create(runtime_getMultiInput(rtarg,runtime_getBinMode(rtarg)),ndbarg,rtarg);
    } else {
      int mode = runtime_getNewMode(rtarg);
      /*    fprintf(stderr,"one_shot: new empty ndb mode is %d\n",mode); */
      switch (mode) {
      case (3): 
        rtn = empty_ndb_create_randomSATformula(ndbarg,rtarg);
        break;
      case (1):
        rtn = empty_ndb_create_onebit(ndbarg,rtarg);
        break;
      case (4):
        rtn = empty_ndb_create_basic(ndbarg,recset_length(ndbarg));
        break;
      case (2):
        rtn = full_ndb_create_basic(ndbarg,recset_length(ndbarg));
        break;
      default:
        /* default by spec */
        rtn = empty_ndb_create(ndbarg,rtarg);
      };
    }
  } else {
    if(1 || cmd != 'Q') {   /* always set lock in case of partial query v.22 */
      if (runtime_read_with_intention_to_write_later(rtarg,runtime_getName(rtarg))) {
	rtn = recset_build(ndbarg,rtarg,runtime_getName(rtarg),runtime_getFormat(rtarg));
      } else { 
	fprintf(stdout,"Database in use, try again later or startover");
	PrintBR(rtarg);
	return err2;
      }
    } else {
      rtn = recset_build(ndbarg,rtarg,runtime_getName(rtarg),runtime_getFormat(rtarg));
    }
  }

  dbsize = recset_size(ndbarg);
  len = recset_length(ndbarg);

  if (rtn == 0 ) {
    if ( dbsize < 0) rtn = err6;          /* v.39 why not allow zero? */
    else
      if (len <= 0) rtn = err7;           
  }

  if (rtn == 0) {
    starttime = time(0);

    switch (cmd) {
    case 'Q' :
      rtn = verify_input_length(rtarg,len);
      if (rtn == 0)
        rtn = online_query(runtime_getInput(rtarg),ndbarg,rtarg);
      break;
      
    case 'A' :
      rtn = verify_input_length(rtarg,len);
      if (rtn == 0)
        rtn = online_add(runtime_getInput(rtarg),ndbarg,rtarg);
      break;
      
    case 'R' :
      rtn = verify_input_length(rtarg,len);
      if (rtn == 0)
        rtn = online_remove(runtime_getInput(rtarg),ndbarg,rtarg);
      break;
      
    case 'C' :
      if(runtime_getCleanTests(rtarg) > 0) {
        test_clean(ndbarg,rtarg);
      } else {
        CleanResults cleanResults;
        cleanup(ndbarg,rtarg,NULL,0,&cleanResults);
      }
      break;
    
    case 'N':
    case 'I':
      fprintf(stdout,"<B>Record Length %d bits, Minimum size %d, Extra bits %d</B>",
              recset_length(ndbarg),
              runtime_getMinBits(rtarg),
              runtime_getN(rtarg)
              );
      PrintBR(rtarg);
      break;

    case 'P' :
      rtn = verify_input_length(rtarg,len);
      if (rtn == 0)
        rtn = online_copyproject(runtime_getInput(rtarg),ndbarg,rtarg);
      break;
      
    case 'X':
      rtn = online_relationaloperation(ndbarg,rtarg);
      break;
      
    case 'M':
      fprintf(stdout,"MAX RECORD SIZE IS %d\n",recset_max_recordsize(ndbarg));
      fprintf(stdout,"MIN RECORD SIZE IS %d\n",recset_min_recordsize(ndbarg));
      break;

    case 'E':
      rtn = recset_notemptyp(ndbarg,rtarg);
      if (rtn == errneg2) {
        fprintf(stdout,"WARNING!! NDB needs a cleanup/morph before proceeding\n");
      } else {
        if (rtn == 0) 
          fprintf(stdout,"\nDB IS EMPTY (no solutions exist)\n");
        else
          fprintf(stdout,"\nDB IS NOT EMPTY (solutions exist)\n");
        rtn = 0;
      }
      break;

    case 'Z':
      rtn = online_compare(ndbarg,rtarg);
      break;

    case 'U':
      rtn = online_complement(ndbarg,rtarg);
      break;
      
    default:
      fprintf(stderr,"one_shot: invalid command [%c]\n",cmd);
      rtn = err3;
    };                          /* end of switch */

    fprintf(stdout,"elapsed time: %ld seconds",time(0) - starttime);
    PrintBR(rtarg);    
  }                             /* end if rtn==0 */   


  PrintBR(rtarg);
  if(recset_ischanged(ndbarg)) {   /* size after update */
    recset_save(ndbarg,rtarg,runtime_getName(rtarg));
    fprintf(stdout,"NDB size is %d (was %d).",
            recset_size(ndbarg),
            dbsize
            );
  } else {
    fprintf(stdout,"(no change) ");  /* v.57 */
    fprintf(stdout,"NDB size is still %d.",
            dbsize
            );
  }
  PrintBR(rtarg);

  if(rtn < 2) {                   /* normal output size results to sizes.dat */  
    if(cmd != 'Q' && cmd != 'X' && cmd != 'M' && cmd != 'Z' && cmd != 'U') 
      print_sizetest(ndbarg,rtarg);
    
#if USE_RECSUBSUMED
    recsubsumed_stats(recset_numunits(ndbarg));
#endif
  }

  if(runtime_getLock(rtarg))
    runtime_unlock(rtarg,runtime_getName(rtarg));

  return rtn;
}


/* remove positive string from database; verify success and return 0
   for normal completion, 1 for failure, and error otherwise */
int online_remove(char * strarg, Recset * ndbarg, Runtime * rtarg) 
{
  int count;
  int numunits, reclen;
  Rec * rec;
  int rtn = 1;
  Negpattgen npg = with_npg;

  if(strarg==NULL) return err4;

  reclen = recset_length(ndbarg);
  numunits = recset_numunits(ndbarg);
  rec = rec_create(numunits);
  rec_string2rec(rec,reclen,strarg,runtime_getBinMode(rtarg));
  
#if NPG_NONE_NEGREMOVE
        npg = wo_npg;     /* v.65 */
#endif

  if (rec_size(rec,numunits) == reclen) {          /* fully specified input */

    if(recset_query(ndbarg,rec)) {
      fprintf(stdout,"oops, record [%s] found in ndb (not in DB)---cannot be \"removed\"",strarg);
      PrintBR(rtarg);

    } else {
  
      count = insert(rec,ndbarg,rtarg,npg,runtime_getN(rtarg));  /* essentially appends */
      
      if(!online_query(strarg,ndbarg,rtarg)){
        fprintf(stdout,"<B>ERROR! Unsuccessful Remove [%s]---created %d negative records</B>",
                strarg,count);
        PrintBR(rtarg);
      
      } else {
      
        PrintBR(rtarg);
        fprintf(stdout,"<B>SUCCESSFULLY Removed [%s]---created %d negative records</B>",
              strarg,count);
        PrintBR(rtarg);
        rtn = 0;
      }
    }
  } else {                                         /* remove a pattern v.40 */
    /* changed from recset_query_match in v.41 */
    /* removed recset_query_subsumed check since it gave false results v.52 */
    
#if USE_APPEND
    count = append(rec,ndbarg,rtarg,npg);
#else
    count = insert(rec,ndbarg,rtarg,npg,runtime_getN(rtarg));
#endif

    rtn = 0;
  }

  if(rtn == 0){                                        /* v.58, v.59, v.60.1 */
    recset_setupdateflag(ndbarg);
    compress(ndbarg,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0)); 
    cleanup_plus_option(ndbarg,rtarg);                 /* v.57 */
  }

  rec_destroy(rec);
  return rtn;
}


/* online_query: for membership queries (a fully specified string
argument is required), and returns false if string is in the database
(not in NDB); and true if string is not in the database (in
NDB). Partial queries (dontcares in the string argument and or theta
specified), returns false and the negative records to the filename and
format specified by commandline args -o and -O, respectively, where
default is PARTIALNDBNAME in ndb (text) format */
int online_query(char * strarg, Recset * ndbarg, Runtime * rtarg)
{
  int numunits, reclen;
  Rec * rec;
  int recsz, ntheta;

  if(strarg==NULL) exit(err4);
  
  reclen = recset_length(ndbarg);
  numunits = recset_numunits(ndbarg);
  rec = rec_create(numunits);

  recsz = rec_string2rec(rec,reclen,strarg,runtime_getBinMode(rtarg));
#if 0
  fprintf(stderr,"online_query: input is <%s>, recsz is <%d>, and the rec is ",
          strarg,recsz); 
  rec_print(rec,reclen,stderr);
#endif
  assert(recsz >= 0);
  
  /* fully specified selects and partial queries (v.22,v.26) here */
  if ((ntheta = runtime_getNegTheta(rtarg)) != O_bool || recsz < recset_length(ndbarg)) {
    if(ntheta == O_bool) {
      runtime_setNegTheta(rtarg,"eq"); /* set default select */
      /* moved test for recsize minimum 2 into recset_partial_query v.53 */ 
    }
    recset_select(rec,ndbarg,rtarg);
    rec_destroy(rec);
    return 0;                 /* there's always something even if empty */
  }
  
  /* 
   *  Original Membership Query 
   */
  {
    int rtn = 1;
    if(!recset_query(ndbarg,rec)) {
      PrintBR(rtarg);
      fprintf(stdout,"<B>[%s] is in the database (not in NDB)</B>",strarg);
      PrintBR(rtarg);
      rtn = 0;
    } else {  
      PrintBR(rtarg);
      fprintf(stdout,"<B>[%s] is not in the database (in NDB)</B>",strarg);
      PrintBR(rtarg);
    }
    rec_destroy(rec);
    return rtn;
  }
  
  return 1;     /* unreachable */
}


/* returns the records in ndbarg that match strarg after coalescing
   them with strarg; used primarily to query a complemented ndb in the
   positive, but also tries to compress any ndb, without changing its
   semantics, when strarg is all *'s and the -k command is specified
   (v.66.1); NOTE: this is functionally the same as Neg Union, except
   the input is a string and a recset instead of two recsets, and it
   has the possibility of the Project option (v.71) */
int online_copyproject(char * strarg, Recset * ndbarg, Runtime * rtarg)
{
  int reclen, recsz;
  Rec * rec;
  Recset partialset;
  
  if(strarg==NULL) exit(err4);
  
  reclen = recset_length(ndbarg);
  rec = rec_create(recset_numunits(ndbarg));

  recsz = rec_string2rec(rec,reclen,strarg,runtime_getBinMode(rtarg));
#if 0
  fprintf(stderr,"online_copyproject: input is <%s>, recsz is <%d>, and the rec is ",
          strarg,recsz); 
  rec_print(rec,reclen,stderr);
#endif
  assert(recsz >= 0);
  
  recset_init(&partialset,reclen);
  recset_partial_copy(rec,ndbarg,&partialset,rtarg); /* v.66.1 */

#if 0
  fprintf(stderr,"online_copyproject: partialset after copy:\n ");
  recset_print(&partialset,stderr);
#endif

  if(runtime_getProjOption(rtarg)){  /* only works for O_ne (v.31) */
    /* insure the ndb has at least two specified bits per record */
    if(recset_min_recordsize(&partialset) < SAT2) {
      if(runtime_getMinBits(rtarg) < SAT2) runtime_setMinBits(rtarg,SAT2);
      recset_expandrecsize(&partialset,rtarg);
    }
    runtime_setNegTheta(rtarg,"eq");
    recset_save_secondary_projected(rec,&partialset,rtarg); 
  } else {
    recset_save_secondary(&partialset,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0));
  }

  rec_destroy(rec);  
  recset_final(&partialset);
  return 0;
}


/* takes as input two read-only negative databases, and outputs a
   third depending upon the relational operator in -x ;
   the join operation also requires two join conditions -y -Y (v.27) */
int online_relationaloperation(Recset * ndbarg, Runtime * rtarg)
{
  Recset * NDB2;
  Recset * NDB3;
  RelOp opcode;
  int rtn;

  if(runtime_read_with_intention_to_write_later(rtarg,runtime_getName2(rtarg))) {
    NDB2 = recset_create();
    rtn = recset_build(NDB2,rtarg,runtime_getName2(rtarg),runtime_getFormat2(rtarg));
  } else { 
    fprintf(stdout,"Database 2 in use, try again later or startover");
    PrintBR(rtarg);
    return err2;
  }

  if(rtn != 0){
    recset_destroy(NDB2);
    return rtn;
  }

  NDB3 = recset_create();
  
  opcode = runtime_getRelOperator(rtarg);

  switch (opcode) {
  case (op_join):
    rtn = recset_join(ndbarg,NDB2,NDB3,rtarg);
    break;
  case (op_intersect):
    rtn = recset_intersection(ndbarg,NDB2,NDB3,rtarg);
    break;
  case (op_cross):
    rtn = recset_crossproduct(ndbarg,NDB2,NDB3,rtarg);
    break;
  case (op_union):
    rtn = recset_union(ndbarg,NDB2,NDB3,rtarg);
    break;
  case (op_binunion):
    rtn = recset_binaryunion(ndbarg,NDB2,NDB3,rtarg);
    break;
  case (op_starnegbu):
    rtn = recset_negbinaryunionstar(ndbarg,NDB2,NDB3,rtarg);
    break;
  case (op_difference):
    rtn = recset_difference(ndbarg,NDB2,NDB3,rtarg);
    break;
  case (op_relevance):
    rtn = recset_relevance(ndbarg,NDB2,NDB3,rtarg);
    break;
  case (op_onlyinfirstdb):
    rtn = recset_setdiff(ndbarg,NDB2,NDB3,rtarg);
    break;
  default:
    fprintf(stderr,"one_shot: no relational operator given -x\n");
    rtn = err3;
    break;
  };

  if(rtn == 0 || rtn == 1){       /* save zero records too v.51 */
    recset_save_secondary(NDB3,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0));
    print_sizetest(NDB3,rtarg);                       /* v.54 */
  }

  recset_destroy(NDB2);
  recset_destroy(NDB3);
  runtime_unlock(rtarg,runtime_getName2(rtarg));
  return rtn;
}


/* takes as input two read-only negative databases, mode 0:
   complements them and compares for each for matching records in the
   other, mode 1 just compresses then compares (v.65); mode 2 looks
   for identical sets (v.72.7); outputs 0 if the same; o.w. 1 when
   different (use echo $? to get exit status) (v.56,v.61.3,v.64) */
int online_compare(Recset * ndbarg, Runtime * rtarg)
{
  Recset * NDB2;
  int rtn;
  int len1, len2;

  if(runtime_read_with_intention_to_write_later(rtarg,runtime_getName2(rtarg))) {
    NDB2 = recset_create();
    rtn = recset_build(NDB2,rtarg,runtime_getName2(rtarg),runtime_getFormat2(rtarg));
  } else { 
    fprintf(stdout,"Database 2 in use, try again later or startover");
    PrintBR(rtarg);
    return err2;
  }

  if(rtn != 0){
    fprintf(stderr,"online_compare: problem with building NDB2 (%d), aborting..\n",rtn);
    recset_destroy(NDB2);
    return 1;
  }

  len1 = recset_length(ndbarg);
  len2 = recset_length(NDB2);
  if(len1 != len2) {
    fprintf(stderr,"online_compare: records lengths different (%d, %d), aborting..\n",len1,len2);
    recset_destroy(NDB2);
    return 1;
  }

  rtn = 1;

  /* mode 0 (requires complementing) v.65 */
  
  /* mode 1: compare directly already complemented ndbs v.65 */
  /* before comparing complements, make sure they have the same minbits 
     accomplished either with compress or expandrecordsize v.69 */
  /* note: without managed growth compiler option, use with -m len */
  
  /* mode 2: compare directly already complemented ndbs for equality
     (best used with fully-specified sets) v.72.7 */
  
  rtn = recset_equivalence(ndbarg,NDB2,rtarg);

  if(rtn == 0) {
    fprintf(stdout,"online_compare: SAME.");
    PrintBR(rtarg);
  } else {
    fprintf(stdout,"online_compare: DIFFERENT.");
    PrintBR(rtarg);
  }
  fprintf(stdout,"%d\n",rtn); 
  
  recset_destroy(NDB2);
  runtime_unlock(rtarg,runtime_getName2(rtarg));
  recset_clearupdateflag(ndbarg);
  return rtn;
}


/* complements (un-negates) input file and outputs as un_NDB.txt if
   not specified in -o commandline arg v.56 */
int online_complement(Recset * ndbarg, Runtime * rtarg)
{
  int sz;
  Recset * compset = recset_create();
  
  sz = recset_complement(ndbarg,compset,rtarg);
  
  fprintf(stdout,"Complement Size is %d records\n",sz);
  
  recset_save_secondary(compset,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0));
  print_sizetest(compset,rtarg);
  
  recset_destroy(compset);
  return 0;
}


static int hold_add(char * strarg, int hwarg, Recset * rsetarg, Runtime * rtarg)
{
  FILE *fd;
  int cleantests;
  char * testdir = runtime_getTestDataDirectory(rtarg);
  if(testdir == NULL) {
    fd = fopen("./DB.hold","a");
  }  else {
    char * fname = new_string_concat(testdir,"/DB.hold");
    if( fname == NULL) {
      fprintf(stderr,"filename creation error for hold add\n"); 
      exit(err12);
    }
    fd = fopen(fname,"a");
    free(fname);
  }

  if(fd==NULL){
    fprintf(stderr,"unable to open DB.hold to save [%s]\n",strarg);
    exit(err14);
  }
  fprintf(fd,"P%d\n",runtime_getPercent(rtarg));
  fprintf(fd,"E%d\n",runtime_getN(rtarg));
  fprintf(fd,"S%d\n",runtime_getSeed(rtarg));
  fprintf(fd,"L%d\n",recset_length(rsetarg));
  fprintf(fd,"K%d\n",runtime_getMinBits(rtarg));
  fprintf(fd,"H%d\n",hwarg);
  fprintf(fd,"A%s\n",strarg);

  cleantests = runtime_getCleanTests(rtarg);
  if(cleantests > 0) 
    fprintf(fd,"C%d\n",cleantests);

  fprintf(stdout,"unsuccessful add [%s], deleted cache size was %d, see DB.hold\n",
          strarg,hwarg);
  fclose(fd);
  return 1;
}


static int online_add_record(Rec * rarg, Recset * ndbarg, Runtime * rtarg);
/* add positive string to database; verify success and return 0 for
   success, 1 for normal completion but failed to add, o.w. error */
/* expand to include patterns, i.e. strings with don'tcares v.38 */
int online_add(char * strarg, Recset * ndbarg, Runtime * rtarg) 
{
  Rec * rec;
  int numunits, reclen;
  int rtn;

  if(strarg == NULL) return err4;

  reclen = recset_length(ndbarg);
  numunits = recset_numunits(ndbarg);
  rec = rec_create(numunits);
  rec_string2rec(rec,reclen,strarg,runtime_getBinMode(rtarg));

  rtn = online_add_record(rec,ndbarg,rtarg);           /* v.59.11 */

  if(rtn == 0) {                                       /* v.59 moved here */
    compress(ndbarg,rtarg,(runtime_getMinBitsSpecifiedFlag(rtarg)?runtime_getMinBits(rtarg):0));
    cleanup_plus_option(ndbarg,rtarg);                 /* v.57 */
    recset_setupdateflag(ndbarg);                      /* v.59 moved here */
  }

  rec_destroy(rec);
  return rtn;
}


static int online_add_record(Rec * rarg, Recset * ndbarg, Runtime * rtarg) 
{
    Recset cache;
    int deleted;
    int count = 0;
    int rtn = 1;
    
    recset_init(&cache,recset_length(ndbarg));
    recset_split_match(rarg,ndbarg,&cache);   /* recset_split_match 
                                           is the same as recset_split for
                                           fully specified rarg
                                           v.41 */
    deleted = recset_size(&cache);
    Print(rtarg,"deleted cache size is %d\n",deleted);
    fflush(stdout);
    
    if(!deleted) {
      
      fprintf(stdout,"oops, record [%s] NOT found in ndb (in DB)---cannot be \"added\"",
              runtime_getInput(rtarg));
      PrintBR(rtarg);

    } else {
      int HWM = runtime_getHighWaterMark(rtarg);
      
      if(HWM && deleted >= HWM){       /* save for another time v.17 */

        recset_merge(&cache,ndbarg);
        hold_add(runtime_getInput(rtarg),deleted,ndbarg,rtarg);
        
      } else {             /* proceed */
        int n = 0;
        Negpattgen npg = with_npg;

#if NPG_NONE_NEGADD
        npg = wo_npg;     /* v.63 */
#endif

        while(n < deleted) {
          count += delete(recset_getrec(&cache,n),rarg,ndbarg,rtarg,npg);
          n++;
        }
        
        if(recset_query(ndbarg,rarg)){
          fprintf(stdout,"<B>ERROR! Unsuccessful Add [%s]---created %d negative records, deleted %d</B>",
                  runtime_getInput(rtarg),count,deleted);
          PrintBR(rtarg);
          
        } else {
          
          PrintBR(rtarg);
          fprintf(stdout,"<B>SUCCESSFULLY Added [%s]---created %d negative records, deleted %d</B>",
                runtime_getInput(rtarg),count,deleted);
          PrintBR(rtarg);

          print_add_data(ndbarg,rtarg,deleted,count);
          rtn = 0;
        }
      }
    }
    recset_final(&cache);    /* clean up */
    return rtn;
}


/* returns nettotal; might exit on error (this is not a unit test) */
int test_clean(Recset * ndbarg, Runtime * rtarg)
{
  int i;
  double avg;
  FILE * fd;
  char * testdir;
  int cleantests;
  int newsize;
  CleanResults cleanResultsTotal; 
  int tau = runtime_getTau(rtarg);
  int ctnum = runtime_getCleanTests(rtarg);  /* say, 1,000 */
  int nettotal = 0;
  int ndbsize = recset_size(ndbarg);
  
  if(ndbsize <= 1)  /* no neg records, nothing to do, exit gracefully v.52 */
    return 0;       /* or only one for that matter v.57 */
  
  if(ctnum <= 0) {
    fprintf(stderr,"Number of clean tests must be greater than zero, not %d\n",
            ctnum);
    exit(err15);
  }
  
  cleanresults_clear(&cleanResultsTotal);
  
  testdir = runtime_getTestDataDirectory(rtarg);
  if(testdir == NULL) {
    fd = fopen("./clean.dat","a");
  }  else {
    char * fname = new_string_concat(testdir,"/clean.dat");
    if( fname == NULL) {
      fprintf(stderr,"filename creation error for print test clean data\n"); 
      exit(err12);
    }
    fd = fopen(fname,"a");    /* changed to append from w v.57 */
    free(fname);
  }
  
  if( fd == NULL ) {
    fprintf(stderr,"file open error on printing test clean data\n"); 
    exit(err14);
  }
  
  fprintf(fd,"# iterate added deleted net passed\n");
  
  /*  cleantests = (1 << runtime_getMinBits(rtarg));   2^r */ 
  /*  cleantests = (cleantests * ctnum) / ndbsize;         */
  cleantests = ctnum;
  fprintf(stdout,"ndb size is %d; doing %d clean tests; Tau is %d ",
          ndbsize,
	  cleantests,
	  tau
	  );
  if(!tau) 
    fprintf(stdout," (no growth)");
  PrintBR(rtarg);
  
  for(i = 0 ; i < cleantests; i++) {
    CleanResults cleanResults;
    int oldsize = recset_size(ndbarg);
    /* int net = new_cleanup(ndbarg,rtarg,fd,i,&cleanResults); */
    int net = cleanup(ndbarg,rtarg,fd,i,&cleanResults);
    newsize = recset_size(ndbarg);
    if(newsize - oldsize != net) {
      fprintf(stderr,"<BR><B>Clean %d: Net %d does not compute against new size %d - original size %d</B><BR>",
              i,
              net,
              newsize,
              oldsize
              );
      exit(err22); 
    }
    cleanresults_sum(&cleanResultsTotal,&cleanResults);
    nettotal += net;
  }
  
  avg = ((1*nettotal)/cleantests);
  fprintf(fd,"# 1 TIMES AVERAGE NET CLEANUP is %f\n",avg);
  fclose(fd);
  
  PrintBR(rtarg);
  fprintf(stdout,"<B># NET CLEANUP is %d (%d adds/%d deletes) after %d cleanup operations--the average is %f</B>",
	  nettotal,
	  cleanresults_getAdds(&cleanResultsTotal),
	  cleanresults_getDeletes(&cleanResultsTotal),
	  cleantests,
	  avg
	  );
  PrintBR(rtarg);
  
  newsize = recset_size(ndbarg);
  
  if(ndbsize + nettotal != newsize)
    fprintf(stderr,"<BR><B>Clean Net %d does not compute against new size %d - original size %d</B><BR>",
            nettotal,
            newsize,
            ndbsize
            );
  
  fprintf(stdout,"<B>#NEW SIZE IS %d</B>", newsize);
  PrintBR(rtarg);
  
  return nettotal;
}


/* Print prints one line to stdout if not webbased demo. */
void Print(Runtime * rtarg, char *fmt,...){
  va_list args; /* points to each unnamed arg in turn */
  if(runtime_getOneShot(rtarg)) 
    return;
  va_start(args,fmt);  /* make args point to 1st unnamed arg */
  vfprintf(stdout,fmt,args);
  fprintf(stdout, "\n");
  va_end(args);
}


/* Print prints one line to stdout if webbased demo. */
void Printd(Runtime * rtarg, char *fmt,...){
  va_list args; /* points to each unnamed arg in turn */
  if(!runtime_getOneShot(rtarg)) 
    return;
  va_start(args,fmt);  /* make args point to 1st unnamed arg */
  vfprintf(stdout,fmt,args);
  fprintf(stdout, "\n");
  va_end(args);
}


void PrintBR(Runtime * rtarg) 
{
  if(runtime_getOneShot(rtarg)) 
    fprintf(stdout,"<BR>");
  else
    fprintf(stdout,"\n");
}
