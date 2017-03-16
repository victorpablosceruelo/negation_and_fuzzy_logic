/* This file is part of the Online Negative Database (NDB), Copyright
   (C) 2008 ackleyshack,LLC and Copyright (C) 2004-2008 elena s ackley
   and the Regents of the University of New Mexico.
 
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
 * Filename      : command_args.c
 * Language      : C using the GNU gcc compiler 
 * 
 * Purpose       : Common functions to process command line arguments
 * 
 * Creator       : Elena Settanni Ackley
 * Creation Date : Nov  8 1999 
 * Updated       : Fri May  9 09:25:25 2008 
 *
 * -------------------------------------------------------------------------
 */
#include <stdarg.h>
#include <stdlib.h>
#include "command_args.h"
#include "runtime.h"
#include "error.h"

typedef struct _cargo{
  char letter;
  short numargs;
  char * args;
  char * helpmessage;
}cargo;

static cargo C_ARGS[] = {
  { 'a', 2, "string",": add positive record"},
  { 'b', 1, " ",": binary mode, default is ascii letters"},
  { 'c', 2, "num",": clean up where 0 insures no growth, 1 to morph (use with -t)"},
  { 'C', 2, "string ",": copy the records in input file that are matched and coalesced with the input string---a query for complements; the resulting output file is named by -o in -O format, default is partial-ndb.txt (see also -P and -k option) "},
  { 'd', 1, " ",": demo on web"},
  { 'D', 2, "name",": test data directory name, defaults to current working dir"},
  { 'e', 1, " ",": returns 0 if an \"easy\" ndb (where -M is at most 2) is \"empty\""},
  { 'E', 2, "num",": num specifies method to use to determine if an \"easy\" ndb is \"empty\" (see -e), where: 0 is all_random, 1 is all_popular, 2 alternates popular and random, 3 uses popular on success and random after backtracking, default is mode 0"},
  { 'f', 2, "name",": ndb filename, default ndb formats is RNDB.txt, default cnf is RNDB.cnf"},
  { 'F', 2, "format",": ndb file format <ndb|cnf>, default is ndb"},
  { 'g', 2, "num",": extra bits to insert, default is 0"},
  { 'h', 2, "num",": high water mark for adds, default is unlimited"},
  { 'i', 2, "string",": new negative database with deceptively q-hidden solutions (use separate -i for each string); requires record bit length (-l)"},
  { 'j', 2, "name",": second read-only ndb filename, default ndb formats is RNDB2.txt, default cnf is RNDB2.cnf"},
  { 'J', 2, "format",": second ndb file format <ndb|cnf>, default is ndb"},
  { 'k', 2, "num",": minimum specified bits in records, as in k-SAT; attempts to compress result(see also -m)"},
  { 'l', 2, "num",": record bit length (1 letter == 8 bits)"},
  { 'L', 1, " ",": returns maximum record bit length"},
  { 'm', 2, "num",": minimum specified bits in records (recsize); 0 uses smallest recsize in ndb (see also -k)"},
  { 'M', 1, " ",": returns maximum and minimum specified bits in records of current ndb (recsize)"},
  { 'n', 1, " ",": new empty negative database with absolutely no solutions per spec; requires record bit length (-l)"}, 
  { 'N', 2, "mode",": new empty negative database, 0 for absolutely no solutions per spec, 1 is one-bit simplification, 2 is for powerset solutions, 3 is random SAT formula with epsilon probability of emptiness when used with 96-bit minimum record length, 4 is basic all *'s; requires record bit length (-l)"},
  { 'o', 2, "name",": partial query and optional secondary output filename, default partial ndb is partial-ndb.txt, default partial cnf is partial-ndb.cnf"},
  { 'O', 2, "format",": partial/secondary output file format <ndb|cnf>, default is ndb"},
  { 'p', 2, "num",": set recsize to k when the number of records with <= k specified bits is < p% of NDB (and -m unspecified/zero), default is the smallest record size"},
  { 'P', 1, " ",": the Project option eliminates from the output the specified positions found in the partial query -q input string when -Q theta is eq, or -C "},
  { 'q', 2, "string",": query for a string (no -Q and no *'s in the string produces a yes/no membership answer; see -P option for strings with *'s) "},
  { 'Q', 2, "theta",": theta select records for -q query constant, where theta is positive <bool,eq,ne,lt,le,gt,ge>; bool is the default for fully specified -q membership queries; for partially specified -q queries, the default is eq (use the project option -P to eliminate the non-* positions; the resulting ndb is output to file named by -o in -O format"},
  { 'r', 2, "string",": remove positive record"},
  { 's', 2, "seed",": seed"},
  { 't', 2, "num",": do cleanup tests this number of times; use with -c"},
  { 'T', 2, "num",": do post-cleanup tests as % (no decimal points) of ndb size times, minimum 100, maximum 3000 (use with other commands, -a, -r, -x, -u)"},
  { 'u', 1, " ",": un-negate ndb (-f file), to -o file, default is un_NDB.txt"},
  { 'v', 1, " ",": version"},
  { 'w', 2, "name",": while 1, execute commands in this file"},
  { 'x', 2, "op",": relational operation on 2 NDBs (-f and -j) [I<ntersection>|U<nion>|J<oin>|C<rossProduct>|B<inaryUnionCDB>][S<tarNegBinaryUnion>][D<ifference>|R<elevanceDB>][O<nlyInFirstDB>]"},
  { 'y', 2, "list",": join condition 1, comma delimited bit indexes/letters, start at 0, dash for inclusive range"},
  { 'Y', 2, "list",": join condition 2, comma delimited bit indexes/letters, start at 0, dash for inclusive range"},
  { 'z', 2, "num",": size test iteration"},
  { 'Z', 2, "mode ",": 3 expands and compares two postive or negative sets  (-f and -j) for equivalence; 2 compares two complemented NDBs for identity match; returns 0 if SAME"},
  { '?', 1, " ", "Help is here"}
};


#define C_ARGScount ((sizeof(C_ARGS)/sizeof(C_ARGS[0])))  


static void command_abort(char * fmtarg,...)
{
  va_list args; /* points to each unnamed arg in turn */
  va_start(args,fmtarg);  /* make args point to 1st unnamed arg */
  vfprintf(stderr,fmtarg,args);
  fprintf(stderr, "\n");
  va_end(args);
  exit(err3);
}

static void short_arglist(void)
{
  int i;
  fprintf(stdout,"Usage: negdb ");
  for (i = 0 ; i < C_ARGScount; i++)
    fprintf(stdout,"[-%c %s]",
	 C_ARGS[i].letter,
	 C_ARGS[i].args
	 );
  fprintf(stdout,"\n");
}


static void long_help(int a)
{
  if(a >= C_ARGScount || a < 0) {
    command_abort("help for argument %d is out of bounds",a);
  }
  fprintf(stdout,"\t-%c %s %s\n",C_ARGS[a].letter, C_ARGS[a].args, C_ARGS[a].helpmessage);
}


static void long_arglist(void)
{
  int i;
  for(i = 0; i < C_ARGScount; i++)
    long_help(i);
}


static caction find_carg(char * a)
{
  int i = 0;
  if(a[0] != '-') return thishelp;
  while(i < lastact && a[1] != C_ARGS[i].letter) i++;
  return (i < lastact ? i : -1); 
}


static int number_arguments(caction a)
{
  if(a >= lastact || a < 0) {
    command_abort("number_arguments: Argument %d is out of bounds",a);
  }
  return C_ARGS[a].numargs;
}


void usage_reminder_abort(void)
{
  short_arglist();
  long_arglist();
  command_abort("Try again please!");
}


int command_arguments(int argc, char ** argv, Runtime * rtarg)
{
  int i = 1;
  char * order1 = NULL;
  char * order2 = NULL;
  int LEN = 0;

  /*  fprintf(stdout,"HELLO COMMANDS %d<BR>",argc); */
  if(argc <= 1) usage_reminder_abort();

  while(i < argc){
    int numa, secondarg = -1;
    caction action = find_carg(argv[i]);
    
    if(action < 0) usage_reminder_abort();
    
    if((numa = number_arguments(action)) == 2){
      if(!argv[i+1]){
	long_help(action);
	command_abort("Invalid argument [%s].",argv[i+1]);
      }
      secondarg = atoi(argv[i+1]);      
    }

    switch (action) {
    case addrec:
      if( !(runtime_setup_oneshot(rtarg,'A',argv[i+1]))){
	long_help(addrec);
	command_abort("Invalid term, or argument [%s].",argv[i+1]);
      }
      i += numa; break;
      
    case binmode:
      runtime_setBinMode(rtarg);
      Print(rtarg,"Expecting Binary input..\n");
      i += numa; break;

    case cleanuprec:
      if( secondarg < 0 || ( !(runtime_setup_oneshot(rtarg,'C',NULL)) && runtime_getCommand(rtarg) != 'C' ) ){
	long_help(action);
	command_abort("Invalid argument [%s].",argv[i+1]);
      }
      runtime_setTau(rtarg,secondarg);
      i += numa; break;
      
    case copyproj:
      if( !(runtime_setup_oneshot(rtarg,'P',argv[i+1]))){
	long_help(action);
	command_abort("Invalid term, or argument [%s].",argv[i+1]);
      }
      i += numa; break;

    case oneshot:
      runtime_setOneShot(rtarg);
      /*fprintf(stdout,"WEBBASED DEMO\n");*/
      i += numa; break;
     
    case testdir:
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setTestDataDirectory(rtarg,argv[i+1]);
      Print(rtarg,"Test Data Directory is [%s]\n",
            runtime_getTestDataDirectory(rtarg));
      i += numa; break;

    case emptyp:
      if(!(runtime_setup_oneshot(rtarg,'E',NULL))) {
	long_help(action);
	command_abort("Invalid term %s.",argv[i]);
      }
      i += numa; break;

    case emptypmodes:
      if( secondarg < 0 || secondarg > 3 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      if(!runtime_setup_oneshot(rtarg,'E',NULL)) {
	long_help(action);
	command_abort("Invalid term %s.",argv[i]);
      }
      runtime_setReduceMode(rtarg,secondarg);
      Print(rtarg,"Using method %d for reduction\n",secondarg);
      i += numa; break;

    case filename: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setName(rtarg,argv[i+1]);
      Print(rtarg,"Negative database from file [%s]\n",
            runtime_getName(rtarg));
      i += numa; break;

    case fileformat: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setFormat(rtarg,argv[i+1]);
      Print(rtarg,"Negative database file format [%s]\n",
            runtime_convert2Format(runtime_getFormat(rtarg)));
      i += numa; break;

    case extrabits: 
      if(secondarg < 0 || secondarg > 8){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setN(rtarg,secondarg);
      Print(rtarg,"Generating %d extra bits to insert (formerly STEP6)\n",secondarg);
      i += numa; break;

    case highwater: 
      if(secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setHighWaterMark(rtarg,secondarg);
      if(secondarg > 0) 
        Print(rtarg,"Allowing deleted cache size of %d for online adds\n",secondarg);
      i += numa; break;
      
    case singleton:
      if(!runtime_setup_oneshot(rtarg,'I',argv[i+1])) {
        if(runtime_getCommand(rtarg) != 'I') {
          long_help(action);
          command_abort("Invalid term, or argument [%s].",argv[i+1]);
        } else {
          runtime_setInput(rtarg,argv[i+1]);
        }
      }
      if(runtime_setup_multiinput(rtarg) != 0) {  /* input freed on normal return */
        command_abort("Invalid input, or argument [%s].",argv[i+1]);
      }
      Print(rtarg,"Making a NEW Singleton NDB..\n");
      i += numa; break;

    case filename2: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setName2(rtarg,argv[i+1]);
      Print(rtarg,"Second negative database from file [%s]\n",
            runtime_getName2(rtarg));
      i += numa; break;

    case fileformat2: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setFormat2(rtarg,argv[i+1]);
      Print(rtarg,"Second negative database file format [%s]\n",
            runtime_convert2Format(runtime_getFormat2(rtarg)));

    case minbitscompress:
      if( secondarg < 0 || (secondarg > 100) ){  /* anything more than 12 is too big */
	long_help(action);
	command_abort("Invalid argument [%s].",argv[i+1]);	  
      } else {
        int mbits = runtime_getMinBits(rtarg);
        if(mbits > 0 && mbits != secondarg) {
          long_help(action);
          command_abort("-m already set to [%d]. Invalid argument [%s].",mbits,argv[i+1]); 
        }
      }
      runtime_setMinBits(rtarg,secondarg);
      runtime_setMinBitsSpecifiedFlag(rtarg);    /* v.57.5 */
      i += numa; break;


    case reclen:
      if( secondarg < 0 || secondarg > MAXBITS ){
	long_help(action);
	command_abort("Invalid argument [%s]. MAXIMUM BIT LENGTH IS %d",argv[i+1],MAXBITS);	  
      }
      LEN = secondarg;
      i += numa; break;

    case maxreclen:
      fprintf(stdout,"%d",MAXBITS); /* v.27 ??? */
      command_abort("");           /* important for php that it doesn't go to stderr */
      i += numa; break;

    case minbits:
      if( secondarg < 0 || (secondarg > 100) ){  /* anything more than 12 is too big */
	long_help(action);
	command_abort("Invalid argument [%s].",argv[i+1]);	  
      } else {
        int mbits = runtime_getMinBits(rtarg);
        if(mbits>0 && mbits != secondarg) {
          long_help(action);
          command_abort("-k already set to [%d]. Invalid argument [%s].",mbits,argv[i+1]); 
        }
      }
      runtime_setMinBits(rtarg,secondarg);
      i += numa; break;

    case maxminrecsz:
      if(!(runtime_setup_oneshot(rtarg,'M',NULL))) {
	long_help(action);
	command_abort("Invalid term %s.",argv[i]);
      }
      i += numa; break;

    case newndb:
      if(!runtime_setup_oneshot(rtarg,'N',NULL)) {
	long_help(action);
	command_abort("Invalid term %s.",argv[i]);
      }        
      Print(rtarg,"Making a NEW Empty NDB..\n");
      i += numa; break;

    case newndbmodes:
      if( secondarg < 0 || secondarg > 4 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      if(!runtime_setup_oneshot(rtarg,'N',NULL)) {
	long_help(action);
	command_abort("Invalid term %s.",argv[i]);
      }
      runtime_setNewMode(rtarg,secondarg);
      Print(rtarg,"Making a NEW Empty NDB..mode %d\n",secondarg);
      i += numa; break;

    case outfilename: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setNameOutputFile(rtarg,argv[i+1]);
      Print(rtarg,"Set to save negative database results to secondary file [%s]\n",
            runtime_getNameOutputFile(rtarg));
      i += numa; break;

    case outfileformat: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setFormatOutputFile(rtarg,argv[i+1]);
      Print(rtarg,"Set to save negative database results in [%s] file format\n",
            runtime_convert2Format(runtime_getFormatOutputFile(rtarg)));
      i += numa; break;

    case percent:
      if( secondarg < 0 || (secondarg > 100) ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);	  
      }
      runtime_setPercent(rtarg,secondarg);
      i += numa; break;

    case projopt:
      runtime_setProjOption(rtarg);
      Print(rtarg,"Projecting output..\n");
      i += numa; break;

    case queryrec:
      if( !(runtime_setup_oneshot(rtarg,'Q',argv[i+1]))){
	long_help(action);
	command_abort("Invalid term, or argument [%s].",argv[i+1]);
      }
      i += numa; break;

    case thetaquery:
      if( secondarg < 0 || runtime_setNegTheta(rtarg,argv[i+1]) < 0){
	long_help(action);
	command_abort("Invalid Theta argument [%s].", argv[i+1]);
      }
      Print(rtarg,"Querying for %s [%d]\n",
            runtime_convert2Theta(runtime_getNegTheta(rtarg)),runtime_getNegTheta(rtarg));
      i += numa; break;
      
      if( !(runtime_setup_oneshot(rtarg,'Q',argv[i+1]))){
	long_help(action);
	command_abort("Invalid term, or argument [%s].",argv[i+1]);
      }
      i += numa; break;

    case rmrec:
      if( !(runtime_setup_oneshot(rtarg,'R',argv[i+1]))){
	long_help(action);
	command_abort("Invalid term, or argument [%s].",argv[i+1]);
      }
      i += numa; break;

    case seed:
      if( secondarg == -1 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);	  
      }
      runtime_seedRandom(rtarg,secondarg);
      fprintf(stdout,"Seed reset to %d",runtime_getSeed(rtarg));
      PrintBR(rtarg);
      i += numa; break;
      
    case cleantests:
      if( secondarg <= 0 || ( !(runtime_setup_oneshot(rtarg,'C',NULL)) && runtime_getCommand(rtarg) != 'C' ) ){
	long_help(action);
	command_abort("Invalid argument [%s].",argv[i+1]);
      }
      runtime_setCleanTests(rtarg,secondarg);
      i += numa; break;

    case cleantestsplus:
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].",argv[i+1]);
      }
      runtime_setCleanTests(rtarg,secondarg);
      i += numa; break;

    case unnegate:
      if( !(runtime_setup_oneshot(rtarg,'U',NULL))){
	long_help(action);
	command_abort("Invalid term [%s].",argv[i]);
      }
      i += numa; break;
      
    case version:
      fprintf(stdout,"version %s\n",runtime_version());
      command_abort("");  /* important for php that it doesn't go to stderr */
      i += numa; break;

    case whileone: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      runtime_setHoldName(rtarg,argv[i+1]);
      Print(rtarg,"Executing commands in file [%s]\n",
            runtime_getHoldName(rtarg));
      i += numa; break;

    case relop:
      if (runtime_setRelOperator(rtarg,argv[i+1]) == op_noop) {
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      i += numa; break;
      
    case joincond1: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      if(order1 != NULL) 
        command_abort("Join Condition 1 (-y) is already set to [%s], [%s] is a problem.",
                      order1,argv[i+1]);
      order1 = argv[i+1];
      i += numa; break;

    case joincond2: 
      if( secondarg < 0 ){
	long_help(action);
	command_abort("Invalid argument [%s].", argv[i+1]);
      }
      if(order2 != NULL) 
        command_abort("Join Condition 2 is already set to [%s], [%s] is a problem.",
                      order2,argv[i+1]);
      order2 = argv[i+1];
      i += numa; break;
      
    case sizeiteration:
      runtime_setSizetest(rtarg,secondarg);
      i += numa; break;
            
    case compare:
      if( secondarg < 0 || secondarg > 3 || !(runtime_setup_oneshot(rtarg,'Z',NULL))){
	long_help(action);
	command_abort("Invalid term [%s].",argv[i]);
      }
      runtime_setCompareMode(rtarg,secondarg);
      Print(rtarg,"Compare mode is %d\n",secondarg);
      i += numa; break;

    case thishelp: usage_reminder_abort();
    default: break;
    };
  }

  /* second pass: exactly 2 join conditions (ordered indices) are
     needed only for the Join operation */
  if(order1 != NULL || order2 != NULL){

    /* note: null join conditions are equivalent to cartesian product;
       2 complete [0-len] join contitions are equivalent to the
       intersection */

    if(order1 == NULL || order2 == NULL)
      command_abort("Two Join Conditions are required (use -y and -Y), or -x CrossProduct -x Intersection without them");

    if( runtime_getCommand(rtarg) != 'X' &&  runtime_getRelOperator(rtarg) != op_join && runtime_getRelOperator(rtarg) != op_binunion )
      command_abort("Only the relational operator 'Join' and 'Binary Union' requires -y -Y (use -x J or -x B)");

    runtime_setOrder1(rtarg,order1);
    runtime_setOrder2(rtarg,order2);
  }

  return LEN;
}
