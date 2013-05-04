#
# E.S. Ackley
# this program does the C compilation 
# for NDB Online Database 
# updated: Thu Oct 11 12:22:18 2007 
# Wed Nov  7 00:50:57 2007 exclude test.c from main executable
# Sat Nov 17 02:25:22 2007 added EXPAND_INPUT_NDB option
# Thu Nov 29 03:44:05 2007 replace NPG_NONE with 4 new options:
# NPG_NONE_NEWNDB0, NPG_NONE_NEGADD, NPG_NONE_UNNEGATE, NPG_USE_NSQL
# Fri Dec  7 09:43:37 2007 added NPG_NONE_NEGREMOVE
# Mon Dec 31 01:38:37 2007 added USE_INSERT_NSQL
# Mon Jan 14 13:15:46 2008 added RNDB-un_1.test and RNDB-un_2.test files
# Wed Apr  2 09:28:17 2008 added libnegdb.a

VERSION = .72.8

FLAGS =  -g -Wall -pedantic
GNUONLYOK = -std=gnu99

# choose one of the following NPG variations to add *'s to negative records:
# -D NPG_ADD_RAND : orig negative pattern generate that adds non-essential bits back randomly
# -D NPG_REMV_RAND : new negative pattern generate that removes non-essential bits randomly
# -D NPG_REMV_BIAS : negative pattern generate that removes non-essential bits biased toward most frequent bits (default).
# -D NPG_DETERMINISTIC : negative pattern generate that removes non-essential bits deterministically starting with leftmost bit 0 (and all randomization).
# -D NPG_NONE_NEWNDB0 : no negative pattern generate in new ndb creation (mode 0) (default is with npg)
# -D NPG_NONE_NEGADD : no negative pattern generate when "adding" positive recs to ndb (default is with npg)
# -D NPG_NONE_NEGREMOVE : no negative pattern generate when "removing" positive recs from ndb (default is with npg)
# -D NPG_NONE_UNNEGATE : no negative pattern generate for unnegate operation (default is with npg)
# -D NPG_USE_NSQL_ALL : use negative pattern generate for all relational operations, except Intersect and Select EQ/NE (default is without npg)
# -D NPG_USE_NSQL_BU : use negative pattern generate for Binary Union only (default is without npg)
#
# -D RECS2ADD randomly determines minimum record sets to insert, default is one set of 'all possible bit combinations' for the randomly selected extra bits.
# -D USE_APPEND, unlike Insert, Append adds no extra bits or records (effects adds, removes, and cleanups). (v.59)
# -D USE_INSERT_NSQL_ALL, Insert, unlike Append (the default for relational operations except Intersect), may add extra bits and records (effects all relational operations). (v.67)
# -D USE_INSERT_NSQL_BU, Insert, unlike Append (the default for relational operations), may add extra bits and records (effects binary union only). (v.66.3)
# -D SKIP_COMPRESS, doesn't try to do any compression; o.w. tries to compress when -k specified (v.59).
# -D SAVE_SORTED_NDB, prints ndb in sorted tree order (0,1,*).
# -D MANAGED_GROWTH, disables expanding record size on build, and avoids adding redundant records (see also USE_APPEND).
# -D EXPAND_INPUT_NDB, enables expanding record size on build (overrides MANAGED_GROWTH).
#
# -D UNNEGATE_HAMMDIS_DETERMINISTIC, permutes unnegate input using next-descent hillclimbing, s.t. the hamming distance between negative records is reduced (v.61). 
# -D UNNEGATE_HAMMDIS_RANDOMHC, permutes unnegate input using random hillclimbing, s.t. the hamming distance between negative records is reduced (v.61).
# -D UNNEGATE_HAMMDIS_NONE (default), unnegate input is not modified (v.61).
#
# -D NPG_VERBOSE_DEBUG for negative pattern generate debugging messages
# -D PERM_VERBOSE_DEBUG for lots of  perm debugging messages
# -D USE_RECSUBSUMED stats to stdout on record UNITS tested in rec_subsumed_query ("matching") [one-shot cmd only]  
# -D NORANDSATLENLIMIT to skip the length check for singletons and random SAT empty, and use smaller r values. [note: fails runtests.]
# -D ISCHARDQR to use the r and q for hard-to-reverse singletons and random SAT empty as in the ISC paper (supercedes NORANDSATLENLIMIT r) for record lengths > 1000 [note: fails runtests with record length 100.]
# -D DISPLAY_RECCOUNT_DELETED for summary of select efficiency savings (used without -D MANAGED_GROWTH).
# -D DISPLAY_RECCOUNT_DELETED_ALL for lots of select efficiency savings messages (per record) (used without -D MANAGED_GROWTH).

# -A -assert is to disable all stand assertions (use only for speed tests).
# -O4 for optimization
# -pg for profiling (it must also be present for linking phase!) 
# -D NORANDSATLENLIMIT -D ISCHARDQR 
# -D UNNEGATE_HAMMDIS_DETERMINISTIC  -D EXPAND_INPUT_NDB  -D NPG_NONE_NEGADD -D NPG_NONE_NEWNDB0 -D NPG_NONE_UNNEGATE -D USE_INSERT_NSQL_BU  -D NPG_USE_NSQL_ALL -D NPG_USE_NSQL_BU
#  (NSH options) -D NPG_DETERMINISTIC -D SAVE_SORTED_NDB  -D NPG_NONE_NEGADD -D NPG_NONE_NEWNDB0  -D NPG_NONE_UNNEGATE -D MANAGED_GROWTH -D NPG_USE_NSQL_BU -D USE_INSERT_NSQL_BU

CCOPT = -O4  -D NPG_DETERMINISTIC -D SAVE_SORTED_NDB  -D NPG_NONE_NEGADD -D NPG_NONE_NEWNDB0  -D NPG_NONE_UNNEGATE -D MANAGED_GROWTH -D NPG_USE_NSQL_BU -D USE_INSERT_NSQL_BU 
CLOPT = 
CPP        = gcc $(FLAGS)
LC         = gcc

TOBJS = test.o
OBJS =  freq.o perm.o rec.o rstree.o recset.o cleanresults.o negdb.o runtime.o command_args.o newndb.o nsql.o easy.o
TESTDEP = test.h RNDB.test RNDB-3673.test RNDB-un_1.test RNDB-un_2.test RNDB-mode1.test
ALLDEP = rec.h perm.h runtime.h negdb.h recset.h command_args.h freq.h cleanresults.h rstree.h newndb.h nsql.h easy.h error.h names.h Makefile

default: negdb

libnegdb.a: ${OBJS}
	ar ruc $@ ${OBJS}
	ranlib $@

negdb: clean main.o ${OBJS} 
	${LC} main.o ${OBJS} ${CLOPT} -o $@

runtests: clean cleantests testmain 
	./testmain

valtests: clean cleantests testmain 
#	valgrind -v --leak-check=yes --db-attach=yes ./testmain
	valgrind -v --leak-check=yes ./testmain

testmain: testmain.o ${OBJS} ${TOBJS}
	${LC} testmain.o ${OBJS} ${TOBJS} ${CLOPT} -o $@

main.o: main.c ${ALLDEP}
	${CPP} -c ${CCOPT} main.c

testmain.o: testmain.c ${ALLDEP} ${TESTDEP}
	${CPP} -c ${CCOPT} testmain.c

perm.o: perm.c ${ALLDEP}
	${CPP} -c ${CCOPT} perm.c

freq.o: freq.c ${ALLDEP}
	${CPP} -c ${CCOPT} freq.c

rec.o: rec.c ${ALLDEP}
	${CPP} -c ${CCOPT} ${GNUONLYOK} rec.c

runtime.o: runtime.c ${ALLDEP}
	${CPP} -c ${CCOPT} runtime.c

negdb.o: negdb.c ${ALLDEP}
	${CPP} -c ${CCOPT} negdb.c

newndb.o: newndb.c ${ALLDEP}
	${CPP} -c ${CCOPT} newndb.c

recset.o: recset.c ${ALLDEP}
	${CPP} -c ${CCOPT} recset.c

rstree.o: rstree.c ${ALLDEP}
	${CPP} -c ${CCOPT} rstree.c

nsql.o: nsql.c ${ALLDEP}
	${CPP} -c ${CCOPT} nsql.c

easy.o: easy.c ${ALLDEP}
	${CPP} -c ${CCOPT} easy.c

test.o: test.c ${ALLDEP} ${TESTDEP}
	${CPP} -c ${CCOPT} test.c

command_args.o: command_args.c ${ALLDEP}
	${CPP} -c ${CCOPT} command_args.c

cleanresults.o: cleanresults.c ${ALLDEP}
	${CPP} -c ${CCOPT} cleanresults.c

clean:
	rm -f *.o

cleantests:
	rm -f rndb-*.txt
	rm -f rndb-*.cnf
	rm -f rndb.sav
	rm -f distrib7-RNDB-0.dat
	rm -f clean.dat
	rm -f partial*.txt
	rm -f partial-ndb*.cnf
	rm -f RNDB.txt
	rm -f RNDB.cnf
	rm -f RNDB3.txt
	rm -f RNDB3.cnf
	rm -f RNDB2.txt
	rm -f RNDB2.cnf
	rm -f un_NDB.txt

cleanbig: clean
	rm -f negdb
	rm -f testmain
	rm -f *~	

cleanall: cleanbig cleantests

distrib1:
	rm -rf negdb-v${VERSION}
	mkdir negdb-v${VERSION}
	cp Makefile ./negdb-v${VERSION}
	cp README ./negdb-v${VERSION}
	cp COPYING ./negdb-v${VERSION}
	cp RNDB*.test ./negdb-v${VERSION}
	cp *.c ./negdb-v${VERSION}
	cp *.h ./negdb-v${VERSION}
	rm -f negdb-v${VERSION}.tar*

distrib: distrib1
	tar -cf negdb-v${VERSION}.tar negdb-v${VERSION}
	gzip -f negdb-v${VERSION}.tar
	rm -rf negdb-v${VERSION}

install2:
	gunzip -f negdb-v${VERSION}.tar.gz
	tar -xvf negdb-v${VERSION}.tar
