:- module(check_files, [seq/2,par_nondet/2,data/1],[]).

:- use_package(andprolog_nd).

:- use_module(library(read)).
:- use_module(library(system)).
:- use_module(library(format), [format/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data(X) :- X = ['files/file1','files/file2','files/file1','files/file2','files/file1','files/file2','files/file1','files/file2'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(L,X) :- ck_seq(L,X).
par_nondet(L,X) :- ck_par(L,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ck_seq([],_).
ck_seq([File|File_rest],BadFile1) :-         
	check_files(File,BadFile1),
	ck_seq(File_rest,BadFile2),
	BadFile1 = BadFile2.

ck_par([File],BadFile) :- 
        !, check_files(File,BadFile).
ck_par([File|File_rest],BadFile1) :- 
        check_files(File,BadFile1) & ck_par(File_rest,BadFile2),
	BadFile1 = BadFile2.


check_files(File, BadFile) :-
    	(
    	    file_exists(File)->
	    open(File, read, S),
 %%   	    files(File,Files),
	    read_files(S, Files),
	    close(S),
	    check_source_files(Files, BadFile)
    	;
    	    format("File does not exist.~n", [])
	).

 %% files('files/file1',['no_exits1', 'no_exits1', 'no_exits1', 'no_exits1', 'no_exitsa', 'files/file1', 'files/file1', 'files/file1', 'files/file1', 'no_exits3', 'no_exits3', 'no_exits3', 'no_exits3', 'no_exitsb', 'files/file1', 'files/file1', 'files/file1', 'files/file1', 'no_exitsc', 'files/file1', 'files/file1', 'files/file1', 'files/file1']).
 %% 
 %% files('files/file2',['files/file1', 'files/file1', 'files/file1', 'files/file1', 'no_exits2', 'no_exits2', 'no_exits2', 'no_exits2', 'no_exitsc', 'no_exits4', 'no_exits4', 'no_exits4', 'no_exitsa', 'files/file1', 'files/file1', 'files/file1', 'files/file1', 'no_exitse', 'files/file1', 'files/file1', 'files/file1', 'files/file1', 'no_exitsb']).

read_files(Stream, Files) :-
	read(Stream, T),
 	(
 	    T = end_of_file -> 
 	    Files = []
 	;
 	    Files = [T|Rest],
 	    read_files(Stream,Rest)
 	).

check_source_files([], _) :- fail.
check_source_files([F|_Files], BadFile) :-
	check_source_files1(F, BadFile).
check_source_files([_F|Files], BadFile) :-
	check_source_files(Files, BadFile).

check_source_files1(F, BadFile) :-
   	pause,
	(
 	    file_exists(F) -> 
 	    fail
 	;
	    BadFile = F
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause :- fib(18,_).

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib(N1, F1),
	fib(N2, F2),
        F is F1 + F2.
