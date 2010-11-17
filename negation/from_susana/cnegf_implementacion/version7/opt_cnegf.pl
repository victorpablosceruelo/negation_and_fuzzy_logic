:- module(opt_cnegf,[main/1,finite/2],[]).
 
:- load_compilation_module('/home/susana/src/Ciao/ciaopp-0.8p24/src/topcpaths').
:- use_package(assertions).

:- use_module(library(file_utils),[file_terms/2]).

:- use_module(ciaopp(m_ciaopp),[precompile/2]).

:- use_module(pplib(database), [db_get/1,trust_complexity/9]).

%:- use_module(cnegf_tr,[predicate/2, finite/2]).

:- data finite/2.
%:- multifile finite/2.

%:- data predicate/2.

% Optimization of the negation calls of a list of programs Files.
main([]).
main([File|Files]):-
	neg_optimize(File,_File_out),
	main(Files).

% Optimization of the negation calls of a list of a program File.
neg_optimize(File,_File_out):-
	m_ciaopp:precompile(File,[none,none,typesfd,shfr,none,none,none,
                                  nf,none,none,none,none,yes]),
	m_ciaopp:precompile(File,[none,none,typesfd,shfr,none,none,upper,
                                  nf,none,none,none,none,yes]),
	atom_concat(File,'.pl',File1),
	file_terms(File1,Terms),
	assert_finite(Terms),
	m_ciaopp:precompile(File,[none,none,none,none,none,none,none,
                                  none,none,none,none,none,yes]).

assert_finite(Terms):-
	member(:-(H,_B),Terms),
	functor(H,F,A),
% display(predicateleido),
% display(F),
	finite_solutions(F,A),
 display(opt_inserta_finito____),
 display(F),
	assertz_fact(finite(F,A)),
	fail.
assert_finite(_).

finite_solutions(F,A):-
	functor(Head0,F,A),
	db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo)),
        trust_complexity(F/A, _Mode, _Measure, _Mutex, 
                         _Solution_Det, _Size, _Relation, Time, _Domain),
        FailInfo == not_fails,
        Time \== inf.
