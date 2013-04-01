:- module(opt_cnegf,_,[]).
 
:- load_compilation_module('/home/susana/src/Ciao/ciaopp-0.8p24/src/topcpaths').

:- use_module(ciaopp(m_ciaopp),[precompile/2]).

:- use_module(pplib(database), [db_get/1,trust_complexity/9]).

:- use_module(cnegf_tr,[predicate/2, finite/2]).

%:- data finite/2.
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
	assert_finite,
	m_ciaopp:precompile(File,[none,none,none,none,none,none,none,
                                  none,none,none,none,none,yes]).

assert_finite:-
	predicate(F,A),
	finite_solutions(F,A),
	assertz_fact(finite(F,A)),
	fail.
assert_finite.

finite_solutions(F,A):-
	functor(Head0,F,A),
	db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo)),
        trust_complexity(F/A, _Mode, _Measure, _Mutex, 
                         _Solution_Det, _Size, _Relation, Time, _Domain),
        FailInfo == not_fails,
        Time \== inf.
