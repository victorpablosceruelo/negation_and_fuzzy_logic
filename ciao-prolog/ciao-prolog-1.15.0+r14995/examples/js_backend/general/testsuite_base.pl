% (Base for testsuite_run and testsuite_ui)

%:- compilation_fact(test_clp).
%:- compilation_fact(test_search).
%:- compilation_fact(test_rec).
%:- compilation_fact(test_arith).

:- if(defined(test_clp)).
:- use_module(.(money_clpfd)).
:- use_module(.(sudoku_clpfd)).
:- use_module(.(bridge_clpfd)).
:- use_module(.(queens_clpfd)).
:- use_module(.(queens_freeze)).
:- use_module(.('sat-bench'(sat))).
:- if((defined(optim_comp), backend(js_backend))).
% TODO: module names should be atoms
all_puzzles := All :-
        All = [~money_clpfd,
	       ~sudoku_clpfd,
	       ~bridge_clpfd,
	       ~queens_clpfd,
	       ~queens_freeze,
	       ~sat].
:- else.
all_puzzles := All :-
        All = [sudoku_clpfd,
	       bridge_clpfd,
	       queens_clpfd,
	       queens_freeze,
	       sat].
:- endif.
:- elif(defined(test_search)).
% Search problems (mostly nondeterministic)
:- use_module(.(testsuite(boyer))).
:- use_module(.(testsuite(crypt))).
:- use_module(.(testsuite(guardians))).
:- use_module(.(testsuite(jugs))).
:- use_module(.(testsuite(knights))).
:- use_module(.(testsuite(population_query))).
:- use_module(.(testsuite(queens11))).
:- if((defined(optim_comp), backend(js_backend))).
all_puzzles := All :-
        All = [~boyer,
               ~crypt,
	       ~guardians,
	       ~jugs,
	       ~knights,
	       ~population_query,
	       ~queens11].
:- else.
all_puzzles := All :-
        All = [boyer,
               crypt,
	       guardians,
	       jugs,
	       knights,
	       population_query,
	       queens11].
:- endif.
:- elif(defined(test_rec)).
% Symbolic (mostly deterministic)
%  - test term creation and recursion
:- use_module(.(testsuite(deriv))).
:- use_module(.(testsuite(nreverse))).
:- use_module(.(testsuite(poly))).
:- use_module(.(testsuite(qsort))).
:- use_module(.(testsuite(tak))).
:- if((defined(optim_comp), backend(js_backend))).
all_puzzles := All :-
	All = [~deriv,
	       ~nreverse,
	       ~poly,
	       ~qsort,
	       ~tak].
:- else.
all_puzzles := All :-
	All = [deriv,
	       nreverse,
	       poly,
	       qsort,
	       tak].
:- endif.
:- elif(defined(test_arith)).
% Numerical (mostly deterministic)
:- use_module(.(testsuite(fft))).
:- use_module(.(testsuite(fib))).
:- use_module(.(testsuite(primes))).
:- if((defined(optim_comp), backend(js_backend))).
all_puzzles := All :-
	All = [~fft,
	       ~fib,
	       ~primes].
:- else.
all_puzzles := All :-
	All = [fft,
	       fib,
	       primes].
:- endif.
:- endif.

