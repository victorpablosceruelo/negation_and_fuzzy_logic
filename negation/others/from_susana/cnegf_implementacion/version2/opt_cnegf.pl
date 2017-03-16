:- module(opt_cnegf,_,[]).
 

:- load_compilation_module('/home/susana/src/Ciao/ciaopp-0.8p24/src/topcpaths').
%:- load_compilation_module('/home/clip/Systems/ciaopp/src/topcpaths').
%:- load_compilation_module('/home/clip/Systems/ciaopp-0.8/src/topcpaths').
:- use_module(ciaopp(m_ciaopp),[precompile/2]).

%:- use_module(pplib(database), [db_get/1,trust_complexity/9]).

:- use_package(runtime_ops).
:- use_package(assertions).

%:- use_module(library(file_utils),[file_terms/2]).

% Optimization of the negation calls of a list of programs Files.
main([]).
main([File|Files]):-
	neg_optimize(File,_File_out),
	%display(File_out),
	main(Files).

% Optimization of the negation calls of a list of a program File.
neg_optimize(File,_File_out):-
	m_ciaopp:precompile(File,[none,    % step(ann).
	                          none,    % step(iap).
                                  typesfd, % step(types).
	                          shfr,    % step(modes).
                                  none,    % step(spec).
			          none,    % step(grain).
                                  none,    % step(cost).
                                  nf,      % step(nf).
                                  none,    % step(det).
                                  none,    % step(trc).
                                  none,    % step(ctcheck).
                                  none,    % step(rtcheck).
			          yes]),   % step(out).
	m_ciaopp:precompile(File,[none,    % step(ann).
	                          none,    % step(iap).
                                  typesfd, % step(types).
	                          shfr,    % step(modes).
                                  none,    % step(spec).
			          none,    % step(grain).
                                  upper,   % step(cost).
                                  nf,      % step(nf).
                                  none,    % step(det).
                                  none,    % step(trc).
                                  none,    % step(ctcheck).
                                  none,    % step(rtcheck).
			          yes]),   % step(out).
	m_ciaopp:precompile(File,[none,    % step(ann).
	                          none,    % step(iap).
                                  none,    % step(types).
	                          none,    % step(modes).
                                  none,    % step(spec).
			          none,    % step(grain).
                                  none,    % step(cost).
                                  none,    % step(nf).
                                  none,    % step(det).
                                  none,    % step(trc).
                                  none,    % step(ctcheck).
                                  none,    % step(rtcheck).
			          yes]).   % step(out).



% finite_solutions(F/A):-
% 	functor(Head0,F,A),
% 	db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo)),
%         trust_complexity(F/A, _Mode, _Measure, _Mutex, 
%                         _Solution_Det, _Size, _Relation, Time, _Domain),
%         FailInfo == not_fails,
%         Time \== inf.


