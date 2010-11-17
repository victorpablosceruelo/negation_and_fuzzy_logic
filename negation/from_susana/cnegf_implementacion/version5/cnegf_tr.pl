%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cnegf to substitute negation calls
% to neg/1 for calls to cnegf/1 when the goal has a finite
% number of solutions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cnegf_tr,[in_cnegf/3],[]).

%:- load_compilation_module('/home/susana/src/Ciao/ciaopp-0.8p24/src/topcpaths').
%:- use_module(ciaopp(m_ciaopp),[precompile/2]).

:- use_module(pplib(database), [db_get/1,trust_complexity/9]).



% in_cnegf(0):-
% 	m_ciaopp:precompile(File,[none,none,typesfd,shfr,none,none,none,
%                                   nf,none,none,none,none,yes]),
% 	m_ciaopp:precompile(File,[none,none,typesfd,shfr,none,none,upper,
%                                   nf,none,none,none,none,yes]).


%%%%%%%%%%%%%%% IN_CNEGF %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in_cnegf(Clause,SentList,Module) substitute all neg(G) calls
% calls for cnegf(G) calls when the analyses determine that G
% is going to have a finite number of soluftions
in_cnegf(end_of_file,[end_of_file],_Module):-
	!.
in_cnegf((H :- B), [(H :- B1)],_Module):-
	finsol(B,B1).

% finsol(Goal,Goal1,H) returns in Goal1 the same term that in Goal
% except if Goal is "neg(Pred)" and Pred is a predicate with a finite
% number of solutions.	
finsol((A,B),(A1,B1)):-
	!,
	finsol(A,A1),
	finsol(B,B1).
finsol((A,B),(A1;B1)):-
	!,
	finsol(A,A1),
	finsol(B,B1).
finsol(neg(Pred),Goal):-
	functor(Pred,F,A),
	finite_solutions(F,A),
	!, 
	Goal= (cnegf(Pred)).
finsol(B,B).

% finite_solutions(F,A).% Siempre da cierto es para probar

finite_solutions(F,A):-
	functor(Head0,F,A),
	db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo)),
        trust_complexity(F/A, _Mode, _Measure, _Mutex, 
                         _Solution_Det, _Size, _Relation, Time, _Domain),
        FailInfo == not_fails,
        Time \== inf.



%%%%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%






