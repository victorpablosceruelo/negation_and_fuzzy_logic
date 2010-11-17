%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cnegf to substitute negation calls
% to neg/1 for calls to cnegf/1 when the goal has a finite
% number of solutions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cnegf_tr,[in_cnegf/3],[]).
%:- module(cnegf_tr,[in_cnegf/3,finite/2,predicate/2],[]).

%:- load_compilation_module('/home/susana/src/Ciao/ciaopp-0.8p24/src/topcpaths').
%:- use_module(ciaopp(m_ciaopp),[precompile/2]).

%:- use_module(pplib(database), [db_get/1,trust_complexity/9]).

:- use_module(opt_cnegf,[finite/2]).

:- data predicate/2.

%:- multifile finite/2.

%%%%%%%%%%%%%%% IN_CNEGF %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in_cnegf(Clause,SentList,Module) substitute all neg(G) calls
% calls for cnegf(G) calls when the analyses determine that G
% is going to have a finite number of soluftions
%in_cnegf(0,[],File):-
% 	m_ciaopp:precompile(File,[none,none,typesfd,shfr,none,none,none,
%                                   nf,none,none,none,none,yes]),
% 	m_ciaopp:precompile(File,[none,none,typesfd,shfr,none,none,upper,
%                                   nf,none,none,none,none,yes]),
%	fail.
in_cnegf(end_of_file, [end_of_file], _Module):-
	!.
in_cnegf((H :- B), [(H :- B1)], _Module):-
	!,
	finsol(B,B1),
	functor(H,F,A),
	assertz_fact(predicate(F,A)).
% display(predicate(F)).
in_cnegf(Fact, [Fact], _Module):-
	functor(Fact,F,A),
	assertz_fact(predicate(F,A)).

% finsol(Goal,Goal1,H) returns in Goal1 the same term that in Goal
% except if Goal is "neg(Pred)" and Pred is a predicate with a finite
% number of solutions.	
finsol((A,B),(A1,B1)):-
	!,
	finsol(A,A1),
	finsol(B,B1).
finsol((A;B),(A1;B1)):-
	!,
	finsol(A,A1),
	finsol(B,B1).
finsol(neg(Pred),Goal):-
	functor(Pred,F,A),
%	finite_solutions(F,A),
 display(tr_Comprueba_finite_con_),
 display(F),
	finite(F,A),
 display(es_finite_),
	!, 
	Goal= (cnegf(Pred)).
finsol(B,B).

% finite_solutions(F,A):-
% 	functor(Head0,F,A),
% 	db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo)),
%         trust_complexity(F/A, _Mode, _Measure, _Mutex, 
%                          _Solution_Det, _Size, _Relation, Time, _Domain),
%         FailInfo == not_fails,
%         Time \== inf.

%%%%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%






