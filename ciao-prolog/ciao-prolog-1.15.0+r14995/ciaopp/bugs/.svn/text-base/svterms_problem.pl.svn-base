% When analyzing with svterms, the following message appears:
%
% ?- module(svterms_problem), analyze(svterms).
% {Loading current module from /home/pawel/working/sv.pl
% {loaded in 1577.761 msec.}
% }
% {Analyzing /home/pawel/working/sv.pl
% {preprocessed for plai in 4.0 msec.}
% SOMETHING WAS WRONG With same value selectors
% SOMETHING WAS WRONG With same value selectors
% SOMETHING WAS WRONG With same value selectors
% {analyzed by plai using svterms in 12.998 msec.}
% }

% yes
% ?- 


:- module(_,[p/2],[assertions]).

:- entry p(A,B): list(A,num).
% p/2 just rewrites a list

p([],[]).
p([A|As],B):-
	app([A],B1,B),
	p(As,B1).


app([],Y,Y).
app([H|X],Y,[H|Z]):-
	app(X,Y,Z).
