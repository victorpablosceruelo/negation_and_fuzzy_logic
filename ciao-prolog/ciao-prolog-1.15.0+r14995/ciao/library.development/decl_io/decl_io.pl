:- module(_,_,[dcg]).

:- use_module(library(dynamic)).

%% % 'Declarative' I/O.
%% 
%% decl_write(Term, S, [stdio_write(Term)|S] ) :- 
%%  	display(Term).
%% 
%% % --------------
%% 
%% :- dynamic state/1.
%% 
%% q(X) -->
%% 	p(X),
%% 	r(_).
%% 
%% % Would need to distinguish between activations for recursive p...
%% p(X,SI,SO) :- 
%% 	retractall(state(p(_))),
%% 	a(X,SI,S1),
%% 	decl_write(X,S1,SO),
%% 	assert(state(p(SO))).
%% p(X,_,SO) :- 
%% 	retract(state(p(SI))),
%% 	b(X,SI,S1),
%% 	decl_write(X,S1,SO),
%% 	assert(state(p(SO))).
%% p(X,_,SO) :- 
%% 	retract(state(p(SI))),
%% 	c(X,SI,S1),
%% 	decl_write(X,S1,SO).
%% 
%% a(a,X,X).
%% b(b,X,X).
%% c(c,X,X).
%% r(r,X,X).
%% 
% --------------

% 'Declarative' I/O.

decl_write(Term, SI-SO,SI-NSO) :- 
  	display(Term),
 	SO = [stdio_write(Term)|NSO] .

% --------------

% Query:
% _I = [stdio_write(x)|_IE], q(X,_I-_IE,O-[]).

:- dynamic state/1.

q(X) -->
	p(X),
	r(_).

% Would need to distinguish between activations for recursive p...
p(X,SI,SO) :- 
	retractall(state(p(_))),
	a(X,SI,S1),
	decl_write(X,S1,SO),
	assert(state(p(SO))).
p(X,_,SO) :- 
	retract(state(p(SI))),
	b(X,SI,S1),
	decl_write(X,S1,SO),
	assert(state(p(SO))).
p(X,_,SO) :- 
	retract(state(p(SI))),
	c(X,SI,S1),
	decl_write(X,S1,SO).

a(a,X,X).
b(b,X,X).
c(c,X,X).
r(r,X,X).

% -------------

%% q(X) -->
%% 	p(X),
%% 	r(_).
%% 
%% p(X) :- 
%% 	a(X),
%% 	write(X).
%% p(X) :- 
%% 	b(X),
%% 	write(X).
%% p(X) :- 
%% 	c(X),
%% 	write(X).
%% 
%% a(a).
%% b(b).
%% c(c).
%% r(r).

% -------------

%% q(X) :-
%% 	p(X),
%% 	r(_).
%% 
%% p(X) :- 
%% 	a(X),
%% 	write(X).
%% p(X) :- 
%% 	b(X),
%% 	write(X).
%% p(X) :- 
%% 	c(X),
%% 	write(X).
%% 
%% a(a).
%% b(b).
%% c(c).
%% r(r).
