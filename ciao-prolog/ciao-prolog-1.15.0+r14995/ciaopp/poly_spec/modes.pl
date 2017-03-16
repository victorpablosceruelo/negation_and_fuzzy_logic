:- module(modes,[abstract/2],[]).

:- use_package(assertions).

:- doc(title,"Implementation of domains for mode analysis").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module implements several domains for
            determining the modes of atom calls").


:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).


:- pred abstract(+Ain,-Aout) # "Abstracts @var{Ain} into
	@var{Aout}. Depends on flag @var{poly_modes}, calls to the
	corresponding abstraction predicate".

abstract(X, d) :- var(X),!.
abstract(X, s) :- 
	atom(X),
	ground(X),!.
abstract(Ain,Aout) :-
	current_pp_flag(poly_modes,Domain),
	Ain =.. [N | Args],
	abstract_args(Domain,Args,NArgs),
	Aout =.. [N | NArgs].



abstract_args(sd,X,Y)  :- abstract_sd(X,Y).
abstract_args(sdl,X,Y) :- abstract_sdl(X,Y),!.
abstract_args(sd_depth2,X,Y) :-  abstract_depth(2,X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  DOMAIN {STATIC,DYNAMIC}


:- pred abstract_sd(+ArgsIn,-ArgsOut) # "Abstracts @var{ArgsIn} using
	the domain {s,d} into @var{ArgsOut}.  For example, p(f(a),X)
	-> p(s,d)".

abstract_sd([],[]):-!.
abstract_sd([H|T],[s|NT]):-
	ground(H),!,
	abstract_sd(T,NT).
abstract_sd([_|T],[d|NT]):-
	abstract_sd(T,NT).	


:- pred abstract_sdl(+ArgsIn,-ArgsOut) # "Abstracts @var{ArgsIn} using
	the domain {s,d,l} into @var{ArgsOut}, where s stands for
	static, d for dynamic, and l for list skeleton.  For example,
	p([],[X,a],[a|T],f(X)) -> p(s,l,d,d) ".

abstract_sdl([],[]):-!.
abstract_sdl([H|T],[s|NT]):-
	ground(H),!,
	abstract_sdl(T,NT).
abstract_sdl([H|T],[l|NT]):-
	proper_list(H),!,
	abstract_sdl(T,NT).
abstract_sdl([_H|T],[d|NT]):-
	abstract_sdl(T,NT).


proper_list(X) :- X==[],!.
proper_list(X) :-
	nonvar(X),
	X=[_|Xs],
	proper_list(Xs).


:- pred abstract_depth(+Ain,+Depth,-Aout) # "Abstracts @var{Ain} using
	the domain {s,d} into @var{Aout}, at a depth level
	@var{Depth}".


abstract_depth(0,Ain,Aout):-
	abstract_sd(Ain,Aout).
abstract_depth(N,Ain,Aout):-
	N>0,
	abstract_depth_(Ain,N,Aout).

abstract_depth_([],_,[]):-!.
abstract_depth_([H|T],N,[H|NT]):-
	atom(H),
	ground(H),!,
	abstract_depth_(T,N,NT).
abstract_depth_([H|T],N,[d|NT]):-
	var(H),!,
	abstract_depth_(T,N,NT).
abstract_depth_([H|T],N,[NH|NT]):-!,
	N1 is N-1,
	H =.. [Pred | Args],
	abstract_depth(N1,Args,NArgs),
	NH =.. [Pred | NArgs],
	abstract_depth_(T,N,NT).
