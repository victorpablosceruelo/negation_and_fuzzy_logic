:- module(pa_osize, [main/0], [hiord]).

% The complexity of call/2 (calln/2) is O(size_goal) (or more,
% depending on the complexity of copy_term/3), while a reasonable
% implementation should be O(number_arguments) (no copy_term
% involved).
%
% Jose F. Morales

:- use_module(library(apply)).
:- use_module(library(lists)).

% Culprit code in hiord_rt:
%
%   call(V, Args) :- calln(V, Args).
% 
%   calln(V, _) :- var(V), !, throw(error(instantiation_error, call/n-1)).
%   calln(Pred, Args) :-
%           Pred = 'PA'(Sh,_H,_B),
%           copy_term(Pred, 'PA'(Sh,Args,Goal)), !,
%           '$meta_call'(Goal).
%   calln(Pred, Args) :-
%           Pred = 'PA'(_Sh,H,_B),
%           functor(H,'',N),
%           functor(Args,_,N), !, % Predicate abstraction OK, argument unif. failed
%           fail.
%   calln(Pred, Args) :-
%           functor(Args,_,N),
%           throw(error(type_error(pred(N),Pred), call/n-1)).

main :-
	display('Testing complexity of call/n (the example should finish in less than 1 second)'), nl,
	test1.

test1 :-
	length(X, 15000),
	display(begin), nl,
	% depending on the complexity of calln/2:
        %  - O(num_args): this call will finish almost instantaneously.
        %      The order of maplist(nop(X),X) should be O(length(X))
	%
	%  - at least O(size_goal): this call will take a
	%    long time to finish.
	%      for X of length 2500 => 0.6 seconds
	%      for X of length 5000 => 2.5 seconds
	%      for X of length 10000 => 10 seconds
	%      for X of length 15000 => 32 seconds
	%      for X of length 20000 => 68 seconds
	%      for X of length 100000 => more than 12min (I gave up)
        %
        %      The order of maplist(nop(X),X) is at least O(length(X)^2).
        %      See the plot of data here:     
        %        http://www.wolframalpha.com/input/?i=quadratic+fit+{2500%2C0.6}%2C{5000%2C2.5}%2C{10000%2C10}%2C{15000%2C32}%2C{20000%2C68}
	maplist(nop(X),X),
	display(end), nl.

nop(_,_).
