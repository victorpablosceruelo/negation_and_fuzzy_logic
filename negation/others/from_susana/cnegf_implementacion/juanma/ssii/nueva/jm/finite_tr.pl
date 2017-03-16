%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  finite_tr.pl
%%                                 Juan Manuel Martinez Barrena
%%                                                  version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(_,[add_finite/3],[assertions]).

:- use_module(library(write)). 


new_finite(T1,T2) :-
	open('finite_facts.pl', append, S),
	write_term(S,(finite(T1,T2)),[]),
	write_term(S,'.',[]),
	put_code(S,10),
	close(S).

% Para crear el archivo finite_facts.pl
% new_finite_facts :-
%	open('finite_facts.pl', write, S),
%	write_term(S,(':-module(finite_facts, [finite/2], [])'),[]),
%	write_term(S,'.',[]),
%	put_code(S,10),
%	put_code(S,10),
%	put_code(S,10),
%	close(S).

% Para consultar si es finite un predicado	
% is_finite(P,A) :- 
%	finite_facts:finite(P,A).

% PARA LA ESCRITURA
% escribir(T) :-
%	open('finite_facts.pl', append, S),
%	write_term(S,(T),[]),
%	write_term(S,'.',[]),
%	put_code(S,10),
%	close(S).

% PARA LA LECTURA
% :- use_module(library(read)).
% leer :- 
%	open('finite_facts.pl', read, L),
%	read_term(L, X, []),
%	write(X),
%	close(L).
	
% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj
conj_to_list((A,B),[A|ListB]):-
 !,
  conj_to_list(B,ListB).
conj_to_list(A,[A]).

perteneceALista(X, [X|_]).
perteneceALista(X, [_|Xs]) :- perteneceALista(X,Xs).

%Prueba, add_sth
%
%Tenemos q buscar:
%:- true pred impar(A)
%>          : term(A)
%>         => nnegint(A)
%>          + ( not_fails, not_covered, step_up(......) ). 
%
% true pred Predicado COSAS => MAS + Conjunto
% conj_to_list(Conjunto, Lista)
% Buscaremos en Lista:
%    perteneceALista(not_fails,Lista)
%    perteneceALista(step_up(_),Lista)
% Metemos:
%    finito(Predicado)
%FINITO:

add_finite(end_of_file, (end_of_file), _):- 
	!.

add_finite(
  (:- true pred Predi : Algo => Tipo + Info),
  (:- true pred Predi : Algo => Tipo + Info), 
  _):-
        conj_to_list(Info, Lista),
	perteneceALista(not_fails,Lista),
	perteneceALista(steps_ub(_),Lista),
	functor(Predi,P,A),
	new_finite(P,A),
	!.
