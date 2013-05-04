:- module(_, [], [foreign_interface]).

:- '$pragma'(analyze_all).

:- export(obtain_list/3).
:- true pred obtain_list(in(N),go(Length),go(List)) :: int * int * int_list
	+ (foreign,size_of(List,Length)).
:- export(show_list/2).
:- true pred show_list(in(Length),in(List)) :: int * int_list
	+ (foreign,size_of(List,Length)).
:- export(random/1).
:- true pred random(-float) + foreign_low(prolog_random).

% TODO: bug in module resolving code? (foreign/foreign is simplified to foreign)
%:- use_foreign_source(.(foreign(foreign_aux_c))).
%:- use_foreign_source(.(foreign_aux_c)).
:- use_foreign_source(+(foreign_aux_c)).
