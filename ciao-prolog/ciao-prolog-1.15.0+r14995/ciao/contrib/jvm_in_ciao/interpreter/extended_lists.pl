:-module(extended_lists,
	[
	    update/4,
	    replace/4,
	    replace_first/4,
	    init/3,
	    nth_g/3,
	    member_g/2,
	    append_g/3
	],
	[assertions, nortchecks, regtypes, nativeprops, basicmodes]).

:- include('extended_lists_i.pl').

:- entry update(+list,+nnegint,+term,-list).
:- pred update(+list,+nnegint,+term,-list) + (eval, is_det).

:- entry replace(+list,+nnegint,+term,-list).
:- pred replace(+list,+term,+term,-list) + (eval, is_det).

:- entry init(List,Term,Int) : (list(List),ground(Term),int(Int)).
:- success init(List,Term,Int) : (list(List),ground(Term),int(Int)) => ground(List).

:- trust comp nth_g(N,_L,_E) : integer(N) + eval.
:- trust comp nth_g(N,_L,_E) : var(N) + memo.
:- trust comp nth_g/3 + (bind_ins,sideff(free)).
nth_g(N, List, Elem) :-
        N >= 1, nthfunc(N, List, Elem).
%nth_g(N, List, Elem) :-
%        var(N),
%        findnth(List, Elem, 1, N).
%nth_g(N, _, _) :-
%        throw(error(type_error(integer, N), nth/3-1)).

:- trust comp nthfunc(N,L,E) : integer(N) + eval.
:- trust comp nthfunc/3 + (bind_ins,sideff(free)).
nthfunc(1, [Elem|_], Elem).
nthfunc(N, [_|List], Elem) :-
	N > 1,
        N1 is N-1,
        nthfunc(N1, List, Elem).

%findnth([Elem|_], Elem, N, N).
%findnth([_|List], Elem, N0, N) :-
%        N1 is N0+1,
%        findnth(List, Elem, N1, N).

:- trust comp update(L,I,_,_) : (list(L),integer(I)) + eval.
:- trust comp update(L,I,_,_) : var(L) + memo.
:- trust comp update(L,I,_,_) : var(I) + memo.
:- trust comp update/4 + (bind_ins,sideff(free)).
update([_|Old],1,Value,[Value|Old]).
update([X|Old],Index,Value,[X|New]):-
	Index>1,
	Index1 is Index - 1,
	update(Old,Index1,Value,New).

% For the moment member_g and replace_first are residual, and so, I assume that they will only be evaluated in PE when they are called from an 'eval' context. In the future we might try to write the conditions.
%:- trust comp replace_first(L,_,_,_) : list(L) + (eval,bind_ins,sideff(free)).
:- trust comp replace_first(L,_,_,_) + (memo,bind_ins,sideff(free)).
replace_first([],_,_,[]).
replace_first([Researched|Old],Researched,Replacement,[Replacement|Old]) :- !.
replace_first([X|Old],Researched,Replacement,[X|New]):-
	replace_first(Old,Researched,Replacement,New).

:- trust comp member_g/2 + (memo,bind_ins,sideff(free)).
member_g(X,[Y|_]) :- X = Y.
member_g(X,[Y|R]) :- X \= Y, nonvar(R), member_g(X,R).

:- trust comp append_g/3 + (memo,bind_ins,sideff(free)).
append_g([],B,B).
append_g([X|R],B,[X|C]) :- append_g(R,B,C).
