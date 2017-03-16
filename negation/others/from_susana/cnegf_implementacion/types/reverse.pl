:- module(reverse, [reverse/2], [assertions]).

% :- use_module(library(lists), [list/2, list1/2]).

:- entry reverse(A,B) : (ground(A), list(A, term), var(B)).

reverse(L,K) :-
        rev(L,[],K).

rev([],L,L).
rev([H|T],L,K) :-
        rev(T,[H|L],K).


%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

