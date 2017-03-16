:- module(varmodulename, [main/1]).

/*
  The code below compiles and executes although member/2 is not visible.

  If "M = lists" is uncommented, an error is raised at run time

*/

main(M):-
%	M = lists,
        M:member(_, [a]),
	display(M).
