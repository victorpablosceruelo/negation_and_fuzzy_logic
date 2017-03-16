% To consult the groundness of the subgoals of the input program
:- use_module(plai(abs_exec),
	[
	    ground/3,
	    not_ground/3,
	]).


%-------------------------------------------------------------------%
% simplify_specialize(+,+,+,+,-,-)                                  %
% simplify_specialize(Abs,Spec,Prog,Dicts,NewProg,NewDicts)         %
% Produces the simplified and specialized version of program Prog,  %
% with dictionaries Dicts, according to the abstract domain Abs and %
% the flag Spec and writes it in NewProg and NewDicts               %
%-------------------------------------------------------------------%

simplify_specialize(none,_Spec,Prog,Dicts,Prog,Dicts):-!.
simplify_specialize(Abs,Spec,Prog,Dicts,NewProg,NewDicts):-


%-------------------------------------------------------------------
simplify_ground([],_,_,[]).
simplify_ground([X|_],Abs,Info,_):-
	not_ground(Abs,X,Info),
	!,
	fail.
simplify_ground([X|L],Abs,Info,NewL):-
	ground(Abs,X,Info),
	!,
	simplify_ground(L,Abs,Info,NewL).
simplify_ground([X|L],Abs,Info,[X|NewL]):-
	simplify_ground(L,Abs,Info,NewL).

%-------------------------------------------------------------------
