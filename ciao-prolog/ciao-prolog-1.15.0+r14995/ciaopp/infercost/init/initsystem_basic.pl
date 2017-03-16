:- module(initsystem_basic, [clause_type/2, clause_type/3], [assertions]).

%
%  Classify the type of a clause.
%    Declaration -- 1	
%    Rule        -- 2
%    Fact        -- 3
%

clause_type(Clause,Type) :-
	functor(Clause,F,N),
	clause_type(F,N,Type).

clause_type((:-),1,1).
clause_type((:-),2,2).
clause_type(F,_,3) :-
	F \== (:-).

