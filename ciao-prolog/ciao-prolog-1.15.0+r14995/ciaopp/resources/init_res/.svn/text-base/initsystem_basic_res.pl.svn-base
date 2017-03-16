:- module(initsystem_basic_res, [clause_type/2], [assertions, nativeprops]).

:- use_module(resources(resources_basic), [clause_t/1]).
%
%  Classify the type of a clause.
%    Declaration -- decl
%    Rule        -- rule
%    Fact        -- fact
%

:- pred clause_type/2 :: term * clause_t + (not_fails).

clause_type((:- _Decl),    decl) :- !.
clause_type((_ :- true:_), fact) :- !.
clause_type((_ :- _),      rule) :- !.
clause_type(_,             fact).
