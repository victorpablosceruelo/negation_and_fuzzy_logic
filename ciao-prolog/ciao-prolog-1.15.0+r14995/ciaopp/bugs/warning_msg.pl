%% Following program makes Ciaopp hang when
%% using shfr. Just try
%% 
%% ?- module(warning_msg), analyze(shfr).
%%
%% The problem is that warning_message/* used
%% by CiaoPP hangs when printing long messages
%% (because sformat/3 does not work properly).
%%
:- module(warning_msg,[foo/8],[assertions]).

:- entry foo/8.

foo(A,B,C,D,E,F,G,H):-
	p(A,B,C,D,E,F,G,H).

:- data p/8.
