:- module(size_rel_basic_res, [generate_size_key/4], [assertions]).

generate_size_key(Literal, LitNum, ArgNum, Key) :-
	(
	    LitNum =:= 0 ->
	    Key = $(0, ArgNum)
	;
	    Key = Literal/ArgNum
	).
