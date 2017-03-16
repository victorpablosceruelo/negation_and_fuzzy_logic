:- module(prolog_read, [parse/2, read/4], [assertions,nativeprops]).

:- use_module(prolog_read2, [read/5]).
:- use_module(syntax_error, [syntax_error/1, syntax_error/2]).

goal :-
        parse([some,tokens,here],_ParseTree).


%%%%:- entry parse(X,Y)
%%%%         : ( term_typing:var(Y), term_typing:ground(X) ).

parse(Tokens,Answer) :-
        'parse/2/1/$disj/1'(Tokens,Term),
        !,
        term_basic:(Answer=Term).

'parse/2/1/$disj/1'(Tokens,Term) :-
        read(Tokens,1200,Term,LeftOver),
        all_read(LeftOver).
'parse/2/1/$disj/1'(Tokens,Term) :-
        syntax_error(Tokens).

all_read([]) :- !.
all_read(S) :-
        syntax_error([operator,expected,after,expression],S).

read([Token|RestTokens],Precedence,Term,LeftOver) :-
        read(Token,RestTokens,Precedence,Term,LeftOver).
read([],_X,_Y,_Z) :-
        syntax_error([expression,expected],[]).




