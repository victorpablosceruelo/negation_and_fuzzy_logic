:- module(_, _, [assertions, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

:- use_module(library(iso_misc)).

delta_max_number_of_unifs(LitInfo, Size) :-
	litinfo_get_lit(LitInfo, Head),
	term_size(Head, Size).

% 	Head = 'color_map:color_map'( _, _, _, _, _ ),
% 	!,
% 	term_size( Head, Size ).
% delta_max_number_of_unifs( _LitInfo, 0 ).

term_size(Term, 1) :-
	var(Term),
	!.
term_size(Term, 1) :-
	atomic(Term),
	!.
term_size(Term, Size) :-
	compound(Term),
	functor(Term, _, N),
	term_size_(N, Term, Size1),
	Size is Size1 + 1.

% :- test term_size_( A, B, C ) :( A = 3, B = p( a, b, c( d ) ) ) => C = 5.
% :- test term_size_( A, B, C ) :( A = 3, B = p( a, b, c( d, e, f ) ) ) => C = 7.

term_size_(0, _, 0) :-
	!.
term_size_(N, Term, Size) :-
	N > 0,
	arg(N, Term, Arg),
	term_size(Arg, Size1),
	N1 is N -1,
	term_size_(N1, Term, Size2),
	Size is Size1 + Size2.

delta_calls_to_builtins(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Head),
	Head = 'color_map:neq'(_, _),
	!.
delta_calls_to_builtins(_LitInfo, 0) :-
	!.
