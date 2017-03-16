:- module(api_internal_types,
	    [
		t_head/1, t_body/1, t_literal/1, t_body_wo_ppi/1,
		t_literal_wo_ppi/1, t_lit_ppi_id/1, t_cls_ppi_id/1, t_cls_key/1,
		t_pred_ppi_id/1, t_clause/1, t_clause_wo_ppi/1, t_acls/1,
		t_cls_key/1, t_ppi_id/1, clid2data/4, litid2data/5, pred2data/3,
		t_cls/1, t_direc/1, t_as/1, t_loc/1, t_pred_type_selector/1,
		t_lit/1, t_comment/1, t_acomm/1, atom_or_str/1],
	    [assertions, api(api_internal_dec), regtypes]).

:- use_module(library(lists),     [append/3]).
:- use_module(program(clidtypes), [bodykey/1, clid/1, predid/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   REGTYPES     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% :- regtype assert_body_type/1.

% assert_body_type(  X  ) :- list( X , assert_body_type__ ).


% assert_body_type__( A ) :-
% 	A = ( _ ; _ ),
% 	!,
% 	abt_only_disj( A ).

% assert_body_type__( _ ).



% abt_only_disj( (A;B) ) :-
% 	!,
% 	list(A),
% 	abt_only_disj__2( B ).



% abt_only_disj__2( (A;B) ) :-
% 	!,
% 	list(A),
% 	abt_only_disj__2( B ).

% abt_only_disj__2( A ) :- list( A ).



% :- regtype conj_disj_type/1

% # "The usual prolog way of writting conjuntions and disjuntions in a
%   body using ',' and ';'".

% % conjuntion
% conj_disj_type( ( _ , B) ) :-
% 	conj_disj_type( B ).

% % disjuntion
% conj_disj_type( ( _ ; B) ) :-
% 	conj_disj_type( B ).

% % a goal
% conj_disj_type( _ ).


% :- regtype t_conj/1

% # "Conjuntions.".

% % conjuntion
% t_conj( ( _ , B) ) :-
% 	t_conj( B ).

% % a goal
% t_conj( _ ).


% :- regtype t_disj/1

% # "Conjuntions.".

% % conjuntion
% t_disj( ( _ , B) ) :-
% 	t_disj( B ).

% % a goal
% t_disj( _ ).

:- regtype t_head/1.

t_head(B) :-
	nonvar(B).

:- regtype t_body/1.

t_body((L1;L2)) :-
%	!,
	t_literal(L1),
	t_literal(L2).

t_body((L1, L2)) :-
%	!,
	t_literal(L1),
	t_literal(L2).

t_body(L) :-
	t_literal(L).

t_body(0). % for directives

:- regtype t_body_wo_ppi/1 #
	"Type body with no program point info id.".

t_body_wo_ppi((L1;L2)) :-
%	!,
	t_literal_wo_ppi(L1),
	t_literal_wo_ppi(L2).

t_body_wo_ppi((L1, L2)) :-
%	!,
	t_literal_wo_ppi(L1),
	t_literal_wo_ppi(L2).

t_body_wo_ppi(L) :-
	t_literal_wo_ppi(L).

t_body_wo_ppi(0). % for directives

:- regtype t_literal/1 #
	"Type literal. It _has_ to have program point info id".

t_literal(_:B) :-
%nonvar( A ) % ???
	t_lit_ppi_id(B).

:- regtype t_literal_wo_ppi/1
# "Type literal with no program point info id.".

t_literal_wo_ppi(B) :-
	term(B).

:- regtype t_lit_ppi_id/1
# "Literal Program Point Info ID.".

t_lit_ppi_id(B) :-
	atom(B),
	litid2data(B, _, _, _, _).

:- regtype t_cls_ppi_id/1 # "Type program point info clause id.".

t_cls_ppi_id(B) :-
	atom(B),
	clid2data(B, _, _, _).

:- regtype t_cls_key/1 # "Clause Key type. It has to be something like
qsort(_,_). It can be used to obtains assertions.".

t_cls_key(_).

:- regtype t_pred_ppi_id/1 # "Type program point info predicate id.".

t_pred_ppi_id(B) :-
	atom(B),
	pred2data(B, _, _).

:- regtype t_clause/1 #
	"Type clause. Literals have program point info id.".

t_clause(clause(B, H)) :-
	t_head(B),
	t_body(H).

:- regtype t_clause_wo_ppi/1 #
	"Same as t_clause but without program point info id in literals.".

t_clause_wo_ppi(clause(B, H)) :-
	t_head(B),
	t_body_wo_ppi(H).

:- regtype t_acls/1
# "It specify the possible terms that can appear for modifiying the
  current clause/assertions database. The prossibles values a/1, e/1
  and u/1 are used to indicate @em{add}, @em{erase} and @em{update}
  resepectively. If a(Action,Whatever) (or u(Action,Whatever)) is
  used, Action can take the values @tt{begin}, @tt{end}, @tt{before/1}
  and @tt{after/1}. The @tt{before/1} and @tt{after/1} takes a
  @tt{predicate key} as argument. @tt{before_clause/1} and
  @tt{after_clause/1} are been discussed to be added.".

t_acls(a(_)).
t_acls(a(Action, _)) :-
	member(Action, [begin, end, before(_), after(_)]).
t_acls(e(_)).
t_acls(u(_)).

:- regtype t_acomm/1
# "Defines the possible actions when adding a comment. There are
  similar to the ones defines in @pred{t_acls}.".

t_acomm(a(_)).
t_acomm(a(Action, _)) :-
	member(Action, [begin, end, before(_), after(_)]).

:- regtype t_ppi_id(X) # "Program Point ID Type".

t_ppi_id(X) :- t_lit_ppi_id(X).
t_ppi_id(X) :- t_cls_ppi_id(X).

:- regtype atom_or_str/1 # "Atom or String".

atom_or_str(X) :- atom(X).
atom_or_str(X) :- string(X).


:- regtype t_cls/1 #
"Clause Type. 
@begin{itemize}

@item ref. This is an internal field. PLEASE do not use it. It
contains the reference got from @pred{asserta_fact/1}or
@pred{current_fact/1}.

@item id. This the program point id. Analysis information uses it as
key.

@item key. This is the @tt{key} needed in predicates that has
something to do with assertions.

@item head. Just a term like @tt{'qsort:qsort'(_,_)}. Note that the
functor is module-expanded.

@item body. A list of literals in the form (A,B,C,D).

@item dic. A dictionary from vndict.

@item locator. Look at @tt{loc} type.
@end{itemize}
".

t_cls(cls(_1, _2, _3, _4, _5, _6, _7)).

:- regtype t_direc/1 # "Directive Type".

t_direc(direc(_1, _2, _3, _4, _5)).

:- regtype t_as/1 # "Assertion Type".

t_as(as${}).

:- regtype t_loc/1 # "Locator Type".

t_loc(loc${}).

:- regtype t_pred_type_selector/1 # "Type of Predicate Selector Type".

t_pred_type_selector(pred_type_selector${}).

:- regtype t_lit/1 # "Literal type".

t_lit(lit${}).

:- regtype t_comment/1.

t_comment(comment${}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   PPI ID definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred litid2data(AtId, F, A, C, G) :: bodykey * atom * num * num * num
# "@var{AtId} identifies the @var{G}th literal of the body of the
	  @var{C}th clause of predicate @var{F}/@var{A}.".

litid2data(Atom, N, A, C, G) :-
	unpack_id(Atom, List),
	append(Name, [LA, LC, LG], List),
	unpack_id(N, Name),
	name(A, LA),
	name(C, LC),
	name(G, LG).

:- pred clid2data(ClId, F, A, C) :: clid * atom * num * num
# "@var{ClId} identifies the @var{C}th clause of predicate
	  @var{F}/@var{A}.".

clid2data(Clid, N, A, C) :-
	unpack_id(Clid, List),
	append(Name, [LA, LC], List),
	unpack_id(N, Name),
	name(A, LA),
	name(C, LC).

:- pred pred2data(PredId, F, A) :: predid * atom * num
# "@var{PredId} identifies the predicate @var{F}/@var{A}.".

pred2data(Atom, N, A) :-
	atom(Atom),
	!,
	unpack_id(Atom, List),
	append(Name, [LA], List),
	unpack_id(N, Name),
	name(A, LA).
pred2data(Atom, N, A) :-
	unpack_id(N, Name),
	name(A, LA),
	append(Name, [LA], List),
	unpack_id(Atom, List).

:- pred unpack_id(Atom, IdList) :: atom * list(list(string))
# "Given an atom @var{String} (which contains 0'/ characters) it
returns a list of substrings of @var{String} contained between 0'/
characters.".

% ALMA MATTER :D
% An atom is given...
unpack_id(A, B) :-
	atom(A),
	atom_codes(A, AA),
	unpack_id_(AA, B),
	!.
% The atom is the output
unpack_id(A, B) :-
	unpack_id_(AA, B),
	!,
	atom_codes(A, AA).

unpack_id_([0'/|R], [[]|L]) :-
	unpack_id_(R, L).
unpack_id_([A|R], [[A|AA]|As]) :-
	unpack_id_(R, [AA|As]).
unpack_id_([], [[]]).

/*
:- pred make_atom(AtomList, Id) : list(atom) * atom
# "@var{Id} is the atom obtained from contatenating each element of
@var{AtomList} with '/' between elements.".

make_atom(AtomList, Key) :-
	namel_2_atoml(AtomList, StrList),
	unpack_id(Key, StrList).

namel_2_atoml([],     []).
namel_2_atoml([A|AA], [S|SS]) :-
	name(A, S),
	namel_2_atoml(AA, SS).
*/
