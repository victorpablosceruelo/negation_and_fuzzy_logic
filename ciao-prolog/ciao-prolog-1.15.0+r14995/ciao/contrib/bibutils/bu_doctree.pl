:- module(bu_doctree, [from_term/2, pretty_print/3, doctree_substitute/4, parse_docstring/2],
	[ciaopaths, dcg, fsyntax]).

:- include(lpdocsrc('Manifest')). % TODO: This file should not be included -JF
:- use_module(lpdocsrc(src(autodoc_parse)), [parse_docstring/3]).
:- use_module(lpdocsrc(src(autodoc_bibrefs)), [parse_commands/3]).

:- use_module(bu_options, [get/2]).
:- use_module(bu_databases, [doc_command/3]).
:- use_module(bu_messages, [note/2, warning/2, error/2]).
:- use_module(bu_i18n).
:- use_module(library(hiordlib), [map/3]).

parse_docstring(DocStr, DocTree) :-
	autodoc_parse:parse_docstring(_, DocStr, DocTree). 
parse_simpleTeX(TeXStr, DocTree):-
	autodoc_bibrefs:parse_commands(DocStr, TeXStr, []), 
	autodoc_parse:parse_docstring(_, DocStr, DocTree). 

from_term(X) := ~translate(X, ~(bu_options:get(lang))).

translate([], _Lang) := 
	[] :- !.
translate([H|T], Lang) := 
	[~translate(H, Lang)|~translate(T, Lang)] :- !.

translate(butype_description(BType), Lang ) := 
	~parse_docstring(~ (bu_i18n:butype_description(BType,  Lang))) :-!.
translate(env(section(Title1), Content1), Lang) := 
 	section_env([level(1)], 
	            [local_label(_)], 
		    ~translate(Title1, Lang), 
		    ~translate(Content1, Lang)) :-!.
translate(env(subsection(Title1), Content1), Lang) := 
 	section_env([level(2)], 
	            [local_label(_)], 
		    ~translate(Title1, Lang), 
		    ~translate(Content1, Lang)) :-!.
translate(env(href(URL), Content1), Lang) :=
	href(URL, ~translate(Content1, Lang)) :-!.
translate(env(item, Content1), Lang) :=
	item(~translate(Content1, Lang)).
translate(env(Env, Content1), Lang) :=
	env_(Env, ~translate(Content1, Lang)):-!.
translate(misc(Misc, Args), Lang) := 
	~substitute(~parse_docstring(DocStr), Vars, ~map(Args, translate(Lang)), _) 
 :- !,
	(
	    bu_i18n:misc_text(Misc, Lang, DocStr, Vars) ->
	    true
	;
	    throw(interal_error(misc(Misc, Args), translate/4))
	).
translate(month(N), Lang) :=
	~parse_docstring(~(bu_i18n:month_to_lang(N, Lang))) :- !.
translate(number(N), _Lang) :=
	string_esc(~number_codes(N)) :-!.
translate(number(N, D), _Lang) :=
	string_esc(~round_number_codes(~number_codes(N), D)) :-!.
translate(atom(N), _Lang) :=
	string_esc(~atom_codes(N)).
translate(list([], _), _Lang) := [] :-!.
translate(list([H|T], atom), Lang) := 
	[~translate(atom(H), Lang) | ~translate(list(T, atom), Lang) ] :-!.
translate(ranking_explanation, Lang) := 
	~substitute(~parse_docstring(DocStr), Vars, 
	        [string_esc("33"), string_esc("66"), string_esc("77"), 
		string_esc("mediana"), 
		string_esc("33"), string_esc("66")], _) 
 :-!,	
		misc_text(ranking_explanation, Lang, DocStr, Vars).
translate(tex_string(TeXStr), _Lang) :=
	~parse_simpleTeX(TeXStr) :- !.
translate(bf(TeXStr), Lang) :=
	bf(~translate(TeXStr, Lang)) :- !.
translate(em(TeXStr), Lang) :=
	em(~translate(TeXStr, Lang)) :- !.

translate(nl, _Lang) := p([]).
translate(space, _Lang) := 
	string_esc(" ") :-!.
translate(comma, _Lang)  := 
	string_esc(", ") :-!.
translate(point, _Lang) := 
	string_esc(". ") :-!.
translate(author, _Lang) :=
	string_esc(~atom_codes(~(bu_options:get(author_string)))).

translate(X, _Lang) := X.
 

substitute([H|T], Args, Values, F) := 
	[ ~substitute(H, Args, Values, F)  |
	  ~substitute(T, Args, Values, F) ] :- !.
substitute(X, Vars, Values, F) := R :-
	X = var([string_esc(Var)]), !,
%	(
%	    Var = "html" || _ ->
%	    bu_messages:error(">> ~w", [Var])
%	; 
%	    true
%	),
	(
	    substitue_(Vars, Var, Values, R)  ->
%	    bu_messages:error(">> 1", []),
	    F = true
	;
	    doc_command(Var, DocStr, []) -> 
%	    bu_messages:error(">> 2", []),
	    parse_docstring(DocStr, R), 
	    F = true
	;
	    % X = error(substitute(var([string_esc(Var)]), Vars, Values))
	    R = X 
	).
substitute(bf(Content1), Vars, Values, F):= 
	bf(~substitute(Content1, Vars, Values, F)).
substitute(em(Content1), Vars, Values, F):= 
	em(~substitute(Content1, Vars, Values, F)).
substitute(X, _, _, _) := X.

substitue_([Var|_], Var, [Value|_]) := Value :-!.
substitue_([_|Vars], Var, [_|Values]) := ~substitue_(Vars, Var, Values).


round_number_codes([], _) := [].
round_number_codes([0'.|T], N) := [0'.|~round_float_part(T, N)] :-!.
round_number_codes([C|T], N) := [C|~round_number_codes(T, N)].

round_float_part(_, 0) := [] :-!.
round_float_part([], N) := [0'0| ~round_float_part([], M)] :-
	!, N > 0,  M is N -1.
round_float_part([H|T], N) := [H| ~round_float_part(T, M)] :-
	!, N > 0,  M is N -1.

  

:- use_module(library(format), [format/3]).

pretty_print([Head|Tail], N, Stream):-!, 
	M is N + 2, 
	format(Stream, "[~n~*c", [M, 0' ]), 
	pretty_print(Head, M, Stream),
	pretty_print_tail(Tail, M, Stream), 
	format(Stream, "~n~*c]", [N, 0' ]).
pretty_print(section_env(Options, Label, Title, Content), N, Stream) :- !,
	M is N + 2, 
	format(Stream, "section_env(~w, ~w, ", [Options, Label]), 
	pretty_print(Title, M, Stream),
	format(Stream, ", ", []),
	pretty_print(Content, M, Stream),
	format(Stream, ")", []).
pretty_print(env_(Env, Content), N, Stream) :- !, 
	M is N + 2, 
	format(Stream, "env_(", []), 
	pretty_print(Env, M, Stream),
	format(Stream, ", ", []),
	pretty_print(Content, M, Stream),
	format(Stream, ")", []).
pretty_print(item(C), N, Stream):-
	pretty_print_1(item, C, N, Stream).
pretty_print(string_esc(S), _N, Stream):-!,
	format(Stream, "string_esc(\"~s\")", [S]).
pretty_print(href(URL, Content), N, Stream):-!, M is N + 2, 
	format(Stream, "href(~s, ", [URL]), 
	pretty_print(Content, M, Stream),
	format(Stream, ")", []).
pretty_print(S, _N, Stream):-!,
	format(Stream, "~w", S).

pretty_print_1(Env, Content, N, Stream):-
	M is N + 2, 
	format(Stream, "~a(", [Env]), 
	pretty_print(Content, M, Stream),
	format(Stream, ")", []).


pretty_print_tail([], _N, _Stream).
pretty_print_tail([Head|Tail], N, Stream):-
	format(Stream, ",~n~*c", [N, 0' ]),
	pretty_print(Head, N, Stream),
	pretty_print_tail(Tail, N, Stream).

doctree_substitute(DocTree, Var, Value, Result) :-
%	bu_messages:error(">> doctree", []),
	substitute_fixpoint(DocTree, Var, Value,  Result).

substitute_fixpoint(DocTree, Var, Value, Result):-
	substitute(DocTree, Var, Value, F, Result_), 
	(
	    var(F) ->
	    Result_ = Result
	;
	    substitute_fixpoint(Result_, Var, Value, Result)
	).
