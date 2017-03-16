:- module(bu_databases, 
	[ reset/1,

          load_bibitem/1, bibitem/2, bibitem_fact/1, 
	  load_topic/1, topic/2, topic_fact/1,
	  load_url/1, url/3, url_fact/1, 

	  load_document/1, doc_option/2,  doc_command/3, doc_string/2, doc_fact/1, 

	  load_alias/2, alias_fact/1,

	  load_citeseer_entry/1, citeseer_entry_fact/1, 
          citeseer_entry/5, citeseer_total_entries/1, citeseer_alias/5,
	  load_citeseerx_entry/1, citeseerx_entry_fact/1, 
          citeseerx_entry/6, citeseerx_total_entries/2, citeseerx_alias/5,
	  load_core_entry/1, core_entry_fact/1, 
          core_conf_entry/3, core_jour_entry/2, core_alias/2, core_alias/5,
	  load_jcr_entry/1, jcr_entry_fact/1, 
          jcr_entry/5, jcr_total_entries/3, jcr_alias/2, jcr_alias/5	 
	], 
	[assertions, regtypes, dcg, hiord]).

:- comment(module, "This library interfaces with dynamically loaded
databases").

:- use_module(library(read), [read/2]).
:- use_module(.(bu_messages)).

:- discontiguous(reset/1).

%%% bibitem/2 %%%%


:- regtype bibitem_fact/1.
:- regtype bib_field/1.

:- regtype prolog_paper_fact_/1.
:- regtype bib_field_/1.
:- regtype int_char/1.

prolog_paper_fact_(prolog_paper(X, Y)):- 
	atom(X), 
	list(Y, bib_field_).

bib_field_([]).
bib_field_([author, X]):- list(X, string).
bib_field_([editor, X]):- list(X, string).
bib_field_([year, X]):- list(X, int_char).
bib_field_([beginpage, X]):- list(X, int_char).
bib_field_([endpage, X]):- list(X, int_char).
bib_field_([npages, X]):- list(X, int_char).
bib_field_([Y, X]):- 
	Y \== author, Y \== editor, 
	Y \== year, Y \== beginpage, Y\== endpage, Y \== npages, 
	string(X).


int_char(0'0).
int_char(0'1).
int_char(0'2).
int_char(0'3).
int_char(0'4).
int_char(0'5).
int_char(0'6).
int_char(0'7).
int_char(0'8).
int_char(0'9).

bibitem_fact(bibitem(X, Y)):-
	atom(X), 
	list(Y, bib_field).

bib_field([author, X]):- list(X, string).
bib_field([editor, X]):- list(X, string).
bib_field([projects, X]):- list(X, atom).
bib_field([butopics, X]):- list(X, atom).
bib_field([year, X]):- integer(X).
bib_field([beginpage, X]):- integer(X).
bib_field([endpage, X]):- integer(X).
bib_field([npages, X]):- integer(X).
bib_field([month, X]):- integer(X).
bib_field([Y, X]):- 
	Y \== author, Y \== editor, Y \== projects, Y \== butopics, 
	Y \== year, Y \== beginpage, Y\== endpage, Y \== npages, 
	string(X).

:- pred convert_bibitem(X, Y) : 
	prolog_paper_fact_(X) => bibitem_fact(Y).
convert_bibitem(prolog_paper(X, Y1), bibitem(X, Y2)):-
	convert_bib_fields(Y1, Y2).

:- pred convert_bib_fields(X, Y) : list(X, bib_field_) => list(Y, bib_field).
convert_bib_fields([], []).
convert_bib_fields([[]|T], S):-!, 
	convert_bib_fields(T, S).
convert_bib_fields([X|T], [Y|S]):-
	convert_bib_field(X, Y), 
	convert_bib_fields(T, S).

:- pred convert_bib_field(X, Y) : bib_field_(X) => bib_field(Y).
convert_bib_field([projects, X], [projects, Y]):-!,
	commas2list(Y, X, []).
convert_bib_field([butopics, X], [butopics, Y]):-!,
	commas2list(Y, X, []).
convert_bib_field([year, X], [year, Y]):-!,
	number_codes(Y, X).
convert_bib_field([beginpage, X], [beginpage, Y]):-!, 
	number_codes(Y, X).
convert_bib_field([endpage, X], [endpage, Y]):-!, 
	number_codes(Y, X).
convert_bib_field([npages, X], [npages, Y]):-!,
	number_codes(Y, X).
convert_bib_field([month, X], [month, Y]):-!,
	month_to_number(X, Y).
convert_bib_field(X, X).

spaces --> " ", !, spaces.
spaces --> "".

comma --> ",".

item_name([X])   -->  other(X).
item_name([X|R]) -->  other(X), item_name(R).

other(X) --> [X], { X \== 0' , X \== 0', }.

commas2list([Y])   --> spaces, item_name(X), {atom_codes(Y, X)}, spaces.
commas2list([Y|R]) --> spaces, item_name(X), {atom_codes(Y, X)}, spaces, comma, 
	                commas2list(R). 

month_to_number("January",1):- !.
month_to_number("jan",1):- !.
month_to_number("February",2):- !.
month_to_number("feb",2):- !.
month_to_number("March",3):- !.
month_to_number("mar",3):- !.
month_to_number("April",4):- !.
month_to_number("apr",4):- !.
month_to_number("May",5):- !.
month_to_number("may",5):- !.
month_to_number("June",6):- !.
month_to_number("jun",6):- !.
month_to_number("July",7):- !.
month_to_number("jul",7):- !.
month_to_number("August",8):- !.
month_to_number("aug",8):- !.
month_to_number("September",9):- !.
month_to_number("sep",9):- !.
month_to_number("October",10):- !.
month_to_number("oct",10):- !.
month_to_number("November",11):- !.
month_to_number("nov",11):- !.
month_to_number("December",12):- !.
month_to_number("dec",12):- !.
month_to_number(S, 0):-
	bu_messages:warning("Month ~s not recognized", [S]).

:- data bibitem/2.

reset(bibitem):-!,
	retractall_fact(bibitem(_, _)).

%:- pred load_prolog_paper(File) : atom(File).
load_bibitem(File):-
	load_term(File, convert_bibitem).
% 	bu_messages:message(2,"[Loading ~w...]~n",[File]),
% 	open(File, read, Stream), 
% 	load_bibitem_loop(Stream), 
% 	close(Stream).

load_bibitem_loop(Stream):-!, 
	read(Stream, Term),
	(
	    Term = end_of_file -> 
	    true
	;
	    Term = (:- _) -> 
	    load_bibitem_loop(Stream)
	;
	    Term = bibitem(X, Y1),
	    atom(X), 
	    list(Y1, bib_field_) ->
 	    convert_bib_fields(Y1, Y2),
	    assertz_fact(bibitem(X, Y2)),
	    load_bibitem_loop(Stream)
	;
	    throw(error(bad_type(Term), load_term/2))
	).
	


:- pred bibitem(X, Y) : atom(X) => list(Y, bib_field).

%%% topic/2 %%%

:- regtype topic_fact/1.
topic_fact(topic(X, Y)):- 
	atom(X), string(Y).

:- data topic/2.

reset(topic):-!,
	retractall_fact(topic(_, _)).

load_topic(File):-
	load_term(File, no_convert(topic_fact)).

:- pred topic(X, Y) : atom(X) => string(X).

%%% url_db/3 %%%

:- regtype url_fact/1.

url_fact(url_db(_X, _Y, _Z)).

:- data url_db/3.

reset(url_db):-!,
	retractall_fact(url_db(_, _, _)).

:- pred load_url(File) : atom(File).
load_url(File):-
	load_term(File, no_convert(url_fact)).

%:- pred url(A, B, C).
url(A, B, C):-
	url_db(A, B, C).


%%% document %%%

:- regtype doc_fact/1.

doc_fact(option(X, Y)):- atom(X), term(Y).
doc_fact(newcommand(X, Y, Z)):- string(X), string(Y), list(Z, string).
doc_fact(lpdoc(X)):- string(X).
doc_fact(raw(X)):- string(X).

:- data option/2.
:- data newcommand/3.
:- data doc_string/2.

reset(doc):-!, 
	retractall_fact(option(_, _)),
        retractall_fact(newcommand(_, _, _)),
	retractall_fact(doc_string(_, _)).
	

:- pred load_document(File) : atom(File).
load_document(File):-
	load_term(File, convert_document).

convert_document(lpdoc(String), doc_string(lpdoc, String)):- string(String), !.
convert_document(raw(String), doc_string(raw, String)):- string(String), !.
convert_document(X, Y):- no_convert(X, doc_fact, Y).

:- pred doc_option(X, Y) : atom(X) => term(Y).
doc_option(X, Y):-
	option(X, Y).

:- pred doc_command(X, Y, Z) : string(X) => (string(Y), list(Z, string)).
doc_command(X, Y, Z) :-
	newcommand(X, Y, Z).


%%% alias %%%

:- regtype db_type/1.

db_type(conf).
db_type(jour).

:- regtype alias_fact/1.

alias_fact(alias(RankingAcronym, RankingType, DbKey, DbType, _Name)):- 
	atom(RankingAcronym), db_type(RankingType), atom(DbKey), atom(DbType), 
	(var(Name);string(Name)).
alias_fact(alias(RankingAcronym, RankingType, na, _DbType, _Name)):- 
	atom(RankingAcronym), db_type(RankingType).
alias_fact(alias(RankingAcronym, RankingType, na, _DbType, _Name)):-
	atom(RankingAcronym), var(RankingType).

:- discontiguous(load_alias/2).

%%% citeseer_db %%%

:- regtype citeseer_entry_fact/1.

citeseer_entry_fact(citeseer_entry(Acronym, Type, Impact, Position, Name)) :-
	atom(Acronym), db_type(Type), number(Impact), integer(Position), string(Name).

:- regtype citeseer_entry_fact_/1.

citeseer_entry_fact_(X):- citeseer_entry_fact(X).
citeseer_entry_fact_(citeseer_total_entries(X)) :- integer(X).

reset(citeseer_db):-!,
	reset(citeseer_entry), 
	reset(citeseer_alias).

:- data citeseer_entry/5.
:- data citeseer_total_entries/1.

reset(citeseer_entry):-!, 
	retractall_fact(citeseer_entry(_, _, _, _, _)), 
	retractall_fact(citeseer_total_entries(_)).

load_citeseer_entry(File):-
	load_term(File, no_convert(citeseer_entry_fact_)).

:- data citeseer_alias/5.

reset(citeseer_alias):-!, 
	retractall_fact(citeseer_alias(_, _, _, _, _)).

convert_citeseer_alias(
	alias(RankingAcronym, RankingType, DbKey, DbType, Name), 
	citeseer_alias(RankingAcronym, RankingType, DbKey, DbType, Name)):-
 alias_fact(alias(RankingAcronym, RankingType, DbKey, DbType, Name)).


load_alias(citeseer, File):-!,
	load_term(File, convert_citeseer_alias).


%%% citeseerx_db %%%

:- regtype citeseerx_entry_fact/1.

citeseerx_entry_fact(citeseerx_entry(Acronym, Type, Year, Impact, Position, Name)) :-
	atom(Acronym), db_type(Type), integer(Year), number(Impact), integer(Position), string(Name).

:- regtype citeseerx_entry_fact_/1.

citeseerx_entry_fact_(X):- citeseerx_entry_fact(X).
citeseerx_entry_fact_(citeseerx_total_entries(Year, X)) :- integer(Year), integer(X).

reset(citeseerx_db):-!,
	reset(citeseerx_entry), 
	reset(citeseerx_alias).

:- data citeseerx_entry/6.
:- data citeseerx_total_entries/2.

reset(citeseerx_entry):-!, 
	retractall_fact(citeseerx_entry(_, _, _, _, _, _)), 
	retractall_fact(citeseerx_total_entries(_, _)).

load_citeseerx_entry(File):-
	load_term(File, no_convert(citeseerx_entry_fact_)).

:- data citeseerx_alias/5.

reset(citeseerx_alias):-!, 
	retractall_fact(citeseerx_alias(_, _, _, _, _)).

convert_citeseerx_alias(
	alias(RankingAcronym, RankingType, DbKey, DbType, Name), 
	citeseerx_alias(RankingAcronym, RankingType, DbKey, DbType, Name)):-
 alias_fact(alias(RankingAcronym, RankingType, DbKey, DbType, Name)).


load_alias(citeseerx, File):-!,
	load_term(File, convert_citeseerx_alias).

%%% core_db %%%

:- regtype core_entry_fact/1.

core_entry_fact(core_conf_entry(Acronym, Impact, Name)) :-
	atom(Acronym), atom(Impact), string(Name).
core_entry_fact(core_jour_entry(Acronym, Impact)) :-
	atom(Acronym), atom(Impact).

reset(core_db):-!, 
	reset(core_entry), 
	reset(core_alias).

:- data core_conf_entry/3.
:- data core_jour_entry/2.
:- data core_alias/2.

reset(core_entry):-!, 
	retractall_fact(core_conf_entry(_, _, _)), 
	retractall_fact(core_jour_entry(_, _)), 
	retractall_fact(core_alias(_, _)).

convert_core_fact(alias(X, Y), core_alias(X, Y)):- atom(X), atom(Y).
convert_core_fact(X, X):- core_entry_fact(X).

load_core_entry(File):-
	load_term(File, convert_core_fact).

:- data core_alias/5.

reset(core_alias):-!, 
	retractall_fact(core_alias(_, _, _, _, _)).

convert_core_alias(
	alias(RankingAcronym, RankingType, DbKey, DbType, Name), 
	core_alias(RankingAcronym, RankingType, DbKey, DbType, Name)):-
 alias_fact(alias(RankingAcronym, RankingType, DbKey, DbType, Name)).


load_alias(core, File):-!,
	load_term(File, convert_core_alias).


%%% jcr_db %%%

:- regtype jcr_entry_fact/1.

jcr_entry_fact(jcr_entry(Acronym, Year, Impact, Position, Subject)) :-
	atom(Acronym), integer(Year), number(Impact), integer(Position), atom(Subject).

convert_jcr_fact(alias(X, Y), jcr_alias(X, Y)):- atom(X), atom(Y).
convert_jcr_fact(jcr_total_entries(Subject, Year, X), jcr_total_entries(Subject, Year, X)):-
	atom(Subject), integer(Year), integer(X).
convert_jcr_fact(X, X):- jcr_entry_fact(X).

reset(jcr_db):- !,
	reset(jcr_entry), reset(jcr_alias).

:- data jcr_entry/5.
:- data jcr_total_entries/3.
:- data jcr_alias/2.

reset(jcr_entry):-!, 
	retractall_fact(jcr_entry(_, _, _, _, _)), 
	retractall_fact(jcr_total_entries(_, _, _)), 
	retractall_fact(jcr_alias(_, _)).

load_jcr_entry(File):-
	load_term(File, convert_jcr_fact).

:- data jcr_alias/5.

reset(jcr_alias):-!, 
	retractall_fact(jcr_alias(_, _, _, _, _)).

convert_jcr_alias(
	alias(RankingAcronym, RankingType, DbKey, DbType, Name), 
	jcr_alias(RankingAcronym, RankingType, DbKey, DbType, Name)):-
 alias_fact(alias(RankingAcronym, RankingType, DbKey, DbType, Name)).


load_alias(jcr, File):-!,
	load_term(File, convert_jcr_alias).



%%% Laoding predicates %%%%

reset(X):- 
	throw(error(unkown_database(X), reset/1-1)).

%:- meta_predicate(no_convert(pred(1), ?, ?)).

no_convert(X, Type, X) :-
	Type(X).

:- meta_predicate(load_term(?, pred(2))).
:- meta_predicate(load_term_loop(?, pred(2))).

load_term(File, Type):- 
	bu_messages:note("Loading ~w...",[File]),
	open(File, read, Stream), 
	load_term_loop(Stream, Type).

load_term_loop(Stream, Type):-!, 
	repeat,
	read(Stream, Input),
%	bu_messages:error("~w:~w", [Input, Type]),
	(
	    Input = end_of_file -> 
	    close(Stream)
	;
	    Input = (:- include(File)), 
	    load_term(File, Type), 
	    fail
	;
	    Input = (:- _) -> 
	    fail
	;
	    Type(Input, Output) ->
	    assertz_fact(Output), 
	    fail
	;
	    throw(error(bad_type(Input), load_term/2))
	), !.

