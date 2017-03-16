:- module(bu_ranking, 
	[ get_acronym/2,
	  publication_type/2,
	  ranks/3
	  ], [assertions]).

:- use_module(library(bibutils(ranking(bu_ranking_db))), 
	[ pub_all_ranks/3 ]).
:- reexport(library(bibutils(ranking(bu_ranking_db))), 
	[ init/0, current_levels_mapping/1, ranks_doctree/4]).
:- use_module(library(bibutils(ranking(bu_ranking_acronym))), 
	[ranking_acronym/2]).

:- use_module(library(bibutils(bu_support))).
:- use_module(library(bibutils(bu_messages)), [error/2, warning/2]).

%%% SUPPORT FOR RANKING 

% get_acronym/2 fails if the entry has not to be ranked.
get_acronym(List, Acronym) :-
	member([butype,Type],List),
	butype_venue_field(Type,Field),
	(
	    member([buconfj, A0], List), 
	    atom_codes(Acronym, A0), !
	;
	    member([Field, A], List),		 
	    bu_support:fix_name(A,A0),
	    one_ranking_acronym(Acronym,A0), !
	).
one_ranking_acronym(Acronym,A) :-
	bu_ranking_acronym:ranking_acronym(Acronym,A),
	!.
one_ranking_acronym(venue_not_in_db,A) :-
	bu_messages:warning("Acronym not found for \"~s\".", [A]).

%% These are treated via the rankings, the rest are treated by butype:
butype_venue_field("article",journal)         :- !.
butype_venue_field("inproceedings",booktitle) :- !.

% butype_venue_field("invited",booktitle)       :- !.
% butype_venue_field("incollection",booktitle)  :- !.
% butype_venue_field("workshop",booktitle)      :- !.

:- discontiguous(non_ranked_butype/1).
:- discontiguous(publication_type/2).

%% These have to be listed separately (they are non-refereed/no rankings):
non_ranked_butype("book").            % All these listed under butype book
publication_type("book","book")             :- !.
non_ranked_butype("editedbook").
publication_type("editedbook","book")       :- !.
non_ranked_butype("mastersthesis").
publication_type("mastersthesis","book")    :- !.
non_ranked_butype("phdthesis").
publication_type("phdthesis","book")        :- !.
non_ranked_butype("proceedings").
publication_type("proceedings","book")      :- !.

non_ranked_butype("invited").
non_ranked_butype("invitedtalk").
non_ranked_butype("incollection").
non_ranked_butype("workshop").

non_ranked_butype("techreport").      % All these listed under butype techreport
publication_type("techreport","techreport") :- !.
non_ranked_butype("manual").          
publication_type("manual","techreport")     :- !.

non_ranked_butype("misc").
non_ranked_butype("unpublished").

publication_type(X,X)                       :- !.

ranks(venue_not_in_db, _Type, 3) :-
	!.
ranks(Acronym, Type, Rank) :-
	(
 	    Type = "inproceedings" -> 
 	    bu_ranking_db:pub_all_ranks(Acronym, conf, Rank)
 	;
 	    Type = "article" ->
 	    bu_ranking_db:pub_all_ranks(Acronym, jour, Rank)
 	;
 	    bu_ranking_db:pub_all_ranks(Acronym, _, Rank)   
 	),!.
ranks(Acronym, _Type, 3) :- 
	bu_messages:warning("No ranking available for \"~w\",", [Acronym]).


